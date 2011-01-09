package parser

import java.io.File
import scala.io.Source
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.CharArrayReader.EofCh

import tree.asm._

private trait ExtraTokens extends Tokens {
  case class Reg(chars: String) extends Token {
    override def toString = "$" + chars
  }
  case class SpecialReg(chars: String) extends Token {
    override def toString = chars
  }
}

private[parser] class SpecialLexical extends StdLexical with ExtraTokens {
  override def token: Parser[Token] =
    (registerToken
      | identChar ~ rep(identCharSecond) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | digit ~ rep(digit) ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
      | elem('#') ~> (hex ~ hex ^^ { case a ~ b => "" + a + b } | hex ^^ { _.toString }) ^^ { s => NumericLit(Integer.parseInt(s, 16).toString) }
      | ('\'' ~> rep(chrExcept('\'', '\n', EofCh)) <~ '\''
        | '\"' ~> rep(chrExcept('\"', '\n', EofCh)) <~ '\"') ^^ { case chars => StringLit(chars mkString "") }
      | EofCh ^^^ EOF
      | '\'' ~> failure("unclosed string literal")
      | '\"' ~> failure("unclosed string literal")
      | delim
      | failure("illegal character"))

  override def identChar = super.identChar | elem('.')
  def identCharSecond = identChar | digit | elem(':')
  def hex = digit | elem('a') | elem('b') | elem('c') | elem('d') | elem('e') | elem('f')

  def registerToken: Parser[Token] =
    elem('$') ~> digit ~ rep(digit) ^^ { case first ~ last => Reg(first :: last mkString "") }
}

object Parser extends StandardTokenParsers {
  override val lexical = new SpecialLexical

  /** A parser which matches a register literal */
  def reg: Parser[Register] =
    elem("register literal", _.isInstanceOf[ExtraTokens#Reg]) ^^ (t => Register(t.chars.toInt))

  lexical.delimiters += ("!", "@", "#", "=", ",")
  lexical.reserved += (".data", ".global", ".p2align", ".section", ".rodata", ".text")

  private def specialRegisterNames = ('A' to 'Z' map { "r" + _ }) ++ (List('B', 'T', 'W', 'X', 'Y', 'Z') map { c => "r" + c + c })
  lexical.reserved ++= specialRegisterNames
  lexical.reserved += ("ADDU")
  lexical.reserved += ("BYTE", "BZ")
  lexical.reserved += ("CMP", "CSN", "CSNN")
  lexical.reserved += ("DIVU")
  lexical.reserved += ("GET", "GETA")
  lexical.reserved += ("IS")
  lexical.reserved += ("JMP")
  lexical.reserved += ("LDT", "LOC")
  lexical.reserved += ("NEGU")
  lexical.reserved += ("PBN", "PBNZ", "PBP", "POP", "PUSHGO", "PUSHJ", "PUT")
  lexical.reserved += ("SET", "SETL", "SLU", "SR", "STTU", "SUBU")

  // Junk
  lexical.reserved += ("Data_section")
  lexical.delimiters += ("@+(4-@)&3")

  val specialRegister = (specialRegisterNames map { _ ^^ { name => SpecialRegister.withName(name) } }).reduceLeft(_ | _)

  def lineNumberInfo = "#" ~> numericLit ~ stringLit ^^^ Noop
  def bang = "!" ~ ident ~ "=" ~ numericLit ~ ident ~ "LOC" ~ ident ^^^ Noop // ! mmixal:= 8H LOC Data_Section
  def data = ".data" ~ "!" ~ ident ~ "=" ~ numericLit ~ ident ~ "LOC" ~ numericLit ~ ident ^^^ Noop // .data ! mmixal:= 8H LOC 9B
  def global = ".global" ~ ident ^^ { case _ ~ name => Global(Label(name)) }
  def label = ident ~ "IS" ~ "@" ^^ { case name ~ _ ~ _ => Label(name) }
  def loc = "LOC" ~ "@+(4-@)&3" ^^^ Noop
  def p2align = ".p2align" ~ numericLit ^^^ Noop
  def section = ".section" ~ ".rodata" ^^^ Noop
  def text = ".text" ~ "!" ~ ident ~ "=" ~ numericLit ~ ident ~ "LOC" ~ numericLit ~ ident ^^^ Noop

  def pre: Parser[ASM] = lineNumberInfo | bang | data | global | label | loc | p2align | section | text

  private def rl[T](f: (Register, Label) => T) = reg ~ "," ~ ident ^^ { case a ~ _ ~ l => f(a, Label(l)) }
  private def rn[T](f: (Register, Int) => T) = reg ~ "," ~ numericLit ^^ { case a ~ _ ~ n => f(a, n.toInt) }
  private def rr[T](f: (Register, Register) => T) = reg ~ "," ~ reg ^^ { case a ~ _ ~ b => f(a, b) }
  private def rnr[T](f: (Register, Int, Register) => T) = reg ~ "," ~ numericLit ~ "," ~ reg ^^ { case a ~ _ ~ b ~ _ ~ c => f(a, b.toInt, c) }
  private def rrn[T](f: (Register, Register, Int) => T) = reg ~ "," ~ reg ~ "," ~ numericLit ^^ { case a ~ _ ~ b ~ _ ~ n => f(a, b, n.toInt) }
  private def rrr[T](f: (Register, Register, Register) => T) = reg ~ "," ~ reg ~ "," ~ reg ^^ { case a ~ _ ~ b ~ _ ~ c => f(a, b, c) }

  def addu = "ADDU" ~> (rrr(ADDU) | rrn(ADDUI))
  def byte = "BYTE" ~>
    ((stringLit ^^ { s => List(s.getBytes("ASCII"): _*) }
      | numericLit ^^ { num => List(num.toByte) }) * ("," ^^^ { (a: List[Byte], b: List[Byte]) => a ::: b }) ^^ { l => BYTE(l) })
  def bz = "BZ" ~> rl(BZ)
  def cmp = "CMP" ~> (rrr(CMP) | rrn(CMPI))
  def csn = "CSN" ~> rrr(CSN)
  def csnn = "CSNN" ~> rrr(CSNN)
  def divu = "DIVU" ~> rrr(DIVU)
  def get = "GET" ~> reg ~ "," ~ specialRegister ^^ { case dest ~ _ ~ src => GET(dest, src) }
  def geta = "GETA" ~> rl(GETA)
  def jmp = "JMP" ~> ident ^^ { label => JMP(Label(label)) }
  def ldt = "LDT" ~> (rrr(LDT) | rrn(LDTI))
  def negu = "NEGU" ~> rnr(NEGU)
  def pbn = "PBN" ~> rl(PBN)
  def pbnz = "PBNZ" ~> rl(PBNZ)
  def pbp = "PBP" ~> rl(PBP)
  def pop = "POP" ~> numericLit ~ "," ~ numericLit ^^ {
    case a ~ _ ~ b =>
      if (b.toInt != 0) throw new IllegalArgumentException("POP can only be called with a 0 offset")
      POP(Register(a.toInt))
  }
  def pushgo = "PUSHGO" ~> rrn { (reg, dest, offset) =>
    if (offset != 0) throw new IllegalArgumentException("PUSHGO can only be called with a 0 offset")
    PUSHGO(reg, dest)
  }
  def pushj = "PUSHJ" ~> rl(PUSHJ)
  def put = "PUT" ~> specialRegister ~ "," ~ reg ^^ { case dest ~ _ ~ src => PUT(dest, src) }
  def set = "SET" ~> rr(SET)
  def setl = "SETL" ~> rn((dest, n) => SETL(dest, n.toShort))
  def slu = "SLU" ~> rrr(SLU)
  def slui = "SLU" ~> rrn(SLUI)
  def sr = "SR" ~> (rrr(SR) | rrn(SRI))
  def sttu = "STTU" ~> (rrr(STTU) | rrn(STTUI))
  def subu = "SUBU" ~> (rrr(SUBU) | rrn(SUBUI))
  def asm: Parser[ASM] =
    addu | byte | bz | cmp | csn | csnn | divu | get | geta | jmp | ldt | negu | pbn | pbnz | pbp | pop | pushgo | pushj | put | set | setl | slu | slui | sr | sttu | subu

  val top = asm | pre

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(top)(tokens)
  }

  def apply(s: String): ASM = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess => throw new IllegalArgumentException("Bad syntax: " + s)
    }
  }

  def apply(file: File): List[ASM] = {
    Source.fromFile(file, "ASCII").getLines.zipWithIndex filter (!_._1.trim.isEmpty) map {
      case (line, lineNum) =>
        parse(line) match {
          case Success(tree, _) => tree
          case e: NoSuccess =>
            throw new IllegalArgumentException("Bad syntax at line: " + (lineNum + 1) + "\n" + line)
        }
    } filter (_ != Noop) toList
  }
}

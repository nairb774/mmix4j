package parser

import org.specs._
import org.specs.runner.JUnit4
import tree.asm._

class ParserTest extends JUnit4(ParserSpec)
object ParserSpec extends Specification {
  private def parse(str: String, expect: ASM) = "parse: " + str >> { Parser(str) mustEqual expect }
  "line note" should {
    parse("""# 5 "main.cc"""", Noop)
  }
  "!" should {
    parse("! mmixal:= 8H LOC Data_Section", Noop)
  }
  ".data" should {
    parse(".data ! mmixal:= 8H LOC 9B", Noop)
  }
  ".global" should {
    parse(".global cpIntArray", Global(Label("cpIntArray")))
    parse(".global getIntArray", Global(Label("getIntArray")))
    parse(".global main", Global(Label("main")))
    parse(".global printIntArray", Global(Label("printIntArray")))
  }
  ".p2align" should {
    parse(".p2align 2", Noop)
  }
  ".section" should {
    parse(".section .rodata", Noop)
  }
  ".text" should {
    parse(".text ! mmixal:= 9H LOC 8B", Noop)
  }
  "label decleration" should {
    parse("cpIntArray IS @", Label("cpIntArray"))
    parse("getIntArray IS @", Label("getIntArray"))
    parse("main IS @", Label("main"))
    parse("printIntArray IS @", Label("printIntArray"))
    parse("L:2 IS @", Label("L:2"))
    parse("L:3 IS @", Label("L:3"))
    parse("L:7 IS @", Label("L:7"))
    parse("L:8 IS @", Label("L:8"))
    parse("L:11 IS @", Label("L:11"))
    parse("L:15 IS @", Label("L:15"))
    parse("L:16 IS @", Label("L:16"))
    parse("LC:0 IS @", Label("LC:0"))
    parse("LC:1 IS @", Label("LC:1"))
    parse("LC:2 IS @", Label("LC:2"))
    parse("LC:3 IS @", Label("LC:3"))
    parse("LC:4 IS @", Label("LC:4"))
    parse("LC:5 IS @", Label("LC:5"))
    parse("LC:6 IS @", Label("LC:6"))
    parse("LC:7 IS @", Label("LC:7"))
    parse("LC:8 IS @", Label("LC:8"))

  }
  "ADDU" should {
    parse("ADDU $0,$0,1", ADDUI(Register(0), Register(0), 1))
    parse("ADDU $0,$0,4", ADDUI(Register(0), Register(0), 4))
    parse("ADDU $0,$254,32", ADDUI(Register(0), Register(254), 32))
    parse("ADDU $3,$2,1", ADDUI(Register(3), Register(2), 1))
    parse("ADDU $4,$4,4", ADDUI(Register(4), Register(4), 4))
    parse("ADDU $4,$254,4", ADDUI(Register(4), Register(254), 4))
    parse("ADDU $254,$254,8", ADDUI(Register(254), Register(254), 8))
    parse("ADDU $254,$254,64", ADDUI(Register(254), Register(254), 64))
  }
  "BYTE" should {
    parse("""BYTE " y",#5b,"3",#5d," into x starting at x",#5b,"2",#5d,#0""", BYTE(List(" y\u005b3\u005d into x starting at x\u005b2\u005d\0".getBytes("ASCII"): _*)))
    parse("""BYTE #9,"%d ",#0""", BYTE(List("\u0009%d \0".getBytes("ASCII"): _*)))
    parse("""BYTE "%d",#0""", BYTE(List("%d\0".getBytes("ASCII"): _*)))
    parse("""BYTE "array is full",#0""", BYTE(List("array is full\0".getBytes("ASCII"): _*)))
    parse("""BYTE "Enter integer ",#5b,"%d to terminate",#5d," : ",#0""", BYTE(List("Enter integer \u005b%d to terminate\u005d : \0".getBytes("ASCII"): _*)))
    parse("""BYTE "Printing x after having copied 4 elements",#a,"from y starting at"""", BYTE(List("Printing x after having copied 4 elements\u000afrom y starting at".getBytes("ASCII"): _*)))
    parse("""BYTE "Read the x array:",#0""", BYTE(List("Read the x array:\0".getBytes("ASCII"): _*)))
    parse("""BYTE "Read the y array:",#0""", BYTE(List("Read the y array:\0".getBytes("ASCII"): _*)))
    parse("""BYTE "The x array is:",#0""", BYTE(List("The x array is:\0".getBytes("ASCII"): _*)))
    parse("""BYTE "The y array is:",#0""", BYTE(List("The y array is:\0".getBytes("ASCII"): _*)))
  }
  "BZ" should {
    parse("BZ $10,L:7", BZ(Register(10), Label("L:7")))
  }
  "CMP" should {
    parse("CMP $9,$9,$1", CMP(Register(9), Register(9), Register(1)))
    parse("CMP $10,$10,$1", CMP(Register(10), Register(10), Register(1)))
    parse("CMP $10,$10,$2", CMP(Register(10), Register(10), Register(2)))
  }
  "CSN" should {
    parse("CSN $8,$8,$9", CSN(Register(8), Register(8), Register(9)))
    parse("CSN $3,$3,$255", CSN(Register(3), Register(3), Register(255)))
  }
  "CSNN" should {
    parse("CSNN $9,$255,$8", CSNN(Register(9), Register(255), Register(8)))
  }
  "DIVU" should {
    parse("DIVU $3,$3,$8", DIVU(Register(3), Register(3), Register(8)))
  }
  "GET" should {
    parse("GET $4,rJ", GET(Register(4), SpecialRegister.rJ))
    parse("GET $6,rJ", GET(Register(6), SpecialRegister.rJ))
    parse("GET $9,rJ", GET(Register(9), SpecialRegister.rJ))
    parse("GET $9,rR", GET(Register(9), SpecialRegister.rR))
  }
  "GETA" should {
    parse("GETA $2,puts", GETA(Register(2), Label("puts")))
    parse("GETA $3,printIntArray", GETA(Register(3), Label("printIntArray")))
    parse("GETA $5,getIntArray", GETA(Register(5), Label("getIntArray")))
    parse("GETA $6,puts", GETA(Register(6), Label("puts")))
    parse("GETA $7,printf", GETA(Register(7), Label("printf")))
    parse("GETA $7,scanf", GETA(Register(7), Label("scanf")))
    parse("GETA $8,LC:4", GETA(Register(8), Label("LC:4")))
    parse("GETA $8,LC:5", GETA(Register(8), Label("LC:5")))
    parse("GETA $8,LC:6", GETA(Register(8), Label("LC:6")))
    parse("GETA $8,LC:7", GETA(Register(8), Label("LC:7")))
    parse("GETA $8,LC:8", GETA(Register(8), Label("LC:8")))
    parse("GETA $8,printf", GETA(Register(8), Label("printf")))
    parse("GETA $10,LC:3", GETA(Register(10), Label("LC:3")))
    parse("GETA $11,LC:0", GETA(Register(11), Label("LC:0")))
    parse("GETA $11,LC:2", GETA(Register(11), Label("LC:2")))
  }
  "JMP" should {
    parse("JMP L:2", JMP(Label("L:2")))
    parse("JMP L:11", JMP(Label("L:11")))
    parse("JMP L:14", JMP(Label("L:14")))
  }
  "LDT" should {
    parse("LDT $1,$254,24", LDTI(Register(1), Register(254), 24))
    parse("LDT $3,$1,$4", LDT(Register(3), Register(1), Register(4)))
    parse("LDT $5,$254,12", LDTI(Register(5), Register(254), 12))
    parse("LDT $5,$254,16", LDTI(Register(5), Register(254), 16))
    parse("LDT $5,$254,20", LDTI(Register(5), Register(254), 20))
    parse("LDT $8,$0,0", LDTI(Register(8), Register(0), 0))
    parse("LDT $11,$4,0", LDTI(Register(11), Register(4), 0))
  }
  "LOC" should {
    parse("LOC @+(4-@)&3", Noop)
  }
  "NEGU" should {
    parse("NEGU $8,0,$9", NEGU(Register(8), 0, Register(9)))
    parse("NEGU $9,0,$8", NEGU(Register(9), 0, Register(8)))
    parse("NEGU $255,0,$3", NEGU(Register(255), 0, Register(3)))
  }
  "PBN" should {
    parse("PBN $9,L:16", PBN(Register(9), Label("L:16")))
  }
  "PBNZ" should {
    parse("PBNZ $9,L:15", PBNZ(Register(9), Label("L:15")))
    parse("PBNZ $10,L:8", PBNZ(Register(10), Label("L:8")))
  }
  "PBP" should {
    parse("PBP $2,L:3", PBP(Register(2), Label("L:3")))
  }
  "PUSHGO" should {
    parse("PUSHGO $7,$2,0", PUSHGO(Register(7), Register(2)))
    parse("PUSHGO $7,$3,0", PUSHGO(Register(7), Register(3)))
    parse("PUSHGO $7,$5,0", PUSHGO(Register(7), Register(5)))
    parse("PUSHGO $9,$5,0", PUSHGO(Register(9), Register(5)))
    parse("PUSHGO $9,$7,0", PUSHGO(Register(9), Register(7)))
    parse("PUSHGO $10,$6,0", PUSHGO(Register(10), Register(6)))
    parse("PUSHGO $10,$7,0", PUSHGO(Register(10), Register(7)))
  }
  "PUSHJ" should {
    parse("PUSHJ $9,putchar", PUSHJ(Register(9), Label("putchar")))
  }
  "PUT" should {
    parse("PUT rJ,$4", PUT(SpecialRegister.rJ, Register(4)))
    parse("PUT rJ,$6", PUT(SpecialRegister.rJ, Register(6)))
    parse("PUT rJ,$9", PUT(SpecialRegister.rJ, Register(9)))
  }
  "POP" should {
    parse("POP 0,0", POP(0))
    parse("POP 1,0", POP(1))
  }
  "SET" should {
    parse("SET $0,$5", SET(Register(0), Register(5)))
    parse("SET $2,$3", SET(Register(2), Register(3)))
    parse("SET $3,$0", SET(Register(3), Register(0)))
    parse("SET $3,$9", SET(Register(3), Register(9)))
    parse("SET $4,$7", SET(Register(4), Register(7)))
    parse("SET $5,$1", SET(Register(5), Register(1)))
    parse("SET $5,$7", SET(Register(5), Register(7)))
    parse("SET $8,$0", SET(Register(8), Register(0)))
    parse("SET $8,$6", SET(Register(8), Register(6)))
    parse("SET $8,$254", SET(Register(8), Register(254)))
    parse("SET $12,$2", SET(Register(12), Register(2)))
  }
  "SETL" should {
    parse("SETL $0,0", SETL(Register(0), 0))
    parse("SETL $2,0", SETL(Register(2), 0))
    parse("SETL $4,0", SETL(Register(4), 0))
    parse("SETL $6,#5", SETL(Register(6), 5))
    parse("SETL $9,#8", SETL(Register(9), 8))
    parse("SETL $10,0", SETL(Register(10), 0))
    parse("SETL $10,#a", SETL(Register(10), 10))
  }
  "SLU" should {
    parse("SLU $2,$2,32", SLUI(Register(2), Register(2), 32))
    parse("SLU $4,$4,32", SLUI(Register(4), Register(4), 32))
    parse("SLU $9,$2,32", SLUI(Register(9), Register(2), 32))
    parse("SLU $9,$3,32", SLUI(Register(9), Register(3), 32))
    parse("SLU $9,$4,32", SLUI(Register(9), Register(4), 32))
    parse("SLU $9,$5,32", SLUI(Register(9), Register(5), 32))
    parse("SLU $10,$0,32", SLUI(Register(10), Register(0), 32))
    parse("SLU $10,$10,2", SLUI(Register(10), Register(10), 2))
    parse("SLU $10,$11,32", SLUI(Register(10), Register(11), 32))
    parse("SLU $11,$8,32", SLUI(Register(11), Register(8), 32))
  }
  "SR" should {
    parse("SR $2,$2,32", SRI(Register(2), Register(2), 32))
    parse("SR $9,$4,32", SRI(Register(9), Register(4), 32))
    parse("SR $9,$9,32", SRI(Register(9), Register(9), 32))
    parse("SR $10,$10,32", SRI(Register(10), Register(10), 32))
    parse("SR $11,$11,32", SRI(Register(11), Register(11), 32))
  }
  "SUBU" should {
    parse("SUBU $2,$2,1", SUBUI(Register(2), Register(2), 1))
    parse("SUBU $254,$254,8", SUBUI(Register(254), Register(254), 8))
    parse("SUBU $254,$254,64", SUBUI(Register(254), Register(254), 64))
  }
  "STTU" should {
    parse("STTU $1,$0,20", STTUI(Register(1), Register(0), 20))
    parse("STTU $3,$0,$4", STTU(Register(3), Register(0), Register(4)))
    parse("STTU $5,$0,8", STTUI(Register(5), Register(0), 8))
    parse("STTU $5,$0,12", STTUI(Register(5), Register(0), 12))
    parse("STTU $5,$0,16", STTUI(Register(5), Register(0), 16))
    parse("STTU $11,$3,$10", STTU(Register(11), Register(3), Register(10)))
  }
}

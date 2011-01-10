package transform.asm2ast

import transform.asm.Divergence.Diverge
import tree.asm._
import tree.ast._

/**
 * Expects to transform a single label worth of instructions at a time (post divergence)
 */
object Transformer {
  val root = new Transformer(
    registers = Map.empty.withDefault(Input(_)),
    pinned = Nil)
  def apply(instructions: List[ASM]) = {
    (root /: instructions)((t, i) => t(i))
  }
}

class Transformer private (
  val registers: Map[Register, AST],
  val pinned: List[AST]) {

  private def copy(
    registers: Map[Register, AST] = registers,
    pinned: List[AST] = pinned) = {
    new Transformer(
      registers = registers,
      pinned = pinned)
  }

  private def apply(insn: ASM): Transformer = insn match {
    case ADDU(dest, l, r) => assign(dest -> AddUnsigned(registers(l), registers(r)))
    case ADDUI(dest, src, constant) => assign(dest -> AddUnsigned(registers(src), Constant(constant)))
    case Diverge(insn, ifFalse) =>
      insn match {
        case BZ(reg, ifTrue) => pin(BranchIfZero(registers(reg), ifTrue, ifFalse))
        case PBN(reg, ifTrue) => pin(BranchIfNegative(registers(reg), ifTrue, ifFalse))
        case PBNZ(reg, ifTrue) => pin(BranchIfNonZero(registers(reg), ifTrue, ifFalse))
        case PBP(reg, ifTrue) => pin(BranchIfPositive(registers(reg), ifTrue, ifFalse))
      }
    case DIVU(dest, n, d) =>
      val rD = registers(Register(SpecialRegister.rD))
      val denominator = registers(d)
      val numerator = registers(n)
      assign(
        dest -> ConditionallySetIfGreater(denominator, rD, DivideUnsigned(rD, numerator, denominator), rD)).assign(
        Register(SpecialRegister.rR) -> ConditionallySetIfGreater(denominator, rD, ModUnsigned(rD, numerator, denominator), numerator))
    case GET(dest, src) => assign(dest -> registers(src))
    case GETA(dest, label) => assign(dest -> GetAddress(label))
    case JMP(label) => pin(Jump(label))
    case NEGU(dest, l, r) => assign(dest -> SubtractUnsigned(Constant(l), registers(r)))
    case POP(0) => pin(Return0)
    case POP(1) => pin(Return1(registers(Register(0))))
    case PUT(dest, src) => assign(dest -> registers(src))
    case SET(dest, src) => assign(dest -> registers(src))
    case SETL(dest, value) => assign(dest -> Constant(value.toLong & 0xFFFFL))
    case SLU(dest, number, amount) => assign(dest -> ShiftLeftUnsigned(registers(number), registers(amount)))
    case SLUI(dest, number, amount) => assign(dest -> ShiftLeftUnsigned(registers(number), Constant(amount)))
    case SRI(dest, number, amount) => assign(dest -> ShiftRight(registers(number), Constant(amount)))
    case SUBU(dest, l, r) => assign(dest -> SubtractUnsigned(registers(l), registers(r)))
    case SUBUI(dest, src, constant) => assign(dest -> SubtractUnsigned(registers(src), Constant(constant)))
    case _ => throw new IllegalArgumentException("Unknown ASM instruction: " + insn)
  }

  private def assign(tuple: (Register, AST)): Transformer = copy(registers = registers + tuple)
  private def pin(ast: AST) = copy(pinned = pinned ::: List(ast))
}

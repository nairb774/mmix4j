package transform.asm

import java.util.UUID
import tree.asm._

/**
 * Given a stream of ASM instructions for a single label (post label collection), breaks the label down to smaller label
 * chunks when it comes across branches in the code.
 */
object Divergence {
  case class Diverge(insn: ASM, follows: Label) extends ASM

  def apply(label: Label, instructions: List[ASM]): Map[Label, List[ASM]] = {
    Map((List(List[ASM](label)) /: instructions) { (list, insn) =>
      insn match {
        case _: BZ => doDiverge(label, list, insn)
        case _: PBN => doDiverge(label, list, insn)
        case _: PBNZ => doDiverge(label, list, insn)
        case _: PBP => doDiverge(label, list, insn)
        case _: PUSHGO => doDiverge(label, list, insn)
        case _: PUSHJ => doDiverge(label, list, insn)
        case _: JMP =>
          // No diverge for jumps, we want to trim the insn stream at them though 
          val newLbl = newLabel(label, list)
          List(newLbl) :: (insn :: list.head) :: list.tail
        case _: POP =>
          // No diverge for pops, we want to trim the insn stream at them though 
          val newLbl = newLabel(label, list)
          List(newLbl) :: (insn :: list.head) :: list.tail
        case _ => (insn :: list.head) :: list.tail
      }
    } filter {
      _.size > 1
    } map { l =>
      val t = l.reverse
      t.head.asInstanceOf[Label] -> t.tail
    }: _*)
  }

  private def doDiverge(label: Label, list: List[List[ASM]], insn: ASM) = {
    val newLbl = newLabel(label, list)
    List(newLbl) :: (Divergence.Diverge(insn, newLbl) :: list.head) :: list.tail
  }

  private def newLabel(label: Label, list: List[List[ASM]]) = Label(label.name + " " + list.size)
}

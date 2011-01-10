package transform.asm

import java.util.UUID
import tree.asm._

/**
 * Given a stream of ASM instructions for a single label (post label collection), breaks the label down to smaller label
 * chunks when it comes across branches in the code.
 */
object Divergence {
  case class Diverge(insn: ASM, ifFalse: Label) extends ASM

  def apply(label: Label, instructions: List[ASM]): Map[Label, List[ASM]] = {
    Map((List(List[ASM](label)) /: instructions) { (list, insn) =>
      insn match {
        case _: BZ => doDiverge(list, insn)
        case _: PBN => doDiverge(list, insn)
        case _: PBNZ => doDiverge(list, insn)
        case _: PBP => doDiverge(list, insn)
        case _: JMP =>
          // No diverge for jumps, we want to trim the insn stream at them though 
          val newLabel = Label(UUID.randomUUID.toString)
          List(newLabel) :: (insn :: list.head) :: list.tail
        case _ => (insn :: list.head) :: list.tail
      }
    } map { l =>
      val t = l.reverse
      t.head.asInstanceOf[Label] -> t.tail
    }: _*)
  }

  private def doDiverge(list: List[List[ASM]], insn: ASM) = {
    val newLabel = Label(UUID.randomUUID.toString)
    List(newLabel) :: (Divergence.Diverge(insn, newLabel) :: list.head) :: list.tail
  }
}

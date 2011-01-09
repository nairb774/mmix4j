package transform.asm

import tree.asm.{ ASM, BYTE, Label, Global, JMP }

/**
 * Takes a 'files worth' of asm instructions and groups them up into blocks based upon the labels available. Also merges
 * data segments into a single BYTE block.
 */
object LabelCollector {
  private val root = new LabelCollector(
    currentLabel = None,
    lastCodeLabel = None,
    globalLabels = Set.empty,
    dataLables = Map.empty.withDefaultValue(BYTE(Nil)),
    codeLabels = Map.empty.withDefaultValue(Nil))

  def apply(asm: List[ASM]) = (root /: asm)((c, a) => c(a))
}

class LabelCollector private (
  currentLabel: Option[Label],
  lastCodeLabel: Option[Label],
  val globalLabels: Set[Label],
  val dataLables: Map[Label, BYTE],
  val codeLabels: Map[Label, List[ASM]]) {

  private def copy(
    currentLabel: Option[Label] = currentLabel,
    lastCodeLabel: Option[Label] = lastCodeLabel,
    globalLabels: Set[Label] = globalLabels,
    dataLables: Map[Label, BYTE] = dataLables,
    codeLabels: Map[Label, List[ASM]] = codeLabels) = {
    new LabelCollector(
      currentLabel = currentLabel,
      lastCodeLabel = lastCodeLabel,
      globalLabels = globalLabels,
      dataLables = dataLables,
      codeLabels = codeLabels)
  }

  private def apply(asm: ASM): LabelCollector = asm match {
    case nextLabel: Label =>
      currentLabel map { currentLabel =>
        if (codeLabels(currentLabel).isEmpty) {
          this
        } else {
          copy(lastCodeLabel = Some(currentLabel)) // Make sure there is a jump there for continuity
        }
      } getOrElse this copy (currentLabel = Some(nextLabel))
    case Global(label) => copy(globalLabels = globalLabels + label)
    case bytes: BYTE =>
      val label = currentLabel.getOrElse(throw new IllegalStateException("Data without a label: " + bytes))
      copy(dataLables = dataLables + (label -> BYTE(dataLables(label).bytes ::: bytes.bytes)))
    case _ =>
      val label = currentLabel.getOrElse(throw new IllegalStateException("Command without a label: " + asm))
      lastCodeLabel map { lastCodeLabel =>
        // add a jump to the last code label for continuity
        val appended = copy(lastCodeLabel = None,
          codeLabels = codeLabels + (lastCodeLabel -> (codeLabels(lastCodeLabel) ::: List(JMP(label)))))
        appended(asm) // Now apply the current command
      } getOrElse copy(codeLabels = codeLabels + (label -> (codeLabels(label) ::: List(asm))))
  }

  override def toString =
    "LabelCollector[globalLabels=" + globalLabels +
      ", dataLables=" + dataLables.keySet +
      ", codeLabels=" + codeLabels.keySet + "]"
}

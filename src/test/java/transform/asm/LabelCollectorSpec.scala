package transform.asm

import org.specs._
import org.specs.runner.JUnit4
import tree.asm._

class LabelCollectorTest extends JUnit4(LabelCollectorSpec)
object LabelCollectorSpec extends Specification {
  "BYTE blocks" should {
    "fail when before a label" >> {
      val insns = List(
        BYTE(List(0, 1, 2)))
      LabelCollector(insns) must throwA[IllegalStateException]
    }
    "be collected as a data block" >> {
      val insns = List(
        Label("A"),
        BYTE(List(0, 1, 2)))
      LabelCollector(insns).dataLables mustEqual Map(Label("A") -> BYTE(List(0, 1, 2)))
    }
    "be merged when multiple show up in a row" >> {
      val insns = List(
        Label("A"),
        BYTE(List(0, 1, 2)),
        BYTE(List(3, 4, 5)))
      LabelCollector(insns).dataLables mustEqual Map(Label("A") -> BYTE(List(0, 1, 2, 3, 4, 5)))
    }
    "follow label changes" >> {
      val insns = List(
        Label("A"),
        BYTE(List(0, 1, 2)),
        Label("B"),
        BYTE(List(3, 4, 5)))
      LabelCollector(insns).dataLables mustEqual Map(
        Label("A") -> BYTE(List(0, 1, 2)),
        Label("B") -> BYTE(List(3, 4, 5)))
    }
  }
  "Global" should {
    "result in a global label" >> {
      val insns = List(
        Global(Label("A")))
      LabelCollector(insns).globalLabels mustEqual Set(Label("A"))
    }
    "generic labels should not become global" >> {
      val insns = List(
        Label("A"))
      LabelCollector(insns).globalLabels mustEqual Set.empty
    }
  }
  "ASM instruction" should {
    "fail when before a label" >> {
      val insns = List(
        JMP(Label("A")))
      LabelCollector(insns) must throwA[IllegalStateException]
    }
    "collect under the current label" >> {
      val insns = List(
        Label("A"),
        JMP(Label("B")))
      LabelCollector(insns).codeLabels mustEqual Map(Label("A") -> List(JMP(Label("B"))))
    }
  }
  "JMP to next label" should {
    "be appended to code labels and point at the next code label" >> {
      val insns = List(
        Label("A"),
        SET(Register(0), Register(0)),
        Label("B"),
        BYTE(Nil),
        Label("C"),
        SET(Register(1), Register(1)))

      LabelCollector(insns).codeLabels mustEqual Map(
        Label("A") -> List(
          SET(Register(0), Register(0)),
          JMP(Label("C"))),
        Label("C") -> List(
          SET(Register(1), Register(1))))
    }
  }
}

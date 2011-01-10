package transform.asm

import org.specs._
import org.specs.runner.JUnit4
import org.specs.matcher.Matcher
import tree.asm._

class DivergenceTest extends JUnit4(DivergenceSpec)
object DivergenceSpec extends Specification {
  case class matchDivergence(k: (Label, ASM)) extends Matcher[Map[Label, List[ASM]]]() {
    def apply(v: => Map[Label, List[ASM]]) = {
      val vv = v
      (vv(k._1).last match {
        case Divergence.Diverge(insn, lbl) if vv.contains(lbl) && insn == k._2 => true
        case _ => false
      }, "", k + "\n" + vv)
    }
  }
  "JMP" should {
    "cause the end of a sub section" >> {
      val insns = List(
        SET(Register(0), Register(1)),
        JMP(Label("B")),
        SET(Register(1), Register(2)))
      Divergence(Label("A"), insns)(Label("A")) mustEqual List(SET(Register(0), Register(1)), JMP(Label("B")))
    }
    "result in another label" >> {
      val insns = List(JMP(Label("B")))
      val result = Divergence(Label("A"), insns) - Label("A")
      result.size mustEqual 1
    }
    "resut in another label that is empty" >> {
      val insns = List(JMP(Label("B")))
      val result = Divergence(Label("A"), insns) - Label("A")
      result.head._2 mustEqual Nil
    }
  }
  "POP" should {
    "cause the end of a sub section" >> {
      val insns = List(
        SET(Register(0), Register(1)),
        POP(0),
        SET(Register(1), Register(2)))
      Divergence(Label("A"), insns)(Label("A")) mustEqual List(SET(Register(0), Register(1)), POP(0))
    }
    "result in another label" >> {
      val insns = List(POP(0))
      val result = Divergence(Label("A"), insns) - Label("A")
      result.size mustEqual 1
    }
    "resut in another label that is empty" >> {
      val insns = List(POP(0))
      val result = Divergence(Label("A"), insns) - Label("A")
      result.head._2 mustEqual Nil
    }
  }
  "BZ" should {
    "result in a Diverge" >> {
      val insns = List(
        SET(Register(0), Register(1)),
        BZ(Register(2), Label("B")),
        SET(Register(1), Register(2)))
      Divergence(Label("A"), insns) must matchDivergence(Label("A") -> BZ(Register(2), Label("B")))
    }
  }
  "PBN" should {
    "result in a Diverge" >> {
      val insns = List(
        SET(Register(0), Register(1)),
        PBN(Register(2), Label("B")),
        SET(Register(1), Register(2)))
      Divergence(Label("A"), insns) must matchDivergence(Label("A") -> PBN(Register(2), Label("B")))
    }
  }
  "PBNZ" should {
    "result in a Diverge" >> {
      val insns = List(
        SET(Register(0), Register(1)),
        PBNZ(Register(2), Label("B")),
        SET(Register(1), Register(2)))
      Divergence(Label("A"), insns) must matchDivergence(Label("A") -> PBNZ(Register(2), Label("B")))
    }
  }
  "PBP" should {
    "result in a Diverge" >> {
      val insns = List(
        SET(Register(0), Register(1)),
        PBP(Register(2), Label("B")),
        SET(Register(1), Register(2)))
      Divergence(Label("A"), insns) must matchDivergence(Label("A") -> PBP(Register(2), Label("B")))
    }
  }
}

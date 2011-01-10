package transform.asm2ast

import org.specs._
import org.specs.runner.JUnit4
import tree.asm._
import tree.ast._
import transform.asm.Divergence.Diverge

class TransformerTest extends JUnit4(TransformerSpec)
object TransformerSpec extends Specification {
  "ADDU" should {
    "become AddUnsigned" >> {
      val insns = List(
        ADDU(Register(0), Register(1), Register(2)))
      Transformer(insns).registers mustEqual Map(Register(0) -> AddUnsigned(Input(Register(1)), Input(Register(2))))
    }
    "support a src and dest of the same register" >> {
      val insns = List(
        ADDU(Register(0), Register(1), Register(0)))
      Transformer(insns).registers mustEqual Map(Register(0) -> AddUnsigned(Input(Register(1)), Input(Register(0))))
    }
  }
  "ADDUI" should {
    "become AddUnsigned with a Constant standin" >> {
      val insns = List(
        ADDUI(Register(0), Register(1), 2))
      Transformer(insns).registers mustEqual Map(Register(0) -> AddUnsigned(Input(Register(1)), Constant(2)))
    }
    "support having the src and dest registers be the same" >> {
      val insns = List(
        ADDUI(Register(0), Register(0), 2))
      Transformer(insns).registers mustEqual Map(Register(0) -> AddUnsigned(Input(Register(0)), Constant(2)))
    }
  }
  "Diverge" should {
    "with a BZ results in a pinned BranchIfZero" >> {
      val insns = List(
        Diverge(BZ(Register(0), Label("T")), Label("F")))
      Transformer(insns).pinned.last mustEqual BranchIfZero(Input(Register(0)), Label("T"), Label("F"))
    }
    "with a PBN results in a pinned BranchIfNegative" >> {
      val insns = List(
        Diverge(PBN(Register(0), Label("T")), Label("F")))
      Transformer(insns).pinned.last mustEqual BranchIfNegative(Input(Register(0)), Label("T"), Label("F"))
    }
    "with a PBNZ results in a pinned BranchIfNonZero" >> {
      val insns = List(
        Diverge(PBNZ(Register(0), Label("T")), Label("F")))
      Transformer(insns).pinned.last mustEqual BranchIfNonZero(Input(Register(0)), Label("T"), Label("F"))
    }
    "with a PBP results in a pinned BranchIfPositive" >> {
      val insns = List(
        Diverge(PBP(Register(0), Label("T")), Label("F")))
      Transformer(insns).pinned.last mustEqual BranchIfPositive(Input(Register(0)), Label("T"), Label("F"))
    }
  }
  "DIVU" should {
    "become DivideUnsigned" >> {
      val insns = List(
        DIVU(Register(0), Register(1), Register(2)))
      Transformer(insns).registers mustEqual Map(
        Register(0) -> ConditionallySetIfGreater(
          Input(Register(2)),
          SpecialInput(SpecialRegister.rD),
          DivideUnsigned(SpecialInput(SpecialRegister.rD), Input(Register(1)), Input(Register(2))),
          SpecialInput(SpecialRegister.rD)))
    }
    "become ModUnsigned" >> {
      val insns = List(
        DIVU(Register(0), Register(1), Register(2)))
      Transformer(insns).specialRegisters mustEqual Map(
        SpecialRegister.rR -> ConditionallySetIfGreater(
          Input(Register(2)),
          SpecialInput(SpecialRegister.rD),
          ModUnsigned(SpecialInput(SpecialRegister.rD), Input(Register(1)), Input(Register(2))),
          Input(Register(1))))
    }
  }
  "GET" should {
    "copy a special register to a normal register" >> {
      val insns = List(
        GET(Register(0), SpecialRegister.rR))
      Transformer(insns).registers mustEqual Map(Register(0) -> SpecialInput(SpecialRegister.rR))
    }
  }
  "GETA" should {
    "become GetAddress" >> {
      val insns = List(
        GETA(Register(0), Label("A")))
      Transformer(insns).registers mustEqual Map(Register(0) -> GetAddress(Label("A")))
    }
  }
  "JMP" should {
    "result in a pinned Jump" >> {
      val insns = List(
        JMP(Label("A")))
      Transformer(insns).pinned.last mustEqual Jump(Label("A"))
    }
  }
  "NEGU" should {
    "become SubtractUnsigned" >> {
      val insns = List(
        NEGU(Register(0), 1, Register(2)))
      Transformer(insns).registers mustEqual Map(Register(0) -> SubtractUnsigned(Constant(1), Input(Register(2))))
    }
  }
  "POP" should {
    "result in a pinned Return" >> {
      val insns = List(
        POP(0))
      Transformer(insns).pinned.last mustEqual Return0
    }
    "result in a pinned Return" >> {
      val insns = List(
        POP(1))
      Transformer(insns).pinned.last mustEqual Return1(Input(Register(0)))
    }
  }
  "PUT" should {
    "copy a register to another register" >> {
      val insns = List(
        PUT(SpecialRegister.rR, Register(1)))
      Transformer(insns).specialRegisters mustEqual Map(SpecialRegister.rR -> Input(Register(1)))
    }
  }
  "SET" should {
    "copy a register to another register" >> {
      val insns = List(
        SET(Register(0), Register(1)))
      Transformer(insns).registers mustEqual Map(Register(0) -> Input(Register(1)))
    }
  }
  "SETL" should {
    "set the lowest 2 bytes of a register and overwrite evrythign else" >> {
      val insns = List(
        SETL(Register(0), 0x1234))
      Transformer(insns).registers mustEqual Map(Register(0) -> Constant(0x1234))
    }
  }
  "SLU" should {
    "become ShiftLeftUnsigned" >> {
      val insns = List(
        SLU(Register(0), Register(1), Register(2)))
      Transformer(insns).registers mustEqual Map(Register(0) -> ShiftLeftUnsigned(Input(Register(1)), Input(Register(2))))
    }
  }
  "SLUI" should {
    "become ShiftLeftUnsigned" >> {
      val insns = List(
        SLUI(Register(0), Register(1), 2))
      Transformer(insns).registers mustEqual Map(Register(0) -> ShiftLeftUnsigned(Input(Register(1)), Constant(2)))
    }
  }
  "SUBU" should {
    "become SubtractUnsigned" >> {
      val insns = List(
        SUBU(Register(0), Register(1), Register(2)))
      Transformer(insns).registers mustEqual Map(Register(0) -> SubtractUnsigned(Input(Register(1)), Input(Register(2))))
    }
    "support a src and dest of the same register" >> {
      val insns = List(
        SUBU(Register(0), Register(1), Register(0)))
      Transformer(insns).registers mustEqual Map(Register(0) -> SubtractUnsigned(Input(Register(1)), Input(Register(0))))
    }
  }
  "SUBUI" should {
    "become SubtractUnsigned with a Constant standin" >> {
      val insns = List(
        SUBUI(Register(0), Register(1), 2))
      Transformer(insns).registers mustEqual Map(Register(0) -> SubtractUnsigned(Input(Register(1)), Constant(2)))
    }
    "support having the src and dest registers be the same" >> {
      val insns = List(
        SUBUI(Register(0), Register(0), 2))
      Transformer(insns).registers mustEqual Map(Register(0) -> SubtractUnsigned(Input(Register(0)), Constant(2)))
    }
  }
}
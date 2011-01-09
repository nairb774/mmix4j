package transform.asm2ast

import org.specs._
import org.specs.runner.JUnit4
import tree.asm._
import tree.ast._

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
  "NEGU" should {
    "become SubtractUnsigned" >> {
      val insns = List(
        NEGU(Register(0), 1, Register(2)))
      Transformer(insns).registers mustEqual Map(Register(0) -> SubtractUnsigned(Constant(1), Input(Register(2))))
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
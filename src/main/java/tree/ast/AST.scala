package tree.ast

import tree.asm.{ Register, SpecialRegister }

trait AST
case class Constant(const: Long) extends AST
case class SpecialInput(register: SpecialRegister.Value) extends AST
case class Input(register: Register) extends AST

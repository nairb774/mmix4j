package tree.ast

import tree.asm.{ Label, Register }

case class Return(reg: Register) extends AST
case class Call(reg: Register, dest: Label) extends AST

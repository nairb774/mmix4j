package tree.ast

import tree.asm.{ Label, Register }

case object Return0 extends AST
case class Return1(reg: AST) extends AST
case class Call(reg: Register, dest: Label) extends AST
case class UnboundCall(reg: Register, method: AST) extends AST

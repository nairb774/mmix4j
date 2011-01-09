package tree.ast

import tree.asm.Label

case class Jump(addr: Label) extends AST

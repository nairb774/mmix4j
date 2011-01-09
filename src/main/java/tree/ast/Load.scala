package tree.ast

import tree.asm.Label

case class GetAddress(label: Label) extends AST
case class LoadTetrabyte(src: AST) extends AST

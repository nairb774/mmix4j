package tree.ast

import tree.asm.Label

case class BranchIfNegative(test: AST, value: Label) extends AST
case class BranchIfNonZero(test: AST, value: Label) extends AST
case class BranchIfPositive(test: AST, value: Label) extends AST
case class BranchIfZero(test: AST, value: Label) extends AST

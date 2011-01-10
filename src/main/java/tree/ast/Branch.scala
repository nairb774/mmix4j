package tree.ast

import tree.asm.Label

case class BranchIfNegative(test: AST, ifTrue: Label, ifFalse: Label) extends AST
case class BranchIfNonZero(test: AST, ifTrue: Label, ifFalse: Label) extends AST
case class BranchIfPositive(test: AST, ifTrue: Label, ifFalse: Label) extends AST
case class BranchIfZero(test: AST, ifTrue: Label, ifFalse: Label) extends AST

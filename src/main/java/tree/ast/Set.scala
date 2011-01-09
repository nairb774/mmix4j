package tree.ast

case class ConditionallySetIfNegative(test: AST, src: AST) extends AST
case class ConditionallySetIfNonNegative(test: AST, src: AST) extends AST
case class ConditionallySetIfGreater(l: AST, r: AST, ifTrue: AST, ifFalse: AST) extends AST

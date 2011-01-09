package tree.ast

case class ShiftLeftUnsigned(value: AST, amount: AST) extends AST
case class ShiftRight(value: AST, amount: AST) extends AST

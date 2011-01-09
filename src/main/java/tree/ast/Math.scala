package tree.ast

case class AddUnsigned(l: AST, r: AST) extends AST
case class DivideUnsigned(nA: AST, nB: AST, d: AST) extends AST
case class ModUnsigned(nA: AST, nB: AST, d: AST) extends AST
case class SubtractUnsigned(l: AST, r: AST) extends AST

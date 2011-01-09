package tree.asm

case class Global(label: Label) extends ASM
case class Label(name: String) extends ASM

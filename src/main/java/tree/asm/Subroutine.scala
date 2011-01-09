package tree.asm

/** pop (3) rJ,rL */
case class POP(reg: Register) extends ASM

/** push registers and go (1) rJ,rL */
case class PUSHGO(reg: Register, dest: Register) extends ASM

/** push registers and jump (3) rJ,rL */
case class PUSHJ(reg: Register, dest: Label) extends ASM

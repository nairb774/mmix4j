package tree.asm

/** store tetrabyte unsigned (1) */
case class STTU(src: Register, dest1: Register, dest2: Register) extends ASM

/** store tetrabyte unsigned immediate (2) */
case class STTUI(src: Register, dest1: Register, dest2: Int) extends ASM

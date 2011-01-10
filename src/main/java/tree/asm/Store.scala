package tree.asm

/** store tetrabyte unsigned (1) */
case class STTU(data: Register, dest1: Register, dest2: Register) extends ASM

/** store tetrabyte unsigned immediate (2) */
case class STTUI(data: Register, dest: Register, offset: Int) extends ASM

package tree.asm

/** store tetrabyte unsigned (1) */
case class STTU(dest: Register, src1: Register, src2: Register) extends ASM

/** store tetrabyte unsigned immediate (2) */
case class STTUI(dest: Register, src1: Register, src2: Int) extends ASM

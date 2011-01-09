package tree.asm

/** compare (1) */
case class CMP(dest: Register, l: Register, r: Register) extends ASM

/** compare immediate (2) */
case class CMPI(dest: Register, l: Register, r: Int) extends ASM

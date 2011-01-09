package tree.asm

/** add unsigned (1) */
case class ADDU(dest: Register, l: Register, r: Register) extends ASM

/** add unsigned immediate (2) */
case class ADDUI(dest: Register, l: Register, r: Int) extends ASM

/** divide unsigned (1) rD,rR */
case class DIVU(dest: Register, n: Register, d: Register) extends ASM

/** negate unsigned (1, Y=unsigned immediate) */
case class NEGU(dest: Register, c: Int, src: Register) extends ASM

/** subtract unsigned (1) */
case class SUBU(dest: Register, l: Register, r: Register) extends ASM

/** subtract unsigned immediate (2) */
case class SUBUI(dest: Register, src: Register, value: Int) extends ASM

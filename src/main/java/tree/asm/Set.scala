package tree.asm

/** conditionally set if negative (1) */
case class CSN(dest: Register, test: Register, src: Register) extends ASM

/** conditionally set if nonnegative (2) */
case class CSNN(dest: Register, test: Register, src: Register) extends ASM

/** put into special register (X=specreg, Y=0, Z=register) rA-rZZ */
case class PUT(dest: SpecialRegister.Value, srg: Register) extends ASM

/** undocumeted */
case class SET(dest: Register, src: Register) extends ASM

/** set to low wyde (3) */
case class SETL(reg: Register, value: Int) extends ASM

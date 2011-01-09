package tree.asm

/** get from special register (X=register, Y=0, Z=specreg) rA-rZZ */
case class GET(reg: Register, special: SpecialRegister.Value) extends ASM

/** get address (3) */
case class GETA(reg: Register, label: Label) extends ASM

/** load tetrabyte (1) */
case class LDT(dest: Register, src1: Register, src2: Register) extends ASM

/** load tetrabyte immediate (2) */
case class LDTI(dest: Register, src1: Register, src2: Int) extends ASM

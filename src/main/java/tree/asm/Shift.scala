package tree.asm

/** shift left unsigned (1) */
case class SLU(dest: Register, value: Register, amount: Register) extends ASM

/** shift left unsigned immediate (2) */
case class SLUI(dest: Register, value: Register, amount: Int) extends ASM

/** shift right (1) rA */
case class SR(dest: Register, value: Register, amount: Register) extends ASM

/** shift right immediate (2) rA */
case class SRI(dest: Register, value: Register, amount: Int) extends ASM

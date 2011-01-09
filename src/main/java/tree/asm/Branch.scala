package tree.asm

/** branch if zero (3) */
case class BZ(reg: Register, value: Label) extends ASM

/** probable branch if negative (3) */
case class PBN(reg: Register, value: Label) extends ASM

/** probable branch if nonzero (3) */
case class PBNZ(reg: Register, value: Label) extends ASM

/** probable branch if positive (3) */
case class PBP(reg: Register, value: Label) extends ASM

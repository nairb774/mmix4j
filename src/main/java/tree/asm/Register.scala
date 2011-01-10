package tree.asm

object Register {
  private val special: Array[Register] = SpecialRegister.values.toArray sortBy (_.id) map { v => new Register(256 + v.id) }
  private val all: Array[Register] = 0.until(256).toArray.map(new Register(_)) ++ special
  def apply(n: Int) = all(n)
  def apply(s: SpecialRegister.Value) = special(s.id)
  def unapply(r: Register) = Some(r.n)
}
class Register private (val n: Int) {
  override def toString = "$" + n
}

object SpecialRegister extends Enumeration {
  val rA = Value
  val rB = Value
  val rC = Value
  val rD = Value
  val rE = Value
  val rF = Value
  val rG = Value
  val rH = Value
  val rI = Value
  val rJ = Value
  val rK = Value
  val rL = Value
  val rM = Value
  val rN = Value
  val rO = Value
  val rP = Value
  val rQ = Value
  val rR = Value
  val rS = Value
  val rT = Value
  val rU = Value
  val rV = Value
  val rW = Value
  val rX = Value
  val rY = Value
  val rZ = Value
  val rBB = Value
  val rTT = Value
  val rWW = Value
  val rXX = Value
  val rYY = Value
  val rZZ = Value
}

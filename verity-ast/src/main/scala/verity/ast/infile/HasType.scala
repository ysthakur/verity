package verity.ast.infile

trait HasType {
  def myType: Type
  def myType_=(newType: Type): Unit
}

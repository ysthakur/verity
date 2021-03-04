package verity.ast.infile

trait HasType {
  def myType: ITypeRef
  def myType_=(newType: ITypeRef): Unit
}

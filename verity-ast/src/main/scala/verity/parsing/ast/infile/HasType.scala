package verity.parsing.ast.infile

trait HasType {
  def myType: ITypeRef
  def myType_=(newType: ITypeRef): Unit
}

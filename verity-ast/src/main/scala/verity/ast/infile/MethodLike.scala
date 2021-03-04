package verity.ast.infile

import verity.ast.infile.Expr

trait MethodLike extends HasModifiers {
  def returnType: TypeRef
  def name: String
  def ctparams: CTParamList
  def params: ParamList
  
  def body: Option[Block] //Option[Block|Expr]
  def body_=(newBody: Option[Block] /*Option[Block|Expr]*/): Unit
}
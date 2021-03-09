package verity.ast

import verity.parsing.TextRange
import verity.ast.infile.{Name, NamedTree, Field, Method}

class TemplateDef(
    val templateType: TemplateDefType,
    val name: Name,
    val fields: List[Field],
    val methods: List[Method]
) extends Tree

class EnumDef(
    name: Name,
    val constants: List[EnumConstant],
    fields: List[Field],
    methods: List[Method]
) extends TemplateDef(TemplateDefType.ENUM, name, fields, methods) {}

class EnumConstant

enum TemplateDefType {
  case CLASS
  case INTERFACE
  case ENUM
}
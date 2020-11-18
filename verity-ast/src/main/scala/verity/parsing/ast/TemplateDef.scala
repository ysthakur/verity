package verity.parsing.ast

import verity.parsing.TextRange
import verity.parsing.ast.infile.ValidId

class TemplateDef(
    val templateType: TemplateDefType,
    val name: ValidId,
    val fields: List[Field],
    val methods: List[Method],
    val textRange: TextRange
) {}

class EnumDef(
    name: ValidId,
    val constants: List[EnumConstant],
    fields: List[Field],
    methods: List[Method],
    textRange: TextRange
) extends TemplateDef(TemplateDefType.ENUM, name, fields, methods, textRange) {}

class EnumConstant

enum TemplateDefType {
  case CLASS
  case INTERFACE
  case ENUM
}

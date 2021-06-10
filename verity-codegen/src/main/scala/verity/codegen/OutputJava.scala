package verity.codegen

import verity.ast._
import verity.ast.infile._

import scala.util.Using
import java.io.{File, FileWriter, Writer}

opaque type OutputJava[-T] = T => Writer ?=> Unit

object OutputJava {
  def output[T](tree: T)(using oj: OutputJava[T], w: Writer) = oj(tree)(using w)

  def outputJavaPkg(pkg: Pkg, outputDir: File): Unit = {
//    if (!outputDir.exists) outputDir.mkdir()

    /*println(
        s"Outputting package ${pkg.name}, pkgs=${pkg.subPkgs.map(_.name)}, files=${pkg.files.map(_.name)}"
    )*/

    pkg.files.foreach { fileNode =>
      outputJavaFile(
        fileNode,
        outputDir,
        File(outputDir, fileNode.name.replace(".verity", ".java"))
      )
    }
    pkg.subPkgs.foreach { subPkg => outputJavaPkg(subPkg, File(outputDir, subPkg.name)) }
  }

  def outputJavaFile(file: FileNode, outputDir: File, outputFile: File): Unit = {
    if (file.isSource) {
      println(s"Outputting file ${file.name}")
      // println(file.text)
      if (!outputDir.exists) outputDir.mkdir()
      if (!outputFile.exists) outputFile.createNewFile()

      Using(FileWriter(outputFile)) { fw =>
        given Writer = fw
        file.packageRef match {
          case Some(pkgStmt) => write(pkgStmt.text)
          case None          =>
        }
        file.imports.foreach(impt => write(impt.text))
        file.classlikes.foreach(output(_))
      }
    }
  }

  given OutputJava[Classlike] = cls => {
    writeModifiers(cls.modifiers)
    write(cls.defType.text)
    write(" ")
    write(cls.name)
    output(cls.typeParams)
    cls match {
      case c: ClassDef =>
        write(" extends ")
        output(c.superClass)
      case _          =>
    }
    write("{")
    cls match {
      case e: EnumDef => //write(e.constants)
      case _          =>
    }
    cls match {
      case c: ClassDef => c.fields.foreach(output)
      case _           =>
    }
    cls.methods.foreach(output)
    write("}")
  }

  given OutputJava[ResolvedOrUnresolvedExpr] = expr => write(expr.text) //todo

  given OutputJava[Block] = block => {
    write("{")
    block.stmts.foreach(output)
    write("}")
  }

  given OutputJava[Statement] = {
    case varDecl: LocalVar =>
      if (!varDecl.isProof && !varDecl.isGiven) write(varDecl.text)
    case other => write(other.text)
  }

  given OutputJava[Method] = mthd => {
    writeModifiers(mthd.modifiers)
    output(mthd.typeParams)
    mthd match {
      case nm: NormMethod =>
        output(mthd.returnType)
        write(" ")
        write(mthd.name)
      case ctor: Constructor =>
        write(ctor.cls.name)
    }
    output(mthd.params)
    mthd.body match {
      case Some(block) => output[Block](block)
      case None        => write(";")
    }
  }

  given OutputJava[Parameter] = param => {
    output(param.typ)
    write(" ")
    write(param.name)
  }

  given OutputJava[ParamList] = paramList => {
    if (paramList.kind != ParamListKind.PROOF) {
      write("(")
      if (paramList.kind == ParamListKind.GIVEN) {
        write("given ")
      }
      writeMultiple(paramList.params, sep = ",", end = ")")
    }
  }

  given OutputJava[Field] = field => {
    writeModifiers(field.modifiers)
    output(field.typ)
    write(" ")
    write(field.name)
    field.initExpr match {
      case Some(expr) =>
        write("=")
        output(expr)
      case _ =>
    }
    write(";")
  }

  given OutputJava[Type] = typ => {
    write(typ.text) //todo
  }

  given OutputJava[TypeParamList] = typeParams => {
    if (typeParams.params.nonEmpty) {
      writeMultiple(typeParams.params, "<", ",", ">")
    }
  }

  given OutputJava[TypeParam] = typeParam => {
    write(typeParam.name) //todo write bounds too
  }

  given OutputJava[Modifier] = mod => write(mod.text)

  given OutputJava[String] = write(_)

  def writeModifiers(mods: Iterable[Modifier])(using writer: Writer): Unit = {
    writeMultiple(mods, sep = " ", end = " ")
  }

  def writeMultiple[T](it: Iterable[T], start: String = "", sep: String = "", end: String = "")(
    using
    writer: Writer,
    writeElem: OutputJava[T]
  ): Unit = {
    write(start)
    var first = true
    for (elem <- it) {
      if (first) {
        first = false
      } else {
        write(sep)
      }
      writeElem(elem)
    }
    write(end)
  }

  def write(text: String)(using writer: Writer): Unit = writer.write(text)
}

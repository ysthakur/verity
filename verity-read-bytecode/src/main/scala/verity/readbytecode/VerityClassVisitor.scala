package verity.readbytecode

import verity.ast._
import verity.ast.infile._
import verity.ast.infile.{unresolved => ur}
import org.objectweb.asm
import org.objectweb.asm.signature.SignatureVisitor

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.ArrayBuffer

private inline val asmApi = asm.Opcodes.ASM9

private type PreField = infile.Field // (Int, String, String, Object)
private type PreMethod = infile.NormMethod //(Int, String, String, Array[String])

private class VerityClassVisitor(rootPkg: RootPkg) extends asm.ClassVisitor(asmApi) {
  private val fields = ArrayBuffer.empty[PreField]
  private val methods = ArrayBuffer[PreMethod]()
  private var access: Int = 0
  private var name: String | Null = _
  private var signature: String | Null = _
  private var superClass: String | Null = _
  private var interfaces: Array[String] = Array.empty

  override def visit(
    version: Int,
    access: Int,
    name: String,
    signature: String,
    superName: String,
    interfaces: Array[String]
  ): Unit = {
    this.access = access
    this.name = name
    this.signature = signature
    this.superClass = superName
    this.interfaces = interfaces

//    println(
//      s"In class! name=$name, superName=$superName, interfaces=${interfaces.mkString(",")}, sign=$signature"
//    )

    if (signature != null) {
      val signReader = asm.signature.SignatureReader(signature)
      signReader.accept(new asm.signature.SignatureVisitor(asmApi) {})
    }
  }

  override def visitField(
    access: Int,
    fieldName: String,
    descriptor: String,
    signature: String,
    value: Object
  ) = {
//    println(s"In field! name=$fieldName, desc=$descriptor, sign=$signature")
    val signReader = asm.signature.SignatureReader(signature)
    var fieldType: infile.Type = null

    if (signature != null) {
      signReader.acceptType(typeSignatureVisitor { typ =>
        fieldType = typ
      })
    } else {
      fieldType = asmTypeToVType(asm.Type.getType(descriptor))
    }

    new asm.FieldVisitor(asmApi) {
      override def visitEnd(): Unit = {
        fields += infile.Field(
          Text(fieldName),
          ArrayBuffer.empty,
          fieldType,
          None //TODO constants
        )//(access, descriptor, signature, value)
//        println(s"created field ${fields.last.text}!")
      }
    }
  }

  //TODO get modifiers, annotations, implicit parameters
  override def visitMethod(
    access: Int,
    methodName: String,
    descriptor: String,
    signature: String,
    exceptions: Array[String]
  ) = {
//    println(s"In method! name=$methodName, desc=$descriptor, sign=$signature")
    val methodType = asm.Type.getMethodType(descriptor)
    //TODO get type arguments of the return type using the method signature
    var returnType = asmTypeToVType(methodType.getReturnType)
    val paramTypes = methodType.getArgumentTypes.map(asmTypeToVType)
    val sigParamTypes = ArrayBuffer[infile.Type]()
    val exceptionTypes = ArrayBuffer[infile.Type]()

    if (signature != null) {
      val signReader = asm.signature.SignatureReader(signature)
      signReader.accept(new asm.signature.SignatureVisitor(asmApi) {
        override def visitTypeVariable(name: String) = {
//          println(s"methodvisitor typevar=$name")
        }
        override def visitParameterType(): SignatureVisitor = typeSignatureVisitor { typ =>
//          println(s"param type is $typ")
          sigParamTypes += typ
        }
        override def visitReturnType() = typeSignatureVisitor { typ =>
//          println(s"method return type is $typ")
          returnType = typ
        }
        override def visitExceptionType(): SignatureVisitor = typeSignatureVisitor { typ =>
//          println(s"exception type is $typ")
          exceptionTypes += typ
        }
      })
    }

    //A list of (parameter name, isFinal)
    val paramNamesAndMods = ArrayBuffer[(String, Boolean)]()

    new asm.MethodVisitor(asmApi) {
      override def visitParameter(name: String, access: Int): Unit =
        paramNamesAndMods += name -> (access == asm.Opcodes.ACC_FINAL)

      override def visitEnd(): Unit = {
        val usedParamTypes = if (signature == null) paramTypes.view else sigParamTypes.view
        val usedParamNamesAndMods =
          if (paramNamesAndMods.isEmpty) usedParamTypes.indices.map(i => ("_" + i, false))
          else paramNamesAndMods
        val paramList =
          infile.ParamList(
            usedParamNamesAndMods
              .lazyZip(usedParamTypes)
              .map { case ((name, isFinal), typ) =>
                infile.Parameter(Nil, typ, Text(name), false, false)
              }
              .toList,
            TextRange.synthetic
          )
//        println(s"paramlist=$paramList, sigparamtypes=$sigParamTypes,paramnames=$paramNamesAndMods")
        methods += new infile.NormMethod(
            ArrayBuffer(),
            infile.TypeParamList(Nil, TextRange.synthetic),
            returnType,
            Text(methodName),
            paramList,
            givenParams = None, //todo retrieve given parameters somehow
            proofParams = None,
            exceptionTypes,
            body = Some(infile.Block.empty(returnType))
          )
//        println(s"constructed method! ${methodMap(methodName).head.text}")
      }
    }
  }

  override def visitEnd(): Unit = {
    val metaclass = infile.ClasslikeType.CLASS //todo determine metaclass somehow

    val path = name.split("/")
    val simpleName = path.last

    getOrMakePkg(rootPkg, path.iterator) match {
      case Some(parentPkg) =>
        val (ctors, normMethods) = methods.partition(_.isCtor)

        val classDef = metaclass match {
          case infile.ClasslikeType.CLASS =>
            infile.ClassDef(
              ArrayBuffer.empty,
              ArrayBuffer.empty,
              simpleName,
              TypeParamList(Nil, TextRange.synthetic), //todo
              null,
              null,
              fields,
              ctors.asInstanceOf[ArrayBuffer[Constructor]],
              methods,
              TextRange.synthetic,
              TextRange.synthetic
            )
          case _ => null
        }

        parentPkg.files += FileNode(s"$simpleName.class", None, Nil, Seq(classDef), java.io.File(""))
      case None => throw Error("foo!@#sadf")
    }
  }
}

@annotation.tailrec
def getOrMakePkg(parent: Pkg, path: Iterator[String]): Option[Pkg] = {
  if (path.isEmpty) {
    Some(parent)
  } else {
    val name = path.next()
    val child = parent.subPkgs.find(_.name == name).getOrElse {
      val pkg = PkgNode(name, ArrayBuffer.empty, ArrayBuffer.empty, parent)
      parent.subPkgs += pkg
      pkg
    }

    getOrMakePkg(child, path)
  }
}

def asmTypeToVType(typ: asm.Type): infile.Type =
  ur.UnresolvedTypeRef(
    Seq(Text(typ.getClassName)),
    infile.TypeArgList(ArrayBuffer(), TextRange.synthetic)
  )

private def typeSignatureVisitor(onFind: infile.Type => Unit): SignatureVisitor =
  new SignatureVisitor(asmApi) {
    var isArrayType = false
    var arrayType: infile.Type = _

    var isTypeRef = false
    var typePath: String = _
    val typeArgs = ArrayBuffer[infile.Type]()

    override def visitArrayType(): SignatureVisitor = {
//      println("In array type!")
      this.isArrayType = true
      typeSignatureVisitor { typ => this.arrayType = typ }
    }

    /** Visit primitive or void type
      */
    override def visitBaseType(descriptor: Char): Unit = {
      onFind(
        asm.Type.getType(descriptor.toString).getSort match {
          case asm.Type.BOOLEAN => infile.PrimitiveType.BooleanType
          case asm.Type.BYTE    => infile.PrimitiveType.ByteType
          case asm.Type.CHAR    => infile.PrimitiveType.CharType
          case asm.Type.SHORT   => infile.PrimitiveType.ShortType
          case asm.Type.INT     => infile.PrimitiveType.IntType
          case asm.Type.FLOAT   => infile.PrimitiveType.FloatType
          case asm.Type.LONG    => infile.PrimitiveType.LongType
          case asm.Type.DOUBLE  => infile.PrimitiveType.DoubleType
          case asm.Type.VOID    => infile.VoidTypeRef(TextRange.synthetic)
        }
      )
    }

    override def visitTypeVariable(name: String): Unit = {
//      println(s"typesigvisitor typevar=$name")
    }

    /** Visit an unbounded wildcard type argument
      */
    override def visitTypeArgument(): Unit = {
      this.isTypeRef = true
      this.typeArgs += ur.UnresolvedWildcard(None, None)
//      println(s"typesigvisitor typearg")
    }

    /** Visit a type argument that may or may not be a wildcard
      */
    override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
      this.isTypeRef = true
      typeSignatureVisitor { typ =>
        this.typeArgs += ((wildcard: @unchecked) match {
          case '+' => ur.UnresolvedWildcard(Some(typ), None)
          case '-' => ur.UnresolvedWildcard(Some(typ), None)
          case '=' => typ
        })
//        println(s"typesigvisitor typearg, wildcard=$wildcard, name=$typePath,args=$typeArgs")
      }
    }

    override def visitClassType(name: String): Unit = {
      this.isTypeRef = true
//      println(s"typesigvis classtyp=$name")
      this.typePath = name
    }

    override def visitEnd(): Unit = {
      if (this.isArrayType) {
        onFind(infile.ArrayType(this.arrayType, TextRange.synthetic))
      } else if (this.isTypeRef) {
//        println(s"returning typeref $typePath")
        onFind(
          ur.UnresolvedTypeRef(
            //Make a path by splitting on '/'
            this.typePath.split("/").toSeq.map(Text(_)),
            infile.TypeArgList(this.typeArgs, TextRange.synthetic),
            None
          )
        )
      } else {
        throw new Error("uh-oh, new kind of type?")
      }
    }
  }

package verity.readbytecode

import verity.ast._
import verity.ast.infile.{unresolved => ur, _}
import org.objectweb.asm
import org.objectweb.asm.signature.SignatureVisitor

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.collection.mutable.ArrayBuffer

private inline val asmApi = asm.Opcodes.ASM9

private type PreField = Field // (Int, String, String, Object)
private type PreMethod = NormMethod //(Int, String, String, Array[String])

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
    var fieldType: Type = null

    if (signature != null) {
      signReader.acceptType(typeSignatureVisitor { typ =>
        fieldType = typ
      })
    } else {
      fieldType = asmTypeToVType(asm.Type.getType(descriptor))
    }

    new asm.FieldVisitor(asmApi) {
      override def visitEnd(): Unit = {
        fields += Field(
          Text(fieldName),
          ArrayBuffer.empty,
          fieldType,
          None //TODO constants
        ) //(access, descriptor, signature, value)
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
    val sigParamTypes = ArrayBuffer[Type]()
    val exceptionTypes = ArrayBuffer[Type]()

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
          ParamList(
            usedParamNamesAndMods
              .lazyZip(usedParamTypes)
              .map { case ((name, isFinal), typ) =>
                Parameter(Nil, typ, Text(name), false, false)
              }
              .toList,
            TextRange.synthetic
          )
//        println(s"paramlist=$paramList, sigparamtypes=$sigParamTypes,paramnames=$paramNamesAndMods")
        methods += new NormMethod(
          ArrayBuffer(),
          TypeParamList(Nil, TextRange.synthetic),
          returnType,
          Nil,
          Text(methodName),
          paramList,
          givenParams = None, //todo retrieve given parameters somehow
          proofParams = None,
          exceptionTypes,
          body = Some(Block.empty(returnType))
        )
//        println(s"constructed method! ${methodMap(methodName).head.text}")
      }
    }
  }

  override def visitEnd(): Unit = {
    val metaclass = ClasslikeType.CLASS //todo determine metaclass somehow

    val path = name.split("[/.]")

    getOrMakePkg(rootPkg, path.iterator) match {
      case Some(parentPkg -> simpleName) =>
        val (ctors, normMethods) = methods.partition(_.isCtor)

        val classDef = metaclass match {
          case ClasslikeType.CLASS =>
            ClassDef(
              ArrayBuffer.empty,
              ArrayBuffer.empty,
              simpleName.stripSuffix(".class"),
              TypeParamList(Nil, TextRange.synthetic), //todo
              BuiltinTypes.objectType,
              Array.empty[Type],
              fields,
              ctors.asInstanceOf[ArrayBuffer[Constructor]],
              methods,
              TextRange.synthetic,
              TextRange.synthetic
            )
          case _ => null
        }

        parentPkg.files += FileNode(s"$simpleName.class", None, Nil, Seq(classDef), None, Nil)
      case None => throw Error("foo!@#sadf")
    }
  }
}

@annotation.tailrec
def getOrMakePkg(parent: Pkg, path: Iterator[String]): Option[(Pkg, String)] = {
  val name = path.next()
  if (path.isEmpty) {
    //Since the rest of the path is empty, return (<parent package>, <this class's name>)
    Some(parent -> name)
  } else {
    val child = parent.subPkgs.find(_.name == name).getOrElse {
      val pkg = PkgNode(name, ArrayBuffer.empty, ArrayBuffer.empty, parent)
      parent.subPkgs += pkg
      pkg
    }

    getOrMakePkg(child, path)
  }
}

def asmTypeToVType(typ: asm.Type): Type = {
  typ.getSort match {
    case asm.Type.BOOLEAN => PrimitiveType.BooleanType
    case asm.Type.BYTE    => PrimitiveType.ByteType
    case asm.Type.CHAR    => PrimitiveType.CharType
    case asm.Type.SHORT   => PrimitiveType.ShortType
    case asm.Type.INT     => PrimitiveType.IntType
    case asm.Type.FLOAT   => PrimitiveType.FloatType
    case asm.Type.LONG    => PrimitiveType.LongType
    case asm.Type.DOUBLE  => PrimitiveType.DoubleType
    case asm.Type.VOID    => VoidTypeRef(TextRange.synthetic)
    case asm.Type.ARRAY   => ArrayType(asmTypeToVType(typ.getElementType), TextRange.synthetic)
    case asm.Type.OBJECT =>
      ur.UnresolvedTypeRef(
        typ.getClassName.split("[/.]").toSeq.map(Text(_)),
        TypeArgList(ArrayBuffer(), TextRange.synthetic)
      )
    case asm.Type.METHOD => throw new Error("Cannot turn method type to Verity type!")
  }
}

private def typeSignatureVisitor(onFind: Type => Unit): SignatureVisitor =
  new SignatureVisitor(asmApi) {
    var isArrayType = false
    var arrayType: Type = _

    var isTypeRef = false
    var typePath: String = _
    val typeArgs = ArrayBuffer[Type]()

    override def visitArrayType(): SignatureVisitor = {
//      println("In array type!")
      this.isArrayType = true
      typeSignatureVisitor { typ => this.arrayType = typ }
    }

    /** Visit primitive or void type
      */
    override def visitBaseType(descriptor: Char): Unit =
      onFind(asmTypeToVType(asm.Type.getType(descriptor.toString)))

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
        onFind(ArrayType(this.arrayType, TextRange.synthetic))
      } else if (this.isTypeRef) {
//        println(s"returning typeref $typePath")
        onFind(
          ur.UnresolvedTypeRef(
            //Make a path by splitting on '/'
            this.typePath.split("[/.]").toSeq.map(Text(_)),
            TypeArgList(this.typeArgs, TextRange.synthetic),
            None
          )
        )
      } else {
        throw new Error("uh-oh, new kind of type?")
      }
    }
  }

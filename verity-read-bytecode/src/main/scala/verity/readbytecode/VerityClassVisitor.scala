package verity.readbytecode

import verity.ast
import verity.ast.infile
import verity.ast.infile.{unresolved => ur}

import org.objectweb.asm
import org.objectweb.asm.signature.SignatureVisitor

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

private inline val asmApi = asm.Opcodes.ASM9

private type PreField = (Int, String, String, Object)
private type PreMethod = ur.UnresolvedNormMethod//(Int, String, String, Array[String])

private class VerityClassVisitor(
  classMap: mutable.HashMap[String, infile.Classlike],
  counter: AtomicInteger
) extends asm.ClassVisitor(asmApi) {
  private val fieldMap: mutable.Map[String, PreField] = mutable.HashMap()
  private val methodMap: mutable.Map[String, mutable.ListBuffer[PreMethod]] = mutable.HashMap()
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

    val signReader = asm.signature.SignatureReader(signature)
    signReader.accept(new asm.signature.SignatureVisitor(asmApi) {})
  }

  override def visitField(
    access: Int,
    name: String,
    descriptor: String,
    signature: String,
    value: Object
  ) = {
    val signReader = asm.signature.SignatureReader(signature)
    signReader.acceptType(new asm.signature.SignatureVisitor(asmApi) {

      override def visitEnd(): Unit = {}
    })
    new asm.FieldVisitor(asmApi) {
      override def visitEnd(): Unit = {
        fieldMap(name) = (access, descriptor, signature, value)
      }
    }
    // VerityFieldVisitor(access, name, descriptor, signature, value, fieldMap)
  }

  //TODO get modifiers, annotations, and thrown exceptions
  override def visitMethod(
    access: Int,
    methodName: String,
    descriptor: String,
    signature: String,
    exceptions: Array[String]
  ) = {
    val methodType = asm.Type.getMethodType(descriptor)
    //TODO get type arguments of the return type using the method signature
    val returnType = asmTypeToVType(methodType.getReturnType)
    val paramTypes = methodType.getArgumentTypes
    //A list of (parameter name, isFinal)
    val paramNamesAndMods = mutable.ListBuffer[(String, Boolean)]()

    //    val signReader = asm.signature.SignatureReader(signature)
    //    signReader.accept(new asm.signature.SignatureVisitor(asmApi) {
    //      override def visitReturnType(): SignatureVisitor = ???
    //
    //    })

    new asm.MethodVisitor(asmApi) {
      override def visitParameter(name: String, access: Int): Unit =
        paramNamesAndMods += name -> (access == asm.Opcodes.ACC_FINAL)

      override def visitEnd(): Unit = {
        val paramList =
          infile.ParamList(
            paramNamesAndMods
              .lazyZip(paramTypes)
              .map { case ((name, isFinal), typ) =>
                infile.Parameter(Nil, asmTypeToVType(typ), ast.Text(name), false, false)
              }
              .toList,
            ast.TextRange.synthetic
          )
        methodMap(methodName) = methodMap.getOrElse(methodName, mutable.ListBuffer.empty) :+
          new ur.UnresolvedNormMethod(
            mutable.ListBuffer(),
            infile.TypeParamList(Nil, ast.TextRange.synthetic),
            returnType,
            ast.Text(methodName),
            paramList,
            None,
            None,
            None
          )
      }
    }

    // VerityMethodVisitor(access, name, descriptor, signature, exceptions, methodMap)
  }

  override def visitEnd(): Unit = {
    import verity.ast.infile._

    val metaclass = ???
  }
}

private class VerityFieldVisitor(
  access: Int,
  name: String,
  descriptor: String,
  signature: String,
  value: Object,
  fieldMap: mutable.Map[String, PreField]
) extends asm.FieldVisitor(asmApi) {
  override def visitEnd(): Unit = {
    fieldMap(name) = (access, descriptor, signature, value)
  }
}

private class VerityMethodVisitor(
  access: Int,
  name: String,
  descriptor: String,
  signature: String,
  exceptions: Array[String],
  methodMap: mutable.Map[String, mutable.ListBuffer[PreMethod]]
) extends asm.MethodVisitor(asmApi) {
  override def visitEnd(): Unit = {
  }
}

def asmTypeToVType(typ: asm.Type): infile.Type =
  ur.UnresolvedTypeRef(
    Seq(ast.Text(typ.getClassName)),
    infile.TypeArgList(mutable.ListBuffer(), ast.TextRange.synthetic)
  )

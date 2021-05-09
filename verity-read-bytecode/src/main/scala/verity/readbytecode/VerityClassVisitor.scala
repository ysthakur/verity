package verity.readbytecode

import verity.ast
import verity.ast.infile

import org.objectweb.asm

import scala.collection.{mutable => mut}

private inline val asmApi = asm.Opcodes.ASM9

private type PreField = (Int, String, String, Object)
private type PreMethod = (Int, String, String, Array[String])

private class VerityClassVisitor(val classMap: mut.HashMap[String, infile.Classlike])
    extends asm.ClassVisitor(asmApi) {
  private var access: Int = 0
  private var name: String | Null = _
  private var signature: String | Null = _
  private var superClass: String | Null = _
  private var interfaces: Array[String] = Array.empty
  private val fieldMap: mut.Map[String, PreField] = mut.HashMap()
  private val methodMap: mut.Map[String, mut.ListBuffer[PreMethod]] = mut.HashMap()

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
    signReader.accept(new asm.signature.SignatureVisitor(asmApi) {
      
    })
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
      
    })
    VerityFieldVisitor(access, name, descriptor, signature, value, fieldMap)
  }

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String]
  ) =  {
    val signReader = asm.signature.SignatureReader(signature)
    signReader.accept(new asm.signature.SignatureVisitor(asmApi) {
      
    })
    VerityMethodVisitor(access, name, descriptor, signature, exceptions, methodMap)
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
    fieldMap: mut.Map[String, PreField]
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
    methodMap: mut.Map[String, mut.ListBuffer[PreMethod]]
) extends asm.MethodVisitor(asmApi) {
  override def visitEnd(): Unit = {
    if (!methodMap.contains(name))
      methodMap(name) = mut.ListBuffer.empty
    methodMap(name) += ((access, descriptor, signature, exceptions))
  }
}

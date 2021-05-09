package verity.readbytecode

import verity.ast
import verity.ast.infile
import org.objectweb.asm
import scala.collection.{mutable => mut}

private inline val asmApi = asm.Opcodes.ASM9

private class VerityClassVisitor extends asm.ClassVisitor(asmApi) {
  private var name: String = ""
  private var superClass: String | Null = null
  private var interfaces: Array[asm.Type] = Array.empty
  private val fieldMap: mut.Map[String, mut.ListBuffer[infile.Field]] = mut.HashMap()
  private val methodMap: mut.Map[String, mut.ListBuffer[infile.Method]] = mut.HashMap()

  override def visit(
      version: Int,
      access: Int,
      name: String,
      signature: String,
      superName: String,
      interfaces: Array[String]
  ): Unit = {
    this.superClass = asm.Type.getType(superName)
    this.interfaces = interfaces.map(asm.Type.getType)
  }

  override def visitField(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      value: Object
  ) =
    new asm.FieldVisitor(asmApi) {
      override def visitEnd(): Unit = {}
    }

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String]
  ) =
    new asm.MethodVisitor(asmApi) {
      override def visitEnd(): Unit = {}
    }

  override def visitEnd(): Unit = {}
}

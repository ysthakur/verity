package verity.readbytecode

import org.junit.Assert
import org.junit.Test
import org.junit.Before

class TestReadBytecode {
  @Test def readClassfile(): Unit = {
    val file = java.io.File("C:/Users/yasht/verity/verity-read-bytecode/src/test/resources/TestASM.class")
    ReadBytecode.readClassFile(file)
  }
}

package verity.compiler.parser

import verity.compiler.Result

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

/** Common trait for parser tests to give them helpers */
trait VerityParserTestSuite extends AnyFunSuite {
  def testResult[T](expected: T)(res: Result[Option[T]]): Assertion = {
    res.value match {
      case Some(actual) =>
        val errors =
            if (res.msgs.nonEmpty)
              "\nThe following error(s) were emitted: \n"
                + res.msgs.toList.mkString("\n")
            else ""
        assertResult(expected, errors)(actual)
      case None =>
        fail(
          "Parsing failed due to the following error(s): \n"
            + res.msgs.toList.mkString("\n")
        )
    }
  }
}

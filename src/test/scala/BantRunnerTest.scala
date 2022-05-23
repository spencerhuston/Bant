import org.scalatest.flatspec.AnyFlatSpec
import BantRunner.Main.parseCmdLine

class BantRunnerTest extends AnyFlatSpec {
  "parseCmdLine" must "fail if Bant source file does not exist" in {
    assert(parseCmdLine(Array("--debug")).isEmpty)
  }

  it should "pass if Bant source exists with --file" in {
    val filepath = "test.bnt"
    val config = parseCmdLine(Array("--file", filepath))
    assert(config.isDefined)

    config match {
      case Some(config) => assert(config.filepath == filepath)
      case _ => assert(false)
    }
  }

  it should "pass if Bant source exists with -f" in {
    val filepath = "test.bnt"
    val config = parseCmdLine(Array("-f", filepath))
    assert(config.isDefined)

    config match {
      case Some(config) => assert(config.filepath == filepath)
      case _ => assert(false)
    }
  }

  it must "fail if --file arg exists but Bant source does not" in {
    assert(parseCmdLine(Array("--file")).isEmpty)
  }

  it must "fail if -f arg exists but Bant source does not" in {
    assert(parseCmdLine(Array("-f")).isEmpty)
  }

  it should "pass regardless of arg order" in {
    assert(parseCmdLine(Array("-f", "test.bnt", "-d")).isDefined)
    assert(parseCmdLine(Array("-d", "-f", "test.bnt")).isDefined)
  }

  it should "fail if flag in between --file and Bant source filepath" in {
    assert(parseCmdLine(Array("-f", "-d", "test.bnt")).isEmpty)
  }
}

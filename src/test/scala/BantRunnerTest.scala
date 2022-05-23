import BantRunner.CmdLineParser.parseCmdLine
import BantRunner.FileReader.readBantSource
import BantRunner.Main
import org.scalatest.flatspec.AnyFlatSpec

class BantRunnerTest extends AnyFlatSpec {
  "CmdLineParser.parseCmdLine" must "fail if Bant source file does not exist" in {
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

  it should "fail if no args provided" in {
    assert(parseCmdLine(Array()).isEmpty)
  }

  "FileReader.readBantSource" must "fail if .bnt extension is not on source file" in {
    assert(readBantSource("test.txt").isEmpty)
  }

  it must "fail if source file does not exist" in {
    assert(readBantSource("test.bnt").isEmpty)
  }

  it must "pass if source String matches source file text" in {
    val bantSource = readBantSource("src/test/testPrograms/test.bnt") match {
      case Some(source) => source
      case _ => ""
    }
    assert(bantSource ==
      """val x = 5;
        |print(x)""".stripMargin)
  }

  "Main.main" should "print the source for a valid file" in {
    assert(Main.main(Array("-f", "src/test/testPrograms/test.bnt")) == ())
  }

  it should "fail on bad command line args" in {
    assert(Main.main(Array("-f", "-d", "src/test/testPrograms/test.bnt")) == None)
  }

  it should "fail on a bad source file" in {
    assert(Main.main(Array("-f", "test.bnt")) == None)
  }
}

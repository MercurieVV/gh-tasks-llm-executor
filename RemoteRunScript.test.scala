class RemoteRunScriptSuite extends munit.FunSuite:
  test("remote launcher includes every top-level production Scala source"):
    val script = os.read(os.pwd / "scripts" / "remote-run.sh")
    val fileBlock = script
      .linesIterator
      .dropWhile(_.trim != "FILES=(")
      .drop(1)
      .takeWhile(_.trim != ")")
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList

    val productionSources =
      os.list(os.pwd)
        .map(_.last)
        .filter(_.endsWith(".scala"))
        .filterNot(_.endsWith(".test.scala"))
        .filterNot(_ == "project.scala")
        .filterNot(_ == "project-remote.scala")
        .toList
        .sorted

    assertEquals(fileBlock.sorted, ("project-remote.scala" :: productionSources).sorted)

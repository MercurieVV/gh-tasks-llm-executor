package com.github.mercurievv.ghllm.cli

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

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

    val rootSources = os.list(os.pwd)
      .filter(os.isFile)
      .map(_.relativeTo(os.pwd).toString)

    val srcSources = if (os.exists(os.pwd / "src" / "main" / "scala")) {
      os.walk(os.pwd / "src" / "main" / "scala")
        .filter(os.isFile)
        .map(_.relativeTo(os.pwd).toString)
    } else {
      Nil
    }

    val productionSources = (rootSources ++ srcSources)
      .filter(_.endsWith(".scala"))
      .filterNot(_.endsWith(".test.scala"))
      .filterNot(_ == "project.scala")
      .filterNot(_ == "project-remote.scala")
      .toList
      .sorted

    assertEquals(fileBlock.sorted, ("project-remote.scala" :: productionSources).sorted)

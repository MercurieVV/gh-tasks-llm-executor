package com.github.mercurievv.ghllm.git

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.cli.TaskArtifact

class DependencyConclusionSuite extends munit.FunSuite:

  test("no dependencies renders no block"):
    assertEquals(GitHub.renderDependencyConclusions(Nil), None)

  test("short conclusions pass through untouched"):
    val rendered = GitHub.renderDependencyConclusions(
      List(TaskNumber(7) -> "Added the parser.", TaskNumber(9) -> "Wired it in.")
    )
    assertEquals(rendered, Some("- #7: Added the parser.\n- #9: Wired it in."))

  test("an oversized conclusion is truncated with the marker"):
    val huge = "x" * (TaskArtifact.DefaultMaxChars * 3)
    val rendered = GitHub.renderDependencyConclusions(List(TaskNumber(7) -> huge)).get
    assert(rendered.endsWith(TaskArtifact.Marker))
    assert(rendered.startsWith("- #7: xxx"))
    // Prefix + bound, not the raw comment.
    assertEquals(rendered.length, "- #7: ".length + TaskArtifact.DefaultMaxChars)

  test("each dependency gets its own budget"):
    // The point of bounding per dependency: one verbose parent must not be able
    // to truncate its siblings out of the block entirely.
    val huge = "x" * (TaskArtifact.DefaultMaxChars * 3)
    val rendered = GitHub
      .renderDependencyConclusions(
        List(TaskNumber(7) -> huge, TaskNumber(9) -> "Wired it in.")
      )
      .get
    assert(rendered.endsWith("- #9: Wired it in."))
    assert(rendered.contains(TaskArtifact.Marker))

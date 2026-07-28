package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.*
import com.github.mercurievv.ghllm.agent.*
import com.github.mercurievv.ghllm.arrow.*
import com.github.mercurievv.ghllm.cli.*
import com.github.mercurievv.ghllm.git.*
import com.github.mercurievv.ghllm.metrics.*

import cats.effect.IO
import munit.CatsEffectSuite

/** The assembly is cyclic: three leaves hold deferred references back to the fully assembled, fully logged program.
  * Forcing the whole structure must therefore terminate rather than recurse into itself.
  */
class WiringSuite extends CatsEffectSuite:
  test("the whole program assembles into a value without diverging"):
    IO(Wiring.businessLogic[IO]).map { logic =>
      assert(logic.program ne null)
      assert(logic.executeRecursive ne null)
      assert(logic.runCandidate ne null)
      assert(logic.executeSelectedCandidates ne null)
      assert(logic.executePreparedTaskInWorktree ne null)
    }

  test("every repair loop and resume path assembles too"):
    IO(Wiring.businessLogic[IO]).map { logic =>
      assert(logic.resumeExistingPullRequest ne null)
      assert(logic.publishChangedTask ne null)
      assert(logic.executeTaskArrows.runAgent.runAgent ne null)
      assert(logic.publicationArrows.publishRemote.publishRemote ne null)
      assert(logic.taskArrows.resumeExistingPullRequest.pullRequest.mergeExistingPullRequest ne null)
    }

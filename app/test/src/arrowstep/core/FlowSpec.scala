package arrowstep.core

import cats.Id
import cats.syntax.all.*

/** Smoke checks for the [[Flow]] arrow: category laws hold (delegated to Kleisli's lawful
  * `ArrowChoice`, so this only guards the opaque encoding), and the DESIGN §7 wiring shape
  * typechecks against stub signatures.
  */
final class FlowSpec extends munit.FunSuite:

  private val inc: Flow[Id, Int, Int] = Flow.lift(_ + 1)
  private val dbl: Flow[Id, Int, Int] = Flow.lift(_ * 2)
  private val show: Flow[Id, Int, String] = Flow.lift(_.toString)

  test("left identity: id >>> f === f") {
    val f = Flow.id[Id, Int] >>> inc
    (0 to 5).foreach(n => assertEquals(f.run(n), inc.run(n)))
  }

  test("right identity: f >>> id === f") {
    val f = inc >>> Flow.id[Id, Int]
    (0 to 5).foreach(n => assertEquals(f.run(n), inc.run(n)))
  }

  test("associativity: (f >>> g) >>> h === f >>> (g >>> h)") {
    val left = (inc >>> dbl) >>> show
    val right = inc >>> (dbl >>> show)
    (0 to 5).foreach(n => assertEquals(left.run(n), right.run(n)))
  }

  test("&&& fans the same input to both arrows") {
    val fan = inc &&& dbl
    assertEquals(fan.run(10), (11, 20))
  }

  test("*** runs both sides of a pair independently") {
    val par = inc *** show
    assertEquals(par.run((10, 4)), (11, "4"))
  }

  test("first carries the second component untouched") {
    assertEquals(inc.first[String].run((10, "keep")), (11, "keep"))
  }

  // DESIGN §7 wiring shape must typecheck against stub signatures.
  test("§7 birdview wiring typechecks") {
    final case class Detected(kind: String)
    final case class AskInput(q: String)
    final case class Valid(ans: String)
    final case class Plan(steps: Int)
    final case class Report(ok: Boolean)

    val inspect: Flow[Id, String, Detected] = Flow.lift(Detected(_))
    val questionsFor: Flow[Id, Detected, AskInput] = Flow.lift(d => AskInput(d.kind))
    val askValidated: Flow[Id, AskInput, Valid] = Flow.lift(a => Valid(a.q))
    val makePlan: Flow[Id, (Detected, Valid), Plan] = Flow.lift { case (_, v) => Plan(v.ans.length) }
    val applyPlan: Flow[Id, Plan, Report] = Flow.lift(p => Report(p.steps >= 0))

    val flow: Flow[Id, String, Report] =
      inspect
        >>> (Flow.id[Id, Detected] &&& (questionsFor >>> askValidated))
        >>> makePlan
        >>> applyPlan

    assertEquals(flow.run("scala"), Report(true))
  }

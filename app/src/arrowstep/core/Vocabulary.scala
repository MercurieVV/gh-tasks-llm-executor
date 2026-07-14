package arrowstep.core

/** The dialogue vocabulary — the L0 wire-protocol message set encoded as L1 types (DESIGN §2, §3).
  *
  * No JSON here: codecs are a runtime concern (P1), so `core` stays free of ujson.
  */

/** One question posed to the agent.
  *
  * @param id      stable key answers are written against (`.agents/answers.json`)
  * @param text    human-readable prompt (rendered to stderr by the runtime)
  * @param kind    free text or a closed choice
  * @param default value used when the agent gives none
  * @param current value already on record, if any (for re-ask / edit flows)
  * @param context extra guidance shown alongside the question
  */
final case class Question(
    id: String,
    text: String,
    kind: QuestionKind,
    default: Option[String],
    current: Option[String],
    context: Option[String]
)

/** Shape of an acceptable answer. */
enum QuestionKind:
  case FreeText
  case Choice(allowed: List[String])

/** A single validation failure, tied to the question that produced it. */
final case class Problem(questionId: String, message: String)

/** Raw agent output: question `id` -> answer string. Unvalidated by construction. */
opaque type Answers = Map[String, String]

object Answers:
  def apply(entries: Map[String, String]): Answers = entries

  extension (self: Answers)
    def get(id: String): Option[String] = self.get(id)
    def toMap: Map[String, String] = self

/** Validated agent output (D7). Opaque with **no public constructor**: the sole factory is
  * [[ValidAnswers.fromValidated]], deliberately `private[core]` so only a `Validator`
  * (p0_ask, #12) can mint one. Unvalidated answers therefore cannot reach the effects stage —
  * it is a compile error, not a convention.
  */
opaque type ValidAnswers = Map[String, String]

object ValidAnswers:
  /** The only door to a `ValidAnswers`. Kept `private[core]`; the intended sole caller is the
    * `Validator` introduced in #12.
    */
  private[core] def fromValidated(entries: Map[String, String]): ValidAnswers = entries

  extension (self: ValidAnswers)
    def get(id: String): Option[String] = self.get(id)
    def toMap: Map[String, String] = self

/** What a program hands the agent when it needs answers. */
final case class AskInput(questions: List[Question], context: Option[String])

/** The L0 message set as an ADT — the birdview encoded as a type. Adding a protocol state means
  * adding a case, and every interpreter fails to compile until it handles it (DESIGN §3).
  */
enum ProgramSays[+R]:
  case NeedInput(context: Option[String], questions: List[Question])
  case Rejected(problems: List[Problem], questions: List[Question])
  case Done(result: R)

object ProgramSays:
  /** Wire-protocol exit codes (L0): `Done` -> 0 (finished), otherwise -> 2 (need input). */
  extension [R](self: ProgramSays[R])
    def exitCode: Int = self match
      case ProgramSays.Done(_)         => 0
      case ProgramSays.NeedInput(_, _) => 2
      case ProgramSays.Rejected(_, _)  => 2

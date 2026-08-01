package com.github.mercurievv.ghllm.arrow

/** Mechanical check that every scope item of a task issue has an acceptance criterion to be held to.
  *
  * `POSTMORTEM-2026-07-31-gh-task-execution.md` failure 2: T20 listed three scope items and two acceptance criteria.
  * The third item — "ensure siblings are launched together, not staggered" — was never implemented, the task closed
  * green, and it was written by hand twelve hours later. The issue's own Notes/Risks section named the exact failure;
  * prose in an issue body stops nothing, only a criterion does.
  *
  * This cannot check that criterion *i* actually covers item *i* — that is judgment, and it is the evaluator's job.
  * What it can check is the arithmetic that made the gap possible: fewer criteria than scope items means at least one
  * item is being taken on trust. Cheap, mechanical, and it fails in the direction of asking rather than of silently
  * dropping work.
  */
object AcceptanceCoverage:

  final case class Coverage(scope: List[String], acceptance: List[String])

  private val ScopeNames = Set("scope", "scope of work", "in scope")
  private val AcceptanceNames =
    Set("acceptance", "acceptance criteria", "acceptance criterion", "done when", "definition of done")

  /** The scope and acceptance lists, or `None` when the body has no Scope section at all.
    *
    * A body with no Scope section is out of this check's contract, not in violation of it: short "ready" tasks that
    * state one change in prose are legitimate, and demanding the section from them would make the evaluator pad every
    * trivial issue into a four-heading template.
    */
  def of(body: String): Option[Coverage] =
    val lines = body.linesIterator.toList
    section(lines, ScopeNames).map(scopeLines =>
      Coverage(
        scope = items(scopeLines),
        acceptance = section(lines, AcceptanceNames).map(items).getOrElse(Nil)
      )
    )

  /** A report of what the body fails to cover, or `None` when it covers everything. The text is written to be handed
    * straight to the evaluator as repair instructions, or to a human as a question.
    */
  def shortfall(body: String): Option[String] =
    of(body).flatMap { coverage =>
      val scopeCount = coverage.scope.size
      val acceptanceCount = coverage.acceptance.size
      Option.when(scopeCount > 0 && acceptanceCount < scopeCount)(
        s"""This task has $scopeCount scope item(s) but only $acceptanceCount acceptance criterion(a), so at least
           |${scopeCount - acceptanceCount} scope item(s) can be skipped and the task will still close green.
           |
           |Scope items:
           |${coverage.scope.map(item => s"- $item").mkString("\n")}
           |
           |Acceptance criteria:
           |${if acceptanceCount == 0 then "- (none)" else coverage.acceptance.map(c => s"- $c").mkString("\n")}
           |
           |Give every scope item its own acceptance criterion, and state each one over the VALUES it accepts,
           |not over the shape of the change: "phase is one of {plan, source-of-truth, implement, test}", not
           |"a phase is passed" — a criterion that the wrong variable of the right type satisfies is not a
           |criterion.""".stripMargin
      )
    }

  // The lines belonging to the first section whose heading matches `names`,
  // heading excluded, ending at the next heading.
  private def section(lines: List[String], names: Set[String]): Option[List[String]] =
    lines.indexWhere(line => headingName(line).exists(names.contains)) match
      case -1  => None
      case idx => Some(lines.drop(idx + 1).takeWhile(line => headingName(line).isEmpty))

  // The heading a line declares, if it is one. Accepts the three forms task
  // bodies actually use: "## Scope", "**Scope**" and "Scope:".
  private def headingName(line: String): Option[String] =
    val trimmed = line.trim
    val undecorated =
      if trimmed.startsWith("#") then Some(trimmed.dropWhile(_ == '#').trim)
      else if trimmed.startsWith("**") && trimmed.endsWith("**") && trimmed.length > 4 then
        Some(trimmed.drop(2).dropRight(2).trim)
      else if trimmed.endsWith(":") && !isItem(trimmed) && trimmed.split("\\s+").length <= 4 then
        Some(trimmed.dropRight(1).trim)
      else None
    undecorated
      .map(_.stripSuffix(":").replace("*", "").trim.toLowerCase)
      .filter(_.nonEmpty)

  private val NumberedItem = "^\\d+[.)]\\s+.*".r

  private def isItem(line: String): Boolean =
    val trimmed = line.trim
    trimmed.startsWith("- ") || trimmed.startsWith("* ") || NumberedItem.matches(trimmed)

  // Top-level list items only: a nested bullet elaborates its parent item, it
  // is not a separate piece of scope, and counting it would let an evaluator
  // satisfy the check by indenting.
  private def items(sectionLines: List[String]): List[String] =
    sectionLines
      .filter(line => isItem(line) && line.takeWhile(_ == ' ').length < 2 && !line.startsWith("\t"))
      .map(_.trim.dropWhile(ch => ch.isDigit || ch == '.' || ch == ')' || ch == '-' || ch == '*').trim)
      .filter(_.nonEmpty)

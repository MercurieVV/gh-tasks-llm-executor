package com.github.mercurievv.ghllm.arrow

import com.github.mercurievv.ghllm.agent.TurnCapExceeded

enum VerificationResult:
  case Green
  case Red(summary: String, detail: String)
  case Failed(error: Throwable)

object VerificationResult:
  // A turn-cap kill is a verdict on the *attempt*, not on the tool, so it maps to
  // `Red` and carries its own summary. Everything else is infrastructure failure.
  def fromThrowable(error: Throwable): VerificationResult = error match
    case TurnCapExceeded(turnCount, cap) =>
      Red("turn cap exceeded", s"runner reported $turnCount turns against a cap of $cap")
    case other => Failed(other)

  extension (self: VerificationResult)
    def isGreen: Boolean = self match
      case Green => true
      case _     => false

    // Bounded seed for the escalated attempt: never the failed transcript.
    def escalationSeed: Option[String] = self match
      case Green     => None
      case Red(s, d) => Some(s"$s\n\n$d")
      case Failed(e) => Option(e.getMessage)

    // Boundary back to the `F[_]` error channel, for when escalation gives up.
    // `Failed` must round-trip its original throwable unchanged so callers that
    // match on a specific exception keep working.
    def asThrowable: Throwable = self match
      case Green         => RuntimeException("Green verification result surfaced as a failure")
      case Red(s, d)     => RuntimeException(s"$s\n$d")
      case Failed(error) => error

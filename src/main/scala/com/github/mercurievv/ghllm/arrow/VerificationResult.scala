package com.github.mercurievv.ghllm.arrow

enum VerificationResult:
  case Green
  case Red(summary: String, detail: String)
  case Failed(error: Throwable)

object VerificationResult:
  def fromThrowable(error: Throwable): VerificationResult = Failed(error)

  extension (self: VerificationResult)
    def isGreen: Boolean = self match
      case Green => true
      case _     => false

    // Bounded seed for the escalated attempt: never the failed transcript.
    def escalationSeed: Option[String] = self match
      case Green     => None
      case Red(s, d) => Some(s"$s\n\n$d")
      case Failed(e) => Option(e.getMessage)

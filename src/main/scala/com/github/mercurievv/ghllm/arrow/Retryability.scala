package com.github.mercurievv.ghllm.arrow

object Retryability:
  type RetryFlow[-->[_, _]] = [A, B] =>> (A --> B) => (A --> B)

  given retryFlowMonoid[-->[_, _]]: Monoid2[RetryFlow[-->]] with
    def empty[A, B]: RetryFlow[-->][A, B] =
      identity

    def combine[A, B](
        first: RetryFlow[-->][A, B],
        second: RetryFlow[-->][A, B]
    ): RetryFlow[-->][A, B] =
      arrow => first(second(arrow))

  def combine[T[_[_, _]], -->[_, _]](
      retry: T[RetryFlow[-->]],
      real: T[-->]
  )(using Apply2K[T]): T[-->] =
    Apply2K[T].apK(retry)(real)

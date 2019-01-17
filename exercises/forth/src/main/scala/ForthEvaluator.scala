import ForthError.ForthError

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value
}

trait ForthEvaluatorState {
  override def toString: String
}

trait ForthEvaluator {
  def eval(text: String): Either[ForthError, ForthEvaluatorState]
}

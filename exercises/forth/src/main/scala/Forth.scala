import ForthError.ForthError

class Forth extends ForthEvaluator {
  import Forth._

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    val words = text.split("\\s+").view.map(_.toUpperCase).toList

    words.foldLeft[Result[ForthState]](Right(initialState)) { (st, word) =>
      for {
        state <- st
        definition <- state.lookup(word)
        newState <- execute(definition)(state)
      } yield newState
    }
  }
}

object Forth {
  type Result[A] = Either[ForthError, A]

  sealed trait Definition

  case class NumLiteral(value: Int) extends Definition

  case class BuiltIn(exec: ForthState => Result[ForthState]) extends Definition

  case class ForthState(stack: List[Int], dictionary: Map[String, Definition]) extends ForthEvaluatorState {
    private[this] def isNumLiteral(word: String) = word.forall(_.isDigit)

    def lookup(word: String): Result[Definition] =
      if (isNumLiteral(word)) {
        Right(NumLiteral(word.toInt))
      } else {
        dictionary.get(word).toRight(ForthError.UnknownWord)
      }

    override def toString: String = stack.reverse.mkString(" ")
  }

  def modifyStack(exec: List[Int] => Result[List[Int]]): ForthState => Result[ForthState] = { state =>
    for { newStack <- exec(state.stack) } yield state.copy(stack = newStack)
  }

  def execute(definition: Definition): ForthState => Result[ForthState] = definition match {
    case NumLiteral(value) => modifyStack(stack => Right(value :: stack))
    case BuiltIn(exec)     => exec
  }

  private val initialState = ForthState(
    List.empty,
    Map[String, Definition](
      "+" -> BuiltIn(modifyStack {
        case a :: b :: rest => Right(a + b :: rest)
        case _              => Left(ForthError.StackUnderflow)
      }),
      "-" -> BuiltIn(modifyStack {
        case a :: b :: rest => Right(b - a :: rest)
        case _              => Left(ForthError.StackUnderflow)
      }),
      "*" -> BuiltIn(modifyStack {
        case a :: b :: rest => Right(a * b :: rest)
        case _              => Left(ForthError.StackUnderflow)
      }),
      "/" -> BuiltIn(modifyStack {
        case 0 :: _ :: _    => Left(ForthError.DivisionByZero)
        case a :: b :: rest => Right(b / a :: rest)
        case _              => Left(ForthError.StackUnderflow)
      }),
      "DUP" -> BuiltIn(modifyStack {
        case stack @ a :: _ => Right(a :: stack)
        case _              => Left(ForthError.StackUnderflow)
      }),
      "DROP" -> BuiltIn(modifyStack {
        case _ :: rest => Right(rest)
        case _         => Left(ForthError.StackUnderflow)
      }),
      "SWAP" -> BuiltIn(modifyStack {
        case a :: b :: rest => Right(b :: a :: rest)
        case _              => Left(ForthError.StackUnderflow)
      }),
      "OVER" -> BuiltIn(modifyStack {
        case stack @ _ :: b :: _ => Right(b :: stack)
        case _                   => Left(ForthError.StackUnderflow)
      })
    )
  )
}

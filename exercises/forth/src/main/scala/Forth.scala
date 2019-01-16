import ForthError.ForthError

class Forth extends ForthEvaluator {
  import Forth._

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    val words = text.split("\\s+").view.map(_.toUpperCase).toList

    words.foldLeft[Result[ForthState]](Right(initialState)) { (st, word) =>
      st.flatMap { state =>
        state.mode match {
          case Interpretation =>
            for {
              definition <- state.lookup(word)
              newState <- execute(definition)(state)
            } yield newState
          case NewDefinition =>
            if (isNumLiteral(word)) {
              Left(ForthError.InvalidWord)
            } else {
              Right(state.copy(mode = Compilation(word, Nil)))
            }
          case Compilation(name, definitions) =>
            if (word == ";") {
              Right(
                state.copy(mode = Interpretation, dictionary = state.dictionary + (name -> UserDefined(definitions))))
            } else {
              for {
                definition <- state.lookup(word)
              } yield state.copy(mode = Compilation(name, definition :: definitions))
            }
        }
      }
    }
  }
}

object Forth {
  type Result[A] = Either[ForthError, A]

  sealed trait Definition

  case class NumLiteral(value: Int) extends Definition

  case class BuiltIn(exec: ForthState => Result[ForthState]) extends Definition

  case class UserDefined(definitions: List[Definition]) extends Definition

  sealed trait Mode

  case object Interpretation extends Mode

  case object NewDefinition extends Mode

  case class Compilation(name: String, definitions: List[Definition]) extends Mode

  case class ForthState(mode: Mode, stack: List[Int], dictionary: Map[String, Definition]) extends ForthEvaluatorState {
    def lookup(word: String): Result[Definition] =
      if (isNumLiteral(word)) {
        Right(NumLiteral(word.toInt))
      } else {
        dictionary.get(word).toRight(ForthError.UnknownWord)
      }

    override def toString: String = stack.reverse.mkString(" ")
  }

  private def isNumLiteral(word: String) = word.forall(_.isDigit)

  def modifyStack(exec: List[Int] => Result[List[Int]]): ForthState => Result[ForthState] = { state =>
    for { newStack <- exec(state.stack) } yield state.copy(stack = newStack)
  }

  def execute(definition: Definition): ForthState => Result[ForthState] = definition match {
    case NumLiteral(value) => modifyStack(stack => Right(value :: stack))
    case BuiltIn(exec)     => exec
    case UserDefined(definitions) =>
      state0 =>
        definitions.foldRight[Result[ForthState]](Right(state0)) { (defn, state) =>
          state.flatMap(execute(defn))
        }
  }

  private val initialState = ForthState(
    Interpretation,
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
      }),
      ":" -> BuiltIn { state =>
        state.mode match {
          case Interpretation => Right(state.copy(mode = NewDefinition))
          case _              => Left(ForthError.InvalidWord)
        }
      }
    )
  )
}

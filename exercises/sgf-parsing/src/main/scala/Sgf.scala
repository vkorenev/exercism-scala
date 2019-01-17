import fastparse.NoWhitespace._
import fastparse._

object Sgf {

  type Tree[A] = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
  type Forest[A] = List[Tree[A]]
  case class Node[A](rootLabel: A, subForest: Forest[A] = List())

  // A tree of nodes.
  type SgfTree = Tree[SgfNode]

  // A node is a property list, each key can only occur once.
  // Keys may have multiple values associated with them.
  type SgfNode = Map[String, List[String]]

  def escapedChar[_: P]: P[String] =
    P("\\" ~ AnyChar.!.map {
      case "\n"                          => ""
      case s if s.charAt(0).isWhitespace => " "
      case s                             => s
    })
  def char[_: P]: P[String] =
    P(CharPred(_ != ']').!.map {
      case s if s.charAt(0).isWhitespace => " "
      case s                             => s
    })
  def text[_: P]: P[String] = P((escapedChar | char).rep.map(_.mkString))
  def propValue[_: P]: P[String] = P("[" ~/ text ~ "]")
  def propIdent[_: P]: P[String] = P(CharIn("A-Z").rep(1).!)
  def property[_: P]: P[(String, List[String])] = P(propIdent ~ propValue.rep(1).map(_.toList))
  def node[_: P]: P[SgfNode] = P(";" ~/ property.rep.map(_.toMap))
  def nodeList[_: P]: P[List[SgfTree]] = P(node.map(Node[SgfNode](_)).rep.map(_.toList))
  def treeList[_: P]: P[List[SgfTree]] = P(gameTree.rep.map(_.toList))
  def subTreeList[_: P]: P[List[SgfTree]] = P(nodeList ~ treeList).map { case (x, y) => x ++ y }
  def gameTree[_: P]: P[SgfTree] = P("(" ~/ node ~ subTreeList ~ ")").map((Node[SgfNode] _).tupled)
  def sgf[_: P]: P[SgfTree] = P(Start ~ gameTree ~ End)

  def parseSgf(text: String): Option[SgfTree] = {
    parse(text, sgf(_)) match {
      case Parsed.Success(value, _) => Some(value)
      case Parsed.Failure(_, _, _)  => None
    }
  }
}

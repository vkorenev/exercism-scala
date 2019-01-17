import scala.annotation.tailrec

object Zipper {
  // A zipper for a binary tree.
  type Zipper[A] = (BinTree[A], Ctx[A])

  // Get a zipper focussed on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = (bt, EmptyCtx())

  // Get the complete tree from a zipper.
  @tailrec
  def toTree[A](zipper: Zipper[A]): BinTree[A] = zipper match {
    case (tree, LeftCtx(value, left, upper))   => toTree((BinTree(value, left, Some(tree)), upper))
    case (tree, RightCtx(value, right, upper)) => toTree((BinTree(value, Some(tree), right), upper))
    case (tree, EmptyCtx())                    => tree
  }

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = zipper._1.value

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (BinTree(value, left, right), ctx) => left.map((_, RightCtx(value, right, ctx)))
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (BinTree(value, left, right), ctx) => right.map((_, LeftCtx(value, left, ctx)))
  }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (tree, LeftCtx(value, left, upper))   => Some((BinTree(value, left, Some(tree)), upper))
    case (tree, RightCtx(value, right, upper)) => Some((BinTree(value, Some(tree), right), upper))
    case (_, EmptyCtx())                       => None
  }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = zipper match {
    case (tree, ctx) => (tree.copy(value = v), ctx)
  }

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = zipper match {
    case (tree, ctx) => (tree.copy(left = l), ctx)
  }

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = zipper match {
    case (tree, ctx) => (tree.copy(right = r), ctx)
  }
}

// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])

sealed trait Ctx[A]

final case class LeftCtx[A](value: A, left: Option[BinTree[A]], upper: Ctx[A]) extends Ctx[A]

final case class RightCtx[A](value: A, right: Option[BinTree[A]], upper: Ctx[A]) extends Ctx[A]

final case class EmptyCtx[A]() extends Ctx[A]

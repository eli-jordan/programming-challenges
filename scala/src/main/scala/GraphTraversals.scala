import scala.annotation.tailrec
import scala.collection.immutable.Queue

object GraphTraversals extends App {

   object Tree {
      def apply[A](value: A, children: Tree[A]*): Tree[A] = {
         new Tree(value, children.toList)
      }
   }

   case class Tree[A](value: A, children: List[Tree[A]] = List.empty)

   class TreeNode(var _value: Int) {
      var value: Int = _value
      var left: TreeNode = null
      var right: TreeNode = null
   }

   def universalPath(tree: TreeNode): Int = {
      def universalPathRec(tree: TreeNode): List[Int] = {
         if(tree == null) {
            List.empty
         } else {
            val length = lengthFromNode(tree)
            List(length) ++ universalPathRec(tree.left) ++ universalPathRec(tree.right)
         }
      }

      val paths = universalPathRec(tree)
      if(paths.isEmpty) 0
      else paths.max
   }

   def lengthFromNode(tree: TreeNode): Int = {
      def lengthFrom(tree: TreeNode, value: Int): Int = {
         if(tree != null && tree.value == value) {
            1 + lengthFrom(tree.left, value) + lengthFrom(tree.right, value)
         } else {
            0
         }
      }

      if(tree == null) {
         0
      } else {
         lengthFrom(tree.left, tree.value) + lengthFrom(tree.right, tree.value)
      }
   }

   def breadthFirstTraversal[A](start: Tree[A]): List[A] = {
      @tailrec
      def bfsRec(values: List[A], queue: Queue[Tree[A]]): List[A] = {
         if (queue.isEmpty) values
         else {
            val (head, tail) = queue.dequeue
            bfsRec(values ++ List(head.value), tail.enqueue(head.children))
         }
      }

      bfsRec(List(start.value), start.children.to[Queue])
   }

   def depthFirstTraversal[A](start: Tree[A]): List[A] = {
      def dfsRec(node: Tree[A], values: List[A]): List[A] = {
         if (node.children.isEmpty) values ++ List(node.value)
         else {
            node.children.foldLeft(values ++ List(node.value)) {
               case (values, node) => dfsRec(node, values)
            }
         }
      }

      dfsRec(start, List.empty)
   }

   val tree = Tree("a", List(
      Tree("b1",
         Tree("c1",
            Tree("d1"),
            Tree("d2"))),
      Tree("b2",
         Tree("c2")),
      Tree("b3",
         Tree("c3"))
   ))

   depthFirstTraversal(tree).foreach(println)
}

import java.util

import scala.collection.mutable

object Challenge10 extends App {

   import scala.util.control.Breaks._
   import scala.io.Source

   def funnel(source: String, target: String): Boolean = {
      var sourceIdx = 0
      var targetIdx = 0
      var skipped = false

      breakable {
         while (sourceIdx < source.length && targetIdx < target.length) {
            if (source.charAt(sourceIdx) == target.charAt(targetIdx)) {
               sourceIdx = sourceIdx + 1
               targetIdx = targetIdx + 1
            } else if (!skipped) {
               sourceIdx = sourceIdx + 1
               skipped = true
            } else {
               break()
            }
         }
      }

      sourceIdx == source.length && targetIdx == target.length && skipped
   }

   require(funnel("leave", "eave") == true)
   require(funnel("reset", "rest") == true)
   require(funnel("dragoon", "dragon") == true)
   require(funnel("eave", "leave") == false)
   require(funnel("leave", "leave") == false)
   require(funnel("sleet", "lets") == false)
   require(funnel("skiff", "ski") == false)

   //   lazy val words = Source.fromInputStream(getClass.getResourceAsStream("words.txt")).getLines().toList
   //   lazy val wordsByLength = words.groupBy(_.length)
   //
   //   def bonus1(word: String): List[String] = {
   //      words.filter(in => funnel(word, in))
   //   }
   //
   //   println(bonus1("dragoon")) // => ["dragon"]
   //   println(bonus1("boats")) // => ["oats", "bats", "bots", "boas", "boat"]
   //   println(bonus1("affidavit")) // => []
   //
   //   wordsByLength(4).foreach(println)

   //   sealed trait Trie {
   //      def ensureChild(c: Char): Trie
   //      def child(c: Char): Option[Branch]
   //   }

   //   object Trie {
   //      def empty: Trie = Root(Map.empty)
   //   }
   //   case class Root(children: Map[Char, Branch]) extends Trie {
   //      def ensureChild(c: Char): Trie = {
   //         children.get(c) match {
   //            case Some(_) => this
   //            case None =>  this.copy(children = children + (c -> Branch(c, Map.empty)))
   //         }
   //      }
   //
   //      override def child(c: Char): Option[Branch] = children.get(c)
   //   }
   //   case class Branch(value: Char, children: Map[Char, Branch]) extends Trie {
   //      def ensureChild(c: Char): Branch = {
   //         children.get(c) match {
   //            case Some(_) => this
   //            case None =>  this.copy(children = children + (c -> Branch(c, Map.empty)))
   //         }
   //      }
   //
   //      override def child(c: Char): Option[Branch] = children.get(c)
   //   }

   case class Trie(value: Option[Char], children: Map[Char, Trie]) {
      def isRoot: Boolean = value.isEmpty

      private def ensureChild(c: Char): Trie = {
         children.get(c) match {
            case Some(_) => this
            case None => this.copy(children = children + (c -> Trie(Some(c), Map.empty)))
         }
      }

      def insert(path: List[Char]): Trie = path match {
         case Nil => this
         case x :: xs => {
            val newThis = ensureChild(x)
            val child = newThis.children(x)
            val newChild = child.insert(xs)
            newThis.copy(children = newThis.children + (newChild.value.get -> newChild))
         }
      }
   }

   object Trie {
      def empty: Trie = Trie(None, Map.empty)
   }

   def funnel(target: String, dict: Trie): List[String] = {
      val queue = new mutable.Queue[Trie]()
      queue.enqueue(dict.children.values.toList:_*)

      var i = 0

      while(!stack.isEmpty) {
         val n = stack.pop()
         if(n.value.get == target.charAt(i)) {
            n.children.values.foreach { stack.push }
            i = i + 1
         } else {

         }

      }
   }


   println(
      Trie.empty
          .insert("oats".toList)
          .insert("bats".toList)
          .insert("bots".toList)
          .insert("boas".toList)
          .insert("boat".toList)
   )

   //   def bonus2 = {
   //      words.map { word =>
   //         val candidates = wordsByLength.get(word.length - 1)
   //         candidates match {
   //            case Some(wds) => (word, wds.filter(in => funnel(word, in)))
   //            case None => (word, List.empty)
   //         }
   //      }.filter(_._2.length == 5)
   //   }

   //   val start = System.currentTimeMillis()
   //   println(bonus2)
   //   val end = System.currentTimeMillis()
   //   println(s"Took ${end - start} ms")
}

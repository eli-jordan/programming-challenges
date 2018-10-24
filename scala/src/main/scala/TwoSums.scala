
import scala.collection.mutable

object TwoSums {
   def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      val complements = mutable.HashMap[Int, Int]() // complement -> index in array
      var i = 0
      while (i < nums.length) {
         val element = nums(i)
         val complement = target - element

         complements.get(complement) match {
            case Some(idx) => return Array(idx, i)
            case None => complements.put(element, i)
         }

         i = i + 1
      }

      Array()
   }
}

object LongestSubstring {

   import scala.collection.mutable

   def lengthOfLongestSubstring(s: String): Int = {
      s.tails.map(pass).max
   }

   def pass(s: String): Int = {
      val seen = mutable.Set[Char]()
      var i = 0
      while (i < s.size) {
         val c = s.charAt(i)
         if (seen.contains(c)) {
            return i + 1
         }
         seen.add(c)
         i = i + 1
      }
      i + 1
   }
}

object LongestPalindrome extends App {

   // abba

   def longestPalindrome(s: String): String = {
      var longest = ""
      var i = 0
      while (i < s.length) {
         val on = expandFrom(s, i, i)
         val between =
            if (i + 1 > s.length) {
               ""
            } else {
               expandFrom(s, i, i + 1)
            }
         longest = List(longest, on, between).maxBy(_.length)
         i = i + 1
      }

      longest
   }

   def expandFrom(s: String, leftStart: Int, rightStart: Int): String = {
      var l = leftStart
      var r = rightStart
      while (l >= 0 && r < s.length && s.charAt(l) == s.charAt(r)) {
         l = l - 1
         r = r + 1
      }
      s.substring(l + 1, r)
   }

   println(expandFrom("cabac", 2, 2)) // "cabac"
   println(expandFrom("cabbac", 2, 3)) //"cabbac"

   println(expandFrom("aab", 0, 0)) // "a"
   println(expandFrom("aab", 0, 1)) // "aa"
   println(expandFrom("aabb", 3, 3)) // "b"
   println(expandFrom("aabb", 2, 3)) // "bb"

   println(longestPalindrome("babad")) // "aba" or "bab"
   println(longestPalindrome("cbbd")) // "bb"
   println(longestPalindrome("a")) // "a"
   println(longestPalindrome("")) // ""

}

object LongestValidParenthesisSubstring extends App {
   import scala.collection.mutable

   def longestValidParentheses(s: String): Int = {
      var max = 0
      var i = 0
      while(i < s.length) {
         val c = s.charAt(i)
         if(c == '(') {
            val fromC = longestFrom(s, i)
            max = math.max(max, fromC)
         }
         i = i + 1
      }
      max
   }

   def longestFrom(s: String, idx: Int): Int = {
      if(idx >= s.length || s.charAt(idx) != '(') {
         0
      } else {
         var counter = 1
         var i = idx + 1
         while(counter != 0 && i < s.length) {
            s.charAt(i) match {
               case ')' => counter = counter - 1
               case '(' => counter = counter + 1
            }
            i = i + 1
         }
         if(counter == 0) {
            (i - idx) + longestFrom(s, i)
         } else {
            0
         }
      }
   }

   println(longestValidParentheses(")()())"))
}

object BinarySearchAndInsert extends App {

   def searchInsert(nums: Array[Int], target: Int): Int = {
      var low = 0
      var hi = nums.length - 1
      while(low <= hi) {
         val mid = (hi + low) / 2
         val midValue = nums(mid)
         if(midValue == target) return mid

         if(midValue < target) {
            low = mid + 1
         } else {
            hi = mid - 1
         }
      }
      println(s"low=$low hi=$hi")
      low
   }

   // println(searchInsert(Array(1, 2, 3, 4, 5, 6), 5)) // 4
   // println(searchInsert(Array(1, 2, 3), 1)) // 0
   // println(searchInsert(Array(1, 2, 3), 3)) // 2
   // println(searchInsert(Array(1, 3, 5, 6), 5)) // 2
   println(searchInsert(Array(1, 3, 5, 6), 4)) // 2
   println(searchInsert(Array(1,3,5,6), 7)) // 4

}

object PeakFinding extends App {
   def findPeakElement(nums: Array[Int]): Int = {
      var lo = 0
      var hi = nums.length - 1
      while(lo <= hi) {
         val mid = (lo + hi) / 2

         val midValue = nums(mid)
         val rightValue = if(mid + 1 >= nums.length) {
            Int.MinValue
         } else {
            nums(mid + 1)
         }

         val leftValue = if(mid - 1 < 0) {
            Int.MinValue
         } else {
            nums(mid - 1)
         }

         if(rightValue > midValue) {
            lo = mid + 1
         } else if(leftValue > midValue) {
            hi = mid - 1
         } else {
            return mid
         }
      }
      -1
   }

   println(findPeakElement(Array(1)))
}


object Search2DMatrix extends App {
   def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
      val rows = matrix.length
      val cols = matrix(0).length // Assume there is at least one row

      var lo = 0
      var hi = (rows * cols) - 1
      while(lo <= hi) {
         val mid = (lo + hi) / 2
         val (rowIdx, colIdx) = coords(mid, rows = rows, cols = cols)

         val midValue = matrix(rowIdx)(colIdx)
         if(midValue < target) {
            lo = mid + 1
         } else if(midValue > target) {
            hi = mid - 1
         } else {
            return true
         }
      }
      false

   }

   def coords(i: Int, rows: Int, cols: Int): (Int, Int) = {
      val colIdx = i % cols
      val rowIdx = (i - colIdx) / cols
      (rowIdx, colIdx)
   }

   // 0  1  2  3
   // 4  5  6  7
   // 8  9  10 11

   require(coords(0, rows = 3, cols = 4) == (0, 0), "0")
   require(coords(1, rows = 3, cols = 4) == (0, 1), "1")
   require(coords(2, rows = 3, cols = 4) == (0, 2), "2")
   require(coords(3, rows = 3, cols = 4) == (0, 3), "3")

   require(coords(4, rows = 3, cols = 4) == (1, 0), "4")
   require(coords(5, rows = 3, cols = 4) == (1, 1), "5")
   require(coords(6, rows = 3, cols = 4) == (1, 2), "6")
   require(coords(7, rows = 3, cols = 4) == (1, 3), "7")

   require(coords(8,  rows = 3, cols = 4) == (2, 0), "8")
   require(coords(9,  rows = 3, cols = 4) == (2, 1), "9")
   require(coords(10, rows = 3, cols = 4) == (2, 2), "10")
   require(coords(11, rows = 3, cols = 4) == (2, 3), "11")
}

object Foo {
   def kSmallestPairs(nums1: Array[Int], nums2: Array[Int], k: Int): List[Array[Int]] = {
      val c = for {
         one <- nums1.take(k)
         two <- nums2.take(k)
      } yield Array(one, two)

      c.sortBy(_.sum).take(k).toList
   }
}

object GenerateMatchingParens extends App {

   // ( -> +1
   // ) -> -1

   val OPEN = "(".charAt(0)
   val CLOSE = ")".charAt(0)

   def generateParenthesis(n: Int): List[String] = {
      generate(n, 0, 0, 0, List.empty)
   }

   def generate(n: Int, opens: Int, closes: Int, count: Int, elems: List[Char]): List[String] = {
      if(n == 0) {
         List.empty
      } else if(opens == n) {
         if(count - (n - closes) == 0) {
            List((elems ++ List.fill(n - closes)(CLOSE)).mkString)
         } else {
            List.empty
         }
      } else if(closes == n) {
         if(count == 0) {
            List(elems.mkString)
         } else {
            List.empty
         }
      } else {
         val openBranch  = generate(n, opens + 1, closes, count + 1, elems ++ List(OPEN))
         val closeBranch = if(count - 1 >= 0) {
            generate(n, opens, closes + 1, count - 1, elems ++ List(CLOSE))
         } else {
            List.empty
         }
         openBranch ++ closeBranch
      }
   }

   (0 to 5).foreach { n =>
      println(s"Answer for $n")
      generateParenthesis(n).foreach(println)
   }
}


object Challenge6 extends App {

   final case class LongestPalindrome(longest: String) extends AnyVal

   def longestPalindrome(input: String): LongestPalindrome = {
      def foldStep(longest: LongestPalindrome, idx: Int): LongestPalindrome = {
         val on = expandFrom(input, idx, idx)
         val between = if (idx + 1 > input.length) "" else expandFrom(input, idx, idx + 1)
         LongestPalindrome(List(longest.longest, on, between).maxBy(_.length))
      }

      (0 until input.length).foldLeft(LongestPalindrome(""))(foldStep)
   }

   private def expandFrom(input: String, leftStart: Int, rightStart: Int): String = {
      var l = leftStart
      var r = rightStart
      while (l >= 0 && r < input.length && input.charAt(l) == input.charAt(r)) {
         l = l - 1
         r = r + 1
      }
      input.substring(l + 1, r)
   }
}

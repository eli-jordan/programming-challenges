import scala.collection.mutable

object Tweets extends App {


   /*

       limit=15

       3, 5, 5, 3, 6, 4, 3, 4, 3
                   ^
       [ 3 5 (+3) = 11 ] -> [ 5 3 (+3) = 11 ]
                         -> [ 5 3 (+4) = 12 ]
                         -> [ 5 3 (+5) = 13 ]

       [ 3 5 (+4) = 12 ] -> [
       [ 3 5 (+5) = 13 ]

    */

   /*
     abcabc defdef ghighi jkljkl
           6      13     20     26

     List(6, 6, 6, 6)

     digits = 1 => 3 always
     digits = 2 => 4 (1 - 9)
                   5 (10)
     digits = 3 => 5 (1 - 9)
                   6 (10 - 99)
                   7 (100 - 999)
     digits = 4 => 6 (1 - 9)
                   7 (10 - 99)
                   8 (100 - 999)
                   9 (1000 - 9999)

     digits = 5 => 7 (1 - 9)
                   8 (10 - 99)
                   9 (100 - 999)
                   10 (1000 - 9999)
                   11 ...

     1/10000
     2/100
     10/100
    */

   def suffixLength(chunk: Int, digits: Int): Int = {
      chunk.toString.length + digits + 2
   }



//   def chunkify(charCount: Int, limit: Int, wordLengths: List[Int]): List[(Int, Int)] = {
//      val guessDigits = (charCount / limit).toString.length
//
//      ???
//   }

   // digits = number of digits in the total number of chunks
   case class RangeTracker(
      breakIndices: List[Int] = List.empty,
      currentStart: Int = 0,
      currentLength: Int = 0
   )
   def chunkifyAssumingDigits(digits: Int, limit: Int, wordLengths: List[Int]): List[Int] = {
      val folded = wordLengths.zipWithIndex.foldLeft(RangeTracker()) { case (tracker, (wordLength, wordIndex)) =>
         if(tracker.currentLength + wordLength + 1 + suffixLength(tracker.breakIndices.size, digits) <= limit) {
            tracker.copy(
               currentLength = tracker.currentLength + wordLength
            )
         } else {
            tracker.copy(
               breakIndices = (wordIndex - 1) :: tracker.breakIndices,
               currentStart = wordIndex,
               currentLength = wordLength
            )
         }
      }
      val finished = (wordLengths.size - 1) :: folded.breakIndices
      finished.reverse
   }

   def joinChunks(words: Iterator[String], markers: List[Int]): Unit = {
      val chunks = markers.size
      markers.zipWithIndex.foreach { case (number, idx) =>
         val chunk = words.take(number).mkString(" ") ++ s" ${idx + 1}/$chunks"
         println(chunk)
      }
   }

   val words = split("the quick brown fox jumped over the lazy dog")
   val sizes = words.map(_.length)

   joinChunks(words.iterator, chunkifyAssumingDigits(1, 15, sizes))

   // "the quick brown fox jumped over the lazy dog"
//   println(chunkifyAssumingDigits(2, 15, List(3, 5, 5, 3, 6, 4, 3, 4, 3)))

   // 140
   //  "ab cd ef gh ij kl mn op qr st uv" limit=6
   //  "ab 1/3" "de 2/3" "de 3/3"
   //
   //
   def splitIntoSms(text: String, limit: Int): List[String] = {
      val words = split(text)

      val messages = mutable.Buffer[String]()

      val current = mutable.Buffer[String]()
      var totalChars = 0
      words.foreach { word =>
         if(totalChars + word.length + 3 <= limit) {
            current.append(word)
            totalChars = totalChars + word.length + 1
         } else {
            messages.append(current.mkString(" "))
            current.clear()
            current.append(word)
            totalChars = word.length + 4
         }
      }

      messages.append(current.mkString(" "))

      val count = messages.size
      messages.toList.zipWithIndex.map { case (message, idx) => s"$message ${idx + 1}/$count" }
   }

   def split(text: String): List[String] =
      text.split("\\s+").toList

//   println(splitIntoSms("the quick brown fox jumped over the lazy dog the quick brown fox jumped over the lazy dog the quick brown fox jumped over the lazy dog", 15))

}


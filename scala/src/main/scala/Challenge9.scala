
import java.util

import scala.concurrent.duration._

object Challenge9 extends App {
   val MAX_LUCKY = 1000000

   val start = System.nanoTime()

   val OddNumbers: util.ArrayList[Int] = new util.ArrayList[Int]()

   // Init the odd numbers
   {
      var i = 0
      while(i < MAX_LUCKY) {
         OddNumbers.add(i * 2 + 1)
         i = i + 1
      }
   }

   object AllDone extends Exception { }

   try {
      var iteration = 1
      while (true) {
         val by = OddNumbers.get(iteration)
         if(by > OddNumbers.size()) throw AllDone
         var i = by
         while (i < OddNumbers.size()) {
            OddNumbers.remove(i - 1)
            i = i + by
         }
         iteration = iteration + 1
      }
   } catch {
      case AllDone => println("All done!")
   }

//   var i = 1
//   while(i < OddNumbers.length) {
//      val n = OddNumbers(i)
//      n.nextIndex = i
//      i = i + 1
//   }
//
////   var i = 0
////   while(i < numbers.length) {
////      val x = numbers(i)
////      i = i + 1
////   }
//
   val end = System.nanoTime()

//   println(OddNumbers.asScala.toList)

   println((end - start).nanos.toMillis + "ms")

}

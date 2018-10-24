import javax.sound.midi._

import scala.concurrent.{Future, Promise}
import scala.io.Source
import scala.util.{Random, Success}

// http://www.java2s.com/Code/JavaAPI/javax.sound.midi/MidiSystemwriteSequenceininttypeFileout.htm

case class Note(name: String, relativePitch: Int)

object Note {
   val  C   = Note("C",  0)
   val `C#` = Note("C#", 1)
   val  D   = Note("D",  2)
   val `D#` = Note("D#", 3)
   val  E   = Note("E",  4)
   val  F   = Note("F",  5)
   val `F#` = Note("F#", 6)
   val  G   = Note("G",  7)
   val `G#` = Note("G#", 8)
   val  A   = Note("A",  9)
   val `A#` = Note("A#", 10)
   val  B   = Note("B",  11)

   val Values = List(C, `C#`, D, `D#`, E, F, `F#`, G, `G#`, A, `A#`, B)

   val RelativePitch: Map[Note, Int] = Map(
       C   -> 0,
      `C#` -> 1,
       D   -> 3,
      `D#` -> 4,
       E   -> 5,
       F   -> 6,
      `F#` -> 7,
       G   -> 8,
      `G#` -> 9,
       A   -> 10,
      `A#` -> 11,
       B   -> 12
   )
}

import Note._

case class Pitch(note: Note, octave: Int) {
   def intValue: Int =
      octave * 12 + RelativePitch(note)
}

object Pitch {
   /**
    * Parse a pitch string in the format {note}{octave}
    *
    * e.g. F3 F#2
    */
   def parse(pitch: String): Pitch = {
      val chars = pitch.toList
      if (chars.length == 2) {
         val octave = s"${chars(1)}".toInt
         val noteString = s"${chars(0)}"
         val note = Note.Values.find(_.name == noteString).get
         Pitch(note = note, octave = octave)
      } else if (chars.length == 3) {
         val octave = s"${chars(2)}".toInt
         val noteString = s"${chars(0)}${chars(1)}"
         val note = Note.Values.find(_.name == noteString).get
         Pitch(note = note, octave = octave)
      } else {
         throw new Exception()
      }
   }
}

case class Sound(pitch: Pitch, startBeat: Double, duration: Double) {
   override def toString: String = s"$pitch $startBeat $duration"
}

case class Measure(sounds: List[Sound]) {
   def normalize(measure: Int): Measure =
      Measure(sounds.map { s => s.copy(startBeat = s.startBeat - measure * 3) })

   override def toString: String =
      sounds.map(_.toString).mkString("\n")
}

object Challenge5 extends App {
   val MeasuresTable = Vector(
      Vector(96, 32, 69, 40, 148, 104, 152, 119, 98, 3, 54),
      Vector(22, 6, 95, 17, 74, 157, 60, 84, 142, 87, 130),
      Vector(141, 128, 158, 113, 163, 27, 171, 114, 42, 165, 10),
      Vector(41, 63, 13, 85, 45, 167, 53, 50, 156, 61, 103),
      Vector(105, 146, 153, 161, 80, 154, 99, 140, 75, 135, 28),
      Vector(122, 46, 55, 2, 97, 68, 133, 86, 129, 47, 37),
      Vector(11, 134, 110, 159, 36, 118, 21, 169, 62, 147, 106),
      Vector(30, 81, 24, 100, 107, 91, 127, 94, 123, 33, 5),
      Vector(70, 117, 66, 90, 25, 138, 16, 120, 65, 102, 35),
      Vector(121, 39, 136, 176, 143, 71, 155, 88, 77, 4, 20),
      Vector(26, 126, 15, 7, 64, 150, 57, 48, 19, 31, 108),
      Vector(9, 56, 132, 34, 125, 29, 175, 166, 82, 164, 92),
      Vector(112, 174, 73, 67, 76, 101, 43, 51, 137, 144, 12),
      Vector(49, 18, 58, 160, 136, 162, 168, 115, 38, 59, 124),
      Vector(109, 116, 145, 52, 1, 23, 89, 72, 149, 173, 44),
      Vector(14, 83, 79, 170, 93, 151, 172, 111, 8, 78, 131)
   )

   val rand = new Random()

   def sounds(lines: List[String]): List[Sound] = {
      lines.map { line =>
         val Array(note, start, duration) = line.split("\\s")
         Sound(Pitch.parse(note), start.toDouble + 1, duration.toDouble)
      }
   }

   def measures(sounds: List[Sound]): Map[Int, Measure] = {
      sounds.groupBy { sound => sound.startBeat.toInt / 3 }
          .map { case (measure, ss) => (measure, Measure(ss.sortBy(_.startBeat)).normalize(measure)) }
   }

   def randomMeasures: List[Int] = (0 until 16).toList map { row: Int =>
      val col = rand.nextInt(6) + rand.nextInt(6)
      MeasuresTable(row)(col)
   }

   def composition(sourceMeasures: Map[Int, Measure]): List[Measure] = {
      val random = (0 to 100).flatMap(_ => randomMeasures)
      val measures = random.map { n =>
         sourceMeasures(n)
      }

      measures.zipWithIndex.map { case (Measure(sounds), idx) =>
         Measure(sounds.map { s => s.copy(startBeat = s.startBeat + idx * 3) })
      }.toList
   }

   val run: List[String] => List[Measure] = sounds _ andThen measures andThen composition

   val sourceLines = Source.fromInputStream(getClass.getResourceAsStream("challenge-5-mozart-dice-starting.txt")).getLines().toList

   def newComposition: List[Sound] = {
      run(sourceLines).flatMap(_.sounds).map { sound =>
         sound.copy(startBeat = sound.startBeat * 4, duration = sound.duration * 4)
      }
   }

   def originalComposition: List[Sound] = sounds(sourceLines).map { sound =>
      sound.copy(startBeat = sound.startBeat * 4, duration = sound.duration * 4)
   }

   MidiPlayer.play(0, originalComposition)

//   val c1 = newComposition
//   val c2 = newComposition
//   val c3 = newComposition
//   val cc = newComposition

//   MidiPlayer.play(instrument = 114, c1)
//   MidiPlayer.play(instrument = 115, c2)
//   MidiPlayer.play(instrument = 122, c3)
}

object MidiPlayer {

   val END_OF_TRACK = 47

   def play(instrument: Int, sounds: List[Sound]): Future[Unit] = {
      val promise = Promise[Unit]()

      val player = MidiSystem.getSequencer
      player.open()
      val sequence = new Sequence(Sequence.PPQ, 4)

      player.setTempoInBPM(120)

      // Let us know when it is done playing
      player.addMetaEventListener((m: MetaMessage) => {
         if (m.getType == END_OF_TRACK) {
            player.close()
            promise.complete(Success(()))
         }
      })

      val track = sequence.createTrack()

      setInstrument(track, instrument)

      for (sound <- sounds) {
         addNote(
            track = track,
            startTick = sound.startBeat.toInt,
            tickLength = sound.duration.toInt,
            key = sound.pitch.intValue,
            velocity = 60
         )
      }

      player.setSequence(sequence)
      player.start()
      promise.future
   }

   def setInstrument(track: Track, instrument: Int): Unit = {
      val sm = new ShortMessage
      sm.setMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0)
      track.add(new MidiEvent(sm, 0))
   }

   def addNote(track: Track, startTick: Int, tickLength: Int, key: Int, velocity: Int): Unit = {
      val on = new ShortMessage
      on.setMessage(ShortMessage.NOTE_ON, 0, key, velocity)

      val off = new ShortMessage
      off.setMessage(ShortMessage.NOTE_OFF, 0, key, velocity)

      track.add(new MidiEvent(on, startTick))
      track.add(new MidiEvent(off, startTick + tickLength))
   }
}

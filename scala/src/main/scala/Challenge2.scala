
import cats.effect._
import cats.implicits._
import fs2.{Stream, io, text}

object Challenge2 extends App {

   case class Message(id: Int, position: Int, size: Int, text: String)

   case class Packet(messages: List[Message] = List.empty, size: Int) {
      def insert(msg: Message): Packet =
         this.copy(messages = msg :: messages)

      def finished: Boolean = messages.size == size

      def format: String = messages.sortBy(_.position)
          .map { msg => s"${msg.id}\t${msg.position}\t${msg.size}\t${msg.text}" }
          .mkString("\n")
   }

   def parseLine[F[_] : Sync](line: String): F[Message] = Sync[F].delay {
      val Array(id, position, size, text) = line.split("\\s", 4)
      Message(id.toInt, position.toInt, size.toInt, text)
   }

   case class PacketTracker(incomplete: Map[Int, Packet] = Map.empty) {
      def receive(msg: Message): (PacketTracker, Option[Packet]) = {
         incomplete.get(msg.id) match {
            case Some(packet) => {
               val newPacket = packet.insert(msg)
               if (newPacket.finished) {
                  (this.copy(incomplete = incomplete - msg.id), Some(newPacket))
               } else {
                  (this.copy(incomplete = incomplete + (msg.id -> newPacket)), None)
               }
            }
            case None => (
                this.copy(incomplete = incomplete + (msg.id -> Packet(messages = List(msg), size = msg.size))),
                None
            )
         }
      }
   }

   def putStrLn[F[_] : Sync](str: String): F[Unit] =
      Sync[F].delay(println(str))

   def packetProgram[F[_] : Sync](lines: Stream[F, String]): Stream[F, Unit] = {
      lines
          .evalMap(parseLine[F])
          .evalScan(PacketTracker()) { (tracker, message) =>
             tracker.receive(message) match {
                case (tracker, Some(packet)) => putStrLn(packet.format).map(_ => tracker)
                case (tracker, None) => tracker.pure[F]
             }
          }
          .map(_ => ())
   }

   def testLines[F[_]]: Stream[F, String] = Stream(
      "6220	1	10	Because he's the hero Gotham deserves, ",
      "6220	9	10	 ",
      "5181	5	7	in time, like tears in rain. Time to die.",
      "6220	3	10	So we'll hunt him. ",
      "6220	5	10	Because he's not a hero. ",
      "5181	6	7	 ",
      "5181	2	7	shoulder of Orion. I watched C-beams ",
      "5181	4	7	Gate. All those moments will be lost ",
      "6220	6	10	He's a silent guardian. ",
      "5181	3	7	glitter in the dark near the Tannhäuser ",
      "6220	7	10	A watchful protector. ",
      "5181	1	7	believe. Attack ships on fire off the ",
      "6220	0	10	We have to chase him. ",
      "5181	0	7	I've seen things you people wouldn't ",
      "6220	4	10	Because he can take it. ",
      "6220	2	10	but not the one it needs right now. ",
      "6220	8	10	A Dark Knight."
   )

   def challengeLines[F[_]]: Stream[F, String] = Stream(
      "7469	1	7	believe. Attack ships on fire off the ",
      "9949	6	10	He's a silent guardian. ",
      "2997	9	19	Force is a pathway to many abilities some",
      "6450	2	11	is a vestige of the vox populi, now vacant, vanished. However, this valorous ",
      "6450	10	11	 ",
      "6450	8	11	veers most verbose, so let me simply add that it's my very good honour to meet ",
      "6450	5	11	and voracious violation of volition! The only verdict is vengeance; a vendetta ",
      "9949	1	10	Because he's the hero Gotham deserves, ",
      "6450	1	11	and villain by the vicissitudes of fate. This visage, no mere veneer of vanity, ",
      "2997	13	19	he did. Unfortunately, he taught his",
      "9949	8	10	A Dark Knight. ",
      "1938	4	17	by the iniquities of the selfish and the ",
      "1938	0	17	You read the Bible, Brett? Well there's ",
      "2997	0	19	Did you ever hear the tragedy of Darth",
      "2997	1	19	Plagueis the Wise? I thought not. It's not a",
      "1938	8	17	of darkness, for he is truly is brother's ",
      "2997	14	19	apprentice everything he knew, then his",
      "6450	3	11	visitation of a bygone vexation stands vivified, and has vowed to vanquish these ",
      "1938	12	17	who attempt to poison and destroy my ",
      "6450	9	11	you and you may call me V.",
      "7469	2	7	shoulder of Orion. I watched C-beams ",
      "2997	10	19	consider to be unnatural. He became so ",
      "1938	1	17	this passage I got memorized, sorta fits ",
      "2997	5	19	Force to influence the midichlorians to",
      "1938	6	17	in the name of charity and good will, ",
      "7469	0	7	I've seen things you people wouldn't ",
      "9949	4	10	Because he can take it. ",
      "6450	7	11	vindicate the vigilant and the virtuous. Verily, this vichyssoise of verbiage ",
      "9949	0	10	We have to chase him. ",
      "9949	7	10	A watchful protector. ",
      "2997	3	19	legend. Darth Plagueis was a Dark Lord of the",
      "6450	6	11	held as a votive, not in vain, for the value and veracity of such shall one day ",
      "2997	8	19	cared about from dying. The dark side of the",
      "1938	10	17	And I will strike down upon thee with ",
      "1938	11	17	great vengeance and furious anger those ",
      "1938	7	17	shepherds the weak through the valley ",
      "1938	2	17	this occasion. Ezekiel 25:17? \"The path ",
      "2997	18	19	 ",
      "9949	9	10	 ",
      "1938	14	17	the Lord when I lay my vengeance upon ",
      "1938	15	17	thee.\" ",
      "1938	9	17	keeper and the finder of lost children. ",
      "1938	13	17	brothers. And you will know my name is ",
      "9949	2	10	but not the one it needs right now. ",
      "2997	16	19	he could have others from death, but not",
      "2997	7	19	dark side that he could even keep the once he",
      "1938	5	17	tyranny of evil men. Blessed is he who, ",
      "2997	17	19	himself. ",
      "2997	6	19	create life...He had such a knowledge of the",
      "2997	12	19	losing his power. Which eventually, of course,",
      "7469	4	7	Gate. All those moments will be lost ",
      "2997	2	19	story the Jedi would tell you. It's a Sith",
      "1938	16	17	 ",
      "2997	4	19	Sith so powerful and so wise, he could use the",
      "1938	3	17	of the righteous man is beset on all sides ",
      "2997	11	19	powerful...The only thing he was afraid of was",
      "7469	6	7	 ",
      "2997	15	19	apprentice killed him in his sleep. Ironic,",
      "7469	5	7	in time, like tears in rain. Time to die.",
      "9949	3	10	So we'll hunt him. ",
      "7469	3	7	glitter in the dark near the Tannhäuser ",
      "6450	4	11	venal and virulent vermin vanguarding vice and vouchsafing the violently vicious ",
      "6450	0	11	Voilà! In view, a humble vaudevillian veteran, cast vicariously as both victim ",
      "9949	5	10	Because he's not a hero. "
   )

   def stdinLines[F[_] : Sync]: Stream[F, String] =
      io.stdin(1)
          .through(text.utf8Decode)
          .through(text.lines)

   packetProgram(challengeLines[IO])
       .compile.drain.unsafeRunSync()
}

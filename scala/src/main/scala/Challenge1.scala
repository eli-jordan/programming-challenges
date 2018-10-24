

object Challenge1 extends App {

  def encrypt(keyword: String, text: String): String = {
    def index(col: Char, row: Char): Char = (((row - 'a' + col - 'a') % 26) + 'a').toChar
    process(keyword, text)(index)
  }

  def decrypt(keyword: String, text: String): String = {
    def index(col: Char, value: Char): Char = (((value + 26 - col) % 26) + 'a').toChar
    process(keyword, text)(index)
  }

  def process(keyword: String, text: String)(fn: (Char, Char) => Char): String =
    Stream.continually(keyword.toList).flatten.zip(text.toList).map(fn.tupled).mkString

  // Encrypt tests
  require(encrypt("snitch", "thepackagehasbeendelivered") == "lumicjcnoxjhkomxpkwyqogywq")
  require(encrypt("bond", "theredfoxtrotsquietlyatmidnight") == "uvrufrsryherugdxjsgozogpjralhvg")
  require(encrypt("train", "murderontheorientexpress") == "flrlrkfnbuxfrqrgkefckvsa")
  require(encrypt("garden", "themolessnuckintothegardenlastnight") == "zhvpsyksjqypqiewsgnexdvqkncdwgtixkx")

  // Decrypt tests
  require(decrypt("snitch", "lumicjcnoxjhkomxpkwyqogywq") == "thepackagehasbeendelivered")
  require(decrypt("bond", "uvrufrsryherugdxjsgozogpjralhvg") == "theredfoxtrotsquietlyatmidnight")
  require(decrypt("train", "flrlrkfnbuxfrqrgkefckvsa") == "murderontheorientexpress")
  require(decrypt("garden", "zhvpsyksjqypqiewsgnexdvqkncdwgtixkx") == "themolessnuckintothegardenlastnight")

  require(decrypt("cloak", "klatrgafedvtssdwywcyty") == "iamtheprettiestunicorn")
  require(decrypt("python", "pjphmfamhrcaifxifvvfmzwqtmyswst") == "alwayslookonthebrightsideoflife")
  require(decrypt("moore", "rcfpsgfspiecbcc") == "foryoureyesonly")
}

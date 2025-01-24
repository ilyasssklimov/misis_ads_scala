import Math.{ceil, sqrt}

class BifidCipher(alphabet: String) {
  private type Square = Vector[Vector[Char]]

  private val square: Square = polybiusSquare(alphabet)
  private val squareMap: Map[Char, (Int, Int)] = polybiusMap(square)

  private def polybiusSquare(alphabet: String): Square = {
    alphabet
      .grouped(ceil(sqrt(alphabet.length)).toInt)
      .map(_.toVector)
      .toVector
  }

  private def polybiusMap(square: Square): Map[Char, (Int, Int)] = {
    (
      for {
        (row, i) <- square.zipWithIndex
        (char, j) <- row.zipWithIndex
      } yield char -> (i + 1, j + 1)
    ).toMap
  }

  def encrypt(message: String): String = {
    val (xCoords, yCoords) = message.flatMap(squareMap.get).unzip
    val combinedCoords = xCoords ++ yCoords

    combinedCoords.grouped(2).map {
      case Seq(x, y) => square(x - 1)(y - 1)
    }.mkString
  }

  def decrypt(message: String): String = {
    val coords = message
      .flatMap(squareMap.get)
      .flatMap({ case (x, y) => Vector(x, y) })
    val (xCoords, yCoords) = coords.splitAt(coords.length / 2)

    xCoords.zip(yCoords).map {
      case (x, y) => square(x - 1)(y - 1)
    }.mkString
  }
}

def normalizeMessage(message: String): String = {
  message.toUpperCase.replace('J', 'I')
}


object Main extends App {
  val cipher = new BifidCipher("ABCDEFGHIKLMNOPQRSTUVWXYZ")
  val message = normalizeMessage("ATTACKATDAWN")

  println(s"Message:   $message")
  val encrypted = cipher.encrypt(message)
  println(s"Encrypted: $encrypted")
  val decrypted = cipher.decrypt(encrypted)
  println(s"Decrypted: $decrypted")
}

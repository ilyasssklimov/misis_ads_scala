import org.scalatest.funsuite.AnyFunSuite


class BifidCipherTest extends AnyFunSuite {
  test("BifidCipher.typical_1") {
    val cipher = new BifidCipher("ABCDEFGHIKLMNOPQRSTUVWXYZ")
    val message = normalizeMessage("ATTACKATDAWN")

    val encrypted = cipher.encrypt(message)
    val decrypted = cipher.decrypt(encrypted)

    assert(encrypted == "DQBDAXDQPDQH")
    assert(decrypted == message)
  }

  test("BifidCipher.example") {
    val cipher = new BifidCipher("BGWKZQPNDSIOAXEFCLUMTHYVR")
    val message = normalizeMessage("FLEEATONCE")

    val encrypted = cipher.encrypt(message)
    val decrypted = cipher.decrypt(encrypted)

    assert(encrypted == "UAEOLWRINS")
    assert(decrypted == message)
  }

  test("BifidCipher.typical_2") {
    val cipher = new BifidCipher("ABCDEFGHIKLMNOPQRSTUVWXYZ")
    val message = normalizeMessage("FLEEATONCE")

    val encrypted = cipher.encrypt(message)
    val decrypted = cipher.decrypt(encrypted)

    assert(encrypted == "HADNAAZDSP")
    assert(decrypted == message)
  }

  test("BifidCipher.big_message") {
    val cipher = new BifidCipher("PLAYFIREXMBCDGHKNOQSTUVWZ")
    val message = normalizeMessage("The invasion will start on the first of January")

    val encrypted = cipher.encrypt(message)
    val decrypted = cipher.decrypt(encrypted)

    assert(encrypted == "VRSYXSIYTMQVIRSKISLPVLDTCKRTCAIVTMATCEX")
    assert(decrypted == "THEINVASIONWILLSTARTONTHEFIRSTOFIANUARY")
  }

  test("BifidCipher.big_message_another_alphabet") {
    val cipher = new BifidCipher((('A' to 'Z') ++ ('0' to '9')).mkString)
    val message = "The invasion will start on the first of January".toUpperCase

    val encrypted = cipher.encrypt(message)
    val decrypted = cipher.decrypt(encrypted)

    assert(encrypted == "TBPDIPHJSPOTAIVMGPCZKNSCN09BFIHK64I7BM4")
    assert(decrypted == "THEINVASIONWILLSTARTONTHEFIRSTOFJANUARY")
  }
}

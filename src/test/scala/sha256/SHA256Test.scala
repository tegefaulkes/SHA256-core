// See README.md for license details.
package sha256

object SHA256Test {

  var K = Array("428a2f98", "71374491", "b5c0fbcf", "e9b5dba5", "3956c25b", "59f111f1", "923f82a4", "ab1c5ed5",
                "d807aa98", "12835b01", "243185be", "550c7dc3", "72be5d74", "80deb1fe", "9bdc06a7", "c19bf174",
                "e49b69c1", "efbe4786", "0fc19dc6", "240ca1cc", "2de92c6f", "4a7484aa", "5cb0a9dc", "76f988da",
                "983e5152", "a831c66d", "b00327c8", "bf597fc7", "c6e00bf3", "d5a79147", "06ca6351", "14292967",
                "27b70a85", "2e1b2138", "4d2c6dfc", "53380d13", "650a7354", "766a0abb", "81c2c92e", "92722c85",
                "a2bfe8a1", "a81a664b", "c24b8b70", "c76c51a3", "d192e819", "d6990624", "f40e3585", "106aa070",
                "19a4c116", "1e376c08", "2748774c", "34b0bcb5", "391c0cb3", "4ed8aa4a", "5b9cca4f", "682e6ff3",
                "748f82ee", "78a5636f", "84c87814", "8cc70208", "90befffa", "a4506ceb", "bef9a3f7", "c67178f2")

  def main(args: Array[String]): Unit = {
    //println(preprocess(BigInt("616263",16)).toString(16))
    //println(preprocess(BigInt("6162636462636465636465666465666765666768666768696768696a68696a6b696a6b6c6a6b6c6d6b6c6d6e6c6d6e6f6d6e6f706e6f7071", 16)).toString(16))

//    val processed = preprocess(BigInt("616263", 16))
    //val processed = preprocess(BigInt("6162636462636465636465666465666765666768666768696768696a68696a6b696a6b6c6a6b6c6d6b6c6d6e6c6d6e6f6d6e6f706e6f7071", 16))
    val processed = preprocess(BigInt("54686520517569636b2042726f776e20466f78204a756d706564204f76657220546865204c617a7920446f67",16))
    //println(processed.toString(32))
    //println(processed.toString(16))
    //println(messageScheduel(processed).length)
    println(hash(processed).toString(16))
    //println(getSubChunk(BigInt("616263646263646563646566", 16), 32, 2, 3).toString(16))

  }

  def getSubBits(in: BigInt, a: Int, b: Int): BigInt = { //WORKING
    val width = a - b + 1
    val mask = BigInt(2).pow(width) - 1
    val out: BigInt = (in & (mask << b)) >> b
    out
  }

  def getSubChunk(in: BigInt, bitWidth:Int, ChunkPosition: Int, chunks:Int): BigInt = { //WORKING
    val mask = BigInt(2).pow(bitWidth) - 1
    val shiftPosition = (chunks - ChunkPosition - 1) * bitWidth
    (in & (mask << shiftPosition)) >> shiftPosition
  }

  def messageScheduel(mess:BigInt):Array[BigInt] = {

    def sig0(x:BigInt):BigInt = {
      rotR32(x, 7) ^ rotR32(x, 18) ^ (x >> 3)
    }

    def sig1(x:BigInt):BigInt = {
      rotR32(x, 17) ^ rotR32(x, 19) ^ (x >> 10)
    }

    val W = new Array[BigInt](64)
    for(i <- 0 to 15){
      W(i) = getSubChunk(mess, 32, i, 16)
    }
    for(j <- 16 to 63){
      W(j) = (sig1(W(j-2)) + W(j-7) + sig0(W(j-15)) + W(j-16)) & (BigInt(2).pow(32)-1)
    }
    W
  }

  def preprocess(mess:BigInt):BigInt = {
    val messageLength = mess.toString(16).length()*4
    val zeroLength = 512 - (messageLength + 65)%512
    (((mess << 1) + 1) << (zeroLength + 64)) + messageLength
  }

  def rotR32(number:BigInt, bits:Int):BigInt = {
    val chunk = number & (BigInt(2).pow(bits)-1)
    ((number >> bits) + (chunk << (32 - bits))) & (BigInt(2).pow(32)-1)
  }

  def hash(mess:BigInt):BigInt = {

    def SIG0(x:BigInt):BigInt = {
      rotR32(x, 2) ^ rotR32(x, 13) ^ rotR32(x, 22)
    }

    def SIG1(x:BigInt):BigInt = {
      rotR32(x, 6) ^ rotR32(x, 11) ^ rotR32(x, 25)
    }

    def Ch(x:BigInt, y:BigInt, z:BigInt):BigInt = {
      ((x & y) ^ (~x & z)) & BigInt(2).pow(32)-1
    }


    def Maj(x:BigInt, y:BigInt, z:BigInt):BigInt = {
      (x & y) ^ (x & z) ^ (y & z)
    }

    println(mess.toString(16))
    val mLength = mess.toString(16).length()/128
    println("blocks " + mLength)

    var H1 = BigInt("6a09e667", 16)
    var H2 = BigInt("bb67ae85", 16)
    var H3 = BigInt("3c6ef372", 16)
    var H4 = BigInt("a54ff53a", 16)
    var H5 = BigInt("510e527f", 16)
    var H6 = BigInt("9b05688c", 16)
    var H7 = BigInt("1f83d9ab", 16)
    var H8 = BigInt("5be0cd19", 16)

    var a = BigInt(0)
    var b = BigInt(0)
    var c = BigInt(0)
    var d = BigInt(0)
    var e = BigInt(0)
    var f = BigInt(0)
    var g = BigInt(0)
    var h = BigInt(0)

    for(subChunk <- 0 until mLength){
      val W = messageScheduel(getSubChunk(mess, 512, subChunk, mLength))
//      BigInt(mess.toString(16).substring(i*128, (i*128)+128), 16)

      a = H1
      b = H2
      c = H3
      d = H4
      e = H5
      f = H6
      g = H7
      h = H8

      for(j <- 0 to 63){
        val T1 = ( h + SIG1(e) + Ch(e, f, g) + BigInt(K(j), 16) + W(j)) & (BigInt(2).pow(32)-1)
        val T2 = ( SIG0(a) + Maj(a, b, c)) & (BigInt(2).pow(32)-1)
        h = g
        g = f
        f = e
        e = (d + T1) & (BigInt(2).pow(32)-1)
        d = c
        c = b
        b = a
        a = (T1 + T2) & (BigInt(2).pow(32)-1)

      }

      H1 = (a + H1) & (BigInt(2).pow(32)-1)
      H2 = (b + H2) & (BigInt(2).pow(32)-1)
      H3 = (c + H3) & (BigInt(2).pow(32)-1)
      H4 = (d + H4) & (BigInt(2).pow(32)-1)
      H5 = (e + H5) & (BigInt(2).pow(32)-1)
      H6 = (f + H6) & (BigInt(2).pow(32)-1)
      H7 = (g + H7) & (BigInt(2).pow(32)-1)
      H8 = (h + H8) & (BigInt(2).pow(32)-1)

      print(H1.toString(16) + " ")
      print(H2.toString(16) + " ")
      print(H3.toString(16) + " ")
      print(H4.toString(16) + " ")
      print(H5.toString(16) + " ")
      print(H6.toString(16) + " ")
      print(H7.toString(16) + " ")
      println(H8.toString(16))

    }

    (H1 << 224) + (H2 << 192) + (H3 << 160) + (H4 << 128) + (H5 << 96) + (H6 << 64) + (H7 << 32) + H8
  }





}

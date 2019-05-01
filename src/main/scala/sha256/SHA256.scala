package sha256

import chisel3._


class messageScheduler extends Module{
  val io = IO(new Bundle {
    val messageIn = Input(UInt(512.W))
    val load      = Input(Bool())
    val en        = Input(Bool())
    val W         = Output(UInt(32.W))
  })

  val registers = Reg(Vec(16, UInt(32.W)))
  val sig0  = (W1(6,0) ## W1(31,7)) ^ (W1(17,0) ## W1(31,18)) ^ (W1 >> 3).asUInt()
  val sig1  = (W2(16,0) ## W2(31,17)) ^ (W2(18,0) ## W2(31,19)) ^ (W2 >> 10).asUInt()
  val W1     = registers(1)
  val W2     = registers(14)

  when (io.load){
    for (i <- 0 to 15){
      registers(i) :=  io.messageIn((i*32)+31, i*32)
    }
  }.elsewhen(io.en) {
    io.W := registers(0)
    for(i <- 1 to 15){
      registers(i-1) := registers(i)
    }
    registers(15) := sig0 +% registers(9) +% sig1 +% registers(0)
  }
}
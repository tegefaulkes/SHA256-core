package sha256

import chisel3._
import chisel3.util._


class messageScheduler extends Module{
  val io = IO(new Bundle {
    val messageIn = Input(UInt(512.W))
    val load      = Input(Bool())
//    val en        = Input(Bool())
    val W         = Output(UInt(32.W))
  })

  val registers = Reg(Vec(16, UInt(32.W)))
  val W1     = registers(1)
  val W2     = registers(14)
  val sig0  = (W1(6,0) ## W1(31,7)) ^ (W1(17,0) ## W1(31,18)) ^ (W1 >> 3).asUInt()
  val sig1  = (W2(16,0) ## W2(31,17)) ^ (W2(18,0) ## W2(31,19)) ^ (W2 >> 10).asUInt()

  io.W := registers(0)

  when (io.load){
    for (i <- 0 to 15){
      registers(i) :=  io.messageIn(((15-i)*32)+31, (15-i)*32)
    }
  }.otherwise {
    for(i <- 1 to 15){
      registers(i-1) := registers(i)
    }
    registers(15) := sig0 +% registers(9) +% sig1 +% registers(0)
  }
}

class compressionFunction(debugTap: Boolean) extends Module{
  val io = IO(new Bundle{
    val messageBlock    = Input(UInt(512.W))
    val start           = Input(Bool())
//    val initialState    = Input(Vec(8, UInt(32.W)))
    val registerOut     = Output(Vec(8, UInt(32.W)))
    val ready           = Output(Bool())
    val stateTap        = if (debugTap) Some(Output(Vec(8, UInt(32.W)))) else None
    val loopCountTap    = if (debugTap) Some(Output(UInt(32.W))) else None
  })

  val scheduler = Module(new messageScheduler)

  val K = VecInit(Array("h_428a2f98".U, "h_71374491".U, "h_b5c0fbcf".U, "h_e9b5dba5".U, "h_3956c25b".U, "h_59f111f1".U, "h_923f82a4".U, "h_ab1c5ed5".U,
                        "h_d807aa98".U, "h_12835b01".U, "h_243185be".U, "h_550c7dc3".U, "h_72be5d74".U, "h_80deb1fe".U, "h_9bdc06a7".U, "h_c19bf174".U,
                        "h_e49b69c1".U, "h_efbe4786".U, "h_0fc19dc6".U, "h_240ca1cc".U, "h_2de92c6f".U, "h_4a7484aa".U, "h_5cb0a9dc".U, "h_76f988da".U,
                        "h_983e5152".U, "h_a831c66d".U, "h_b00327c8".U, "h_bf597fc7".U, "h_c6e00bf3".U, "h_d5a79147".U, "h_06ca6351".U, "h_14292967".U,
                        "h_27b70a85".U, "h_2e1b2138".U, "h_4d2c6dfc".U, "h_53380d13".U, "h_650a7354".U, "h_766a0abb".U, "h_81c2c92e".U, "h_92722c85".U,
                        "h_a2bfe8a1".U, "h_a81a664b".U, "h_c24b8b70".U, "h_c76c51a3".U, "h_d192e819".U, "h_d6990624".U, "h_f40e3585".U, "h_106aa070".U,
                        "h_19a4c116".U, "h_1e376c08".U, "h_2748774c".U, "h_34b0bcb5".U, "h_391c0cb3".U, "h_4ed8aa4a".U, "h_5b9cca4f".U, "h_682e6ff3".U,
                        "h_748f82ee".U, "h_78a5636f".U, "h_84c87814".U, "h_8cc70208".U, "h_90befffa".U, "h_a4506ceb".U, "h_bef9a3f7".U, "h_c67178f2".U))

  val sReady :: sLoop :: Nil = Enum(2)
  val state = RegInit(sReady)
  val registers = Reg(Vec(8, UInt(32.W)))
  val loopCount = RegInit(0.U(7.W))

  val W1 = registers(4)
  val W2 = registers(0)
  val SIG1 = Cat(W1(5,0) , W1(31,6)) ^ Cat(W1(10,0) , W1(31,11)) ^ Cat(W1(24,0) , W1(31,25))
  val SIG0 = Cat(W2(1,0) , W2(31,2)) ^ Cat(W2(12,0) , W2(31,13)) ^ Cat(W2(21,0) , W2(31,22))
  val Ch = (registers(4) & registers(5)) ^ ((registers(4) ^ "h_FFFFFFFF".U) & registers(6))
  val Maj = (registers(0) & registers(1)) ^ (registers(0) & registers(2)) ^ (registers(1) & registers(2))

  val T1 = registers(7) +% SIG1 +% Ch +% K(loopCount) +% scheduler.io.W
  val T2 = SIG0 +% Maj

  scheduler.io.messageIn := io.messageBlock
  io.registerOut := VecInit(Array(0.U,0.U,0.U,0.U,0.U,0.U,0.U,0.U))
  if(debugTap) {
    io.stateTap.get := registers
    io.loopCountTap.get := loopCount
  }

  scheduler.io.load := true.B
  io.ready := true.B


  switch (state){
    is (sReady){
      loopCount := 0.U
      scheduler.io.load := true.B
      io.ready := true.B


      when(io.start){
        state := sLoop
        registers(0) := "h_6a09e667".U(32.W)  //FIXME, have the correct output WHILE ready = true.
        registers(1) := "h_bb67ae85".U(32.W)
        registers(2) := "h_3c6ef372".U(32.W)
        registers(3) := "h_a54ff53a".U(32.W)
        registers(4) := "h_510e527f".U(32.W)
        registers(5) := "h_9b05688c".U(32.W)
        registers(6) := "h_1f83d9ab".U(32.W)
        registers(7) := "h_5be0cd19".U(32.W)

        //registers := io.initialState
      }
    }
    is (sLoop){
      scheduler.io.load := false.B
      io.ready := false.B
      when(loopCount < 64.U) {
        registers(7) := registers(6)
        registers(6) := registers(5)
        registers(5) := registers(4)
        registers(4) := registers(3) +% T1
        registers(3) := registers(2)
        registers(2) := registers(1)
        registers(1) := registers(0)
        registers(0) := T1 +% T2
        loopCount := loopCount +% 1.U
      }.otherwise{
        io.ready := true.B
        io.registerOut := registers
        state := sReady
      }
    }
  }
}
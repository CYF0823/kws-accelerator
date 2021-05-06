package acceleration
import chisel3._

class Memory(data_width: Int,addr_width:Int) extends Module {
  val io = IO(new Bundle {
    val rdAddr = Input(UInt(addr_width.W))
    val rdData = Output(UInt(data_width.W))
    val wrEna  = Input(Bool())
    val wrData = Input(UInt(data_width.W))
    val wrAddr = Input(UInt(addr_width.W))
  })

  val mem = SyncReadMem(2^addr_width,UInt(data_width.W))

  io.rdData := mem.read(io.rdAddr)
  when(io.wrEna){
    mem.write(io.wrAddr,io.wrData)
  }
}

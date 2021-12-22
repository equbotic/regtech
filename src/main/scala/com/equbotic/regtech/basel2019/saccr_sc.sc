import com.equbotic.regtech.basel2019._

val z1 = saccr.calcSACCRunmargined(
  derivats        = Seq(DerivatSACCR(1, 400, 20)),
  hedgingSets     = Seq(Seq(RfAddon(101, 123), RfAddon(102, 456))),
  NICA            = 90,
  // VM              = Some(2000),
  // marginSchwelle  = Some(10000)
)
z1

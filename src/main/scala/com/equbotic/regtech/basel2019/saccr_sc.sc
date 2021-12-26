import com.equbotic.regtech.basel2019._

val z1 = saccr.calcSACCRunmargined(
  derivats        = Seq(DerivatHedge(1, 400, 20, Seq(RfHdg(101, 123, 1, 1000000)) ),
                        DerivatHedge(2, 700, 50, Seq(RfHdg(102, 456, 1, 2000000)) ) ),
  NICA            = 90 //net independent collateral amount
)

val z2 = saccr.calcSACCRmargined(
  derivats        = Seq(DerivatHedge(1, 400, 20, Seq(RfHdg(101, 123, 1, 1000000)) ),
                        DerivatHedge(2, 700, 50, Seq(RfHdg(102, 456, 1, 2000000)) ) ),
  NICA            = 200,    //net independent collateral amount
  VM              = 100,    //variation margin
  TH              =  33,    //margin threshold
  MTA             =  90     //minimum transfer amount
)
import com.equbotic.regtech.basel2019._

val z1 = saccr.calcSaccrUnmargined(
                            //derivId,marketValue,addon, ((rfId,addon,assetTyp,hedgeTyp))
//derivats        = Seq(DerivatHedge(1, 400.0, 20.0, Seq(RfHdg(101, 123.0, 1, 1000000)) ),
//                      DerivatHedge(2, 700.0, 50.0, Seq(RfHdg(102, 456.0, 1, 2000000)) ) ),
  derivats        = Seq(DerivatHedge(1, 400.0, Seq(RfHdg(101, 123.0, 1, 1000000)) ),
                        DerivatHedge(2, 700.0, Seq(RfHdg(102, 456.0, 1, 2000000)) ) ),
  NICA            = 90.0    //net independent collateral amount
)

val z2 = saccr.calcSaccrMargined(
//derivats        = Seq(DerivatHedge(1, 400.0, 20.0, Seq(RfHdg(101, 123.0, 1, 1000000)) ),
//                      DerivatHedge(2, 700.0, 50.0, Seq(RfHdg(102, 456.0, 1, 2000000)) ) ),
  derivats        = Seq(DerivatHedge(1, 400.0, Seq(RfHdg(101, 123.0, 1, 1000000)) ),
                        DerivatHedge(2, 700.0, Seq(RfHdg(102, 456.0, 1, 2000000)) ) ),
  NICA            = 200.0,  //net independent collateral amount
  VM              = 100.0,  //variation margin
  TH              =  33.0,  //margin threshold
  MTA             =  90.0   //minimum transfer amount
)
package com.equbotic.regtech.basel2019

/** SACCR
 *  https://www.bis.org/basel_framework/chapter/CRE/52.htm
 *  The Standardised Approach for Counterparty Credit Risk (SA-CCR)
 *
 *  http://data.europa.eu/eli/reg/2019/876/oj
 *  Regulation (EU) 2019/876 of the European Parliament and of the Council of 20 May 2019
 *  amending Regulation (EU) No 575/2013 (Regulation (EU) 575/2013 (CRR))
 *
 *  https://www.bis.org/publ/bcbs279.htm
 *  The standardised approach for measuring counterparty credit risk exposures
 */

type Nomi   = Double    //Nominal Positiv
type Quot   = Double    //Quote   Positiv
type Numb   = Double    //Double  Positiv
type Dbl    = Double
type Any1   = Int       //-1 , +1
type Idnt   = Long      //Identification
//rfId = derivatId + lfn (rf pro derivat)
type RfAssT = Int       //Riskfaktor-AssetType
type RfHdgT = Int       //Riskfaktor-HedgeType (hedge + crr1) ?

val IRD :RfAssT = 1  //Interest rate derivatives
val FXD :RfAssT = 2  //Foreign exchange derivatives
val CRD :RfAssT = 3  //Credit derivatives
val EQD :RfAssT = 4  //Equity derivatives
val CMD :RfAssT = 5  //Commodity derivatives
val OTD :RfAssT = 6  //Other derivatives

/** --------------------------------------------------------------------
 *  traits for the riskfaktors */
trait Rf2 { def addon: Nomi; def assetTyp: RfAssT; def hedgeTyp: RfHdgT}
case class RfHdg(rfId :Idnt, addon: Nomi, assetTyp: RfAssT, hedgeTyp: RfHdgT) extends Rf2 //with Rf1

/** --------------------------------------------------------------------
 *  traits for the derivates c*/
//fields used in calcSACCR - summarize PostionValue over HedgingSets
trait DerivatHdg { def marketValue: Nomi; def addon: Nomi; def riskFacts: Seq[Rf2] }
case class DerivatHedge(derivId :Idnt, marketValue: Nomi, addon: Nomi, riskFacts: Seq[Rf2]) extends DerivatHdg

case class DerivatOption(derivId :Idnt, currentPrice: Quot, units :Numb, buysell :Any1, putcall :Any1,
                         yrsToStart :Quot, yrsToEnd :Quot, yrsToExpi :Quot) extends DerivatHdg
{
  val marketValue :Nomi    = currentPrice * units

  val effectiveNotional = {//D = d * MF * δ  = adjusted notional (d) * maturity factor (MF) * supervisory delta (δ)
    val adjustedNotional = marketValue
    val sign             = buysell * putcall
    val supervisoryDelta = 1.0  //vola as param + blackScholes
    1000.0
  }

  val addon      :Nomi     = 1000
  val riskFacts  :Seq[Rf2] = Seq(RfHdg(101, 123, 1, 1000000))

}

case class DerivatOther (derivId :Idnt, currentPrice: Quot, units :Numb, buysell :Any1,
                         yrsToStart :Quot, yrsToEnd :Quot, yrsToExpi :Quot) extends DerivatHdg
{{
  val marketValue :Nomi    = currentPrice * units

  val effectiveNotional = {//D = d * MF * δ  = adjusted notional (d) * maturity factor (MF) * supervisory delta (δ)
    val adjustedNotional = marketValue
    val sign             = buysell

    val supervisoryDelta = 1.0
    1000.0
  }

  val addon      :Nomi     = 1000
  val riskFacts  :Seq[Rf2] = Seq(RfHdg(101, 123, 1, 1000000))

}

/** --------------------------------------------------------------------
 *  SACCR c*/
object saccr {
  def MAX3(d1 :Double, d2 :Double, d3 :Double) :Double = d1.max(d2).max(d3)
  def MAX (d1 :Double, d2 :Double)             :Double = d1.max(d2)
  def MIN (d1 :Double, d2 :Double)             :Double = d1.min(d2)

  // #####################################################################
  // #####################################################################
  /** --------------------------------------------------------------------
   * get the Hedgingsets with the riskfaktors of the derivats */
  def getHedgingSets(derivats: Seq[DerivatHdg]
                    ): Map[RfHdgT, Seq[Rf2]] = {
    val rfAll  = derivats.flatMap(_.riskFacts)
    val hdgSet = rfAll.groupBy(_.hedgeTyp)
    hdgSet
  }
  /** --------------------------------------------------------------------
   * multiplier for PFE ; article 278 */
  def calcMultiplier (sumAddon :Nomi,
                      zz       :Nomi
                     ): Double = {
    val floor = 0.05
    val yy    = 2.0 * (1 - floor) * sumAddon

    if (zz < 0 && yy != 0)  MIN (1.0, floor + (1.0 - floor) * math.exp(zz / yy) )
    else  1.0
  }
  /** --------------------------------------------------------------------
   * calcPotentialFutureExposur = multiplier * sumAddOn ; article 278 */
  def calcPotentialFutureExposure(multiplier :Quot, sumAddon :Nomi): Double =
    multiplier * sumAddon

  /** --------------------------------------------------------------------
   * calcReplacementCostUnmargined = max(CMV - NICA, 0) ; article 275 */
  def calcReplacementCostUnmargined(CMV :Nomi, NICA :Nomi): Double =
    MAX(CMV - NICA, 0.0)

  /** --------------------------------------------------------------------
   * calcReplacementCostMargined = RC = max(CMV - VM - NICA, TH + MTA - NICA, 0) ; article 275 */
  def calcReplacementCostMargined(CMV :Nomi, NICA :Nomi, VM :Nomi, TH :Nomi, MTA :Nomi): Double =
    MAX3(CMV - VM - NICA, TH + MTA - NICA, 0.0)

  /** --------------------------------------------------------------------
   * calcExposureValue = a · (RC + PFE) ; *a=1.4 *; article 274 */
  def calcExposureValue(RC :Nomi, PFE :Nomi): Double =
    1.4 * RC * PFE   //replacementCost - potentialFutureExposure

  /** --------------------------------------------------------------------
   *  get the EAD Value for netted Derivats */
  def calcSaccrUnmargined( derivats        : Seq[DerivatHdg], //for the netting set
                           NICA            : Nomi           //net independent collateral amount calculated only for transactions that are included in netting set
                         ): Double = {
    val CMV        = derivats.map(_.marketValue).sum                //the current market value

    val hedgeSets  = getHedgingSets(derivats)     //: Seq[Seq[Rf1]]
    val sumAddon   = hedgeSets.map(_._2.map(_.addon).sum).sum       //summe der addons ueber hedgingsets

    val zz         = CMV - NICA
    val multiplier = calcMultiplier(sumAddon, zz)
    val PFE        = calcPotentialFutureExposure(multiplier, sumAddon)

    val RC         = calcReplacementCostUnmargined  (CMV, NICA)

    val exposureValue = calcExposureValue(RC, PFE)
    exposureValue  //exposure value of a netting set
  }
  /** --------------------------------------------------------------------
   *  get the EAD Value for netted Derivats */
  def calcSaccrMargined( derivats        : Seq[DerivatHdg], //for the netting set
                         NICA            : Nomi,          //net independent collateral amount calculated only for transactions that are included in netting set
                         VM              : Nomi,          //variation margin in margined netting set
                         TH              : Nomi,          //margin threshold
                         MTA             : Nomi           //minimum transfer amount
                         //  marginSchwelle  : Nomi           //if defined, then margined with this schwelle
                       ): Double = {
    val CMV        = derivats.map(_.marketValue).sum          //the current market value

    val hedgeSets  = getHedgingSets(derivats)     //: Seq[Seq[Rf1]]
    val sumAddon   = hedgeSets.map(_._2.map(_.addon).sum).sum       //summe der addons ueber hedgingsets

    val zz         = CMV - VM - NICA
    val multiplier = calcMultiplier(sumAddon, zz)
    val PFE        = calcPotentialFutureExposure(multiplier, sumAddon)

    val RC         = calcReplacementCostMargined  (CMV, NICA, VM, TH, MTA)

    val exposureValue = calcExposureValue(RC, PFE)
    exposureValue                                         //exposure value of a netting set
  }

}
/** abbreviations
CMV   = current market value
PFE   = potential future exposure
RC    = replacement cost;  RC for netting sets that are not subject to a margin agreement
NICA  = net independent collateral amount calculated only for transactions that are included in netting set
VM    = the volatility-adjusted value of the net variation margin received or posted, as applicable, to the netting set on a regular basis to mitigate changes in the netting set's CMV;
TH    = the margin threshold applicable to the netting set under the margin agreement below which the institution cannot call for collateral; and
MTA   = the minimum transfer amount applicable to the netting set under the margin agreement.

CSA  Credit Support Annex (1 of 4 parts of ISDA Master Agreement, not mandatory)
OGA Organismus für gemeinsame Anlagen -> in Wertpapieren (OGAW)
CIU collective investment undertakings -> undertaking for collective investment in transferable securities (UCITS)
*/

package com.equbotic.regtech.basel2019

/** SACCR
 *  https://www.bis.org/basel_framework/chapter/CRE/52.htm
 *  CRE - Calculation of RWA for credit risk
 *  CRE52 - Standardised approach to counterparty credit risk
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

/** CRE 52.24 */
val IRD :RfAssT = 1  //Interest rate derivatives
val FXD :RfAssT = 2  //Foreign exchange derivatives
val CRD :RfAssT = 3  //Credit derivatives
val EQD :RfAssT = 4  //Equity derivatives
val CMD :RfAssT = 5  //Commodity derivatives
val OTD :RfAssT = 6  //Other derivatives

/** --------------------------------------------------------------------
 *  traits for the riskfaktors */
trait      Rf   {           def addOn: Nomi; def assetTyp: RfAssT; def hedgeTyp: RfHdgT}
case class RfHdg(rfId :Idnt,    addOn: Nomi,     assetTyp: RfAssT,     hedgeTyp: RfHdgT) extends Rf //(>>no deeper traits: with Rf1)

/** --------------------------------------------------------------------
 *  traits for the derivates c*/
//fields used in calcSACCR - summarize PostionValue over HedgingSets
//trait      DerivatHdg  {               def marketValue: Nomi; def addOn: Nomi; def riskFacts: Seq[Rf] }
//case class DerivatHedge(derivId :Idnt,     marketValue: Nomi,     addOn: Nomi,     riskFacts: Seq[Rf]) extends DerivatHdg

trait      DerivatHdg  {               def marketValue: Nomi; def riskFacts: Seq[Rf] }
case class DerivatHedge(derivId :Idnt,     marketValue: Nomi,     riskFacts: Seq[Rf]) extends DerivatHdg

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

//val addOn      :Nomi     = 1000
  val riskFacts  :Seq[Rf] = Seq(RfHdg(101, 123, 1, 1000000))

}

case class DerivatOther (derivId :Idnt, currentPrice: Quot, units :Numb, buysell :Any1,
                         yrsToStart :Quot, yrsToEnd :Quot, yrsToExpi :Quot) extends DerivatHdg
{
  val marketValue :Nomi    = currentPrice * units

  val effectiveNotional = {//D = d * MF * δ  = adjusted notional (d) * maturity factor (MF) * supervisory delta (δ)
    val adjustedNotional = marketValue
    val sign             = buysell

    val supervisoryDelta = 1.0
    1000.0
  }

//val addOn      :Nomi     = 1000
  val riskFacts  :Seq[Rf] = Seq(RfHdg(101, 123, 1, 1000000))

}

/** --------------------------------------------------------------------
 *  SACCR c*/
object saccr {
  def MAX3(d1 :Double, d2 :Double, d3 :Double) :Double = d1.max(d2).max(d3)
  def MAX (d1 :Double, d2 :Double)             :Double = d1.max(d2)
  def MIN (d1 :Double, d2 :Double)             :Double = d1.min(d2)

  // #####################################################################
  /** CRE 52.1  / article 274 --------------------------------------------------------------------
   * calcExposureValue = a · (RC + PFE) ; a=1.4 */
  def calcExposureValue(RC :Nomi, PFE :Nomi): Nomi =
    1.4 * (RC + PFE)   //alpha * ( replacementCost + potentialFutureExposure)

  /** CRE 52.10 / article 275 --------------------------------------------------------------------
   * calcReplacementCostUnmargined = max(CMV - NICA, 0) */
  def calcReplacementCostUnmargined(CMV :Nomi, NICA :Nomi): Nomi =
    MAX(CMV - NICA, 0.0)    // currentMarketValue (V) - netIndependentCollateralAmount (C)

  /** CRE 52.18 / article 275 --------------------------------------------------------------------
   * calcReplacementCostMargined = RC = max(CMV - VM - NICA, TH + MTA - NICA, 0) */
  def calcReplacementCostMargined(CMV :Nomi, NICA :Nomi, VM :Nomi, TH :Nomi, MTA :Nomi): Nomi =
    MAX3(CMV - (NICA + VM), TH + MTA - NICA, 0.0) // currentMarketValue (V) - (netIndependentCollateralAmount + variationMargin) (C)
                                                       // marginTHreshold + minimumTransferAmount - netIndependentCollateralAmount

  /** CRE 52.20 / article 278 --------------------------------------------------------------------
   * calcPotentialFutureExposure = multiplier * sumAddOn */
  def calcPotentialFutureExposure(multiplier :Quot, sumAddOn :Nomi): Nomi =
    multiplier * sumAddOn

  /** CRE 52.23 / article 278 --------------------------------------------------------------------
   * multiplier for PotentialFutureExposure */
  def calcMultiplier (sumAddOn :Nomi,
                      haircutValue  :Nomi  //(C) unmargined: CMV - NICA / margined: CMV - (NICA + VM)
                     ): Quot =
  {
    val floor = 0.05
    val denominator = 2.0 * (1 - floor) * sumAddOn

    if (haircutValue < 0 && denominator != 0)  MIN (1.0, floor + (1.0 - floor) * math.exp(haircutValue / denominator) )
    else  1.0
  }

  /** CRE 52.24 --------------------------------------------------------------------
   * get the Hedgingsets with the riskfaktors of the derivats */
  def getHedgingSets(derivats: Seq[DerivatHdg]  ): Map[RfHdgT, Seq[Rf]] =
  {
    val rfAll              :Seq[Rf]  = derivats.flatMap(_.riskFacts)  //flatmap all Riskfactors of derivats
    val hdgSet :Map[RfHdgT, Seq[Rf]] = rfAll.groupBy(_.hedgeTyp)      //group the riskfactors by hedgeTyp
    hdgSet
  }
  /** CRE 52.25 --------------------------------------------------------------------
   * get the Hedgingsets with the riskfaktors of the derivats */
  def calcAddOnAggregate(hedgeSets: Map[RfHdgT, Seq[Rf]] ) : Nomi =
  {
    val addOnAgg :Nomi = hedgeSets.map(                    //for every hedgeSet
                                 _._2.map( _.addOn ).sum   //build the sum of addOns
                                ).sum                      //the sum of the hedgeSets addOn
    addOnAgg
  }

  /** --------------------------------------------------------------------
  *  get the EAD Value for a set of unmargined netted Derivats */
  def calcSaccrUnmargined( derivats        : Seq[DerivatHdg], //the derivats with riscfactors of the unmargined netting set
                           NICA            : Nomi             //net independent collateral amount calculated only for transactions that are included in the netting set
                         ): Nomi =                            //the exposure (ead) of the netting set
  {
    val CMV           :Nomi = derivats.map(_.marketValue).sum             //the currentMarketValue of the netting set = sum of marketvalues of the netting set derivates

    val hedgeSets     :Map[RfHdgT, Seq[Rf]] = getHedgingSets(derivats)   //Build Map of HedgingSets
    val addOnAgg      :Nomi = calcAddOnAggregate(hedgeSets)              //summe der addOns ueber hedgingsets

    val haircutValue  :Nomi = CMV - NICA                                 //currentMarketValue (V) - netIndependentCollateralAmount (C)
    val multiplier    :Quot = calcMultiplier(addOnAgg, haircutValue)
    val PFE           :Nomi = calcPotentialFutureExposure(multiplier, addOnAgg)

    val RC            :Nomi = calcReplacementCostUnmargined  (CMV, NICA)

    val exposureValue :Nomi = calcExposureValue(RC, PFE)
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
                       ): Nomi =
  {
    val CMV           :Nomi = derivats.map(_.marketValue).sum          //the current market value as sum of all derivats

    val hedgeSets     :Map[RfHdgT, Seq[Rf]] = getHedgingSets(derivats) //Build Map of HedgingSets
    val addOnAgg      :Nomi = calcAddOnAggregate(hedgeSets)            //summe der addOns ueber hedgingsets

    val haircutValue  :Nomi = CMV - (NICA + VM)                        //currentMarketValue (V) - (netIndependentCollateralAmount + variationMargin) (C)
    val multiplier    :Quot = calcMultiplier(addOnAgg, haircutValue)
    val PFE           :Nomi = calcPotentialFutureExposure(multiplier, addOnAgg)

    val RC            :Nomi = calcReplacementCostMargined  (CMV, NICA, VM, TH, MTA)

    val exposureValue :Nomi = calcExposureValue(RC, PFE)
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

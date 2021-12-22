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

type Nomi   = Double    //Nominal
type Quot   = Double    //Quote
type Idnt   = Long      //Identification
//rfId = derivatId + lfn (rf pro derivat)
type RfAssT = Int       //Riskfaktor-AssetType
type RfHdgT = Int       //Riskfaktor-HedgeType (hedge + crr1) ?

/** --------------------------------------------------------------------
 *  traits for the riskfaktors */
trait Rf1 { def addon: Nomi }
trait Rf2 { def addon: Nomi; def hedgeTyp: RfHdgT; def assetTyp: RfAssT}
trait Rf3 { def addon: Nomi; def hedgeTyp: RfHdgT; def assetTyp: RfAssT; def toUse :Boolean}

case class RfAddon(rfId :Idnt, addon: Nomi) extends Rf1
case class RfHedge(rfId :Idnt, addon: Nomi, hedgeTyp: RfHdgT, assetTyp: RfAssT) extends Rf2 with Rf1
case class RfAll  (rfId :Idnt, addon: Nomi, hedgeTyp: RfHdgT, assetTyp: RfAssT) extends Rf2 with Rf1
/** --------------------------------------------------------------------
 *  traits for the derivates c*/
//fields used in calcSACCR - summarize PostionValue over HedgingSets
trait Derivat1 { def marketValue: Nomi; def addon: Nomi }
trait Derivat2 { def marketValue: Nomi; def addon: Nomi; def riskFacts: Seq[Rf2] }
trait Derivat3 { def marketValue: Nomi; def addon: Nomi; def riskFacts: Seq[Rf3] }

case class DerivatHedge(derivId :Idnt, marketValue: Nomi, addon: Nomi, riskFacts: Seq[Rf2]) extends Derivat1
case class DerivatSACCR(derivId :Idnt, marketValue: Nomi, addon: Nomi)                      extends Derivat1

/** --------------------------------------------------------------------
 *  SACCR c*/
object saccr {
  def MAX3(d1 :Double, d2 :Double, d3 :Double) = d1.max(d2).max(d3)
  def MAX (d1 :Double, d2 :Double)             = d1.max(d2)

  def MIN (d1 :Double, d2 :Double)             = d1.min(d2)

  /** --------------------------------------------------------------------
   */
  def relevRfs (rfs :Seq[Rf3]
               ) = {
    //rfs.sortWith((rf1, rf2) => rf1.assetTyp < rf2.assetTyp &&  rf1.addon > rf2.addon)

    val rfGrp = rfs.groupBy(_.assetTyp).values.map(_.maxBy(_.addon))//.maxBy(_.addon)  //.sortWith(_ > _)

    val rf1 = rfs  .filter(_.hedgeTyp == 123)  //caps/floor
    val rf2 = rfGrp.filter(_.hedgeTyp != 123)

    val rfOut = rf1 ++ rf2 //rfs.map(_.toUse)
    rfOut
  }
  /** --------------------------------------------------------------------
   */
  def findRelevRf (derivats: Seq[Derivat3]
                  ) = {
    val derivs = derivats.map(rfSet => relevRfs(rfSet.riskFacts) )
    derivs
  }
  /** --------------------------------------------------------------------
   * get the Hedgingsets with the riskfaktors of the derivats */
  def getHedgingSets(derivats: Seq[Derivat2]
                    ) = {
    val rfAll  = derivats.map (_.riskFacts).flatten
    val hdgSet = rfAll.groupBy(_.hedgeTyp)
    hdgSet
  }
  /** --------------------------------------------------------------------
   * multiplier for PFE ; article 278 */
  def calcMultiplier (sumAddon :Nomi,
                      zz       :Nomi
                     ) = {
    val floor = 0.05
    val yy    = 2.0 * (1 - floor) * sumAddon

    if (zz < 0 && yy != 0)  MIN (1.0, (floor + (1.0 - floor) * math.exp(zz / yy)) )
    else  1.0
  }
  /** --------------------------------------------------------------------
   * calcPotentialFutureExposur = multiplier * sumAddOn ; article 278 */
  def calcPotentialFutureExposure(multiplier :Quot, sumAddon :Nomi) =
    multiplier * sumAddon

  /** --------------------------------------------------------------------
   * calcReplacementCostUnmargined = max(CMV - NICA, 0) ; article 275 */
  def calcReplacementCostUnmargined(CMV :Nomi, NICA :Nomi) =
    MAX(CMV - NICA, 0.0)

  /** --------------------------------------------------------------------
   * calcReplacementCostMargined = RC = max(CMV - VM - NICA, TH + MTA - NICA, 0) ; article 275 */
  def calcReplacementCostMargined(CMV :Nomi, NICA :Nomi, VM :Nomi, TH :Nomi, MTA :Nomi) =
    MAX3(CMV - VM - NICA, TH + MTA - NICA, 0.0)

  /** --------------------------------------------------------------------
   * calcExposureValue = a · (RC + PFE) ; *a=1.4 *; article 274 */
  def calcExposureValue(RC :Nomi, PFE :Nomi) =
    1.4 * RC * PFE   //replacementCost - potentialFutureExposure

  /** --------------------------------------------------------------------
   *  get the EAD Value for netted Derivats */
  def calcSACCRunmargined(
                           derivats        : Seq[Derivat1], //for the netting set
                           hedgingSets     : Seq[Seq[Rf1]],
                           NICA            : Nomi           //net independent collateral amount calculated only for transactions that are included in netting set
                         ) = {
    val CMV        = derivats.map(_.marketValue).sum               //the current market value
    val sumAddon   = hedgingSets.map(_.map(_.addon).sum).sum  //summe der addons ueber hedgingsets

    val zz         = CMV - NICA
    val multiplier = calcMultiplier(sumAddon, zz)
    val PFE        = calcPotentialFutureExposure(multiplier, sumAddon)

    val RC         = calcReplacementCostUnmargined  (CMV, NICA)

    val exposureValue = calcExposureValue(RC, PFE)
    exposureValue  //exposure value of a netting set
  }
  /** --------------------------------------------------------------------
   *  get the EAD Value for netted Derivats */
  def calcSACCRmargined(
                         derivats        : Seq[Derivat1], //for the netting set
                         hedgingSets     : Seq[Seq[Rf1]],
                         NICA            : Nomi,          //net independent collateral amount calculated only for transactions that are included in netting set
                         VM              : Nomi,          //variation margin in margined netting set
                         TH              : Nomi,
                         MTA             : Nomi
                         //  marginSchwelle  : Nomi           //if defined, then margined with this schwelle
                       ) = {
    val CMV        = derivats.map(_.marketValue).sum          //the current market value
    val sumAddon   = hedgingSets.map(_.map(_.addon).sum).sum  //summe der addons ueber hedgingsets

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

OGA Organismus für gemeinsame Anlagen -> in Wertpapieren (OGAW)
CIU collective investment undertakings -> undertaking for collective investment in transferable securities (UCITS) */

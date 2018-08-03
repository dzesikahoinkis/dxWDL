package dxWDL

import org.scalatest.{FlatSpec, Matchers}
import spray.json._
import wom.types._

class WdlVarLinksTest extends FlatSpec with Matchers {

    it should "import JSON values" in {
        val wvl = WdlVarLinks.importFromDxExec(WomBooleanType,
                                               JsBoolean(true))
        wvl.womType should equal(WomBooleanType)

        val wvl2 = WdlVarLinks.importFromDxExec(WomMaybeEmptyArrayType(WomFloatType),
                                                JsArray(Vector(JsNumber(1), JsNumber(2.3))))
        wvl2.womType should equal(WomMaybeEmptyArrayType(WomFloatType))

        val wvl3 = WdlVarLinks.importFromDxExec(WomNonEmptyArrayType(WomStringType),
                                                JsArray(Vector(JsString("hello"),
                                                               JsString("sunshine"),
                                                               JsString("ride"))))
        wvl3.womType should equal(WomNonEmptyArrayType(WomStringType))
    }
}

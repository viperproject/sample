package ch.ethz.inf.pm.sample.oorepresentation.sil

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

class SampleConverterTest extends FunSuite with ShouldMatchers {
  val pp = sample.DummyProgramPoint
  val intType = sample.IntType
  val refType = sample.RefType()

  def convert(exp: sample.Expression): sil.Exp = DefaultSampleConverter.convert(exp)

  test("access path identifier conversion") {
    // Types must be checked explicitly as they are not always part of the
    // first case class parameter list
    val sampleIntId = VariableIdentifier("val")(intType, pp)
    val sampleRefId = VariableIdentifier("ref")(refType, pp)

    val localVar = convert(sample.AccessPathIdentifier(sampleIntId))
    localVar should be(sil.LocalVar("val")(sil.Int))
    localVar.typ should be(sil.Int)

    val localVar2 = convert(sample.AccessPathIdentifier(sampleRefId))
    localVar2 should be(sil.LocalVar("ref")(sil.Ref))
    localVar2.typ should be(sil.Ref)

    val fieldAccess = convert(sample.AccessPathIdentifier(List(sampleRefId, sampleIntId))).asInstanceOf[sil.FieldAccess]
    fieldAccess should be(sil.FieldAccess(sil.LocalVar("ref")(sil.Ref), sil.Field("val", sil.Int)())())
    fieldAccess.rcv.typ should be(sil.Ref)
    fieldAccess.field.typ should be(sil.Int)

    val fieldAccess2 = convert(sample.AccessPathIdentifier(List(sampleRefId, sampleRefId))).asInstanceOf[sil.FieldAccess]
    fieldAccess2 should be(sil.FieldAccess(sil.LocalVar("ref")(sil.Ref), sil.Field("ref", sil.Ref)())())
    fieldAccess2.rcv.typ should be(sil.Ref)
    fieldAccess2.field.typ should be(sil.Ref)
  }
}

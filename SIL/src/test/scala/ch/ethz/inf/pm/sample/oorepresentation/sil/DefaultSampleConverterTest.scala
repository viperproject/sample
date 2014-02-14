package ch.ethz.inf.pm.sample.oorepresentation.sil

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import semper.sil.{ast => sil}

class DefaultSampleConverterTest extends FunSuite with ShouldMatchers {
  val pp = sample.DummyProgramPoint
  val intType = sample.IntType
  val refType = sample.RefType()

  def convert(exp: sample.Expression): sil.Exp = DefaultSampleConverter.convert(exp)

  test("access path identifier conversion") {
    // Types must be checked explicitly as they are not always part of the
    // first case class parameter list
    val localVar = convert(sample.AccessPathIdentifier(List("i"))(intType, pp))
    localVar should be(sil.LocalVar("i")(sil.Ref))
    localVar.typ should be(sil.Int)

    val localVar2 = convert(sample.AccessPathIdentifier(List("x"))(refType, pp))
    localVar2 should be(sil.LocalVar("x")(sil.Ref))
    localVar2.typ should be(sil.Ref)

    val fieldAccess = convert(sample.AccessPathIdentifier(List("x", "val"))(intType, pp)).asInstanceOf[sil.FieldAccess]
    fieldAccess should be(sil.FieldAccess(sil.LocalVar("x")(sil.Ref), sil.Field("val", sil.Int)())())
    fieldAccess.rcv.typ should be(sil.Ref)
    fieldAccess.field.typ should be(sil.Int)

    val fieldAccess2 = convert(sample.AccessPathIdentifier(List("x", "next"))(refType, pp)).asInstanceOf[sil.FieldAccess]
    fieldAccess2 should be(sil.FieldAccess(sil.LocalVar("x")(sil.Ref), sil.Field("next", sil.Ref)())())
    fieldAccess2.rcv.typ should be(sil.Ref)
    fieldAccess2.field.typ should be(sil.Ref)
  }
}

import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyStringType, Type}
import ch.ethz.inf.pm.sample.test.SemanticDomainTest

/**
  * @author Lucas Brutschy
  */
trait StringDomainTest[T <: StringDomain[T]] extends SemanticDomainTest[T] {
  override def typ: Type = DummyStringType
  override def values = super.values ++ Set(
    Constant("a",typ),
    Constant("bbbbbbbbbbbbbbbbbbbbbb",typ),
    Constant("",typ)
  )
}

class PrefixTest extends StringDomainTest[Prefix] {
  override def factory: Prefix = new Prefix()
}
class SuffixTest extends StringDomainTest[Suffix] {
  override def factory: Suffix = new Suffix()
}
class BricksTest extends StringDomainTest[Bricks] {
  override def factory: Bricks = new Bricks()
}
class MaybeContainedCharactersTest extends StringDomainTest[MaybeContainedCharacters] {
  override def factory: MaybeContainedCharacters = new MaybeContainedCharacters()
}
class SurelyContainedCharactersTest extends StringDomainTest[SurelyContainedCharacters] {
  override def factory: SurelyContainedCharacters = new SurelyContainedCharacters()
}
trait NonrelationalStringDomainTest[T <: StringValueSetDomain[T]] extends StringDomainTest[NonrelationalStringDomain[T]]
class StringKSetDomainTest extends StringDomainTest[NonrelationalStringDomain[StringKSetDomain]] {
  override def factory: NonrelationalStringDomain[StringKSetDomain] = new NonrelationalStringDomain[StringKSetDomain](StringKSetDomain.Bottom(2))
}
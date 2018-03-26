package scalaz

import scalaprops.{Property, Scalaprops}
import scalaprops.ScalapropsMagnolia._
import scalaz.std.anyVal._

private case class XXX(x: Int, y: Byte)
private object XXX {
  import syntax.semigroup._
  val instance: Monoid[XXX] =
    new Monoid[XXX] {
      def append(a: XXX, b: => XXX) =
        XXX(a.x |+| b.x, a.y |+| b.y)
      val zero =
        XXX(Monoid[Int].zero, Monoid[Byte].zero)
    }
}

object MonoidTest extends Scalaprops {

  override def param = super.param.copy(minSuccessful = 10000)

  val testSemigroup = Property.forAll { (x: XXX, y: XXX) =>
    import ScalazMagnoliaSemigroup._
    XXX.instance.append(x, y) == Semigroup[XXX].append(x, y)
  }

  val testMonoidZero = Property.forAll {
    import ScalazMagnoliaMonoid._
    Monoid[XXX].zero == XXX.instance.zero
  }

  val testMonoid = Property.forAll { (x: XXX, y: XXX) =>
    import ScalazMagnoliaMonoid._
    XXX.instance.append(x, y) == Monoid[XXX].append(x, y)
  }

}

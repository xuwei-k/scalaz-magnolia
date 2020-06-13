package scalaz

import scalaprops.{scalazlaws, Property, Scalaprops}
import scalaprops.ScalapropsMagnoliaGen._
import scalaz.std.anyVal._
import scalaz.std.tuple._

sealed trait A
case class B(x: Boolean, y: Boolean) extends A
case class C(value: Boolean \/ Boolean) extends A
case class D(value: Tuple2[Boolean, Boolean]) extends A
case object E extends A

object A {
  private[this] val instanceB: Order[B] =
    Divide[Order].dividing2(Function.unlift(B.unapply))

  private[this] val instanceC: Order[C] =
    Order[Boolean \/ Boolean].contramap(_.value)

  private[this] val instanceD: Order[D] =
    Order[Tuple2[Boolean, Boolean]].contramap(_.value)

  val instance: Order[A] = Order.order {
    case (x: B, y: B) =>
      instanceB.order(x, y)
    case (x: C, y: C) =>
      instanceC.order(x, y)
    case (x: D, y: D) =>
      instanceD.order(x, y)
    case (E, E) =>
      Ordering.EQ
    case (_: B, (_: C) | (_: D) | E) =>
      Ordering.LT
    case (_: C, _: B) =>
      Ordering.GT
    case (_: C, (_: D) | E) =>
      Ordering.LT
    case (_: D, (_: B) | (_: C)) =>
      Ordering.GT
    case (_: D, E) =>
      Ordering.LT
    case (E, _: B | _: C | _: D) =>
      Ordering.GT
  }
}

object OrderTest extends Scalaprops {
  override def param = super.param.copy(minSuccessful = 10000)

  val testEqual = Property.forAll { (x: A, y: A) =>
    import ScalazMagnoliaEqual._
    A.instance.equal(x, y) == Equal[A].equal(x, y)
  }

  val testOrder = Property.forAll { (x: A, y: A) =>
    import ScalazMagnoliaOrder._
    A.instance.order(x, y) == Order[A].order(x, y)
  }

  val equalLaw = {
    import ScalazMagnoliaEqual._
    scalazlaws.equal.all[A]
  }

  val orderLaw = {
    import ScalazMagnoliaOrder._
    scalazlaws.order.all[A]
  }
}

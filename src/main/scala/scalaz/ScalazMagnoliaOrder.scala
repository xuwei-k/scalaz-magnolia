package scalaz

import magnolia.{CaseClass, Magnolia, SealedTrait}
import scalaz.std.list._
import scalaz.std.anyVal._

object ScalazMagnoliaOrder {
  type Typeclass[A] = scalaz.Order[A]

  def combine[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] =
    new Order[A] {
      override def order(x: A, y: A): Ordering =
        Foldable[List].foldMap(ctx.parameters.toList) { p => p.typeclass.order(p.dereference(x), p.dereference(y)) }

      override def equal(x: A, y: A): Boolean =
        ctx.parameters.forall { p => p.typeclass.equal(p.dereference(x), p.dereference(y)) }
    }

  def dispatch[A](ctx: SealedTrait[Typeclass, A]): Typeclass[A] =
    new Order[A] {
      override def order(a: A, b: A): Ordering = {
        val aa = ctx.subtypes.indexWhere(_.cast.isDefinedAt(a))
        val bb = ctx.subtypes.indexWhere(_.cast.isDefinedAt(b))

        if (aa >= 0 && bb >= 0) {
          if (aa == bb) {
            val sub = ctx.subtypes(aa)
            sub.typeclass.order(sub.cast(a), sub.cast(b))
          } else {
            Order[Int].order(aa, bb)
          }
        } else {
          sys.error(s"bug? $ctx $a $b $aa $bb")
        }
      }

      override def equal(x: A, y: A): Boolean =
        ctx.dispatch(x) { sub => sub.cast.isDefinedAt(y) && sub.typeclass.equal(sub.cast(x), sub.cast(y)) }
    }

  implicit def scalazOrderAutoInstance[A]: Typeclass[A] =
    macro Magnolia.gen[A]
}

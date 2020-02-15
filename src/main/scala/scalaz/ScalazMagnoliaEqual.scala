package scalaz

import magnolia.{CaseClass, Magnolia, SealedTrait}

object ScalazMagnoliaEqual {
  type Typeclass[A] = scalaz.Equal[A]

  def combine[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] =
    Equal.equal[A] { (a, b) => ctx.parameters.forall { p => p.typeclass.equal(p.dereference(a), p.dereference(b)) } }

  def dispatch[A](ctx: SealedTrait[Typeclass, A]): Typeclass[A] =
    Equal.equal[A] { (a, b) =>
      ctx.dispatch(a) { sub => sub.cast.isDefinedAt(b) && sub.typeclass.equal(sub.cast(a), sub.cast(b)) }
    }

  implicit def scalazEqualAutoInstance[A]: Typeclass[A] =
    macro Magnolia.gen[A]
}

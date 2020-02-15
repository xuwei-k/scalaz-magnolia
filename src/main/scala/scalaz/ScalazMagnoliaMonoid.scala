package scalaz

import magnolia.{CaseClass, Magnolia, SealedTrait}

object ScalazMagnoliaMonoid {
  type Typeclass[A] = scalaz.Monoid[A]

  def combine[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] =
    new Monoid[A] {
      def append(a: A, b: => A): A =
        ctx.construct { p => p.typeclass.append(p.dereference(a), p.dereference(b)) }

      def zero: A =
        ctx.construct(_.typeclass.zero)
    }

  def dispatch[A](ctx: SealedTrait[Typeclass, A]): Typeclass[A] =
    throw new NotImplementedError("ScalazMagnoliaMonoid does not support dispatch")

  implicit def scalazMonoidAutoInstance[A]: Typeclass[A] =
    macro Magnolia.gen[A]
}

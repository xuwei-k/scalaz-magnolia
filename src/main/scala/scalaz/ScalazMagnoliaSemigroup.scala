package scalaz

import magnolia.{CaseClass, Magnolia, SealedTrait}

object ScalazMagnoliaSemigroup {
  type Typeclass[A] = scalaz.Semigroup[A]

  def combine[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] =
    Semigroup.instance[A] { (a, b) =>
      ctx.construct { p =>
        p.typeclass.append(p.dereference(a), p.dereference(b))
      }
    }

  def dispatch[A](ctx: SealedTrait[Typeclass, A]): Typeclass[A] =
    throw new NotImplementedError("ScalazMagnoliaSemigroup does not support dispatch")

  implicit def scalazSemigroupAutoInstance[A]: Typeclass[A] =
    macro Magnolia.gen[A]
}

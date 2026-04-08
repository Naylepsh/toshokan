package core.types

type PositiveInt = PositiveInt.Type
object PositiveInt extends neotype.Subtype[Int]:
  override inline def validate(input: Int): Boolean | String = input > 0

opaque type AtLeastTwoUnique[A] = Set[A]
object AtLeastTwoUnique:
  def fromList[A](list: List[A]): Either[String, AtLeastTwoUnique[A]] =
    val set = list.toSet
    if set.size >= 2 then Right(set) else Left("Need at least 2 unique elements")

  extension [A](self: AtLeastTwoUnique[A])
    def toList: List[A] = self.toList
    def contains(a: A): Boolean = self.contains(a)

  given [A: io.circe.Decoder]: io.circe.Decoder[AtLeastTwoUnique[A]] =
    io.circe.Decoder[List[A]].emap(fromList)

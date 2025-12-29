package core.types

type PositiveInt = PositiveInt.Type
object PositiveInt extends neotype.Subtype[Int]:
  override inline def validate(input: Int): Boolean | String = input > 0

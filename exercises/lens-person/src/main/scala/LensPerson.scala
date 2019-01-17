import java.time.LocalDate

import monocle.{Iso, Lens}
import monocle.macros.GenLens

object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int,
    _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  // Implement these.

  private val personLens = GenLens[Person]

  private val born: Lens[Person, Born] = personLens(_._born)

  private val address: Lens[Person, Address] = personLens(_._address)

  private val bornLens = GenLens[Born]

  private val bornAt: Lens[Born, Address] = bornLens(_._bornAt)

  private val bornOn: Lens[Born, EpochDay] = bornLens(_._bornOn)

  private val addressLens: GenLens[Address] = GenLens[Address]

  private val street: Lens[Address, String] = addressLens(_._street)

  private val gregorianLens = GenLens[Gregorian]

  private val month: Lens[Gregorian, Int] = gregorianLens(_._month)

  private def epochDayToGreg(epochDay: EpochDay) = {
    val localDate = LocalDate.ofEpochDay(epochDay)
    Gregorian(localDate.getYear, localDate.getMonthValue, localDate.getDayOfMonth)
  }

  private def gregorianToEpochDay(gregorian: Gregorian) =
    LocalDate.of(gregorian._year, gregorian._month, gregorian._dayOfMonth).toEpochDay

  private val epochDayToGregorian: Iso[EpochDay, Gregorian] =
    Iso[EpochDay, Gregorian](epochDayToGreg)(gregorianToEpochDay)

  val bornStreet: Born => String = (bornAt composeLens street).get

  val setCurrentStreet: String => Person => Person = (address composeLens street).set

  val setBirthMonth: Int => Person => Person =
    (born composeLens bornOn composeIso epochDayToGregorian composeLens month).set

  // Transform both birth and current street names.
  val renameStreets: (String => String) => Person => Person = { f =>
    val addressStreetLens = address composeLens street
    val bornAtStreetLens = born composeLens bornAt composeLens street
    addressStreetLens.modify(f) compose bornAtStreetLens.modify(f)
  }
}

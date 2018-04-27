package services

object NumberToStringService {

  def numberToString(num: Int): String =
    if (num < 0 || num >= 10000) outOfRangeMessage
    else if (num == 0) "zero"
    else nameMap.foldLeft(List.empty[String], num) {
      case ((string, currentNum), (index, map)) =>
        map.get(currentNum / index) match {
          case Some(numberName) =>
            val remainder = currentNum % index

            val newName = if ((num >= 100) && (!(string contains "and") && (currentNum < 100))) {
              string :+ "and" :+ numberName
            } else {
              string :+ numberName
            }

            (newName, remainder)
          case None =>
            (string, currentNum)
        }
    }._1.mkString(" ")

  private val onesMap = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine"
  )

  private val teensMap = Map(
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen"
  )

  private val tensMap = Map(
    2 -> "twenty",
    3 -> "thirty",
    4 -> "forty",
    5 -> "fifty",
    6 -> "sixty",
    7 -> "seventy",
    8 -> "eighty",
    9 -> "ninety"
  )

  private val nameMap = Map(
    1000 -> (onesMap mapValues (_ + " thousand")),
    100 -> (onesMap mapValues (_ + " hundred")),
    10 -> tensMap,
    1 -> (onesMap ++ teensMap)
  )


  private val outOfRangeMessage = "out of range"
}

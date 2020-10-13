package aula2

object exercicios {

  type Entry = (String, String, String) // Name <-> Phone Number <-> Email Address
  type LTelef = List[Entry]

  def main(args: Array[String]): Unit = {
    println(transf(List(1, 2, 3)))
    println(productOfAllElements(List(1, 2, 3)))
    println(productOfAllElements(List(5)))

    println(placesAtEnd_1(List(2, 4), 6))

    println(concatenate(List(1, 2), List(3, 4)))

    println(sumEven(List((1, 1), (2, 2), (3, 3), (4, 4))))

    println("--------")

    println(getSizeAndSum(List(0)))

    println(customAverage(List(1, 2, 3, 4)))

    println(divideList(List(1, 2, 3)))

    println(belowValue(List(1, 2, 3, 4, 5, 6, 7), 4))

    println(superiorToAverage(List(1, 2, 3, 4, 5, 6)))

    println("--------")

    var e1: Entry = ("Miguel", "92917214", "marrf")
    var e2: Entry = ("Maria", "21111", "maria_email")
    var e3: Entry = ("Martim", "2134", "martim_email")

    var ltelef: LTelef = List(e1, e2, e3)

    println(specificEmails(ltelef))

    println("--------")

    println(getEmailAndPhone(ltelef, "Miguel"))

  }

  def transf[A](ls1: List[A]): List[A] = ls1 match {
    case Nil => Nil
    case a :: Nil => List(a)
    case head :: b :: tail => b :: head :: transf(tail)
  }

  def productOfAllElements(lst: List[Int]): Int = lst match {
    case Nil => 1
    case head :: tail => head * productOfAllElements(tail)
  }

  def placesAtEnd[A](lst: List[A], item: A): List[A] = lst match {
    case Nil => List(item)
    case a :: tail => a :: placesAtEnd(tail, item)
  }

  //Or we could also do the following;
  def placesAtEnd_1[A](lst: List[A], item: A): List[A] = (lst ++ List(item))

  def concatenate[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case head :: tail => head :: concatenate(tail, l2)
  }

  def sumEven(list: List[(Int, Int)]): Int = {
    auxSumEven(list, 1, 0)
  }

  def auxSumEven(lst: List[(Int, Int)], counter: Int, accumulator: Int): Int = {
    lst match {
      case Nil => accumulator

      case head :: tail =>
        if (counter == 2 || counter == 4)
          auxSumEven(tail, counter + 1, head._1 + head._2 + accumulator)
        else
          auxSumEven(tail, counter + 1, accumulator)
    }
  }

  def getSizeAndSumAux(lst: List[Double], length: Int, sum: Double): (Int, Double) = lst match {
    case Nil => (length, sum)
    case head :: tail => {
      getSizeAndSumAux(tail, length + 1, head + sum)
    }
  }

  def getSizeAndSum(lst: List[Double]): (Int, Double) = {
    getSizeAndSumAux(lst, 0, 0)
  }

  def customAverage(lst: List[Double]): Double = {
    val aux = getSizeAndSum(lst)
    aux._2 / aux._1
  }

  def divideList[A](l1: List[A]): (List[A], List[A]) = {
    (divideListAux(l1, (l1.length + 1) / 2), l1.takeRight(l1.length / 2))
  }

  def divideListAux[A](l1: List[A], counter: Int): List[A] = {
    l1 match {
      case Nil => Nil
      case head :: tail => counter match {
        case 0 => Nil
        case _ => head :: divideListAux(tail, counter - 1)
      }
    }
  }

  def belowValue(lst: List[Double], value: Double): (List[Double], List[Double]) = lst match {
    case Nil => (Nil, Nil)
    case head :: tail => if (head < value)
      (head :: belowValue(tail, value)._1, belowValue(tail, value)._2)
    else
      (belowValue(tail, value)._1, head :: belowValue(tail, value)._2)
  }

  def superiorToAverage(lst: List[Double]): List[Double] = {
    val aux = customAverage(lst)
    superiorToAverageAux(lst, aux)
  }

  def superiorToAverageAux(lst: List[Double], average: Double): List[Double] = lst match {
    case Nil => Nil
    case head :: tail => if (head > average) (head :: superiorToAverageAux(tail, average))
    else
      superiorToAverageAux(tail, average)
  }


  def specificEmails(lst: LTelef): List[String] = {
    lst match {
      case Nil => Nil
      case (x, y, email) :: tail => if (y(0) == '2') email :: (specificEmails(tail))
      else
        specificEmails(tail)
    }
  }

  def getEmailAndPhone(lst: LTelef, given_name: String): (String, String) = lst match {
    case Nil => ("", "")
    case (name, phoneNumber, email) :: tail =>
        if (name == given_name) return (phoneNumber, email)
      else
        getEmailAndPhone(tail, given_name)
  }

}

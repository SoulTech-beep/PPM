package aula2

object exercicios1 {

  def main(args: Array[String]): Unit = {

    println(fatorialWithoutIf(4))
    println(fatorialWithIf(4))
    println(fatorialTailRecursive(4))

    println(lazyListRange(1, 100).take(3).toList)

    println(ex3a(List(1,2,3), List(4,5,6)))


    var L1 = List(1,2,3)
    var L2 = List(4,5,6)

    var f = (a: Int,b: Int) => a*b

    println(zipWith(L1, L2, (x: Int,y:Int) => x*y))

    var L3 = List( (1,2), (4,3), (5, 10))

    println(Paresord(L3))

    var L4 = List("Miguel", "António", "Ramos","Ferreira")
    println(myconcat(L4))

    println(Maximum(List((2.0, 4.0),(3.2, 1.9))))

    println(indicative("253",List("253116787", "213448023", "253119905")))

    println(abbreviateNames(List("Miguel Ramos Ferreira", "Manuel Vicente Roberto")))

    println(abbreviateNames_1(List("Miguel Ramos Ferreira", "Manuel Vicente Roberto")))

  }

  def fatorialWithoutIf( n : Int) : Int = n match {
    case 0 => 1
    case x => x * fatorialWithIf(x-1)
  }

  def fatorialWithIf(n : Int) : Int = {
    if(n == 0) 1
    else
      n * fatorialWithoutIf(n - 1)
  }

  // Cases são pattern matching e não são considerados como ifs!

  def fatorialTailRecursive( n : Int ) : Int = {
    def loop( m : Int, acc: Int): Int = {
      if( m > n) acc
      else loop(m+1, m*acc)
    }
    loop(1,1)
  }

  def lazyListRange(lo : Int, hi: Int): LazyList[Int] = {
    println(lo)
    if(lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo+1, hi)
  }

  def ex3a( l1 : List[Int], l2: List[Int]): List[Int] =  (l1, l2) match{
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a::b, c::d) => (a+c) :: ex3a(b, d)
  }

  def zipWith[A, B, C](l1: List[A], l2:List[B], funcao: (A, B) => C): List[C] = (l1, l2) match{
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a::b, c::d) => funcao(a, c) :: zipWith(b,d, funcao)
  }

  def generica(op: (Int, Int) => Int, funcao: Int => Int, default: Int, a: Int, b: Int) : Int = {
    if(a > b) default
    else op(funcao(a), generica(op, funcao, default, a+1, b))
  }

  def Paresord( lst : List[(Int, Int)]  ) : List[(Int, Int)] = lst match {
    case Nil => Nil
    case (a,b)::(tail) => if(a < b) (a,b) :: Paresord(tail)
    else Paresord(tail)
  }

  def Paresord_1( lst : List[(Int, Int)]  ) : List[(Int, Int)] = lst match {
    case Nil => Nil
    case head::(tail) => if(head._1 < head._2) (head._1,head._2) :: Paresord(tail)
    else Paresord(tail)
  }
  //Esta faz um Filter

  def myconcat( lst : List[String] ) : String = lst match {
    case Nil => ""
    case head::tail => head + myconcat(tail)
  }
  // Esta faz um folding

  def Maximum( lst: List[(Double, Double)] ) : List[Double] = lst match {
    case Nil => Nil
    case (a,b)::tail => if(a < b ) b :: Maximum(tail)
    else a :: Maximum(tail)
  }
  // Esta fez um map

  def indicative(ind: String, telefs: List[String]) : List[String] = telefs match {
    case Nil => Nil
    case head::tail => if(head.substring(0, ind.length).equals(ind)) head :: indicative(ind, tail)
    else indicative(ind, tail)
  }

  def abbreviateNames( lst: List[String] ): List[String] = lst map( x => s"${x(0)}. ${x.split(' ').last}")

  def abbreviateNames_1(lst: List[String] ):List[String] = lst map(x => s"${f(x)._1}. ${f(x)._2}")

  def f(s : String) : (Char, String) = {
    (s(0), s.split(' ').last)
  }
}

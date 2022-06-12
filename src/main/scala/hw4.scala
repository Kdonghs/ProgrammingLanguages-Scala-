object hw4 {
  def list_add(l1: List[Int], l2: List[Int]): List[Int] = {
    /*두 개의 리스트를 원소 단위로 더하여 하나의 리스트로 반환*/
    l1.reverse.zipAll(l2.reverse,0,0).map(x => x._1 + x._2)
  }

  def insort(l: List[Int], m: Int): List[Int] = {
    /*리스트에 정수형 숫자 m를 추가하고 정렬하여 반환*/
    (l :+ m).sorted
  }

  def ltake(l: List[Int], i: Int): List[Int] = {
    /*리스트의 끝에서부터 i개의 원소를 포함하는 리스트를 반환*/
    l.takeRight(i)
  }

  def lall(f: Int => Boolean, l: List[Int]): Boolean = {
    /*정수 리스트 l과 정수를 입력받아 불 값을 출력하는 함수 f를 입력받아 리스트의 모든 원소가 f를 적용했을 때
    true로 평가되면 true, 아니면 false를 반환하는 함수*/
    !l.map(f).contains(false)
  }

  def lmap(f: Int => Int, l: List[Int]): List[Int] = {
    /*리스트의 모든 원소에 함수 f를 적용한 결과값들의 리스트를 반환*/
    l.map(f)
  }

  def lfilter(f:Int => Boolean, l: List[Int]): List[Int] = {
    /*리스트의 모든 원소에 함수 f를 적용하고 결과값이 true인 값들만 모아 리스트로 반환*/
    l.filter(f)
  }

  def ltabulate(n: Int, f: Int => Int): List[Int] = {
    /*정수 n과 함수 f를 입력받아, 제곱를 순서대로 갖는 리스트를 반환((9, 4, 1, 0))*/
    (0 until n).toList.reverse.map(f)
  }

  def lrev(l: List[Int]): List[Int] = {
    /*리스트를 반대로 반환*/
    l.reverse
  }

  def lconcat(l: List[List[Int]]): List[Int] = {
    /*리스트 여러 개를 하나의 리스트로 합쳐 반환*/
    l.flatten
  }

  def lfoldr(f: (Int, Int) => Int, l: List[Int], e: Int): Int = {
    /*시작 값 e와 리스트 l, 함수 f가 주어졌을 때 아래 식을 계산하는 함수*/
    l.foldRight(e)(f)
  }

  def lzip(a: List[String], b: List[Int]): List[(String, Int)] = {
    /*리스트 두 개를 하나의 리스트로 묶어 반환*/
    a.drop(a.length - Math.min(a.length,b.length)).zip(b.drop(b.length - Math.min(a.length,b.length)))
  }

  def split(l: List[Int]): (List[Int], List[Int]) = {
    /*주어진 리스트를 홀수 번째 원소들의 리스트와 짝수 번째 원소들의 리스트로 묶어 반환*/
    l.partition(x => l.indexOf(x) % 2 == 0)
  }

  def cartprod(l1: List[Int], l2: List[Int]) = {
    /*두 개의 리스트의 데카르트 곱(Cartesian product)을 반환*/
    /*(1,2)(1,2,3)->1,1  1,2  1,3  2,1  2,2  2,3*/
    l1.flatMap(x => l2.map(y => (x, y)))
  }

  def main(args: Array[String]): Unit = {
    println(list_add(List(1, 2, 3), List(1, 2, 3, 4, 5)))

    println(insort(List(1, 2, 4, 5), 3))

    println(ltake(List(1, 2, 3, 4), 2))

    println( lall(x => x > 0, List(1, 2, 3, 4)))

    println(lmap(x => x + 1, List(1, 2, 3)))

    println(lfilter(x => x > 2, List(1, 2, 3)))

    println(ltabulate(4, (x:Int) => x * x))

    println( lrev(List(1, 2, 3)))

    println(lconcat(List(List(1, 2, 3), List(4, 5, 6))))

    println(lfoldr((x:Int, y:Int) => x - y, List(1, 2, 3), 0))

    println(lzip(List("A", "B", "C", "D"), List(1, 2, 3, 4, 5, 6)))

    println( split(List(1, 2, 3, 4, 5)))

    println(cartprod(List(1, 2), List(3, 4, 5)))
  }

}

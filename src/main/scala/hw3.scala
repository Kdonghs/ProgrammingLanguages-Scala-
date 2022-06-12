object hw3 {
  sealed trait Tree[Int]
  case class Leaf[Int](elem: Int) extends Tree[Int]
  case class Node[Int](elem: Int, left: Tree[Int], right: Tree[Int]) extends Tree[Int]

  sealed trait Exp[Int]
  case class INT[Int](value: Int)extends Exp[Int]
  case class ADD[Int](a: Exp[Int], b: Exp[Int])extends Exp[Int]
  case class SUB[Int](a: Exp[Int], b: Exp[Int])extends Exp[Int]
  case class MUL[Int](a: Exp[Int], b: Exp[Int])extends Exp[Int]
  case class DIV[Int](a: Exp[Int], b: Exp[Int])extends Exp[Int]

  sealed trait Bool
  case class And(a: Bool, b: Bool)extends Bool
  case class Or(a: Bool, b: Bool)extends Bool
  case class Neg(a: Bool)extends Bool
  case class Imply(a: Bool, b: Bool)extends Bool
  case object True extends Bool
  case object False extends Bool
  // 입력 n은 0보다 큰 정수라고 가정합니다.

  def sum(n: Int): Int = {
    // 자기호출 함수를 사용하여 계산합니다.
    if (n==1){
      return 1
    }else{
      return sum(n-1)+n
    }
  }

  // 입력 n은 0보다 큰 정수라고 가정합니다.

  def fac(n: Int): Int = {
    // n이 1이면 1로 출력합니다
    // 그렇지 않으면 재귀함수를 사용하여 계산합니다.
    if (n==1){
      return 1
    }else{
      return fac(n-1)*n
    }
  }
  // 입력 n은 0보다 큰 정수라고 가정합니다.

  def fib(n: Int): Int = n match {
    // n 이 1 또는 2면 1을 출력합니다.
    // n이 3 이상인 경우에는
    case 1=> return 1
    case 2=> return 1
    case _ => fib(n-1)+fib(n-2)
  }

  // 입력 m과 n은 0 이상이고 m+n은 0보다 크다고 가정합니다.

  def gcd(n: Int, m: Int): Int = {
    // 작은 값과 큰 값을 먼저 출력합니다.
    // 유클리드 호제법을 사용하여 자기호출 함수로 계산하세요
    if (m<n%m){
      println("gcd " + m+" " + n%m)
    }else{
      println("gcd " + n%m +" "+ m)
    }

    if(n%m==0){
      return m
    }else{
      return gcd(m,n%m)
    }
  }

  // 입력 list는 List(3, 4, 14, 7, 2)와 같은 형태의 정수 리스트라고 가정합니다.

  def max(list: List[Int]): Int = {
    // 정수 리스트에서 가장 큰 값을 반환하는 자기호출 함수를 작성하세요.
    // 빈 리스트를 입력받으면 0을 반환하세요.
    // 그렇지 않다면..
    if (list.isEmpty){
      return 0
    }else{
      if (list.length==1){
        return list.last
      }
      var com = max(list.drop(1))
      if (list(0) > com){
        return list(0)
      }else{
        return com
      }
    }
  }

  def sum_tree( t: Tree[Int]): Int = {
    //재귀함수를 이용하여 트리의 합을 구하세요.
    t match {
      case Node(elem, left, right) => return elem + sum_tree(left) + sum_tree(right)
      case Leaf(elem) =>return elem
    }
  }

  def depth(t: Tree[Int]): Int ={
    //재귀함수를 이용하여 트리의 깊이를 구하세요.
    t match {
      case Node(elem, left, right) => return if (depth(left) > depth(right)){return depth(left)+1}else{depth(right)+1}
      case Leaf(elem) => return 0
    }
  }

  def bin_search(t: Tree[Int], x: Int):Boolean =  {
    // 재귀함수를 이용하여 이진 검색을 수행합니다.
    t match {
      case Node(elem, left, right) => if (elem==x){return true}else if(bin_search(left,x)){
        return true
      }else if(bin_search(right,x)){
        return true
      }
      case Leaf(elem) => if (elem==x){return true}
    }
    return false
  }

  def interp(exp: Exp[Int]): Int =  {
    // 재귀함수를 사용하여 함수를 작성하세요.
    exp match {
      case ADD(a, b) => return interp(a)+interp(b)
      case SUB(a, b) => return interp(a)-interp(b)
      case MUL(a, b) => return interp(a)*interp(b)
      case DIV(a, b) => return interp(a)/interp(b)
      case INT(value) => return value
    }
    return 0
  }

  def formulaJudge(a:Bool): Boolean = {
    var i = true
    if (a!=True & a!=False) {
      i = formula(a)
    }else if(a==True){
      i=true
    }else{
      i=false
    }

    return i
  }

  def formula(fma: Bool): Boolean = fma match {
    // 재귀함수를 사용하여 Neg, Or, And, Imply 논리 연산자로 구성된 논리식을 계산하는 함수를 만드세요
      case Or(a, b) =>
        var i = formulaJudge(a)
        var j = formulaJudge(b)

        if (i==false & j==false){return false}else{return true}

      case And(a, b) =>
        var i = formulaJudge(a)
        var j = formulaJudge(b)

        if (i==true &  j==true){return true}else{return false}

      case Imply(a, b) =>
        var i = formulaJudge(a)
        var j = formulaJudge(b)

        if (i==true &  j==false){return false}else{return true}

      case Neg(a) =>
        if (a==False){return true}else{return false}

      case True => return true

      case False => return false

  }



  def main(args: Array[String]): Unit = {
    println("** p1 **")
    println(sum(100))

    println("** p2 **")
    println(fac(9))

    println("** p3 **")
    println(fib(10))

    println("** p4 **")
    println(gcd(15,20))

    println("** p5 **")
    var x=List(3,4,14,7,2)
    println(max(x))

    println("** p6 **")
    val tree = Node(7, Node(3, Leaf(1), Leaf(2)), Leaf(4))
    println("Sum if tree nodes: " + sum_tree(tree))

    println("** p7 **")
    println("depth of tree nodes: " + depth(tree))

    println("** p8 **")
    println("bin_search tree nodes: " + bin_search(tree,3))

    println("** p9 **")
    val exp = ADD(SUB(INT(100), INT(10)), MUL(INT(2), INT(8)))
    println("Interpretation of the expression" +  interp(exp))

    println("** p10 **")
    val fma = Imply(And(True, Or(True, False)), False)
    println(formula(fma))
  }
}

object hw2 {
  def sum(n:Int): Int = {
    // n이 0보다 작거나 같으면 출력값은 0이 됩니다.
    // n이 0보다 크면 공식으로 계산합니다.
    if (n<=0){
      return 0
    }else{
      return n*(n+1)/2
    }

  }

  def circle(r:Double): Double={
    // r이 0.0보다 작거나 같으면 원의 넓이는 0이 됩니다.
    // r이 0.0보다 크면 공식을 사용하여 계산합니다.
    if (r<=0){
      return 0
    }else{
      return 3.14*r*r
    }
  }

  def concat(name: String): String = {
    // "Hello " 를 삽입합니다.
    var hello = "Hello "
    return hello+name
  }

  def xor(x: Boolean, y: Boolean) : Boolean = {
    // 위의 표와 같은 조건을 사용하여
    // x,y의 입력에 따라 출력값을 정해줍니다.
    if (x==y){
      return false
    }else{
      return true
    }
  }

  def triangle(x: Int, y:Int, z:Int) : Boolean = {
    // 세 변 중에 하나라도 0보다 작거나 같으면 삼각형은 존재하지 않습니다.
    // 세 변이 모두 0보다 크면 위의 조건을 사용하여 삼각형이 존재하는지 확인해줍니다.
    var li = Array(x,y,z).sorted
    if ((li(0) + li(1)>li(2))){
      return true
    }else{
      return false
    }
  }

  def int_if_then_else(b: Boolean, x: Int, y: Int) : Int = {
    // b가 true이면 두 수의 합을
    // b가 false이면 두 수의 차를 구합니다.
    if(b){
      return x+y
    }else{
      if(x>y){
        return x-y
      }else{
        return y-x
      }
    }
  }

  def sum_of_fun_val(a:Int, b:Int, c:Int, d:Int, e:Int): Int = {
    // f(d) + f(e) 를 구해줍니다.
    var A = a*d*d + b*d + c
    var B = a*e*e + b*e + c
    return A+B
  }

  def comp3(a:Int, b:Int, c:Int, d:Int): Int = {
    // f(f(f(d)))를 구해줍니다.
    var A=d
    for (i<- 1 to 3) {
      A = a*A*A + b*A + c
    }
    return A
  }

  def string2(s: String): String = {
    // 입력한 문자열 s를 두 번 반복하는 값을 구해줍니다.
    return s+s
  }

  def string256(s: String): StringBuilder = {
    // StringBuilder를 사용하여 구합니다.
    val sb: StringBuilder = new StringBuilder

    for (i <-1 to 256){
      sb.append(s)
    }
    return sb
  }
  def string256_use_string2(s: String): String = {
    // string2를 호출하여 구합니다.
    var S = ""
    for (i<-1 to 128){
      S += string2(s)
    }
    return S
  }

  def main(args: Array[String]): Unit = {
    println("** p1 **")
    println(sum(-10))
    println(sum(100))

    println("** p2 **")
    println(circle(-10.1))
    println(circle(4.2))

    println("** p3 **")
    println(concat("Bob!"))
    println(concat("Alice!"))

    println("** p4 **")
    println(xor(true,true))
    println(xor(true,false))
    println(xor(false,true))
    println(xor(false,false))

    println("** p5 **")
    println(triangle(-3,3,1))
    println(triangle(3,4,5))
    println(triangle(100,1,2))

    println("** p6 **")
    println(int_if_then_else(true, 2, 100))
    println(int_if_then_else((100<2), 2, -2))

    println("** p7 **")
    println(sum_of_fun_val(1,2,1,3,4))
    println(sum_of_fun_val(1,-3,-1,200,123))

    println("** p8 **")
    println(comp3(1,1,1,1))
    println(comp3(1,-2,1,3))

    println("** p9 **")
    println(string2("hi"))
    println(string2("abcde"))

    println("방법 1")
    println(string256("a"))

    println("** p10 **")
    println("방법 2")
    println(string256_use_string2("a"))
  }
}

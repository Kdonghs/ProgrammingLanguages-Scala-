object hw1 {
  sealed trait date
  case object MON extends date
  case object TUE extends date
  case object WED extends date
  case object THU extends date
  case object FRI extends date
  case object SAT extends date
  case object SUN extends date

  def main(args: Array[String]): Unit = {
    val grade = Array("Aplus", "A", "Bplus", "B", "Cplus", "C", "Dplus", "D", "F")

    var student_num = 201811586 // 학번, 정수
    var email = "kkk951744@icloud.com" // 이메일 주소, 문자열
    var phone_number = "01041879517" // 폰 번호, 문자열
    var favorite_day = FRI // 좋아하는 요일, trait 활용
    var expected_grade = grade(2) // 예상 학점, 문자열 배열
    var is_male = true // 성별, Boolean

    println("학번 : " + student_num)
    println("이메일 주소 : " + email)
    println("핸드폰 번호 : " + phone_number)
    println("좋아하는 요일 : " + favorite_day)
    println("예상 학점 : " + expected_grade)
    println("나는 남자이다 : " + is_male)
    println("10.0 / 0.4 = " + div(10,0.4))
  }
  def div(n1:Double,n2:Double): Double= n1 / n2
}

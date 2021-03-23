package u04lab.code

import Lists._
import u04lab.code.Lists.List.{Cons, appendByFold, length}
import u04lab.code.Try.courses // import custom List type (not the one in Scala stdlib)


object University {

  trait Student {
    def name: String

    def year: Int

    def enrolling(courses: Course*): Unit // the student participates to a Course
    def courses: List[String] // names of course the student participates to
    def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
  }

  trait Course {
    def name: String
    def teacher: String
  }

  object Student {
    def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

    private case class StudentImpl(override val name: String, override val year: Int) extends Student {
      private var coursesSet: List[Course] = List.nil
      override def enrolling(courses: Course*): Unit = for (course <- courses) {
        coursesSet = Cons(course, coursesSet)
      }
      override def courses: List[String] = List.map(coursesSet)(_.name)
      override def hasTeacher(teacher: String): Boolean = List.contains(List.map(coursesSet)(_.teacher), teacher)
    }

  }

  object Course {
    def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

    private case class CourseImpl(override val name: String, override val teacher: String) extends Course {
      assert(name != null && teacher != null)
    }
  }

  object sameTeacher {
    def unapply(courses: List[Course]): Option[String] = {
      val firstTeacher: String = courses match {
        case Cons(h, _) => h.teacher
      }
      Option.when(List.foldLeft(courses)(true)((b, c) => b && c.teacher == firstTeacher))(firstTeacher)
    }
  }

  def printSameTeacher(courses: List[Course]): Unit = courses match {
    case sameTeacher(t) => println(s" $courses have same teacher $t")
    case _ => println(s" $courses have different teachers ")
  }
}

object Try extends App {
  import University._

  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS, cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true
  println(s1.hasTeacher("Ghini")) // false

  val cOOP = Course("PCD","Viroli")
  val courses = List(cPPS, cOOP)
  printSameTeacher(courses) // Cons(CourseImpl(PPS,Viroli),Cons(CourseImpl(PCD,Viroli),Nil())) have same teacher Viroli

  val coursesEmpty = List(cPPS)
  printSameTeacher(coursesEmpty) // Cons(CourseImpl(PPS,Viroli),Nil()) have same teacher Viroli

  val coursesDifferent = List(cPPS, cSDR)
  printSameTeacher(coursesDifferent) //Cons(CourseImpl(PPS,Viroli),Cons(CourseImpl(SDR,D'Angelo),Nil())) have different teachers
}

/** Hints:
  * - simply implement Course, e.g. with a case class
  * - implement Student with a StudentImpl keeping a private Set of courses
  * - try to implement in StudentImpl method courses with map
  * - try to implement in StudentImpl method hasTeacher with map and find
  * - check that the two println above work correctly
  * - refactor the code so that method enrolling accepts a variable argument Course*
  */

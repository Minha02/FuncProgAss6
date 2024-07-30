import scala.io.StdIn

object Ass6q2{
    def getStudentInfo():(String, Int, Int, Double, Char)  ={
        println("Enter student's Name: ")
        val name = StdIn.readLine()
        println("Enter student's marks: ")
        val marks = StdIn.readInt()
        println("Enter total possible marks ")
        val totalMarks = StdIn.readInt()

        val percentage  = (marks.toDouble/totalMarks)*100
        val grade = calculateGrade(percentage)

        (name,marks,totalMarks,percentage,grade) 
    }

    def printStudentRecord(student: (String,Int,Int,Double,Char)):Unit = {
        val (name, marks, totalMarks, percentage, grade) = student
        println(s"Name: $name")
        println(s"Marks: $marks/$totalMarks")
        println(f"Percentage: $percentage%.2f%%")
        println(s"Grade: $grade")
    }

    def validateInput(name: String, marks: Int, totalMarks: Int):(Boolean, Option[String]) ={
        if (name.isEmpty) {
            (false, Some("Name cannot be empty."))
        } else if (marks < 0 || marks > totalMarks) {
            (false, Some("Marks must be between 0 and total possible marks."))
        } else {
            (true, None)
    }
    }

    def getStudentInfoWithRetry():(String, Int, Int, Double, Char)  ={
        var validInput = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

    while (!validInput) {
      studentInfo = getStudentInfo()
      val (name, marks, totalMarks, _, _) = studentInfo
      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)

      if (isValid) {
        validInput = true
      } else {
        println(s"Error: ${errorMessage.getOrElse("Unknown error")}")
      }
    }
    studentInfo
    }

    def calculateGrade(percentage:Double): Char = {
        if (percentage >= 90) 'A'
        else if (percentage >= 75) 'B'
        else if (percentage >= 50) 'C'
        else 'D'
    }

    def main(args: Array[String]): Unit = {
        val studentInfo = getStudentInfoWithRetry()
        printStudentRecord(studentInfo)
  }
}
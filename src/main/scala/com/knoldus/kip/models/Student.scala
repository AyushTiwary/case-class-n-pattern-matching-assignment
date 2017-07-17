package com.knoldus.kip.models

case class Student(id: Int, firstName: String, middleName: Option[String], lastName: String, rollNumber: Int, age: Option[Int],
                   gender: String, enrollemntNumber: Int, address: Option[String] ) extends ModelIdentifier
{

    def getAddress:String = address.fold("n/a"){_.toString}

    def getMiddleName:String =middleName.map(_.split(" ")(0)).getOrElse("")

}

case class Subject(id: Int, name: String, maxMarks: Int, obtainedMarks: Float) extends ModelIdentifier

case class Course(id: Int, name: String, category: String, subjects: List[Subject] ) extends ModelIdentifier


case class ScoreCard(id: Int, student: Student, subjects: List[Subject], total: Float, percentage: Float, grade: String ) extends ModelIdentifier
{

  def getSubjectWithHighestMarks : List[Subject] = {
    val highest = subjects.map(_.obtainedMarks).max
    subjects.filter(_.obtainedMarks == highest)
  }

  def getSubjectWithLowestMAarks : List[Subject] = {
    val lowest = subjects.map(_.obtainedMarks).min
    subjects.filter(_.obtainedMarks == lowest)
  }

}

object ScoreCard {

  def apply(student: Student, subjects: List[Subject]): ScoreCard = {

    val total = subjects.map(_.obtainedMarks) sum
    val percentage = total/subjects.map(_.maxMarks).sum

    val grade = if(percentage >= 95)
      {
        "A1"
      }
    else if(percentage >= 90)
      {
        "A2"
      }
    else if(percentage >= 80)
      {
        "B1"
      }
    else if(percentage >= 70)
      {
        "B2"
      }
    else if(percentage >= 60)
      {
        "C1"
      }
    else if(percentage >= 40)
      {
        "C2"
      }
    else
      {
        "Fail"
      }

    new ScoreCard(student.id, student, subjects, total, percentage, grade)
  }

}

case class CoursePerformance(id: Int, year: Int, course: Course, scoreCards: List[ScoreCard]) extends ModelIdentifier
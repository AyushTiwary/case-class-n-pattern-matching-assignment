package com.knoldus.kip.service
import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._


//import com.knoldus.kip.models.{Scoreboard, CoursePerformance}

trait Principal {


  def findOutIfCSE(id: Int): CoursePerformance = {

    val coursePerformance : Option[CoursePerformance] = RamDatabase.getById(id)

    coursePerformance.map(_.course.name) match {

      case Some("CSE") => coursePerformance.get
      case Some(_) => throw new Exception("Other than course CSE")
      case None => throw new Exception("Invalid Entry")

    }

  }

  def findOutIfAnyCourse(id: Int, courseName: String): CoursePerformance = {


    val coursePerformance : Option[CoursePerformance] = RamDatabase.getById(id)

    coursePerformance.map(_.course.name) match {

      case Some(`courseName`) => coursePerformance.get
      case Some(_) => throw new Exception("No Course-Name With The Given ID")
      case None => throw new Exception("No Course-Name With The Given ID")

    }

  }

  def expression(mod: Any): String = mod match {

    case Student(_, _, _, _, _, _, _, _, _) => "Shup-Up"
    case ScoreCard(_,_,_,_,_,_) => "Hmmm ...."
    case Subject(_,_,_,_) => "aha "
    case _ => "!!! ???"

  }

  def checkScorecard(scoreboard: List[ScoreCard]): List[String] = {

    for{scorecard  <- scoreboard
       subject <- scorecard.subjects}

        yield {

          subject match {

            case _  => scorecard.student.firstName + " " + subject.name + " " + subject.obtainedMarks

          }

        }

    }


  def expressionRevisited: PartialFunction[ModelIdentifier, String] = {


    case Student(_, _, _, _, _, _, _, _, _) => "Shup-Up"
    case ScoreCard(_,_,_,_,_,_) => "Hmmm ...."
    case Subject(_,_,_,_) => "aha "

  }

  /*  def apply(scoreboard: List[ScoreCard]): List[String] = {

      for {scorecard <- scoreboard
           subject <- scorecard.subjects}

        yield {

          subject match {

            case _ => scorecard.student.firstName + " " + subject.name + " " + subject.obtainedMarks

          }

        }

    }

    def isDefinedAt(scoreboard :List[ScoreCard] ) =

  }

*/

  }
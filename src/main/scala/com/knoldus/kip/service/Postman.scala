package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{ScoreCard, CoursePerformance}
//import com.knoldus.kip.objective1.models.{Scorecard, Student}

trait Postman {


  def getTheFirstAddressOfFirstYearPerformance(id: Int) : String =  {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val scoreCard : Option[ScoreCard] = coursePerformance.flatMap(_.scoreCards.headOption)

    val address : String = scoreCard.flatMap(_.student.address).getOrElse("Invalid I.D.")

    address

  }l

}

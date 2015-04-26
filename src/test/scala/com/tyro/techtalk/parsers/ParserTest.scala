package com.tyro.techtalk.parsers

import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  val ThreatFragment =
    """
    """.stripMargin

  val TargetFragment =
    """
    """.stripMargin

  val ScenariosFragment =
    """
    """.stripMargin

  val DSL =
    s"""
       |$ThreatFragment
        |
        |$TargetFragment
        |
        |$ScenariosFragment
    """.stripMargin

  val expectedThreats = Set()

  val expectedTargets = Set()

  "The DSL Parser" should "parse a valid identifier" in {
    pending
//    result should contain only "one"
  }

  it should "parse a list of identifiers" in {
    pending
//    result should contain inOrderOnly ("one", "two")
  }

  it should "parse a collection of threats" in {

    pending
    //    result should contain theSameElementsAs expectedThreats
  }

  it should "parse a collection of targets" in {

    pending
    //    targets should contain theSameElementsAs expectedTargets
  }

  it should "parse a collection of scenarios" in {

    pending
    //    results should contain theSameElementsInOrderAs expectedResults
  }

  it should "parse the entire DSL" in {
    pending
    //    results should contain theSameElementsInOrderAs expectedResults
  }
}

package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel.{Compromised, Target, Threat}
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  val ThreatFragment =
    """Threats:
      |  Furby is crippled by talking.
      |  TonyAbbot is crippled by [talking, budgieSmugglers].
      |  Dropbear has no weaknesses
    """.stripMargin

  val furby = Threat("Furby", "talking")
  val pm = Threat("TonyAbbot", "talking", "budgieSmugglers")
  val dropbear = Threat("Dropbear")

  val expectedThreats = List(furby, pm, dropbear)

  val TargetFragment =
    """Targets:
      |   TheEarth is defended by budgieSmugglers.
      |   Tyro is defended by jost.
      |   Superannuation is undefended
    """.stripMargin

  val earth = Target("TheEarth", "budgieSmugglers")
  val tyro = Target("Tyro", "jost")
  val superannuation = Target("Superannuation")

  val expectedTargets = List(earth, tyro, superannuation)

  val ScenariosFragment =
    """Scenarios:
      |  Superannuation vs TonyAbbot.
      |  Tyro vs Furby.
      |  TheEarth vs Dropbear
    """.stripMargin

  val expectedScenarios = List(Compromised, Compromised, Compromised)

  val DSL =
    s"""
       |$ThreatFragment
        |
        |$TargetFragment
        |
        |$ScenariosFragment
    """.stripMargin

  "The DSL Parser" should "parse a valid identifier" in {
    val input = "one"
    val result = Parser.parseAll(Parser.identList, input) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }
    result should contain only "one"
  }

  it should "parse a list of identifiers" in {
    val input = "[one, two]"
    val result = Parser.parseAll(Parser.identList, input) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain inOrderOnly("one", "two")
  }

  it should "parse a collection of threats" in {

    val result = Parser.parseAll(Parser.threatSection, ThreatFragment) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsAs expectedThreats
  }

  it should "parse a collection of targets" in {

    val result = Parser.parseAll(Parser.targetSection, TargetFragment) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsAs expectedTargets
  }

  it should "parse a collection of scenarios" in {

    val result = Parser.parseAll(Parser.scenarioSection(expectedThreats, expectedTargets), ScenariosFragment) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsInOrderAs expectedScenarios
  }

  it should "parse the entire DSL" in {

    val result = Parser.parseAll(Parser.entireDsl, DSL) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }
    result should contain theSameElementsInOrderAs expectedScenarios
  }
}

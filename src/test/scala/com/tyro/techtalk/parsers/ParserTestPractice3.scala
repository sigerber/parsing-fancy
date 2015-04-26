package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel._
import org.scalatest.{FlatSpec, Matchers}

class ParserTestPractice3 extends FlatSpec with Matchers {

  val ThreatFragment =
    """Threats {
      |   Pirates are vulnerable to cannons,
      |   Zombies are vulnerable to shotguns and thorns,
      |   Robots are vulnerable to hackers and lightning and RPGs,
      |   Ninjas are invulnerable
       }
    """.stripMargin

  val pirates = Threat("Pirates", "cannons")
  val zombies = Threat("Zombies", "shotguns", "thorns")
  val robots = Threat("Robots", "hackers", "lightning", "RPGs")
  val ninjas = Threat("Ninjas")

  val expectedThreats = Set(pirates, zombies, robots, ninjas)

  val TargetFragment =
    """Targets {
      | Forts are protected by cannons,
      | Tyro is protected by hackers and lego,
      | Plants are protected by gaia and druids and thorns,
      | Costco is unprotected
      | }
    """.stripMargin

  val forts = Target("Forts", "cannons")
  val tyro = Target("Tyro", "hackers", "lego")
  val plants = Target("Plants", "gaia", "druids", "thorns")
  val costco = Target("Costco")

  val expectedTargets = Set(forts, tyro, plants, costco)

  val ScenariosFragment =
    """Scenarios {
       What if Plants were attacked by Zombies?
       What if Tyro was attacked by Ninjas?
       }
    """.stripMargin

  val expectedScenarios = List(UnCompromised, Compromised)

  val DSL =
    s"""
       |$ThreatFragment
        |
        |$TargetFragment
        |
        |$ScenariosFragment
    """.stripMargin

  "The DSL Parser" should "parse a valid identifier" in {
    val fragment = "one"
    val result = ParserPractice3.parseAll(ParserPractice3.identList, fragment) match {
      case ParserPractice3.Success(parseResult, _) => parseResult
      case ParserPractice3.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }
    result should contain only "one"
  }

  it should "parse a list of two identifiers" in {
    val fragment = "one and two"
    val result = ParserPractice3.parseAll(ParserPractice3.identList, fragment) match {
      case ParserPractice3.Success(parseResult, _) => parseResult
      case ParserPractice3.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }
    result should contain inOrderOnly("one", "two")
  }

  it should "parse a collection of threats" in {

    val result = ParserPractice3.parseAll(ParserPractice3.threatSection, ThreatFragment) match {
      case ParserPractice3.Success(parseResult, _) => parseResult
      case ParserPractice3.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsAs expectedThreats
  }

  it should "parse a collection of targets" in {

    val result = ParserPractice3.parseAll(ParserPractice3.targetSection, TargetFragment) match {
      case ParserPractice3.Success(parseResult, _) => parseResult
      case ParserPractice3.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsAs expectedTargets
  }

  it should "parse a collection of scenarios" in {

    val result = ParserPractice3.parseAll(ParserPractice3.scenarioSection(expectedThreats, expectedTargets), ScenariosFragment) match {
      case ParserPractice3.Success(parseResult, _) => parseResult
      case ParserPractice3.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsInOrderAs expectedScenarios
  }

  it should "parse the entire DSL" in {
    val result = ParserPractice3(DSL) match {
      case ParserPractice3.Success(parseResult, _) => parseResult
      case ParserPractice3.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsInOrderAs expectedScenarios
  }
}

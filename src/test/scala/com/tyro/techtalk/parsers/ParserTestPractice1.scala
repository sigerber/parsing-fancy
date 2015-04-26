package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel._
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ParserTestPractice1 extends FlatSpec with Matchers {

  val ThreatFragment =
    """Threats:
      |  Pirates are defeated by cannons,
      |  Zombies are defeated by [priests, shotguns, nature],
      |  Robots are defeated by [hackers, WillSmith],
      |  Ninjas are never defeated
    """.stripMargin

  val TargetFragment =
    """Targets:
      |  Village is defended by priests,
      |  Fort is defended by cannons,
      |  America is defended by shotguns,
      |  Plants are defended by nature,
      |  Tyro is defended by [hackers, lego]
    """.stripMargin

  val ScenariosFragment =
    """Scenarios:
      |  Plants vs. Zombies,
      |  Tyro vs. Robots,
      |  America vs. Zombies,
      |  America vs. Ninjas
    """.stripMargin

  val DSL =
    s"""
       |$ThreatFragment
       |
       |$TargetFragment
       |
       |$ScenariosFragment
    """.stripMargin

  private val pirates: Threat = Threat("Pirates", "cannons")
  private val zombies = Threat("Zombies", "priests", "shotguns", "nature")
  private val robots = Threat("Robots", "hackers", "WillSmith")
  private val ninjas = Threat("Ninjas", Set.empty[CounterMeasure])

  val expectedThreats = Set(pirates, zombies, robots, ninjas)

  private val village = Target("Village", "priests")
  private val plants = Target("Plants", "nature")
  private val america = Target("America", "shotguns")
  private val tyro = Target("Tyro", "hackers", "lego")
  private val fort = Target("Fort", "cannons")

  val expectedTargets = Set(village, plants, america, tyro, fort)

  "The DSL Parser" should "parse a valid identifier" in {
    val fragment = "one"
    val result = ParserPractice1.parseAll(ParserPractice1.identList, fragment) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    result should contain only "one"
  }

  it should "parse a list of identifiers" in {
    val fragment = "[one, two]"

    val result = ParserPractice1.parseAll(ParserPractice1.identList, fragment) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    result should contain inOrderOnly ("one", "two")
  }

  it should "parse a collection of threats" in {

    val threats = ParserPractice1.parseAll(ParserPractice1.threatSection, ThreatFragment) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    threats should contain theSameElementsAs expectedThreats
  }

  it should "parse a collection of targets" in {

    val targets = ParserPractice1.parseAll(ParserPractice1.targetSection, TargetFragment) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    targets should contain theSameElementsAs expectedTargets
  }

  it should "parse a collection of scenarios" in {
    val scenarioFragment =
      """
        |Scenarios:
        |   Plants vs. Zombies,
        |   Tyro vs. Robots,
        |   America vs. Zombies,
        |   America vs. Ninjas
      """.stripMargin

    val expectedResults = Seq(UnCompromised, UnCompromised, UnCompromised, Compromised)

    val results = ParserPractice1.parseAll(ParserPractice1.scenarioSection(expectedThreats, expectedTargets), scenarioFragment) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    results should contain theSameElementsInOrderAs expectedResults
  }

  it should "parse the entire DSL" in {
    val expectedResults = Seq(UnCompromised, UnCompromised, UnCompromised, Compromised)

    val results = ParserPractice1(Source.fromString(DSL)) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    results should contain theSameElementsInOrderAs expectedResults
  }
}

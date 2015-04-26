package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel.{Compromised, Target, Threat, UnCompromised}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ParserTestPractice2 extends FlatSpec with Matchers {

  val ThreatFragment =
    """THREATS -
        Zombies countered by shotguns,
        Pirates countered by <cannons, scurvy>,
        Ninjas cannot be countered
    """.stripMargin

  val TargetFragment =
    """TARGETS -
       ShoppingMalls use shotguns for defense,
       Forts use <cannons, potatoes> for defense,
       NationalParks are undefended
    """.stripMargin

  val ScenariosFragment =
    """SCENARIOS -
       ShoppingMalls attacked by Zombies,
       Forts attacked by Ninjas,
       NationalParks attacked by Pirates
    """.stripMargin

  val DSL =
    s"""$ThreatFragment
        |
        |$TargetFragment
        |
        |$ScenariosFragment
    """.stripMargin

  val zombies = Threat("Zombies", "shotguns")
  val pirates = Threat("Pirates", "cannons", "scurvy")
  val ninjas = Threat("Ninjas")

  val expectedThreats = Set(zombies, pirates, ninjas)

  val shoppingmalls = Target("ShoppingMalls", "shotguns")
  val forts = Target("Forts", "cannons", "potatoes")
  val nationalParks = Target("NationalParks")

  val expectedTargets = Set(shoppingmalls, forts, nationalParks)

  "The DSL Parser" should "parse a valid identifier" in {
    val fragment = "one"

    val result = ParserPractice1.parseAll(ParserPractice1.identList, fragment) match {
      case ParserPractice1.Success(parseResult, _) => parseResult
      case ParserPractice1.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

    result should contain only "one"
  }

  it should "parse a list of identifiers" in {
    val fragment = "<one, two>"

    val result = ParserPractice2.parseAll(ParserPractice2.identList, fragment) match {
      case ParserPractice2.Success(parseResult, _) => parseResult
      case ParserPractice2.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain inOrderOnly("one", "two")
  }

  it should "parse a collection of threats" in {

    val result = ParserPractice2.parseAll(ParserPractice2.threatSection, ThreatFragment) match {
      case ParserPractice2.Success(parseResult, _) => parseResult
      case ParserPractice2.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsAs expectedThreats
  }

  it should "parse a collection of targets" in {

    val result = ParserPractice2.parseAll(ParserPractice2.targetSection, TargetFragment) match {
      case ParserPractice2.Success(parseResult, _) => parseResult
      case ParserPractice2.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    result should contain theSameElementsAs expectedTargets
  }

  it should "parse a collection of scenarios" in {

    val expectedResults = Seq(UnCompromised, Compromised, Compromised)

    val results = ParserPractice2.parseAll(ParserPractice2.scenariosSection(expectedThreats, expectedTargets), ScenariosFragment) match {
      case ParserPractice2.Success(parseResult, _) => parseResult
      case ParserPractice2.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    results should contain theSameElementsInOrderAs expectedResults
  }

  it should "parse the entire DSL" in {
    val expectedResults = Seq(UnCompromised, Compromised, Compromised)
    val results = ParserPractice2(Source.fromString(DSL)) match {
      case ParserPractice2.Success(parseResult, _) => parseResult
      case ParserPractice2.NoSuccess(msg, _) => throw new scala.RuntimeException(msg)
    }

    results should contain theSameElementsInOrderAs expectedResults
  }
}

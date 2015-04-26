package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel.{AttackScenario, CounterMeasure, Target, Threat}

import scala.util.parsing.combinator.JavaTokenParsers

object ParserPractice3 extends JavaTokenParsers {

  def apply(dsl: String) = parseAll(threatsTargetsScenarios, dsl)

  private def threatsTargetsScenarios = threatsAndTargets >> (scenarioSection _).tupled

  private def threatsAndTargets = threatSection ~ targetSection ^^ { case a ~ b => (a, b) }

  private[parsers] def scenarioSection(threats: Iterable[Threat], targets: Iterable[Target]) = {
    "Scenarios" ~> "{" ~> scenarios <~ "}" ^^ {
      val threatMap = threats.mapBy(_.name)
      val targetMap = targets.mapBy(_.name)
      _.map { case target ~ threat =>
        AttackScenario.modelAttack(targetMap(target), threatMap(threat))
      }
    }
  }

  private def scenarios = rep1(scenario)

  private def scenario = (scenarioPrefix ~> ident) ~ (vs ~> ident <~ "?")

  private def scenarioPrefix = "What" ~ "if"

  private def vs = ("were" | "was") ~ "attacked" ~ "by"

  private[parsers] def targetSection = "Targets" ~> "{" ~> targets <~ "}"

  private def targets = rep1sep(target, ",")

  private def target = ident ~ defenses ^^ {
    case name ~ countermeasures => Target(name, countermeasures.toSet)
  }

  private def defenses = ("are" | "is") ~> (someDefenses | noDefenses) ^^ {
    _.map { CounterMeasure }
  }

  private def someDefenses = "protected" ~> "by" ~> identList

  private def noDefenses = "unprotected" ^^^ { List.empty }

  private[parsers] def threatSection = "Threats" ~> "{" ~> threats <~ "}"

  private def threats = rep1sep(threat, ",")

  private def threat = ident ~ vulnerabilities ^^ {
    case name ~ countermeasures => Threat(name, countermeasures.toSet)
  }

  private def vulnerabilities = noVulnerabilities | someVulnerabilities ^^ {
    _.map { CounterMeasure }
  }

  private def someVulnerabilities = "are" ~> "vulnerable" ~> "to" ~> identList

  private def noVulnerabilities = "are" ~ "invulnerable" ^^^ { List.empty }

  private[parsers] def identList = rep1sep(ident, "and")
}

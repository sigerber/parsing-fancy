package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel.{AttackScenario, CounterMeasure, Target, Threat}

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by sgerber on 30/04/15.
 */
object Parser extends JavaTokenParsers {

  def entireDsl = threatsTargets >> (scenarioSection _).tupled

  def threatsTargets = (threatSection ~ targetSection) ^^ { case a ~ b => (a, b)}

  def scenarioSection(threats: Iterable[Threat], targets: Iterable[Target]) = "Scenarios:" ~> scenarios ^^ {
    val threatMap = threats.mapBy(_.name)
    val targetMap = targets.mapBy(_.name)

    _.map {
      case targetName ~ threatName =>
        val threat = threatMap(threatName)
        val target = targetMap(targetName)
        AttackScenario.modelAttack(target, threat)
    }
  }

  def scenarios = rep1sep(scenario, ".")

  def scenario = (ident <~ "vs") ~ ident

  def targetSection = "Targets:" ~> targets

  def targets = rep1sep(target, ".") ^^ {
    _ map {
      case name ~ defenses => Target(name, defenses.toSet)
    }
  }

  def target = ident ~ defenses

  def defenses = (someDefenses | noDefenses) ^^ {
    _ map { CounterMeasure }
  }

  def someDefenses = ("is" ~ "defended" ~ "by") ~> identList

  def noDefenses = "is undefended" ^^^ { List.empty }

  def threatSection = "Threats" ~> ":" ~> threats

  def threats = rep1sep(threat, ".") ^^ {
    _ map {
      case name ~ weaknesses => Threat(name, weaknesses.toSet)
    }
  }

  def threat = ident ~ weaknesses

  def weaknesses = noWeaknesses | someWeaknesses ^^ {
    _.map { CounterMeasure }
  }

  def noWeaknesses = "has no weaknesses" ^^^ { List.empty }

  def someWeaknesses = ("is" ~ "crippled" ~ "by") ~> identList

  def identList = oneIdent | manyIdent

  def oneIdent = rep1sep(ident, ",")

  def manyIdent = "[" ~> oneIdent <~ "]"
}

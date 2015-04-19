package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel._

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  def threatModelScenarios = threatSection >> targetSection >> scenarioSection

  private[parsers] def threatSection = "Threats:" ~> threats

  private[parsers] def targetSection(threats: Iterable[Threat]) = "Targets:" ~> targets(threats) ^^ { (threats, _) }

  private[parsers] def scenarioSection(threatsAndTargets: (Iterable[Threat], Iterable[Target])) = {

    val threatMap = threatsAndTargets._1.mapBy { _.name }
    val targetMap = threatsAndTargets._2.mapBy { _.name }

    def scenarios = rep1sep(scenario, ",")

    def scenario = (ident <~ "vs.") ~ ident ^^ {
      case targetName ~ threatName =>
        val target = targetMap(targetName)
        val threat = threatMap(threatName)

        AttackScenario.modelAttack(target, threat)
    }

    "Scenarios:" ~> scenarios
  }

  private def targets(threats: Iterable[Threat]) = {

    def target = (ident <~ ("is" | "are")) ~ defences ^^ {
      case name ~ counterMeasureNames =>
        val counterMeasures = counterMeasureNames.map(CounterMeasure).toSet
        Target(name, counterMeasures)
    }

    rep1sep(target, ",")
  }

  private def defences = noDefences | ("defended by" ~> identList)

  private def noDefences = "undefended" ^^^ { Set.empty }

  private def threats = rep1sep(threat, ",")

  private def threat = ident ~ counterMeasures ^^ {
    case name ~ counterMeasureNames =>
      val counterMeasures = counterMeasureNames.map(CounterMeasure)
      Threat(name, counterMeasures.toSet)
  }

  private def counterMeasures = "are" ~> (someCounterMeasures | noCounterMeasures)

  private def noCounterMeasures = "never defeated" ^^^ { Set.empty }

  private def someCounterMeasures = "defeated by" ~> identList

  private[parsers] def identList = singleElement | multiElement

  private def singleElement = repN(1, ident)

  private def multiElement = "[" ~> rep1sep(ident, ",") <~ "]"

  def apply(source: Source): ParseResult[Seq[AttackResult]] = {
    parseAll(threatModelScenarios, source.mkString)
  }
}
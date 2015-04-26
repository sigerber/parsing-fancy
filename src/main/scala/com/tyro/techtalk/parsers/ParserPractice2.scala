package com.tyro.techtalk.parsers

import com.tyro.techtalk.parsers.threatmodel._

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object ParserPractice2 extends JavaTokenParsers {

  def apply(source: Source) = parseAll(threatModelScenarios, source.mkString)

  def threatModelScenarios = threatsAndTargets >> (scenariosSection _).tupled

  def threatsAndTargets = threatSection ~ targetSection ^^ { case threats ~ targets => threats -> targets }

  def threatSection = "THREATS" ~> "-" ~> threats

  def targetSection = "TARGETS" ~> "-" ~> targets

  def scenariosSection(threats: Iterable[Threat], targets: Iterable[Target]) =
    "SCENARIOS" ~> "-" ~> scenarios ^^ {
      val threatMap = threats.mapBy(_.name)
      val targetsMap = targets.mapBy(_.name)
      _.map { case targetName ~ threatName =>
        val target = targetsMap(targetName)
        val threat = threatMap(threatName)
        AttackScenario.modelAttack(target, threat)
      }
    }

  def scenarios = rep1sep(scenario, ",")

  def scenario = (ident <~ "attacked by") ~ ident

  def threats = rep1sep(threat, ",")

  def threat = ident ~ vulnerabilities ^^ { case name ~ vulnerabilities => Threat(name, vulnerabilities.toSet) }

  def vulnerabilities = someVulnerabilities | noVulnerabilities

  def someVulnerabilities = "countered by" ~> identList ^^ { _.map(CounterMeasure) }

  def noVulnerabilities = "cannot be countered" ^^^ { List.empty[CounterMeasure] }

  private def targets = rep1sep(target, ",")

  private def target = ident ~ defenses ^^ { case name ~ defenses => Target(name, defenses.toSet) }

  private def defenses = someDefenses | noDefenses

  private def someDefenses = "use" ~> identList <~ "for defense" ^^ { _.map { CounterMeasure } }

  private def noDefenses = "are undefended" ^^^ { List.empty[CounterMeasure] }

  private[parsers] def identList = singleElement | multipleElements

  private def singleElement = repN(1, ident)

  private def multipleElements = "<" ~> rep1sep(ident, ",") <~ ">"
}

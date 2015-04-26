package com.tyro.techtalk.parsers.threatmodel

case class CounterMeasure(name: String)

case class Target(name: String, defendedBy: Set[CounterMeasure])

case class Threat(name: String, counteredBy: Set[CounterMeasure])

object Target {
  def apply(name: String, defendedBy: String*): Target = {
    val counterMeasures = defendedBy.map(CounterMeasure)
    Target(name, counterMeasures.toSet)
  }
}

object Threat {
  def apply(name: String, counteredBy: String*): Threat = {
    val counterMeasures = counteredBy.map(CounterMeasure)
    Threat(name, counterMeasures.toSet)
  }
}
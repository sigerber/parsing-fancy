package com.tyro.techtalk.parsers.threatmodel

case class Threat(name: String, counteredBy: Set[CounterMeasure])

object Threat {
  def apply(name: String, counteredBy: String*): Threat = {
    val counterMeasures = counteredBy.map(CounterMeasure)
    Threat(name, counterMeasures.toSet)
  }
}
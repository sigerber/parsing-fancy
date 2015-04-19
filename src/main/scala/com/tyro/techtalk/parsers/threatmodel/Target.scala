package com.tyro.techtalk.parsers.threatmodel

case class Target(name: String, defendedBy: Set[CounterMeasure])

object Target {
  def apply(name: String, defendedBy: String*): Target = {
    val counterMeasures = defendedBy.map(CounterMeasure)
    Target(name, counterMeasures.toSet)
  }
}

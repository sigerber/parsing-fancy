package com.tyro.techtalk.parsers.threatmodel

object AttackScenario {
  def modelAttack(target: Target, threat: Threat): AttackResult = {
    val validCounterMeasures = threat.counteredBy.intersect(target.defendedBy)
    if (validCounterMeasures.isEmpty) Compromised else UnCompromised
  }
}

sealed abstract class AttackResult

case object Compromised extends AttackResult

case object UnCompromised extends AttackResult
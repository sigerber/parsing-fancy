package com.tyro.techtalk.parsers.threatmodel

object AttackScenario {

  def modelAttack(target: Target, threat: Threat): AttackResult = Compromised
}

sealed abstract class AttackResult

case object Compromised extends AttackResult

case object UnCompromised extends AttackResult
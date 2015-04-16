package com.tyro.techtalk.parsers.threatmodel

case class Target(name: String, vulnerableTo: Set[Threat], defendedBy: Set[CounterMeasure])

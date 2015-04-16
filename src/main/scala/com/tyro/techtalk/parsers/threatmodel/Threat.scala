package com.tyro.techtalk.parsers.threatmodel

case class Threat(name: String, counteredBy: Set[CounterMeasure])
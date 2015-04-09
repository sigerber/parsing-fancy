package com.tyro.techtalk.parsers

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ParserTest extends FlatSpec with Matchers {

  val DSL = "40"

  "The DSL Parser" should "parse a DSL" in {

    val result = Parser(Source.fromString(DSL)) match {
      case Parser.Success(parseResult, _) => parseResult
      case Parser.NoSuccess(msg, _) => throw new RuntimeException(msg)
    }
  }
}

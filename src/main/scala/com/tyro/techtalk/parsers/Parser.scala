package com.tyro.techtalk.parsers

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def apply(source: Source): ParseResult[String] = {
    parseAll(wholeNumber, source.mkString)
  }
}
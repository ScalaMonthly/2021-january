package com.scalamonthly

import cats.parse.{Parser, Parser1}
import cats.parse.Parser._
import cats.parse.Rfc5234.{char => _, _}
import cats.syntax.all._

object challenge {

    import model._

    def parse(input: String): Either[Parser.Error, Game] = parser.parse(input).map(_._2)

    private val parser: Parser[Game] = ???

}
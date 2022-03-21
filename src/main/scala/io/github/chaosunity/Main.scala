package io.github.chaosunity

import Parser._

object Main:
  def main(args: Array[String]): Unit =
    println(delimited(char('('), preceded(hexDigit0, take_until(_ == ')')), char(')'))("(abcek)fghij"))

package io.github.chaosunity

import Parser._

object Main:
  def main(args: Array[String]): Unit =
    println(delimited(char('('), preceded(multispace1, take_until(_ == ')')), char(')'))("( abce)fghij"))

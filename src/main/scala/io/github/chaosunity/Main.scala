package io.github.chaosunity

import Parser._

object Main:
  def main(args: Array[String]): Unit =
    println(delimited(char('('), tag("abce"), char(')'))("(abce)fghij"))

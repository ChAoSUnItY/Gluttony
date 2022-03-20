package io.github.chaosunity

import Parser._

object Main:
  def main(args: Array[String]): Unit =
    println(terminate(char('a'), tag("bc"))("abcefghij"))

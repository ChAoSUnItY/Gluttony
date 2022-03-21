package io.github.chaosunity

import Parser._

object Main:
  def main(args: Array[String]): Unit =
    println(delimited(char('('), take_until_unbalanced('(', ')'), char(')'))("(abcek)fghij"))
    println(take_until_unbalanced('(', ')')("u()rl)abc"))

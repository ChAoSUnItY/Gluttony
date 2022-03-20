package io.github.chaosunity

object Parser:
  type Functor[T] = T => Option[(T, T)]

  def preceded[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    (input: T) =>
      p1(input) match
        case Some((_, result)) => p2(result)
        case None => None

  def terminate[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    (input: T) =>
      p1(input) match {
        case Some((remain, result)) =>
          p2(remain) match {
            case Some((remain, _)) =>
              Some(remain, result)
            case None => None
          }
        case None => None
      }

  def tag(tag: String): Functor[String] =
    (input: String) =>
      if input startsWith tag then
        Some(input replaceFirst(s"^$tag", ""), tag)
      else None

  def char(char: Char): Functor[String] =
    (input: String) =>
      val charStr = char.toString
      if input startsWith charStr then
        Some(input drop 1, charStr)
      else
        None

  def multispace0(): Functor[String] =
    (input: String) =>
      Some(input takeWhile (c => c == ' ' || c == '\t' || c == '\r' || c == '\n'), input)

  def multispace1(): Functor[String] =
    (input: String) =>
      multispace0()(input) match
        case Some((split, input)) =>
          split match {
            case split if split.isEmpty => None
            case _ => Some(split, input)
          }
        case None => None


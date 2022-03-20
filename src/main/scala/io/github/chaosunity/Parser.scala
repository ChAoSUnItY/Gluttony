package io.github.chaosunity

object Parser:
  type Functor[T] = T => Option[(T, T)]

  private val WHITESPACE_PREDICATE: Char => Boolean =
    c => c == ' ' || c == '\t' || c == '\r' || c == '\n'

  def preceded[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    (input: T) =>
      p1(input) match
        case Some((_, result)) => p2(result)
        case None => None

  def terminated[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    (input: T) =>
      p1(input) match
        case Some((remain, result)) =>
          p2(remain) match
            case Some((remain, _)) =>
              Some(remain, result)
            case None => None
        case None => None

  def delimited[T](p1: Functor[T], p2: Functor[T], p3: Functor[T]): Functor[T] =
    (input: T) =>
      p1(input) match
        case Some((remain, _)) =>
          p2(remain) match
            case Some((remain, result)) =>
              p3(remain) match
                case Some((remain, _)) =>
                  Some(remain, result)
                case None => None
            case None => None
        case None => None

  def tag(tag: String): Functor[String] =
    (input: String) =>
      if input startsWith tag then
        Some(input drop tag.length, tag)
      else None

  def char(char: Char): Functor[String] =
    (input: String) =>
      tag(char.toString)(input)

  def take_until(predicate: Char => Boolean): Functor[String] =
    (input: String) =>
      input takeWhile (!predicate(_)) match
        case result if result.nonEmpty =>
          Some(input dropWhile (!predicate(_)), result)
        case _ => None

  def multispace0(): Functor[String] =
    (input: String) =>
      Some(input takeWhile WHITESPACE_PREDICATE, input dropWhile WHITESPACE_PREDICATE)

  def multispace1(): Functor[String] =
    (input: String) =>
      multispace0()(input) match
        case Some((split, input)) =>
          split match
            case split if split.isEmpty => None
            case _ => Some(split, input)
        case None => None


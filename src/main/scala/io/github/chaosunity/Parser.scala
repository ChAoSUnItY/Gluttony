package io.github.chaosunity

import scala.util.matching.Regex

object Parser:
  type Functor[T] = T => Option[(T, T)]

  private val HEX_DIGIT_RANGE: IndexedSeq[Char] =
    ('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')

  def preceded[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    p1(_) match
      case Some((remain, _)) => p2(remain)
      case None              => None

  def terminated[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    p1(_) match
      case Some((remain, result)) =>
        p2(remain) match
          case Some((remain, _)) =>
            Some(remain, result)
          case None => None
      case None => None

  def delimited[T](p1: Functor[T], p2: Functor[T], p3: Functor[T]): Functor[T] =
    p1(_) match
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
      if input startsWith tag then Some(input drop tag.length, tag)
      else None

  def char(char: Char): Functor[String] =
    tag(char.toString)(_)

  def take_until(predicate: Char => Boolean): Functor[String] =
    (input: String) =>
      input takeWhile (!predicate(_)) match
        case result if result.nonEmpty =>
          Some(input dropWhile (!predicate(_)), result)
        case _ => None

  def take_until_unbalanced(
      opening_char: Char,
      closing_char: Char
  ): Functor[String] =
    (input: String) =>
      val matchSet = Set(opening_char, closing_char, '\\')
      var (index, level) = (0, 0)
      while (index < input.length && level >= 0)
        input slice (index, input.length) indexWhere matchSet.contains match
          case i if i != -1 =>
            index += i
            input charAt index match
              case `opening_char` =>
                index += 1
                level += 1
              case `closing_char` =>
                index += 1
                level -= 1
              case '\\' =>
                index += 2
          case _ => level = -2
      level match
        case -2 => None
        case -1 =>
          index -= 1
          val (result, remain) = input splitAt index
          Some(remain, result)
        case 0 =>
          Some("", input)
        case _ => None

  private def predicate0(predicate: Char => Boolean): Functor[String] =
    (input: String) =>
      Some(input dropWhile predicate, input takeWhile predicate)

  private def predicate1(predicateFunc: Functor[String]): Functor[String] =
    predicateFunc(_) match
      case Some((input, split)) =>
        split match
          case split if !split.isEmpty =>
            Some(input, split)
          case _ => None
      case None => None

  def multispace0: Functor[String] =
    predicate0(_.isWhitespace)

  def multispace1: Functor[String] =
    predicate1(multispace0)

  def alpha0: Functor[String] =
    predicate0(_.isLetter)

  def alpha1: Functor[String] =
    predicate1(alpha0)

  def alphanumeric0: Functor[String] =
    predicate0(_.isLetterOrDigit)

  def alphanumeric1: Functor[String] =
    predicate1(alphanumeric0)

  def hexDigit0: Functor[String] =
    predicate0(HEX_DIGIT_RANGE contains _)

  def hexDigit1: Functor[String] =
    predicate1(hexDigit0)

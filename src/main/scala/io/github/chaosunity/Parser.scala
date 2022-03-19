object Parser {
  private type Functor[T] = (T) => Option[(T, T)]

  def preceded[T](p1: Functor[T], p2: Functor[T]): Functor[T] =
    (input: T) =>
      p1(input) match {
        case Some((_, result)) => p2(result)
        case None              => None
      }

  def tag(tag: String): Functor[String] =
    (input: String) =>
      input match {
        case input if input.startsWith(tag) =>
          Some(tag, input.replaceFirst(s"^$tag", ""))
        case _ => None
      }

  def char(char: Char): Functor[String] =
    (input: String) =>
      input match {
        case input if input.startsWith(char.toString()) =>
          Some(char.toString(), input.drop(1))
        case _ => None
      }

  def multispace0(): Functor[String] =
    (input: String) => {
      val split =
        input.takeWhile(c => c == ' ' || c == '\t' || c == '\r' || c == '\n')
      Some(split, input)
    }

  def multispace1(): Functor[String] =
    (input: String) => {
      val split =
        input.takeWhile(c => c == ' ' || c == '\t' || c == '\r' || c == '\n')
      split match {
        case split if !split.isEmpty =>
          Some(split, input)
        case _ => None
      }
    }
}

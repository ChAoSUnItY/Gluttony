object Parser {
  private type Functor[Input] = (Input) => Option[(Input, Input)]

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
}

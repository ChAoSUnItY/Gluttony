object Main {
  def tag(tag: String) =
    (input: String) =>
      input match {
        case input if input.startsWith(tag) =>
          Some(tag, input.replaceFirst("^abc", ""))
        case _ => None
      }

  def main(args: Array[String]) =
    println(tag("abc")("abcd"))
}

object Main {
  def main(args: Array[String]) =
    println(Parser.preceded(Parser.multispace0, Parser.tag("ab"))("abcd"))
}

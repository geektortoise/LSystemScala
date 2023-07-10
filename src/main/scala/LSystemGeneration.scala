trait LSystemGeneration {
  def rule(origin: Char): List[Char]
  def axiom:String

  private def generateSeq(word: Seq[Char]):Seq[Char] =
    val result = for character <- word yield rule(character)
    result.flatten

  private def generateSeq(word: Seq[Char], iteration: Int):Seq[Char] =
    iteration match
      case 0 => word
      case _ => generateSeq(generateSeq(word), iteration - 1)

  def generate(iteration:Int):String =
    generateSeq(axiom.toCharArray, iteration).mkString
}


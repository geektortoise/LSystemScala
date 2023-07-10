abstract class LSystem (
                              rule : Char => List[Char]
                   ) extends LSystemGeneration {
}

case object LevyCurveLSystem extends LSystemGeneration {
  def axiom = "F"
  def rule(origin: Char): List[Char] =
    origin match
      case 'F' => List('-', 'F', '+', '+', 'F', '-')
      case _ => List(origin)
}

case object RingsLSystem extends LSystemGeneration {
  def axiom = "F+F+F+F"
  def rule(origin: Char): List[Char] =
    origin match
      case 'F' => "FF+F+F+F+F+F-F".toList
      case _ => List(origin)
}

case object VanKochSnowflakeLSystem extends LSystemGeneration {
  def axiom = "F++F++F"
  def rule(origin: Char): List[Char] =
    origin match
      case 'F' => "F-F++F-F".toList
      case _ => List(origin)
}

case object PlantLSystem extends LSystemGeneration {
  def axiom = "----F"
  def rule(origin: Char): List[Char] =
    origin match
      case 'F' => "FF+[+F-F-F]-[-F+F+F]".toList
      case _ => List(origin)
}

case object PentaplexLSystem extends LSystemGeneration {
  def axiom = "F++F++F++F++F"
  def rule(origin: Char): List[Char] =
    origin match
      case 'F' => "F++F++F|F-F++F".toList
      case _ => List(origin)
}

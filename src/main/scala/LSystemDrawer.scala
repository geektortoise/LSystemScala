import java.io.{BufferedWriter, FileWriter, Writer}
import java.util.{ArrayDeque, ArrayList, Deque}
import scala.math._

object LSystemDrawer {
  def draw(lSystem: LSystemGeneration, iterationNumber: Int, angle: Float, step: Int, output: Any) =
    val coordinates: List[PenPointer] = determineCoordinates(lSystem, iterationNumber, angle, step)
    output match
      case w:Writer => {
        w.write(svgCreation(coordinates))
      }
      case _ => System.err.println("Unsupported output") //Maybe a graphical outpul as other case


  def determineCoordinates(lSystem: LSystemGeneration, iterationNumber: Int, angle: Float, step: Int): List[PenPointer] =

    var x:Double = 0
    var y:Double = 0
    var currentAngle: Float = 0f
    val stack: List[PenPointer] = List()

    val sentence:String = lSystem.generate(iterationNumber)
    if(360/angle %1 == 0) {
      val oTurn:Int = 360/angle.toInt
      sentence.replace("+" * oTurn, "").replace("-" * oTurn, "")
    }

    val coordinates: List[PenPointer] = determineCoordinates(sentence.toList, List(), stack, step, angle, 0, 0, 0)
    return coordinates.reverse


  private def determineCoordinates(chars: List[Char], coordinates: List[PenPointer], stack: List[PenPointer], step:Int, angle:Float, x:Double, y:Double, currentAngle:Float):List[PenPointer] =
    if(chars.isEmpty) return coordinates
    chars.head match
      case '-' => determineCoordinates(chars.tail, coordinates, stack, step, angle, x, y, currentAngle - angle)
      case '+' => determineCoordinates(chars.tail, coordinates, stack, step, angle, x, y, currentAngle + angle)
      case '|' => determineCoordinates(chars.tail, coordinates, stack, step, angle, x, y, currentAngle + 180)
      case 'F' => {
        val x2 = x + (step * Math.cos(Math.toRadians(currentAngle)))
        val y2 = y + (step * Math.sin(Math.toRadians(currentAngle)))
        determineCoordinates(chars.tail, PenPointer(x2, y2, currentAngle) :: coordinates, stack, step, angle, x2, y2,currentAngle)
      }
      case '[' => determineCoordinates(chars.tail, coordinates, PenPointer(x, y, currentAngle) :: stack, step, angle, x, y, currentAngle)
      case ']' => { //pop de la stack;
        val pp: PenPointer = stack.head
        determineCoordinates(chars.tail, coordinates, stack.tail, step, angle, pp.x, pp.y, pp.angle)
      }
      case _ => determineCoordinates(chars.tail, coordinates, stack, step, angle, x, y, currentAngle)

  private def svgCreation(coordinates:List[PenPointer]):String =
    val svg: StringBuilder = new StringBuilder
    svg.append("<svg id=\"svg\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"")

    val offset: Int = 5

    val minX: Int = coordinates.map(_.x).min.toInt - offset
    val minY: Int = coordinates.map(_.y).min.toInt - offset
    val maxX: Int = coordinates.map(_.x).max.toInt + offset
    val maxY: Int = coordinates.map(_.y).max.toInt + offset

    svg.append(minX + " " + minY + " " + (maxX - minX) + " " + (maxY - minY) + " ")

    svg.append("\">")
    svg.append("<path d=\"M0 0L")

    for c <- coordinates do
      svg.append(c.x + " " + c.y + " ")

    svg.append("\" fill=\"none\" stroke-width=\"1\" stroke=\"#008000\"></path>")
    svg.append("</svg>")

    return svg.toString

}

case class PenPointer (x:Double, y:Double, angle:Float)

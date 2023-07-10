import java.io.{BufferedWriter, File, FileWriter}
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try, Using}
@main def hello(): Unit =

  val result = Plant()

  result match
    case Failure(exception) =>
      System.err.println("Error with the IO filesystem")
      exception.printStackTrace()
    case Success(_) => // What happens here?
      println("Success")


def LevyCurve(): Try[Unit] =
  Using(new BufferedWriter(new FileWriter(new File("levy.svg")))) { fileWriter =>
    val drawer = LSystemDrawer
    drawer.draw(LevyCurveLSystem, 15, 45.0, 10, fileWriter)
    fileWriter.close()
  }

def Plant(): Try[Unit] =
  Using(new BufferedWriter(new FileWriter(new File("plant.svg")))) { fileWriter =>
    val drawer = LSystemDrawer
    drawer.draw(PlantLSystem, 6, 22.5, 10, fileWriter)
    fileWriter.close()
  }

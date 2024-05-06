package ex3

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((y, x)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  type Position = (Int, Int)
  type Solution = Seq[Position] // Alias.
  def placeMarks(w: Int, h: Int): Iterable[Solution] = (w, h) match
    case (0, 0) => Set()
    case (width, height) if width > 0 && height > 0 =>
      for
        solutions <- placeMarks(width - 1, height - 1)
        x <- 1 until width
        y <- 1 until height
        probableSolution = (x, y)
        if isCorrect(probableSolution, solutions)
      yield
        solutions.toSeq :+ probableSolution

  def isCorrect(probableSolution: Position, other: Seq[Position]): Boolean =
    !(other contains probableSolution) && hasCorrectCoordinates(probableSolution, other.last)

  def hasCorrectCoordinates(probableSolution: Position, lastNumber: Position): Boolean = {
    val dr = math.abs(probableSolution._1 - lastNumber._1)
    val dc = math.abs(probableSolution._2 - lastNumber._2)
    (dr == 2 && dc == 2) || // Diagonal movement
      (dr == 0 && dc == 3) || // Vertical movement
      (dr == 3 && dc == 0) // Horizontal movement
  }

  println(render(solution = Seq((0, 0), (2, 1), (1, 2)), width = 3, height = 3))
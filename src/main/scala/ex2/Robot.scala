package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var amount: Int) extends Robot:
  export robot.{act =>_, *}
  private var batteryLevel: Int = 100;
  override def act(): Unit = batteryLevel match
    case b if b <= 0 => ()
    case _ => {robot.act(); batteryLevel = batteryLevel - amount}

class RobotCanFail(val robot: Robot, var possibility: Double) extends Robot:
  export robot.{act =>_, *}
  override def act(): Unit =
    val r: Double = Random.nextDouble()
    r match
      case fail if r < possibility => ()
      case _ => robot.act()

class RobotRepeated(val robot: Robot, var n: Int) extends Robot:
  export robot.{act =>_, *}
  override def act(): Unit = actOnce(n)
  def actOnce(nTimes: Int): Unit = nTimes match
    case 0 => ()
    case _ => {robot.act(); actOnce(nTimes - 1)}




@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

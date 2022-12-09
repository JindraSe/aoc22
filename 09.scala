import scala.io.Source


case class Direction(val xdiff: Int, val ydiff: Int):
  def shorten: Direction = Direction(xdiff.sign, ydiff.sign)
  def is_short: Boolean = xdiff.abs <= 1 && ydiff.abs <= 1


object Direction:
  def fromChar(ch: Char): Direction =
    ch match
      case 'U' => Direction(0, 1)
      case 'D' => Direction(0, -1)
      case 'R' => Direction(1, 0)
      case 'L' => Direction(-1, 0)
      case _ => ???
    
  def fromString(str: String): (Direction, Int) =
    (Direction.fromChar(str(0)), str.split(' ')(1).toInt)


case class Position(val x: Int, val y: Int):
  def move(direction: Direction): Position =
    Position(this.x + direction.xdiff, this.y + direction.ydiff)
  
  def chase(other: Position): Position =
    val diff = Direction(other.x - this.x, other.y - this.y)
    if diff.is_short then this else this.move(diff.shorten)

object Position:
  def origin: Position = Position(0, 0)


class RopeHistory(knots: Array[Position], visited: Set[Position]):
  def move(direction: Direction, repeat: Int): RopeHistory =
    if repeat <= 0 then this
    else
      val new_knots = knots.foldLeft(Array.empty: Array[Position])(
        (new_knots_tmp: Array[Position], pos: Position) =>
          new_knots_tmp :+ (
            if new_knots_tmp.isEmpty then pos.move(direction)
            else pos.chase(new_knots_tmp.last))
      )
      val new_visited = visited + new_knots.last
      RopeHistory(new_knots, new_visited).move(direction, repeat - 1)
  
  def totalVisited = visited.size


object RopeHistory:
  def origin(size: Int) =
    RopeHistory(Array.fill(size)(Position.origin), Set(Position.origin))


def read_rope_history_and_count_visited(size: Int): Int =
  Source.fromFile("../09-input.txt").getLines().foldLeft(
    RopeHistory.origin(size)
  )((history: RopeHistory, line: String) =>
      val (direction, repeat) = Direction.fromString(line)
      history.move(direction, repeat)
  ).totalVisited


def task1: Unit = println(read_rope_history_and_count_visited(2))
def task2: Unit = println(read_rope_history_and_count_visited(10))


@main def day9 =
  task1
  task2
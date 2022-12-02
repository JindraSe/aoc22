import scala.io.Source


enum RPS_Outcome(val score: Int):
  case Loss extends RPS_Outcome(0)
  case Draw extends RPS_Outcome(3)
  case Win extends RPS_Outcome(6)


object RPS_Outcome:
  def from_letter(letter: Char): RPS_Outcome =
    letter match
      case 'X' => RPS_Outcome.Loss
      case 'Y' => RPS_Outcome.Draw
      case 'Z' => RPS_Outcome.Win
      case _ => ???


enum RPS_Shape(value: Int):
  case Rock extends RPS_Shape(1)
  case Paper extends RPS_Shape(2)
  case Scissors extends RPS_Shape(3)

  def +(other: Int): RPS_Shape =
    RPS_Shape.from_value(value + other)

  def play(other: RPS_Shape) =
    other match 
      case shape if shape == this => RPS_Outcome.Draw
      case shape if shape == this+1 => RPS_Outcome.Loss
      case _ => RPS_Outcome.Win

  def game_score(other: RPS_Shape): Int = value + this.play(other).score

  def move_against(desired_outcome: RPS_Outcome): RPS_Shape =
    desired_outcome match
      case RPS_Outcome.Draw => this
      case RPS_Outcome.Win => this + 1
      case RPS_Outcome.Loss => this + (-1)


object RPS_Shape:
  def from_letter(letter: Char): RPS_Shape =
    letter match
      case 'A' | 'X' => RPS_Shape.Rock
      case 'B' | 'Y' => RPS_Shape.Paper
      case 'C' | 'Z' => RPS_Shape.Scissors
      case _ => ???

  def from_value(value: Int): RPS_Shape =
    value % 3 match
      case 1 => RPS_Shape.Rock
      case 2 => RPS_Shape.Paper
      case 0 => RPS_Shape.Scissors


def sum_rps_score(as_outcome: Boolean): Int =
  Source.fromFile("../02-input.txt", "utf8").getLines().map(
    _ match
      case line if line.trim.nonEmpty && line.length >= 3 =>
        val elfs_shape = RPS_Shape.from_letter(line.apply(0))

        val my_shape =
          if as_outcome then
            elfs_shape.move_against(RPS_Outcome.from_letter(line.apply(2)))
          else
            RPS_Shape.from_letter(line.apply(2))

        my_shape.game_score(elfs_shape)

      case _ => 0
  ).sum


def task1() : Unit =
  println(sum_rps_score(false))


def task2(): Unit =
  println(sum_rps_score(true))


@main def day2 =
  task1()
  task2()

import scala.io.Source


class ID_Range(val from: Int, val to: Int):
  def contains(other: ID_Range): Boolean =
    other.from >= from && other.to <= to
  
  def overlaps(other: ID_Range): Boolean =
    other.from <= to && from <= other.to

object ID_Range:
  def fromString(str: String): ID_Range =
    val split_str = str.split('-')
    ID_Range(split_str(0).toInt, split_str(1).toInt)

def read_and_count_overlapping(only_fully_contained: Boolean): Int =
  Source.fromFile("../04-input.txt", "utf8").getLines(
    ).filter(_.trim.nonEmpty
    ).map(_.split(',').map(ID_Range.fromString)
    ).filter(
      if only_fully_contained then
        arr => arr(0).contains(arr(1)) || arr(1).contains(arr(0))
      else
        arr => arr(0).overlaps(arr(1))
    ).length


def task1: Unit = println(read_and_count_overlapping(true))
def task2: Unit = println(read_and_count_overlapping(false))


@main def day3 =
  task1
  task2
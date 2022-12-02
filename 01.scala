import scala.io.Source

trait IntAccumulator:
  def update(new_int: Int): IntAccumulator
  def result(): Int


class SingleMaxInt(val value: Int) extends IntAccumulator:
  def update(new_int: Int): SingleMaxInt = SingleMaxInt(value.max(new_int))
  def result(): Int = value


class ThreeMaxInts(val biggest: Int, val middle: Int,
                   val smallest: Int) extends IntAccumulator:
  def update(other: Int) =
    if other > biggest then
      ThreeMaxInts(other, biggest, middle)
    else if other > middle then
      ThreeMaxInts(biggest, other, middle)
    else if other > smallest then
      ThreeMaxInts(biggest, middle, other)
    else
      this

  def result(): Int = biggest + middle + smallest


def accumulate_elf_calories(initial: IntAccumulator) : Int =
  var current_elf_total = 0
  var accumulator = initial

  for line <- Source.fromFile("../01-input.txt", "utf8").getLines() do
    if line == "" then
      accumulator = accumulator.update(current_elf_total)
      current_elf_total = 0
    else
      current_elf_total += line.toInt

  accumulator.result()


def task1() : Unit =
  println(accumulate_elf_calories(SingleMaxInt(0)))


def task2() : Unit =
  println(accumulate_elf_calories(ThreeMaxInts(0, 0, 0)))


@main def day1 =
  task1()
  task2()

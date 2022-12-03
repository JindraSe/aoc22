import scala.Option
import scala.io.Source


def item_in_both_halves(line: String): Option[Char] =
  val (left, right) = line.splitAt(line.length / 2)
  left.intersect(right).headOption


def item_priority(ch: Char): Int =
  ch.toInt + (if ch.isLower then 1 - 'a'.toInt else 27 - 'A'.toInt)
  

def read_and_sum_item_priorities(): Int =
  Source.fromFile("../03-input.txt", "utf8").getLines.map(
    line => item_in_both_halves(line).map(item_priority).getOrElse(0)
  ).sum


class ElfGroup(val elf_items: Array[String]):
  def is_full: Boolean = elf_items.length == 3
  
  def add_elf(new_items: String): ElfGroup =
    if new_items.trim.nonEmpty then
      ElfGroup(elf_items.appended(new_items))
    else
      this

  def badge: Option[Char] =
    elf_items.reduce((x, y) => x.intersect(y)).headOption


def read_and_find_and_sum_badges(): Int =
  var current_group = ElfGroup(Array())
  var badge_sum = 0
  
  for line <- Source.fromFile("../03-input.txt", "utf8").getLines do
    current_group = current_group.add_elf(line)
  
    if current_group.is_full then
      badge_sum += current_group.badge.map(item_priority).get
      current_group = ElfGroup(Array())

  badge_sum


def task1(): Unit = println(read_and_sum_item_priorities())
def task2(): Unit = println(read_and_find_and_sum_badges())


@main def day3 =
  task1()
  task2()

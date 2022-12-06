import scala.io.Source


class CharWindow(last_chars: Array[Char], buffer_size: Int):
  def is_full: Boolean = last_chars.length == buffer_size
  def all_distinct: Boolean = last_chars.distinct.length == last_chars.length
  def updated(ch: Char) = CharWindow(
    ch +: (if this.is_full then last_chars.dropRight(1) else last_chars),
    buffer_size
  )


def read_and_find_marker(buffer_size: Int): Int =
  var window = CharWindow(Array.empty, buffer_size)
  var res = 0
  for (ch, i) <- Source.fromFile("../06-input.txt", "utf8").zipWithIndex do
    window = window.updated(ch)
    if window.is_full && window.all_distinct && res == 0 then res = i + 1
  res


def task1: Unit = println(read_and_find_marker(4))
def task2: Unit = println(read_and_find_marker(14))


@main def day6 =
  task1
  task2
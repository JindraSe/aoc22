import scala.io.Source


def is_visible(point: (Int, Int), tree_grid: Array[Array[Int]]) =
  val (x, y) = point
  val tree_height = tree_grid(y)(x)
  val row = tree_grid(y)
  val col = tree_grid.map(_(x))

  val is_still_visible = (visible_so_far: Boolean, current_height: Int) =>
    visible_so_far && current_height < tree_height

  Array(row.drop(x + 1), row.take(x),
        col.drop(y + 1), col.take(y)
      ).filter(_.foldLeft(true)(is_still_visible)).nonEmpty


def scenic_score(point: (Int, Int), tree_grid: Array[Array[Int]]): Int =
  val (x, y) = point
  val tree_height = tree_grid(y)(x)
  val row = tree_grid(y)
  val col = tree_grid.map(_(x))

  val still_sees_trees = 
    (seen_and_ended: (Int, Boolean), current_height: Int) =>
      val (seen, ended) = seen_and_ended
      if ended then (seen, ended) else (seen + 1, current_height >= tree_height)

  Array(row.drop(x + 1), row.take(x).reverse,
        col.drop(y + 1), col.take(y).reverse
      ).map(_.foldLeft((0, false))(still_sees_trees)._1
      ).fold(1)((left: Int, right: Int) => left * right)


def points_from_array(array: Array[Array[Int]]): Array[(Int, Int)] =
  array.map(_.indices).zipWithIndex.flatMap((range_i: (Range, Int)) =>
    val (range, i) = range_i
    range.zipAll(Array.empty: Array[Int], 0, i)
  )


def read_tree_grid: Array[Array[Int]] =
  Source.fromFile("../08-input.txt").getLines().map(_.map(_.toInt).toArray).toArray


def count_visible_points: Int =
  val tree_grid = read_tree_grid
  
  points_from_array(tree_grid).filter(
    (point: (Int, Int)) => is_visible(point, tree_grid)
  ).size


def heighest_scenic_score: Int =
  val tree_grid = read_tree_grid

  points_from_array(tree_grid).map(
    (point: (Int, Int)) => scenic_score(point, tree_grid)
  ).max


def task1: Unit = println(count_visible_points)
def task2: Unit = println(heighest_scenic_score)


@main def day8 =
  task1
  task2
import scala.io.Source


sealed trait FileSystemElement:
  def size: Int
  def name: String
  def directory: Directory


case class File(file_name: String, file_size: Int) extends FileSystemElement:
  def size: Int = file_size
  def name: String = file_name
  def directory: Directory = ???


case class Directory(dir_name: String,
                     elems: Map[String, FileSystemElement] = Map.empty)
      extends FileSystemElement:
  
  def size: Int = elems.values.map(_.size).sum
  def name: String = dir_name
  def directory: Directory = this

  def updated(elem: FileSystemElement) =
    Directory(name, elems.updated(elem.name, elem))

  def open_subdirectory(subdir_name: String): Directory =
    elems(subdir_name).directory
  
  def all_subdirectories: Vector[Directory] =
    elems.values.foldLeft(Vector(this))(
      (res: Vector[Directory], elem: FileSystemElement) =>
        elem match
          case dir: Directory => res ++ dir.all_subdirectories
          case _ => res
    )


object FileSystemElement:
  def fromString(str: String) =
    if str.slice(0, 3) == "dir" then
      Directory(str.drop(4))
    else
      val split_str = str.split(' ')
      File(split_str(1), split_str(0).toInt)


class CurrentPath(path: Vector[Directory]):
  def updated(new_elem: FileSystemElement): CurrentPath =
    var cur_elem = new_elem
    CurrentPath(path.map((dir: Directory) =>
      cur_elem = dir.updated(cur_elem)
      cur_elem.directory
    ))

  def open_dir(name: String): CurrentPath =
    name match
      case ".." => CurrentPath(path.drop(1))
      case "/" => CurrentPath(path.takeRight(1))
      case _ => CurrentPath(path.head.open_subdirectory(name) +: path)
  
  def last: Directory = path.last
  

object CurrentPath:
  def root: CurrentPath = CurrentPath(Vector(Directory("/")))


def read_device_directories =
  var current_path: CurrentPath = CurrentPath.root

  for line <- Source.fromFile("../07-input.txt").getLines() do
    current_path = line match
      case nothing if !(line.trim.nonEmpty) => current_path
      case ls if line == "$ ls" => current_path
      case cd if line.slice(0, 4) == "$ cd" =>
        current_path.open_dir(cd.drop(5))
      case dir_elem =>
        current_path.updated(FileSystemElement.fromString(dir_elem))

  current_path.last.all_subdirectories


def sum_sizes_of_small_directories =
  read_device_directories.map(_.size).filter(_ <= 100000).sum


def size_of_smallest_big_directory =
  val dir_sizes = read_device_directories.map(_.size)
  val space_needed = 30000000 - (70000000 - dir_sizes.max)

  dir_sizes.filter(_ >= space_needed).min


def task1: Unit = println(sum_sizes_of_small_directories)
def task2: Unit = println(size_of_smallest_big_directory)

@main def day7 =
  task1
  task2
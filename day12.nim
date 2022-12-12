import sequtils
import std/streams
import std/options
import std/sets
import std/heapqueue
import std/hashes


type
  Position = tuple
    x: int
    y: int

  PositionCost = tuple
    pos: Position
    cost: int

  Heightmap = seq[seq[char]]


# basic methods

func `==`(pos1, pos2: Position): bool =
  pos1.x == pos2.x and pos1.y == pos2.y


func `<`(pc1, pc2: PositionCost): bool =
  pc1.cost < pc2.cost


func hash(pos: Position): Hash =
  hash(pos.x) !& hash(pos.y)


iterator neighbors(pos: Position): Position =
  yield (pos.x - 1, pos.y)
  yield (pos.x, pos.y - 1)
  yield (pos.x, pos.y + 1)
  yield (pos.x + 1, pos.y)


func height(ch: char): int =
  if ch == 'S': return 0
  if ch == 'E': return 25
  return ord(ch) - ord('a')


func in_bounds(heightmap: Heightmap, pos: Position): bool =
  (pos.y >= 0 and pos.y < heightmap.len and
   pos.x >= 0 and pos.x < heightmap[pos.y].len)


iterator traversable_neighbors(heightmap: Heightmap,
                               pos: Position): Position =
  let pos_height = heightmap[pos.y][pos.x].height()
  
  for neighbor in pos.neighbors():
    if not heightmap.in_bounds(neighbor):
      continue

    let neighbor_height = heightmap[neighbor.y][neighbor.x].height()

    if neighbor_height <= pos_height + 1:
      yield neighbor


iterator find_all_occurances(heightmap: Heightmap, ch: char): Position =
  for y in 0 ..< heightmap.len:
    for x in 0 ..< heightmap[y].len:
      if heightmap[y][x] == ch:
        yield (x, y)


func find_first_occurance(heightmap: Heightmap, ch: char): Option[Position] =
  for pos in find_all_occurances(heightmap, ch):
    return some(pos)
  return none(Position)


func find_start(heightmap: Heightmap): Option[Position] =
  heightmap.find_first_occurance('S')


func find_target(heightmap: Heightmap): Option[Position] =
  heightmap.find_first_occurance('E')


# reading input

proc read_heightmap(): Heightmap =
  let istream = openFileStream("../12-input.txt")
  defer: istream.close()

  for line in istream.lines():
    result.add(line.toSeq)


# actual solution

func dijkstra(heightmap: Heightmap, start, target: Position): Option[int] =
  var visited = [start].toHashSet()
  var expandable_nodes = [(start, 0)].toHeapQueue()

  while expandable_nodes.len() > 0:
    let (cur, cost) = expandable_nodes.pop()
    for neighbor in heightmap.traversable_neighbors(cur):
      if neighbor in visited:
        continue

      visited.incl(neighbor)
      
      if neighbor == target:
        return some(cost + 1)

      expandable_nodes.push((neighbor, cost + 1))

  return none(int)


proc task1() =
  let heightmap = read_heightmap()
  let start = heightmap.find_start().get()
  let target = heightmap.find_target().get()
  echo dijkstra(heightmap, start, target).get()


# bruteforcing, 'cause it's easy; but it could be done
# more effectively by starting at `E`, and keeping
# track of all distances, I may rewrite it
proc task2() =
  let heightmap = read_heightmap()
  let target = heightmap.find_target().get()
  let orig_start = heightmap.find_start().get()

  var cur_min = dijkstra(heightmap, orig_start, target).get()

  for start in heightmap.find_all_occurances('a'):
    let cur_res = dijkstra(heightmap, start, target)
    if cur_res.isSome(): cur_min = min(cur_min, cur_res.get())
  
  echo cur_min


task1()
task2()

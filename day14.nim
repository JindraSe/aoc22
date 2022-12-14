import std/hashes
import std/options
import std/sets
import std/sequtils
import std/streams
import std/strutils


type
  Position = tuple
    x: int
    y: int
  
  Scan = HashSet[Position]

  FallResultKind = enum into_abyss, rests_at, source_blocked
  
  FallResult = object
    case kind: FallResultKind
      of rests_at: pos: Position
      else: nil


# basic methods

func hash(pos: Position): Hash =
  hash(pos.x) !& hash(pos.y)


func get_max_y(scan: Scan): int =
  result = low(int)
  for (x, y) in scan.items():
    if y > result:
      result = y


func occuppied(scan: Scan, pos: Position, floor: Option[int]): bool =
  (floor.isSome() and pos.y == floor.get()) or pos in scan


func let_grain_fall(scan: Scan, floor: Option[int] = none(int)): FallResult =
  let max_y = if floor.isSome: floor.get() else: scan.get_max_y()
  var current_position: Position = (500, 0)

  if current_position in scan:
    return FallResult(kind: source_blocked)

  while current_position.y < max_y:
    let directly_bellow = (current_position.x, current_position.y + 1)
    if not scan.occuppied(directly_bellow, floor):
      current_position = directly_bellow
      continue
    
    let bellow_left = (current_position.x - 1, current_position.y + 1)
    if not scan.occuppied(bellow_left, floor):
      current_position = bellow_left
      continue
    
    let bellow_right = (current_position.x + 1, currentPosition.y + 1)
    if not scan.occuppied(bellow_right, floor):
      current_position = bellow_right
      continue
    
    return FallResult(kind: rests_at, pos: current_position)

  return FallResult(kind: into_abyss)


# reading input

func toPosition(str: string): Position =
  let split_str = str.split(',')
  return (split_str[0].parseInt(), split_str[1].parseInt())


proc add_line(scan: var Scan, from_p, to_p: Position): void =
  if from_p.y == to_p.y:
    for x in min(from_p.x, to_p.x) .. max(from_p.x, to_p.x):
      scan.incl((x, from_p.y))
    return

  for y in min(from_p.y, to_p.y) .. max(from_p.y, to_p.y):
    scan.incl((from_p.x, y))


proc read_scan(): Scan =
  let iostream = openFileStream("../14-input.txt")
  defer: iostream.close()

  for line in iostream.lines():
    var first = true
    var last_pos: Position
    for cur_pos in line.split(" -> ").map(toPosition):
      if first:
        last_pos = cur_pos
      
      result.add_line(last_pos, cur_pos)

      first = false
      last_pos = cur_pos


# solution

proc task1(): void =
  var scan = read_scan()
  var total_resting = 0

  while true:
    let grain_fall_res = scan.let_grain_fall()
    case grain_fall_res.kind
      of into_abyss: break
      of source_blocked: assert(false)
      of rests_at:
        scan.incl(grain_fall_res.pos)
        total_resting += 1
  
  echo total_resting


proc task2(): void =
  var scan = read_scan()
  var total_resting = 0
  let floor = some(scan.get_max_y() + 2)

  while true:
    let grain_fall_res = scan.let_grain_fall(floor)
    case grain_fall_res.kind
      of into_abyss: assert(false)
      of source_blocked: break
      of rests_at:
        scan.incl(grain_fall_res.pos)
        total_resting += 1
  
  echo total_resting


task1()
task2()
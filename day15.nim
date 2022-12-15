import std/hashes
import std/sequtils
import std/streams
import std/strutils
import std/sets


type
  Position = tuple
    x: int
    y: int
  
  SensorBeaconPair = tuple
    sensor: Position
    beacon: Position
  

# basic methods


func hash(pos: Position): Hash =
  hash(pos.x) !& hash(pos.y)


func manhattan(pos1, pos2: Position): int =
  abs(pos1.x - pos2.x) + abs(pos1.y - pos2.y)


func `+`(pos1, pos2: Position): Position =
  (pos1.x + pos2.x, pos1.y + pos2.y)


iterator all_of_distance_from_origin(distance: int): Position =
  for i in 0 .. distance:
    yield (distance - i, i)
    yield (i - distance, i)
    yield (distance - i, -i)
    yield (i - distance, -i)


iterator all_of_distance_from(pos: Position, distance: int): Position =
  for new_pos in all_of_distance_from_origin(distance):
    yield new_pos + pos


iterator invalidated_at_y(sensor, beacon: Position, y: int): Position =
  let max_distance = manhattan(sensor, beacon)
  let distance_to_y = abs(sensor.y - y)

  if distance_to_y != 0 and max_distance > distance_to_y:
    yield (sensor.x, y)

  for i in 1 .. (max_distance - distance_to_y):
    if (sensor.x + i, y) != beacon:
      yield (sensor.x + i, y)
    if (sensor.x - i, y) != beacon:
      yield (sensor.x - i, y)


func invalidated_by(pos, sensor, beacon: Position): bool =
  return manhattan(sensor, beacon) >= manhattan(sensor, pos)


# reading io

func parse_point(str: string): Position =
  let split_str = str.split(", ")
  assert len(split_str) == 2

  return (split_str[0][2..^1].parseInt(), split_str[1][2..^1].parseint())


func parse_sensor_and_beacon(line: string): SensorBeaconPair =
  let str_points = line[10..^1].split(": closest beacon is at ")
  assert len(str_points) == 2

  let sensor = str_points[0].parse_point()
  let beacon = str_points[1].parse_point()

  return (sensor, beacon)


iterator load_sensors_and_beacons(): SensorBeaconPair =
  let istream = openFileStream("../15-input.txt")
  defer: istream.close()

  for line in istream.lines():
    if line != "":
      yield line.parse_sensor_and_beacon()


# solutions

proc task1() =
  var total = 0
  var skipping_positions: HashSet[Position]

  for (sensor, beacon) in load_sensors_and_beacons():
    skipping_positions.incl(beacon)
    for no_beacon in invalidated_at_y(sensor, beacon, 2000000):
      if no_beacon in skipping_positions:
        continue
      skipping_positions.incl(no_beacon)
      total += 1
  
  echo total


proc task2() =
  let sensor_beacon_pairs = load_sensors_and_beacons().toSeq()
  
  for (sensor, beacon) in sensor_beacon_pairs.items():
    let manhatt = sensor.manhattan(beacon)
    for x, y in sensor.all_of_distance_from(manhatt + 1):
      if x < 0 or y < 0 or x > 4000000 or y > 4000000:
        continue
      var was_invalidated = false
      for (sensor, beacon) in sensor_beacon_pairs.items():
        if (x, y).invalidated_by(sensor, beacon):
          was_invalidated = true
          break
      if not was_invalidated:
        echo 4000000 * x + y
        return


task1()
task2()
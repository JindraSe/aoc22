import std/streams
import std/strutils


type
  PacketKind = enum list, integer
  Packet = object
    case kind: PacketKind
    of list: elements: seq[Packet]
    of integer: value: int


func to_list(packet: Packet): Packet =
  case packet.kind:
    of list: packet
    of integer: Packet(kind: list, elements: @[packet])


func `<`(left, right: Packet): bool =
  if left.kind == integer and right.kind == integer:
    return left.value < right.value

  let left_list = left.to_list()
  let right_list = right.to_list()

  for i in 0 ..< min(left_list.elements.len, right_list.elements.len):
    if left_list.elements[i] < right_list.elements[i]:
      return true
    if left_list.elements[i] > right_list.elements[i]:
      return false
  
  return left_list.elements.len < right_list.elements.len


func parse_packet(line: string): Packet =
  var packet_stack: seq[Packet] = @[]
  var digit_stack: seq[char] = @[]

  for ch in line:
    if ch.isDigit():
      digit_stack.add(ch)
      continue

    elif digit_stack.len > 0:
      let parsed_number = digit_stack.join().parseInt()
      let new_packet = Packet(kind: integer, value: parsed_number)

      packet_stack[^1].elements.add(new_packet)
      digit_stack.setLen(0)
    
    if ch == '[':
      packet_stack.add(Packet(kind: list, elements: @[]))
    
    elif ch == ']':
      let finished_packet = packet_stack.pop()
      
      if packet_stack.len == 0:
        return finished_packet
      
      packet_stack[^1].elements.add(finished_packet)

  return Packet(kind: list, elements: @[])


iterator load_packet_pairs(): (Packet, Packet)=
  let istream = openFileStream("../13-input.txt")
  defer: istream.close()

  while not istream.atEnd():
    while istream.peekLine() == "":
      let _ = istream.readLine()

    let left = istream.readLine().parse_packet()
    let right = istream.readLine().parse_packet()

    yield (left, right)


iterator load_packets(): Packet =
  let istream = openFileStream("../13-input.txt")
  defer: istream.close()

  for line in istream.lines():
    if line != "":
      yield line.parse_packet()


proc task1() =
  var index_sum = 0
  
  var idx = 1
  for left, right in load_packet_pairs():
    if left < right:
      index_sum += idx
    idx += 1
  
  echo index_sum


proc task2() =
  let divider_packet_one = "[[2]]".parse_packet()
  let divider_packet_two = "[[6]]".parse_packet()

  var divider_one_index = 1
  var divider_two_index = 2

  for packet in load_packets():
    if packet < divider_packet_one:
      divider_one_index += 1
      divider_two_index += 1
    elif packet < divider_packet_two:
      divider_two_index += 1
  
  echo divider_one_index * divider_two_index


task1()
task2()

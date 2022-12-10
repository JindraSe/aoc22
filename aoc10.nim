import std/enumerate
import std/streams
import std/strutils
import functools


type
  DeviceState = object
    cycle: int
    register: int

  OperationKind = enum noOp, addX

  Operation = ref object
    case kind: OperationKind
    of noOp: nil
    of addX: val: int
  
  DisplayState = object
    pixel: int


proc signal_strength(state: DeviceState): int =
  return state.cycle * state.register


iterator execop(state: DeviceState, op: Operation): DeviceState =
  yield DeviceState(cycle: state.cycle + 1, register: state.register)
  if op.kind == addX:
    yield DeviceState(cycle: state.cycle + 2,
                      register: state.register + op.val)


proc overlap(device_state: DeviceState, display_state: DisplayState): bool =
  return (display_state.pixel mod 40 >= device_state.register - 1 and
          display_state.pixel mod 40 <= device_state.register + 1)


proc to_operation(line: string): Operation =
  let split_str = line.split(' ')
  if split_str[0] == "noop":
    return Operation(kind: noOp)
  return Operation(kind: addX, val: split_str[1].parseInt)


iterator read_ops(): Operation {.closure.} =
  let istream = openFileStream("../10-input.txt")
  for line in istream.lines():
    yield to_operation(line)
  istream.close()


iterator read_signal_strengths(): int {.closure.} =
  var state = DeviceState(cycle: 1, register: 1)

  for op in read_ops():
    var before_instruction_state = state
    for op_state in before_instruction_state.execop(op):
      state = op_state
      if (state.cycle + 20) mod 40 == 0:
        yield state.signal_strength


iterator get_pixels(): bool {.closure.} =
  var device_state = DeviceState(cycle: 1, register: 1)
  var display_state = DisplayState(pixel: 0)
  
  for op in read_ops():
    var before_instruction_state = device_state
    for op_state in before_instruction_state.execop(op):
      yield device_state.overlap(display_state)
      display_state.pixel += 1
      device_state = op_state


proc draw_screen(pixel_iterator: iterator(): bool): void =
  let ostream = openFileStream("../10-output.txt", fmWrite)
  for i, is_lit in enumerate(pixel_iterator()):
    ostream.write(if is_lit: '#' else: '.')
    if (i+1) mod 40 == 0: ostream.write('\n')
  ostream.close()


echo read_signal_strengths.sum()
draw_screen(get_pixels)

import std/deques
import std/options
import std/sequtils
import std/streams
import std/strutils
import std/math


type
  Operator = enum plus, times

  Operation = object
    operator: Operator
    left: Option[int]
    right: Option[int]

  Monkey = object
    items: Deque[int]
    op: Operation
    test_divisor: int
    on_yes: int
    on_no: int
    inspected_items: int

  ThrownItem = object
    to_monkey: int
    worry_level: int


# basic methods

func exec(op: Operation, old: int): int =
  let left = op.left.get(old)
  let right = op.right.get(old)
  case op.operator
    of Operator.plus: return left + right
    of Operator.times: return left * right


proc throw_item(monkey: var Monkey, relief: bool = true): ThrownItem =
  monkey.inspected_items += 1

  var worry_level = monkey.op.exec(monkey.items.popFirst())
  worry_level = if relief: worry_level div 3 else: worry_level
  
  let to_monkey =
    if (worry_level mod monkey.test_divisor == 0): monkey.on_yes
    else: monkey.on_no
  
  return ThrownItem(to_monkey: to_monkey, worry_level: worry_level)


iterator throw_all_items(monkey: var Monkey,
                         relief: bool = true): ThrownItem =
  while monkey.items.len > 0: yield monkey.throw_item(relief)


proc receive_item(monkey: var Monkey, item: ThrownItem): void =
  monkey.items.addLast(item.worry_level)


# reading input

func toOperation(line: string): Operation =
  let tokens = line[19..^1].split(' ')

  let left = if tokens[0] == "old": none(int) else: some(tokens[0].parseInt)
  let operator = if tokens[1] == "+": Operator.plus else: Operator.times
  let right = if tokens[2] == "old": none(int) else: some(tokens[2].parseInt)
  
  return Operation(operator: operator, left: left, right: right)


proc load_monkey(istream: Stream): Monkey =
  let _ = istream.readLine()

  let items = istream.readLine()[18..^1].split(", ").map(parseInt).toDeque
  let op = istream.readLine().toOperation()
  let test_divisor = istream.readLine()[21..^1].parseInt
  let on_yes = istream.readLine()[29..^1].parseInt
  let on_no = istream.readLine()[30..^1].parseInt

  if not istream.atEnd:
    let _ = istream.readLine()

  return Monkey(items: items, op: op, test_divisor: test_divisor,
                on_yes: on_yes, on_no: on_no, inspected_items: 0)


proc load_monkeys(): seq[Monkey] =
  let istream = openFileStream("../11-input.txt")
  result = @[]
  
  while not istream.atEnd:
    result.add(load_monkey(istream))
  
  istream.close()


# simulating monkeys

proc assign_item(monkeys: var seq[Monkey], thrown_item: ThrownItem): void =
  monkeys[thrown_item.to_monkey].receive_item(thrown_item)


proc monkey_round(monkeys: var seq[Monkey], relief: bool = true): void =
  let total_mod = lcm(monkeys.map(proc(m: Monkey): int = m.test_divisor))
  for idx in 0 ..< monkeys.len:
    for item in monkeys[idx].throw_all_items(relief):
      var reduced_item = ThrownItem(to_monkey: item.to_monkey,
                                    worry_level: item.worry_level mod total_mod)
      assign_item(monkeys, reduced_item)


func monkey_business(monkeys: seq[Monkey]): int =
  var max1 = 0
  var max2 = 0
  
  for monkey in monkeys:
    if monkey.inspected_items > max1:
      max2 = max1
      max1 = monkey.inspected_items
    elif monkey.inspected_items > max2:
      max2 = monkey.inspected_items
  
  return max1 * max2


proc task1(): int =
  var monkeys = load_monkeys()
  for _ in countup(1, 20):
    monkey_round(monkeys)
  return monkeys.monkey_business()


proc task2(): int =
  var monkeys = load_monkeys()
  for i in countup(1, 10000):
    monkey_round(monkeys, false)
  return monkeys.monkey_business()


echo task1()
echo task2()

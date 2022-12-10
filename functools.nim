import std/enumerate


proc map*[T, S](it: iterator(): T,
                f: proc(elem: T): S): iterator(): S =
  result = iterator(): S =
    for elem in it():
      yield f(elem)


proc filter*[T](it: iterator(): T,
                f: proc(elem: T): bool): iterator(): T =
  result = iterator(): T =
    for elem in it():
      if f(elem):
        yield elem


proc accum*[T, S](it: iterator(): T,
                  f: proc(so_far: S, elem: T): S,
                  init: S): S =
  result = init
  for elem in it():
    result = f(result, elem)


proc min*[T](it: iterator(): T): T =
  for i, elem in enumerate(it()):
    if i == 0 or elem < result:
      result = elem


proc max*[T](it: iterator(): T): T =
  for i, elem in enumerate(it()):
    if i == 0 or result < elem:
      result = elem


proc sum*[T](it: iterator(): T): T =
  for i, elem in enumerate(it()):
    if i == 0:
      result = elem
      continue
    result += elem


proc prod*[T](it: iterator(): T): T =
  for i, elem in enumerate(it()):
    if i == 0:
      result = elem
      continue
    result *= elem


proc all*[T](it: iterator(): T): bool =
  for elem in it():
    if not elem.bool:
      return false
  return true


proc any*[T](it: iterator(): T): bool =
  for elem in it():
    if elem.bool:
      return true
  return false


package de.unisaarland.control

case class Fn(x: Value.Variable, b: Body)

enum Value:
  case Constant(i: Int)
  case Variable(id: Int)
  case Label(fn: Fn)

enum Body:
  case Let(x: Value.Variable, i: Instruction, b: Body)
  case TailCall(callee: Value, arg: Value)

enum Instruction:
  case PrimOp(op: String, ops: List[Value])
  case Control(x: Value.Variable, b: Body)

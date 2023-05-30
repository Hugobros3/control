package de.unisaarland.control

import Body.*, Value.*, Instruction.*

def substitute(x: Value, s: Variable, w: Value): Value = if x == s then w else x
def substitute(b: Body, s: Variable, w: Value): Body = b match {
  case Let(x, i, t) => assert(x != s); Let(x, substitute(i, s, w), substitute(t, s, w))
  case TailCall(c, a) => TailCall(substitute(c, s, w), substitute(a, s, w))
}
def substitute(i: Instruction, s: Variable, w: Value): Instruction = i match {
  case PrimOp(op, ops) => PrimOp(op, ops.map(vn => substitute(vn, s, w)))
  case Control(x, b) => assert(x != s); Control(x, substitute(b, s, w))
}
case class Ctx(jps: List[Variable])
def step(p: Body, ctx: Ctx = Ctx(List())): Body = p match {
  case TailCall(Variable(0), v) => throw Exception(s"done, final result: $v")
  case TailCall(Label(Fn(x, b)), v) => substitute(b, x, v) // beta-reduction
  case Let(x, Control(j1, TailCall(j2, y)), b) if j1 == j2 => substitute(b, x, y) // join immediate
  case Let(x, Control(_, TailCall(j2, y)), b) => assert(ctx.jps.contains(j2)); TailCall(j2, y) // join non-immediate
  case Let(x, Control(j, b), t) => Let(x, Control(j, step(b, Ctx(ctx.jps ++ List(j)))), t) // step in control
  case Let(x, PrimOp("add", List(Constant(l), Constant(r))), b) => substitute(b, x, Constant(l + r)) // add
  case Let(x, PrimOp(op, _), b) => throw Exception(s"unimplemented prim op $op")
  case _ => throw Exception("stuck")
}

def exec(p: Body): Unit = {
  try {
    var state = p;
    while true do {
      state = step(state)
    }
  } catch {
    case e: Exception => println(e);
  }
}
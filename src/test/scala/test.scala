package de.unisaarland.control

import Body.*, Value.*, Instruction.*

import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {
  test("basic") {
    val program: Body = TailCall(Variable(0), Constant(1))
    exec(program)
  }
  test("add") {
    val program: Body = Let(Variable(1), PrimOp("add", List(Constant(1), Constant(2))), TailCall(Variable(0), Variable(1)))
    exec(program)
  }
  test("yield") {
    // let x = control j {
    //   let y = 1 + 2
    //   in j(y)
    // in return(x)
    val program: Body = Let(Variable(1),
                        Control(Variable(2), Let(Variable(3),
                                             PrimOp("add", List(Constant(1), Constant(2))),
                                             TailCall(Variable(2), Variable(3)))),
                        TailCall(Variable(0), Variable(1)))
    exec(program)
  }
  test("yield_far") {
    // let x = control j {
    //   let y = 1 + 2
    //   in let z = control k {
    //     j(y)
    //   } in return(0) // dead code !
    // in return(x)
    val program: Body = Let(Variable(1),
                          Control(Variable(2), Let(Variable(3),
                                                   PrimOp("add", List(Constant(1), Constant(2))),
                                                   Let(Variable(4),
                                                       Control(Variable(5), TailCall(Variable(2), Variable(3))),
                                                       TailCall(Variable(0), Constant(0))))),
                          TailCall(Variable(0), Variable(1)))
    exec(program)
  }
}
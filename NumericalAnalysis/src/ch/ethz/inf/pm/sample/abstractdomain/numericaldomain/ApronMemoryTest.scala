package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron._

object ApronMemoryTest {

  def main(args: Array[String]) {

    //val manager = new Octagon()
    var i = 0

    while (true) {

      val vars = List("a").toArray
      val env = new Environment().add(vars,new Array[String](0))
      //val state = new Abstract1(manager,env)

      //state.assign(manager,vars(0),new Texpr1Intern(env,new Texpr1CstNode(new apron.Interval(0,0))),null)

      //println(state)
      //println(env)
      println(vars)

      i = i + 1

    }



  }



}

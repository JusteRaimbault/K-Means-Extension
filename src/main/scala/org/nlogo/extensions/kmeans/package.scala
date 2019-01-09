package org.nlogo.extensions


import java.util.Locale

import org.nlogo.api
import org.nlogo.agent
import org.nlogo.core.Token

package object kmeans {

  def toTurtleSet(
    turtles: Traversable[api.Turtle],
    world: api.World): api.AgentSet = {
    val agents = turtles
      .collect { case t: agent.Turtle => t }
      .toArray[agent.Agent]
    new agent.ArrayAgentSet(
      agents(0).kind,
      agents.mkString(" "),
      agents
      )
  }

  def splitAgentSet(
    agentSet: api.AgentSet,
    rng: api.MersenneTwisterFast,
    world: api.World): Seq[api.AgentSet] = {
    val xs = Seq.newBuilder[api.AgentSet]
    val it = agentSet.asInstanceOf[agent.AgentSet].shufflerator(rng)
    while (it.hasNext) {
      val next = it.next
      xs += new agent.ArrayAgentSet(agentSet.kind, next.toString, Array(next))
    }
    xs.result
  }

  def canonicalVarName(variable: AnyRef) = variable match {
    case s: String => s.toUpperCase(Locale.ENGLISH)
    case t: Token  => t.text.toString.toUpperCase(Locale.ENGLISH)
  }

}

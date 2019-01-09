package org.nlogo.extensions.kmeans

import scala.collection.JavaConverters._
import org.nlogo.api._
import org.nlogo.core.{AgentKind, LogoList, Syntax}
import org.nlogo.core.Syntax._
import org.nlogo.api.ScalaConversions._
import edu.uci.ics.jung.algorithms.util.KMeansClusterer

class KMeansExtension extends DefaultClassManager {
  def load(primitiveManager: PrimitiveManager) {
    primitiveManager.addPrimitive("cluster-by-xy", KMeansClustersXYPrim)
    primitiveManager.addPrimitive("cluster-by-feature", KMeansClustersFeaturePrim)
  }
}

object KMeansClustersXYPrim extends Reporter {
  override def getSyntax = Syntax.reporterSyntax(right = List(AgentsetType, NumberType, NumberType, NumberType),ret = ListType)

  override def report(args: Array[Argument], context: Context) = {
    val agentSet = args(0).getAgentSet
    if (agentSet.kind != AgentKind.Turtle)
      throw new ExtensionException("Expected input to be a turtle set.")
    try {
      KMeans.clusters(
        agentSet, // turtles
        args(1).getIntValue, // nbClusters
        args(2).getIntValue, // maxIterations
        args(3).getDoubleValue, // convergenceThreshold
        context.getRNG,
        context.getAgent.world)
    } catch {
      case e: IllegalArgumentException => throw new ExtensionException(e.getMessage)
    }
  }
}

object KMeansClustersFeaturePrim extends Reporter {
  override def getSyntax: Syntax = Syntax.reporterSyntax(right = List(AgentsetType, NumberType, NumberType, NumberType,StringType),ret = ListType)

  override def report(args: Array[Argument], context: Context) = {
    val agentSet = args(0).getAgentSet
    if (agentSet.kind != AgentKind.Turtle)
      throw new ExtensionException("Expected input to be a turtle set.")
    try {
      KMeans.clusters(
        agentSet, // turtles
        args(1).getIntValue, // nbClusters
        args(2).getIntValue, // maxIterations
        args(3).getDoubleValue, // convergenceThreshold
        context.getRNG,
        context.getAgent.world,
        { case t: org.nlogo.agent.Turtle => {
          val variable = canonicalVarName(args(4).getString) //args(4).getString
          t.world.turtlesOwnIndexOf(variable) match {
            case -1 => t.getBreedVariable(variable).asInstanceOf[LogoList].toVector.map{_.asInstanceOf[Double]}.toArray
            case i => t.getVariable(i).asInstanceOf[LogoList].toVector.map{_.asInstanceOf[Double]}.toArray
          }
          //t.getVariable(variable+8).asInstanceOf[LogoList].toVector.map{_.asInstanceOf[Double]}.toArray
        }
        }
      )
    } catch {
      case e: IllegalArgumentException => throw new ExtensionException(e.getMessage)
    }
  }

}

object KMeans {

  def clusters(
    agentSet: AgentSet,
    nbClusters: Int,
    maxIterations: Int,
    convergenceThreshold: Double,
    rng: MersenneTwisterFast,
    world: World,
    featureFunction: Turtle => Array[Double] = { case t: Turtle =>  Array(t.xcor, t.ycor) }
              ): LogoList = {

    val features = agentSet.agents.asScala
      .collect { case t: Turtle => t -> featureFunction(t) }
      .toMap
    val nbTurtles = agentSet.count
    lazy val nbDistinctFeatures = features.values.map(_.deep).toSet.size

    // Jung's clusterer is a lot pickier than I think we should be, so
    // we handle the edge cases ourselves and only defer to the actual
    // clusterer when we are sure it's in a position to do its job.
    val cs: Seq[AgentSet] = nbClusters match {

      // Not enough turtles:
      case n if n > agentSet.count => throw new IllegalArgumentException(
        "Not enough turtles to form the requested number of clusters.")

      // zero clusters results in empty list:
      case 0 => Seq()

      // only one cluster requires no clustering:
      case 1 => Seq(agentSet)

      // Not enough distinct turtles:
      case n if n > nbDistinctFeatures => throw new IllegalArgumentException(
        "Not enough turtles at distinct locations to form the requested number of clusters.")

      // Same of number of (verified distinct) turtles as clusters:
      case n if n == nbTurtles => splitAgentSet(agentSet, rng, world)

      // every other case handled by the actual clusterer:
      case _ =>
        object Clusterer extends KMeansClusterer[Turtle] {
          rand = rng
          setMaxIterations(maxIterations)
          setConvergenceThreshold(convergenceThreshold)
        }
        Clusterer
          .cluster(features.asJava, nbClusters)
          .asScala
          .map(c => toTurtleSet(c.keySet.asScala.toSeq, world))
          .toSeq
    }
    cs.toLogoList
  }
}

package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{CommonNode}
import collection.mutable.Map


class Env(d: Map[String, CommonNode], l: Map[String, CommonNode]) {
  val defs = d
  val locals = l

  override def toString =
        "Env(" + locals + ", " + defs + ")"

}
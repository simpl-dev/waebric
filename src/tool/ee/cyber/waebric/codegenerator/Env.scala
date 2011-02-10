package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{CommonNode}
import collection.mutable.Map


class Env(val defs: Map[String, CommonNode], val locals: Map[String, CommonNode]) {

  override def toString =
        "Env(" + locals + ", " + defs + ")"

}
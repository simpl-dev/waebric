package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{CommonNode}

class Env(defs: Map[String, CommonNode], locals: Map[String, CommonNode]) {



  override def toString =
        "Env(" + locals + ", " + defs + ")"

}
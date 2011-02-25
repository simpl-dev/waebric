package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{CommonNode}
import ee.cyber.waebric.lexer._
import collection.mutable.Map
import xml.NodeSeq


class Env(val defs: Map[String, FunctionDef],
          val locals: Map[String, NodeSeq]) {
  override def toString =
        "Env(" + locals + ", " + defs + ")"
}
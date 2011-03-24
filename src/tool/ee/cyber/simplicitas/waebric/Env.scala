package ee.cyber.simplicitas.waebric

import ee.cyber.simplicitas.{CommonNode}
import xml.NodeSeq

// Represents function objects in the environment.
class FunObj(val args: List[IdCon], val statements: List[Statement], var env: Env)

object FunObj {
    def apply(binding: FuncBinding, env: Env): FunObj =
        new FunObj(binding.args, List(binding.statement), env)

    def apply(binding: FunctionDef, env: Env): FunObj =
        new FunObj(binding.args, binding.statements, env)
}

class Env(val parent: Env, val defs: Map[String, FunObj],
          val locals: Map[String, NodeSeq]) {
    var yieldValue: NodeSeq = null

    def expand(funs: Map[String, FuncBinding],  locals: Map[String, NodeSeq]) =
        new Env(this, funs.mapValues(FunObj(_, this)).toMap, locals)

    def varExpand(locals: Map[String, NodeSeq]): Env =
        expand(Map.empty, locals)

    // returns (statements, argument names, env)
    def resolveFunction(name: String): FunObj =
        if (defs.contains(name))
            defs(name)
        else
            parent.resolveFunction(name)

    def resolveVariable(name: String): NodeSeq =
        if (locals.contains(name))
            locals(name)
        else
            parent.resolveVariable(name)

    def resolveYield(): NodeSeq =
        if (yieldValue ne null)
            yieldValue
        else
            parent.resolveYield

    override def toString =
        "Env(locals: " + locals + ",\ndefs: " + defs + ")"
}

object Env {
    def topLevel(topDefs: Map[String, FunctionDef]) = {
        val defMap = topDefs.mapValues(FunObj(_, null)).toMap
        val env = new Env(null, defMap, Map.empty) {
            override def resolveFunction(name: String) =
                if (defs.contains(name))
                    defs(name)
                else
                    null

            override def resolveVariable(name: String) = null

            override def resolveYield = NodeSeq.Empty
        }

        // Toplevel functions are mutually recursive and reference env that
        // contains their peers.
        for (fo <- env.defs.values) {
            fo.env = env
        }

        env
    }
}
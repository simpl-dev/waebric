package ee.cyber.simplicitas.waebric

import ee.cyber.simplicitas.{CommonNode}
import collection.mutable.Map
import xml.NodeSeq

// Represents function objects in the environment.
case class FunObj(name: String, args: List[IdCon],
                  statements: List[Statement], env: Env)

class Env(val parent: Env, val defs: Map[String, FunctionDef],
          val locals: Map[String, NodeSeq]) {
    var funcs: Map[String, FuncBinding] = Map.empty
    var functionEnv: Env = null
    var yieldValue: NodeSeq = null

    def expand(funs: Map[String, FuncBinding], funEnv: Env,
               locals: Map[String, NodeSeq]): Env = {
        val env = new Env(this, Map.empty, locals)
        env.funcs ++= funs
        if (!funs.isEmpty) {
            env.functionEnv = funEnv
        }
        env
    }

    def varExpand(locals: Map[String, NodeSeq]): Env =
        expand(Map.empty, null, locals)

    // returns (statements, argument names, env)
    def resolveFunction(name: String): Tuple3[List[Statement], List[IdCon], Env] = {
        D.ebug("Resolve function " + name)
        if (funcs.contains(name)) {
            D.ebug("Function found")
            val f = funcs(name)
            return (List(f.statement), f.args, functionEnv)
        }
        if (defs.contains(name)) {
            D.ebug("Definition found")

            val fun = defs(name)
            return (fun.statements, fun.args, functionEnv)
        }
        if (parent ne null) {
            return parent.resolveFunction(name)
        } else {
            return null
        }
    }

    def resolveVariable(name: String): NodeSeq = {
        D.ebug("Resolve variable " + name)
        if (locals.contains(name)) {
            D.ebug("Variable found")
            return locals(name)
        }
        if (parent ne null) {
            return parent.resolveVariable(name)
        }
        return null
    }

    def resolveYield(): NodeSeq = {
        if (yieldValue ne null) {
            return yieldValue
        }
        if (parent ne null) {
            return parent.resolveYield
        }
        return NodeSeq.Empty
    }

    override def toString =
        "Env(locals: " + locals + ",\ndefs: " + defs + ",\nfuncs: " + funcs + ",\n funcEnv: " + functionEnv + ")"
}
package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{CommonNode}
import ee.cyber.waebric.lexer._
import collection.mutable.Map
import xml.NodeSeq



class Env(val parent: Env, val defs: Map[String, FunctionDef],
          val locals: Map[String, NodeSeq]) {
  var funcs: Map[String, FuncBinding] = Map.empty
  var functionEnv: Env = null
  var yieldValue: NodeSeq = null

  def expand(fnsAndEnv: Tuple2[Map[String, FuncBinding], Env], locals: Map[String, NodeSeq]): Env = {
    val env: Env = new Env(this, Map.empty, locals)
    env.funcs ++= fnsAndEnv._1
    if (!fnsAndEnv._1.isEmpty) {
      env.functionEnv = fnsAndEnv._2
    }
    return env

  }

  def varExpand(locals: Map[String, NodeSeq]): Env =
      expand((Map.empty, null), locals)

  def setYield(y: NodeSeq) = {yieldValue = y}

  // returns (statements, argument names, env)
  def resolveFunction(name: String): Tuple3[List[Statement], List[IdCon], Env] = {
      D.ebug("Resolve function " + name)
      if (funcs.contains(name)) {
          D.ebug("Function found")
          funcs(name) match {
              case FuncBinding(idCon, null, statement) =>
                  return (List(statement), List.empty, functionEnv)
              case FuncBinding(idCon, FuncArguments(first, rest), statement) =>
                  return (List(statement), first :: rest, functionEnv)
              case _ => return null
          }
      }
      if (defs.contains(name)) {
          D.ebug("Definition found")
          defs(name) match {
              case FunctionDef(Function(_, FunctionArgs(Formals(first, rest))), statements, _) =>
                  return (statements, first :: rest, functionEnv)
              case FunctionDef(Function(_, FunctionArgs(null)), statements, _) =>
                  return (statements, List.empty, functionEnv)
              case FunctionDef(FunctionName(_), statements, _) =>
                  return (statements, List.empty, functionEnv)
              case _ => return null
          }
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
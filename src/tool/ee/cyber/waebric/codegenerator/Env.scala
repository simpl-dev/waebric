package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{CommonNode}
import ee.cyber.waebric.lexer._
import collection.mutable.Map
import xml.NodeSeq



class Env(val parent: Env, val defs: Map[String, FunctionDef],
          val locals: Map[String, NodeSeq]) {
  var funcs: Map[String, FuncBinding] = Map.empty

  private var yieldValue: NodeSeq = null

  def expand(functions: Map[String, FuncBinding], locals: Map[String, NodeSeq]) : Env = {
      val env: Env = new Env(this, Map.empty, locals)
      env.funcs ++= functions
      return env
  }

  def setYield(y: NodeSeq) = {yieldValue = y}

  // returns statements, argument names
  def resolveFunction(name: String): Tuple2[List[Statement], List[IdCon]] = {
      D.dbg("Resolve function " + name)
      if (funcs.contains(name)) {
          D.dbg("Function found")
          funcs(name) match {
              case FuncBinding(idCon, null, statement) =>
                  return (List(statement), List.empty)
              case FuncBinding(idCon, FuncArguments(first, rest), statement) =>
                  return (List(statement), first :: rest)
              case _ => return null
          }
      }
      if (defs.contains(name)) {
          D.dbg("Definition found")
          defs(name) match {
              case FunctionDef(Function(_, FunctionArgs(Formals(first, rest))), statements, _) =>
                  return (statements, first :: rest)
              case FunctionDef(Function(_, FunctionArgs(null)), statements, _) =>
                  return (statements, List.empty)
              case FunctionDef(FunctionName(_), statements, _) =>
                  return (statements, List.empty)
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
      D.dbg("Resolve variable " + name)
      if (locals.contains(name)) {
          D.dbg("Variable found")
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
        "Env(" + locals + ", " + defs + ")"
}
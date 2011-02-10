package ee.cyber.waebric.codegenerator

import collection.mutable.ArrayBuffer
import collection.mutable.Map

import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode, SourceMessage}
import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.waebric.lexer._


private class Generator(tree: Program) {

  val errors = ArrayBuffer[SourceMessage]()

  def generate() {
    val globalEnv: Env = new Env(Map.empty, Map.empty)

    processDefs(tree, globalEnv)
    println("processDefs result: " + globalEnv)

  }

  def processDefs(node: Program, env: Env) {

    println("ProcessDefs called")

    // todo, check whether functions with a same name are allowed

    for (definition <- node.definitions.definition) {
      var fnName:String = ""
      definition match {
        case FunctionDef(Function(name, _), _, _) =>
         fnName = name.idCon.text
         env.defs += makeDefBinding(name.idCon, definition)
        case FunctionDef(FunctionName(idCon), _, _) =>
          fnName = idCon.text
          env.defs += makeDefBinding(idCon, definition)
        case _ => ()
      }
      println("def " + fnName + " found")
    }

  }

  def makeDefBinding(name: IdCon, definition: CommonNode) =
        (name.text, definition)


}

object WaebricGenerator extends MainBase {
  def main(arg: Array[String]) {
    parseOptions(arg)
    val grammar = new WaebricSimplGrammar
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)

      val gen = new Generator(grammar.tree);
      gen.generate


    }
  }
}

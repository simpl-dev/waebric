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
      definition match {
        case FunctionDef(idCon, arguments, _, _) =>
          println("def " + idCon.text + " found")
          env.defs += makeBinding(idCon)

        case _ => ()
      }
    }

  }

  def makeBinding(id: IdCon) =
        (id.text, id)


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

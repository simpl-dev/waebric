package ee.cyber.waebric.codegenerator

import collection.mutable.ArrayBuffer

import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode, SourceMessage}
import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.waebric.lexer._


private class Generator(tree: Program) {

  val errors = ArrayBuffer[SourceMessage]()

  def generate() {
    val globalEnv: Env = new Env(Map.empty, Map.empty)
    processDefs(tree, globalEnv)


  }

  def processDefs(expr: Program, env: Env) {

    println("ProcessDefs called")
    println(env)
  }

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

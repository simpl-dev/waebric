package ee.cyber.waebric.codegenerator

import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode}
import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.waebric.lexer._


private class CodeGenerator(tree: Program) {

  //type Env = Map[String, CommonNode]


  def generate() {
    val globalEnv: Env = new Env(Map.empty, Map.empty)
    processDefs(tree, globalEnv)


  }

  def processDefs(expr: Program, env: Env) {

    println("ProcessDefs called")
  }

}

object WaebricSimplCodeGenerator extends MainBase {
  def main(arg: Array[String]) {
    parseOptions(arg)
    val grammar = new WaebricSimplGrammar
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)

      val gen = new CodeGenerator(grammar.tree);
      gen.generate


    }
  }
}

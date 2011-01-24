package ee.cyber.waebric.lexer;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import Dump._


class WaebricSimplGenerator(destDir: String)
        extends GeneratorBase(destDir) {
//  val templates = getTemplates("WaebricSimpl.stg")

  def generate(tree: Program) {
      val dumped = Dump.dumpNode(tree)
      print(dumped);
      println("RAW:")
      print(tree);
  }
}

object WaebricSimplAST extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new WaebricSimplGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)

      new WaebricSimplGenerator(destDir).generate(grammar.tree)
    }
  }
}

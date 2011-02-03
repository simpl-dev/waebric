package ee.cyber.waebric.lexer;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import ee.cyber.simplicitas.PrettyPrint._


class WaebricSimplGenerator(destDir: String)
        extends GeneratorBase(destDir) {

  def generate(tree: Program) {

      // Just print the syntax tree - nice one and RAW
      println(prettyPrint(tree))
      //val dumped = Dump.dumpNode(tree)
      //print(dumped);
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

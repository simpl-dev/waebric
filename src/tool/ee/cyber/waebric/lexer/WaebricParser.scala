package ee.cyber.waebric.lexer;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import ee.cyber.simplicitas.PrettyPrint._


object WaebricASTTest extends  MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new WaebricSimplGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      println(prettyPrint(grammar.tree))
      println("RAW:")
      print(grammar.tree);
    }
  }
}

object WaebricAST extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new WaebricSimplGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      println(prettyPrint(grammar.tree))
    }
  }
}

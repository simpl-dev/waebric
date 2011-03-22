package ee.cyber.waebric.lexer;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import ee.cyber.simplicitas.PrettyPrint._

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

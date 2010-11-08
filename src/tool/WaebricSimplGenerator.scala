package ee.cyber.simplicitas.waebric;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class WaebricSimplGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("WaebricSimpl.stg")
    
  def generate(tree: Program) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
  }
}
  
object WaebricSimplMain extends MainBase {
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

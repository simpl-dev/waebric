package ee.cyber.waebric.codegenerator

import collection.mutable.ArrayBuffer
import collection.mutable.Map
//import xml.Elem
//import xml.XML
import org.xml.sax._

import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode, SourceMessage}
import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.waebric.lexer._


private class Generator(tree: Program) {

  val errors = ArrayBuffer[SourceMessage]()
  var buffer: StringBuffer = new StringBuffer

  def generate() {
    import xml._

    val globalEnv: Env = new Env(Map.empty, Map.empty)


    processDefs(tree, globalEnv)
    println("processDefs result: " + globalEnv)

    // todo: first process sites. if not found, try with "main"
    generate(globalEnv.defs.get("main").get, globalEnv);
    println(buffer.toString)

    var outputXml: Elem = null
    var outputText: String = null
    try {
      outputXml = XML.loadString(buffer.toString)
    } catch {
      // likely, it's not xml, root element missing, etc.

      case e =>
        println("Error generating XML, trying to add <html> tags")
        try {
          outputXml = XML.loadString(initHtml + buffer.toString + closeHtml)
        } catch {
          case e =>
            println("Error generating XML document, outputing raw...")
            outputText = buffer.toString
        }
    }
    if (outputXml ne null) {
      println("Xml doc found")
      println(outputXml.buildString(false))
    } else {
      println("Raw output")
      println(outputText)
    }



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

  def generate(node: CommonNode, env: Env) {
    node match {
      case FunctionDef(Function(name, args), statements, _) =>
        //todo: deal with args
        processStatements(statements, env)
      case FunctionDef(FunctionName(idCon), statements, _) =>
        processStatements(statements, env)
      case EchoStatement(echoBody, _) =>
        println("EchoStatement found")
        generate(echoBody, env)
        buffer.append("\n")
      case CatExpression(left, right) =>
        generate(left, env)
        generate(right, env)
      case Text(text) =>
        buffer.append(stripQuote(text))
      case NatCon(text) =>
        buffer.append(text)

      case _ => ()
    }

  }

  def processStatements(statements: List[Statement], env: Env) {
    for (statement <- statements) {
      generate(statement, env)
    }
  }

  def stripQuote(s: String) = s.substring(1, + s.length - 1)
  def initHtml = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + "<html>" + "\n"
  def closeHtml= "</html>"

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

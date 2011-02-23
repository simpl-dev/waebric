package ee.cyber.waebric.codegenerator

import collection.mutable.ArrayBuffer
import collection.mutable.Map
//import xml.Elem
//import xml.XML
import org.xml.sax._
import xml._


import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode, SourceMessage}
import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.waebric.lexer._


private class Generator(tree: Program) {

  val errors = ArrayBuffer[SourceMessage]()
  var buffer: StringBuffer = new StringBuffer
  var nodes: NodeSeq = List.empty


  def generate() {

    val globalEnv: Env = new Env(Map.empty, Map.empty)


    processDefs(tree, globalEnv)
    println("processDefs result: " + globalEnv)

    // todo: first process sites. if not found, try with "main"
    nodes = List.empty
    generate(globalEnv.defs.get("main").get, globalEnv);
    println(nodes.toString)



    /*
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
    */



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
      case _ => ()
    }

  }

  def processStatements(statements: List[Statement], env: Env) {
    for (statement <- statements) {
      generate(statement, env)
      statement match {
        case EchoStatement(echoBody, _) =>
          val ret = processExpressions(echoBody, env)
          println("EchoStatement returned: " + ret.toString)

        case _ => ()

      }

    }
  }

  //def processExpressions(exp: CommonNode, env: Env) : NodeSeq = processExpressions(exp, env, null)

  def processExpressions(exp: CommonNode, env: Env) : NodeSeq = {
    exp match {
      /* Terminal handling */
      case Txt(text) => Text(stripQuote(text))
      case NatCon(text) => Text(text)
      case PreText(text) => Text(stripPre(text))
      case PostText(text) => Text(stripPost(text))


      /* Expression handling */
      case CatExpression(left, right) =>
        processExpressions(left, env) ++ processExpressions(right, env)
      case Embedding(preText, embed, textTail) =>
        processExpressions(preText, env) ++ processExpressions(embed, env) ++
            processExpressions(textTail, env)
      case _ => Text("")
    }

  }

  def textElem(name: String, text: String) =  Elem(null, name, Null, TopScope, Text(text))
  //def generalElem(name: String, child: Node*) = new Elem(null, name, Null, TopScope, child)

  def stripQuote(s: String) = s.substring(1, s.length - 1)
  def stripPre(s: String) = s.substring(1, s.length)
  def stripPost(s: String) = s.substring(0, s.length - 1)
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

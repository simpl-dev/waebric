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
    generate(globalEnv.defs("main"), globalEnv);

    if (nodes.size != 1 || (nodes.size == 1 && !nodes.head.isInstanceOf[Elem])) {
      // not bounded
      nodes = generalElem("html", nodes)
    }
    println(nodes.toString)
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
          nodes ++= ret
        case _ => ()

      }

    }
  }

  def processExpressions(exp: CommonNode, env: Env) : NodeSeq = {

    def resolveArguments(markup: Markup) : NodeSeq = {
      /*println("Processing arguments of markup")
      if (markup.markupArguments eq null) {
        println("MarkupArguments null")
        return Text("")
      }
      markup.markupArguments match  {
        case AttrArg(idCon, expression) =>
          println("attrArg found")
          Text("")
        case Arguments(first, rest) =>
          println("Arguments found")
          Text("")
        case _ =>
          println("Expression found (probably)")
          processExpressions(markup.markupArguments, env)
      } */
      Text("")

    }

    def processMarkups(markups: List[Markup], exp: Expression, env: Env) : NodeSeq = {
      markups match {
        case head::tail =>

          generalElem(head.designator.idCon.text,
            if (tail.size > 0) processMarkups(tail, exp, env) else processExpressions(exp, env))

        case Nil => Text("")
      }
    }

    exp match {
      /* Terminal handling */
      case Txt(text) => Text(stripEdges(text))
      case NatCon(text) => Text(text)
      case PreText(text) => Text(stripEdges(text))
      case PostText(text) => Text(stripEdges(text))
      case MidText(text) => Text(stripEdges(text))


      /* Expression handling */
      case CatExpression(left, right) =>
        processExpressions(left, env) ++ processExpressions(right, env)
      case Embedding(preText, embed, textTail) =>
        processExpressions(preText, env) ++ processExpressions(embed, env) ++
          processExpressions(textTail, env)
      case EmbedMarkup(markups) =>
        println("EmbedMarkups found: " + markups.size)
        processMarkups(markups, null, env)
      case EmbedExp(markups, exp) =>
        println("EmbedExp found")
        processMarkups(markups, exp, env)
      case TextTailMid(midText, embed, textTail) =>
        processExpressions(midText, env) ++ processExpressions(embed, env) ++
          processExpressions(textTail, env)
      case _ => Text("")
    }
  }

  def textElem(name: String, text: String) =  Elem(null, name, Null, TopScope, Text(text))
  def generalElem(name: String, child: NodeSeq) = new Elem(null, name, Null, TopScope, child: _*)

  def stripEdges(s: String) = s.substring(1, s.length - 1)
  def stripPre(s: String) = s.substring(1, s.length)
  def stripPost(s: String) = s.substring(0, s.length - 1)

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

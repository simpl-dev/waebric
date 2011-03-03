package ee.cyber.waebric.codegenerator

import collection.mutable.ArrayBuffer
import collection.mutable.Map
//import xml.Elem
//import xml.XML
import org.xml.sax._
import xml._


import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode,
    SourceMessage, PrettyPrint}
import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.waebric.lexer._


private class Generator(tree: Program) {

  val errors = ArrayBuffer[SourceMessage]()
  var buffer: StringBuffer = new StringBuffer

  def generate() {

    val globalEnv: Env = new Env(null, Map.empty, Map.empty)

    println(PrettyPrint.prettyPrint(tree))

    processDefs(tree, globalEnv)
    //println("processDefs result: " + globalEnv)

    // todo: first process sites. if not found, try with "main"
    var nodes = evalStatement(
        MarkupStatement(
          Markup(
            Designator(IdCon("main"), List.empty),
            null),
          MarkupSemi(Semicolon(";"))),
        globalEnv);

    if (nodes.size != 1 ||
            (nodes.size == 1 && !nodes.head.isInstanceOf[Elem])) {
      // not bounded
      nodes = elem("html", nodes)
    }
    println("\n" + new PrettyPrinter(65, 2).formatNodes(nodes))
  }

  def processDefs(node: Program, env: Env) {
    println("ProcessDefs called")

    // todo, check whether functions with a same name are allowed

    for (definition <- node.definitions.definition) {
      var fnName:String = ""
      definition match {
        case FunctionDef(Function(name, _), _, _) =>
         fnName = name.idCon.text
         env.defs += (name.idCon.text -> definition.asInstanceOf[FunctionDef])
        case FunctionDef(FunctionName(idCon), _, _) =>
          fnName = idCon.text
          env.defs += (idCon.text -> definition.asInstanceOf[FunctionDef])
        case _ => ()
      }
      println("def " + fnName + " found")
    }
  }

  def evalStatements(statements: List[Statement], env: Env): NodeSeq =
    statements.map(evalStatement(_, env)).foldLeft(NodeSeq.Empty)(_ ++ _)

  def evalStatement(statement: Statement, env: Env): NodeSeq =
    statement match {
      case EchoStatement(echoBody, _) =>
        val ret = evalExpr(echoBody, env)
        println("EchoStatement returned: " + ret.toString)
        ret
      case MarkupStatement(markup, chain) =>
        val chainRet = evalMarkupChain(chain, env)
        evalMarkup(markup, chainRet, env)
      case LetStatement(assignments, statements, _) =>
        val newEnv = applyAssignments(assignments, env)
        evalStatements(statements, newEnv)
      case BlockStatement(statements) =>
        evalStatements(statements, env)
      case _ =>
        NodeSeq.Empty
    }

  def evalMarkupChain(chain: MarkupChain, env: Env): NodeSeq = chain match  {
    case MarkupSemi(_) =>
      NodeSeq.Empty
    case MarkupExp(expr, _) =>
      evalExpr(expr, env)
    case MarkupEmb(embed, _) =>
      evalExpr(embed, env)
    case s: Statement =>
      evalStatement(s, env)
  }

  def evalMarkups(markups: List[Markup], body: NodeSeq, env: Env): NodeSeq =
    markups.foldLeft(body)(
      (b: NodeSeq, m: Markup) =>
        evalMarkup(m, b, env))

  def evalMarkup(markup: Markup, body: NodeSeq, env: Env): NodeSeq = {
    println("evalMarkup(" + markup + ", " + body + ")")
    // TODO: process designator attributes.
    val desText = markup.designator.idCon.text
    val fun = env.resolveFunction(desText)
    if (fun ne null) {
      val newEnv = bindParameters(fun._2, markup, env)

      // TODO: somehow pass the body to the statement.
      evalStatements(fun._1, newEnv)
    } else {
      // TODO: check if is XHTML tag.
      // TODO: resolve arguments and/or attributes
      addXHTMLAttributes(elem(desText, body), markup, env)
    }
  }

  // Processes expressions and embeddings.
  def evalExpr(exp: CommonNode, env: Env) : NodeSeq = {

    exp match {
      /* Terminal handling */
      case Txt(text) => Text(stripEdges(text))
      case NatCon(text) => Text(text)
      case PreText(text) => Text(stripEdges(text))
      case PostText(text) => Text(stripEdges(text))
      case MidText(text) => Text(stripEdges(text))


      /* Expression handling */
      case IdCon(text) => env.resolveVariable(text)
      case CatExpression(left, right) =>
        evalExpr(left, env) ++ evalExpr(right, env)
      case Embedding(preText, embed, textTail) =>
        evalExpr(preText, env) ++
                evalExpr(embed, env) ++
                evalExpr(textTail, env)
      case EmbedMarkup(markups) =>
        println("EmbedMarkups found: " + markups.size)
        evalMarkups(markups, NodeSeq.Empty, env)
      case EmbedExp(markups, exp) =>
        println("EmbedExp found")
        evalMarkups(markups, evalExpr(exp, env), env)
      case TextTailMid(midText, embed, textTail) =>
        evalExpr(midText, env) ++
                evalExpr(embed, env) ++
                evalExpr(textTail, env)
      case _ =>
        Text("")
    }
  }

  private def applyAssignments(assignments: List[Assignment], env: Env): Env = {
      val aMap = assignments filter { a => a.isInstanceOf[VarBinding] } map {
          a => (a.asInstanceOf[VarBinding].idCon.text, evalExpr(a.asInstanceOf[VarBinding].expression, env))} toMap;
      val fMap = assignments filter { f => f.isInstanceOf[FuncBinding] } map {
          f => (f.asInstanceOf[FuncBinding].func.text, f.asInstanceOf[FuncBinding]) } toMap;
      println("aMap: " + aMap)
      println("fMap: " + fMap)
      return env.expand(collection.mutable.Map(fMap.toSeq: _*), collection.mutable.Map(aMap.toSeq: _*))

  }

  private def getMarkupArguments(markup: Markup): List[Argument] = markup.markupArguments match {
        case MarkupArguments(first, rest) =>
            first :: rest
        case null =>
            println("No markupArguments found")
            List.empty
      }

  private def bindParameters(funArgs: List[IdCon], markup: Markup, env: Env): Env = {
      val markupArgs: List[Argument] = getMarkupArguments(markup)
      // check whether number of arguments and number of parameters match
      if (markupArgs.size < funArgs.size) throw new Exception("Wrong number of arguments: " + markupArgs.size)

      val bindMap = funArgs zip markupArgs map { f => (f._1.text, f._2 match {
                  case AttrArg(_, exp) => evalExpr(exp, env)
                  case _ => evalExpr(f._2, env)})} toMap;

      return env.expand(Map.empty, collection.mutable.Map(bindMap.toSeq: _*))

  }

  private def addXHTMLAttributes(elem: Elem, markup: Markup, env: Env): NodeSeq = {
      val markupArgs: List[Argument] = getMarkupArguments(markup)

      // We are interested only in AttrArg type
      val argList = markupArgs filter { f => f.isInstanceOf[AttrArg]} map {
          f => f match {
            case AttrArg(idCon, exp) => Attribute(idCon.text, evalExpr(exp, env), Null)
          }}
      println("argList: " + argList)
      //argList.foldLeft(elem)((e: Elem, m: MetaData) => e % m)
      argList.foldLeft(elem)(_ % _)

  }

  def elem(name: String, text: String) =
    Elem(null, name, Null, TopScope, Text(text))

  def elem(name: String, children: NodeSeq) =
    Elem(null, name, Null, TopScope, children: _*)

  def stripEdges(s: String) = s.substring(1, s.length - 1)
  def stripPre(s: String) = s.substring(1, s.length)
  def stripPost(s: String) = s.substring(0, s.length - 1)
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

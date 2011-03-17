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


private case class ListNodeSeq(var list: List[NodeSeq]) extends NodeSeq {
  def theSeq: Seq[Node] = {
    var ret: Seq[Node] = Seq.empty
    list.foreach(l => ret ++= l.toSeq)
    ret
  }
}

trait RecordNodes {
  def get(k: String): NodeSeq
}

private case class RecordNodeSeq(var records: List[KeyValueNodeSeq]) extends NodeSeq with RecordNodes {
  def theSeq: Seq[Node] = {
    var ret: Seq[Node] = Seq.empty
    records.foreach(v => ret ++= v.theSeq)
    ret
  }
  def get(k: String): NodeSeq = {
    for (f <- records) {
      if (f.get(k) != NodeSeq.Empty) {
        return f.get(k)
      }
    }
    return NodeSeq.Empty
  }
}

private case class KeyValueNodeSeq(var key: String, var value: NodeSeq) extends NodeSeq with RecordNodes {
  def theSeq: Seq[Node] = value.toSeq
  def get(k: String): NodeSeq = {
    if (k == key) {
      return value
    } else {
      return NodeSeq.Empty
    }
  }
}


private class Generator(tree: Program) {

  val errors = ArrayBuffer[SourceMessage]()
  var buffer: StringBuffer = new StringBuffer

  def generate() {

    val globalEnv: Env = new Env(null, Map.empty, Map.empty)

    println(PrettyPrint.prettyPrint(tree))

    processDefs(tree, globalEnv)

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
      nodes = elem("html", nodes)
    }
    println("\n" + new PrettyPrinter(65, 2).formatNodes(nodes))
  }

  def processDefs(node: Program, env: Env) {
    println("ProcessDefs called")

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
      case EachStatement(idCon, exp, statement) =>
        val expressions = evalExpr(exp, env)
        expressions match {
          case ListNodeSeq(list) =>
            var ret: NodeSeq = NodeSeq.Empty;
            list.foreach(f => ret ++= evalStatement(statement, env.expand(Map.empty, Map(idCon.text -> f))))
            ret
          case RecordNodeSeq(records) =>
            var ret: NodeSeq = NodeSeq.Empty
            records.foreach{f => ret ++= evalStatement(statement, env.expand(Map.empty, Map(idCon.text -> f)))}
            ret
          case _ =>  NodeSeq.Empty
        }
      case IfStatement(p, ifStat, elseStat) =>
        if (resolvePredicateChain(if (p.rest ne null) List(p.left) ++ p.rest else List(p.left), p.op, env)) {
          return evalStatement(ifStat, env)
        } else if (elseStat ne null) {
          return evalStatement(elseStat, env)
        }
        NodeSeq.Empty
      case YieldStatement(_) =>
        env.resolveYield

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
    val desText = markup.designator.idCon.text
    val fun = env.resolveFunction(desText)
    if (fun ne null) {
      val newEnv = bindParameters(fun._2, markup, env)
      newEnv.setYield(body)
      evalStatements(fun._1, newEnv)
    } else {
      // TODO: check if is XHTML tag.
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
      case KeyValuePair(idCon, expression) =>
        KeyValueNodeSeq(idCon.text, evalExpr(expression, env))
      case ListExpression(first, rest) =>
        if (first eq null) {
          return ListNodeSeq(List.empty)
        }
        new ListNodeSeq((List(first) ++ rest) map {f => evalExpr(f, env)} toList)
      case RecordExpression(first, rest) =>
        if (first eq null) {
          return RecordNodeSeq(List.empty)
        }
        new RecordNodeSeq((List(first) ++ rest) map {
          f => (evalExpr(f.asInstanceOf[KeyValuePair], env)).asInstanceOf[KeyValueNodeSeq]} toList)
      case FieldExpression(prim, idCon) =>
        evalExpr(prim, env) match {
          case r: RecordNodes =>
            println("RecordNodes " + evalExpr(prim, env)   + " found")
            println("Value: " + r.get(idCon.text))
            return r.get(idCon.text)
        }
        NodeSeq.Empty
      case _ =>
        Text("")
    }
  }


  private def resolvePredicateChain(p: List[PrimPredicate], op: List[PredicateOp], env: Env): Boolean = {
    def resolvePredicate(p: PrimPredicate, env: Env): Boolean = {
      p match {
        case NotPredicate(prim) =>
          println("NotPredicate found")
          return !resolvePredicate(prim, env)
        case IsAPredicate(exp, null) =>
          println("IsAPredicate found with null predtype")
          val e = evalExpr(exp, env)
          return (e ne null) && (e != NodeSeq.Empty)
        case IsAPredicate(exp, t) =>
          println("IsAPredicate found with  " + t + " type")
          t match {
            case PredType("list") => exp.isInstanceOf[ListExpression]
            case PredType("record") => exp.isInstanceOf[RecordExpression]
            case PredType("string") => exp.isInstanceOf[StrCon]
            case _ => false
          }
        case _ =>      return false;

      }
    }
    if (p.size == 1) {
      return resolvePredicate(p.head, env)
    }
    op.head match {
      case PredicateOp("&&") => return resolvePredicate(p.head, env) && resolvePredicateChain(p.tail, op.tail, env)
      case PredicateOp("||") => return resolvePredicate(p.head, env) || resolvePredicateChain(p.tail, op.tail, env)
      case _ => true
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
      if (markupArgs.size < funArgs.size) throw new Exception("Wrong number of arguments(" + markupArgs.size
          + ") for function " + markup.designator.idCon.text + ". Expected: " + funArgs.size)

      val bindMap = funArgs zip markupArgs map { f => (f._1.text, f._2 match {
                  case AttrArg(_, exp) => evalExpr(exp, env)
                  case _ => evalExpr(f._2, env)})} toMap;

      return env.expand(Map.empty, collection.mutable.Map(bindMap.toSeq: _*))

  }

  private def addXHTMLAttributes(elem: Elem, markup: Markup, env: Env): NodeSeq = {
      val markupArgs: List[Argument] = getMarkupArguments(markup)

      //def concatAttrValue()
      val map: Map[String, NodeSeq] = Map.empty

      def updateMap(pair: Tuple2[String, NodeSeq]) = {
        val v = map.get(pair._1)
        if (v ne None) {
          map.put(pair._1, (v.get ++ Text(" ") ++ pair._2))
        } else {
          map.put(pair._1, pair._2)
        }
      }

      markup.designator.attribute map {
          f => val pair = f match {
              case IdAttr(id) => ("id", Text(id.text))
              case ClassAttr(cl) => ("class", Text(cl.text))
              case NameAttr(name) => ("name", Text(name.text))
              case TypeAttr(t) => ("type", Text(t.text))
              case WidthHeightAttr(w, h) => updateMap(("width", Text(w.text)))
                  ("height", Text(h.text))
              case HeightAttr(h) => ("height", Text(h.text))
          }
          updateMap(pair)
      }
      // We are interested only in the AttrArg type
      markupArgs filter { f => f.isInstanceOf[AttrArg]} map {
          f => f match { case AttrArg(idCon, exp) =>  updateMap((idCon.text, evalExpr(exp, env)))}
      }
      val attrs = map.keySet map {f => Attribute(f, map.get(f).get, Null)} toList;
      //attrs.foldLeft(elem)((e: Elem, m: MetaData) => e % m)
      attrs.foldLeft(elem)(_ % _)
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

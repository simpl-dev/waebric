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

object D {
  val enableDebug = false

  def ebug(str: String) = {
    if (enableDebug == true) {
      println(str)
    }
  }
}

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
  //def theSeq: Seq[Node] = Text(key) ++ Text("--") ++ value.toSeq
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

    D.ebug(PrettyPrint.prettyPrint(tree))
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
      D.ebug("def " + fnName + " found")
    }
    // make sure that all defs can see each other
    env.functionEnv = env
  }

  def evalStatements(statements: List[Statement], env: Env): NodeSeq =
    statements.map(evalStatement(_, env)).foldLeft(NodeSeq.Empty)(_ ++ _)

  def evalStatement(statement: Statement, env: Env): NodeSeq =
    statement match {
      case EchoStatement(echoBody, _) =>
        val ret = evalExpr(echoBody, env)
        D.ebug("EchoStatement returned: " + ret.toString)
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
            list.foreach(f => ret ++= evalStatement(statement, env.varExpand(Map(idCon.text -> f))))
            ret
          case RecordNodeSeq(records) =>
            var ret: NodeSeq = NodeSeq.Empty
            records.foreach{f => ret ++= evalStatement(statement, env.varExpand(Map(idCon.text -> f)))}
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
      case CommentStatement(_, strCon, _) =>
        new Comment(stripEdges(strCon.text))
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
    D.ebug("evalMarkup(" + markup + ", " + body + ")")
    val desText = markup.designator.idCon.text
    val fun = env.resolveFunction(desText) //(statements, argNames, function environment)
    if (fun ne null) {
      val newEnv = bindParameters((fun._2, fun._3), markup, env)
      newEnv.yieldValue = body
      evalStatements(fun._1, newEnv)
    } else {
      try {
        XHTMLTags.withName(desText toUpperCase)
        addXHTMLAttributes(elem(desText, body), markup, env)
      } catch {
        case e: java.util.NoSuchElementException =>
          throw new Exception("Function " + desText + " not defined nor it is a XHTML tag")
      }
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
      case SymbolCon(text) => Text(stripPre(text))


      /* Expression handling */
      case IdCon(text) => env.resolveVariable(text)
      case CatExpression(left, right) =>
        evalExpr(left, env) ++ evalExpr(right, env)
      case Embedding(preText, embed, textTail) =>
        evalExpr(preText, env) ++
                evalExpr(embed, env) ++
                evalExpr(textTail, env)
      case EmbedMarkup(markups) =>
        evalMarkups(markups, NodeSeq.Empty, env)
      case EmbedExp(markups, exp) =>
        evalMarkups(markups, evalExpr(exp, env), env)
      case TextTailMid(midText, embed, textTail) =>
        evalExpr(midText, env) ++
                evalExpr(embed, env) ++
                evalExpr(textTail, env)
      case KeyValuePair(idCon, expression) =>
        D.ebug("KeyValuePair found " + idCon + ", " + expression)
        KeyValueNodeSeq(idCon.text, evalExpr(expression, env))
      case ListExpression(first, rest) =>
        if (first eq null) {
          return ListNodeSeq(List.empty)
        }
        new ListNodeSeq((List(first) ++ rest) map {f => evalExpr(f, env)} toList)
      case RecordExpression(first, rest) =>
        D.ebug("RecordExpression found " + first + ", " + rest)
        if (first eq null) {
          return RecordNodeSeq(List.empty)
        }
        new RecordNodeSeq((List(first) ++ rest) map {
          f => (evalExpr(f.asInstanceOf[KeyValuePair], env)).asInstanceOf[KeyValueNodeSeq]} toList)
      case FieldExpression(prim, idCon) =>
        evalExpr(prim, env) match {
          case r: RecordNodes =>
            D.ebug("RecordNodes " + evalExpr(prim, env)   + " found")
            D.ebug("Value: " + r.get(idCon.text))
            return r.get(idCon.text)
          case _ => NodeSeq.Empty
        }
      case _ =>
        NodeSeq.Empty
    }
  }


  private def resolvePredicateChain(p: List[PrimPredicate], op: List[PredicateOp], env: Env): Boolean = {

    def checkString(e: Expression): Boolean =  {
      val n: NodeSeq = evalExpr(e, env)
      return (n.size == 1 && n.head.isInstanceOf[Text])
      //this might not be the best way to do it.
    }
    def resolvePredicate(p: PrimPredicate, env: Env): Boolean = {
      p match {
        case NotPredicate(prim) =>
          return !resolvePredicate(prim, env)
        case IsAPredicate(exp, null) =>
          val e = evalExpr(exp, env)
          return (e ne null) && (e != NodeSeq.Empty)
        case IsAPredicate(exp, t) =>
          t match {
            case PredType("list") => return evalExpr(exp, env).isInstanceOf[ListNodeSeq]
            case PredType("record") => return evalExpr(exp, env).isInstanceOf[RecordNodeSeq]
            case PredType("string") => return exp.isInstanceOf[StrCon] || checkString(exp)
          }
        case _ => return false;

      }
    }
    if (p.size == 1) {
      return resolvePredicate(p.head, env)
    }
    op.head match {
      case PredicateOp("&&") => return resolvePredicate(p.head, env) && resolvePredicateChain(p.tail, op.tail, env)
      case PredicateOp("||") => return resolvePredicate(p.head, env) || resolvePredicateChain(p.tail, op.tail, env)
      case _ => true; //should never happen
    }

  }

  private def applyAssignments(assignments: List[Assignment], env: Env): Env = {
      //let's process assignments in the order and expand the env for each assignment
    var newEnv: Env = env
    for (a <- assignments) {
      if (a.isInstanceOf[VarBinding]) {
        newEnv = newEnv.varExpand(
          Map(a.asInstanceOf[VarBinding].idCon.text -> evalExpr(a.asInstanceOf[VarBinding].expression, env)))
      } else {
        newEnv = newEnv.expand((Map(a.asInstanceOf[FuncBinding].func.text -> a.asInstanceOf[FuncBinding]), newEnv),
          Map.empty)
      }
    }
    return newEnv
      /*// variable assignments are by default scoped lexically as they are evaluated during the env expand
      val aMap = assignments filter { a => a.isInstanceOf[VarBinding] } map {
          a => (a.asInstanceOf[VarBinding].idCon.text, evalExpr(a.asInstanceOf[VarBinding].expression, env))} toMap;
      val newEnv: Env = env.expand((Map.empty, null), collection.mutable.Map(aMap.toSeq: _*))
      // lexical scoping is needed for function as it is not evaluated during the env expand
      val fMap = assignments filter { f => f.isInstanceOf[FuncBinding] } map {
          f => (f.asInstanceOf[FuncBinding].func.text, f.asInstanceOf[FuncBinding]) } toMap;
      D.ebug("aMap: " + aMap)
      D.ebug("fMap: " + fMap)
      return newEnv.expand((collection.mutable.Map(fMap.toSeq: _*), newEnv), Map.empty)
      */

  }

  private def getMarkupArguments(markup: Markup): List[Argument] = markup.markupArguments match {
        case MarkupArguments(null, rest) => List.empty
        case MarkupArguments(first, rest) => first :: rest
        case null => List.empty
      }

  //funArgs contains the env for the function
  private def bindParameters(funArgs: Tuple2[List[IdCon], Env], markup: Markup, env: Env): Env = {
      val markupArgs: List[Argument] = getMarkupArguments(markup)
      // check whether number of arguments and number of parameters match
      if (markupArgs.size < funArgs._1.size) throw new Exception("Wrong number of arguments(" + markupArgs.size
          + ") for function " + markup.designator.idCon.text + ". Expected: " + funArgs._1.size)

      val bindMap = funArgs._1 zip markupArgs map { f => (f._1.text, f._2 match {
                  case AttrArg(_, exp) => evalExpr(exp, env)
                  case _ => evalExpr(f._2, env)})} toMap;
      // Use the function environment for the expansion
      return funArgs._2.varExpand(collection.mutable.Map(bindMap.toSeq: _*))

  }

  private def addXHTMLAttributes(elem: Elem, markup: Markup, env: Env): NodeSeq = {
      val markupArgs: List[Argument] = getMarkupArguments(markup)
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
      //markupArgs filter { f => f.isInstanceOf[AttrArg]} map {
      // or not:
      markupArgs map {
          f => f match {
            case AttrArg(idCon, exp) =>  updateMap((idCon.text, evalExpr(exp, env)))
            case _ => updateMap(("value", evalExpr(f, env)))
          }
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

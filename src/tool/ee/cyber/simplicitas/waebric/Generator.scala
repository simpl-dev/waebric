package ee.cyber.simplicitas.waebric

import xml._


import ee.cyber.simplicitas.{GeneratorBase, MainBase, CommonNode,
SourceMessage, PrettyPrint}
import PrettyPrint._

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
    var errors: List[String] = List.empty

    def generate() {
        D.ebug(PrettyPrint.prettyPrint(tree))

        val globals = processDefs(processImports(tree), tree)
        val globalEnv = Env.topLevel(globals)

        var sitesFound = false
        tree.definition foreach {
            case Site(_, Mappings(mappings, _), _) =>
                sitesFound = true
                mappings.foreach(processSiteMappings(_, globalEnv))
            case _ => ()
        }
        showErrors
        if (sitesFound) {
            return
        }
        // first process sites. if not found, try with "main"
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
        showErrors
    }

    def processSiteMappings(mapping: Mapping, env: Env) {
        var out: java.io.FileWriter = null
        try {
            out = new java.io.FileWriter(mapping.path.text)
        } catch {
            case e =>
                errors :+= "Could not open the file " + mapping.path.text + " for writing."
        }
        if (out ne null) {
            out.write(new PrettyPrinter(65, 2).formatNodes(
                evalStatement(
                    MarkupStatement(mapping.markup,
                        MarkupSemi(Semicolon(";"))), env)))
            out.close()
        }
    }

    def processImports(program: Program) = {
        var ret = Map.empty[String, FunctionDef]

        program.definition foreach {
            case Import(ModuleId(ids)) =>
                val path = ids.map(_.text).mkString("", "/", ".wae")
                val f = new java.io.File(path)
                if (!f.exists)
                    errors :+= "Cannot import from " + path + ", file not found"
                else
                    ret = processDefs(ret, ImportLoader.load(path))
            case _ => ()
        }

        ret
    }

    def processDefs(oldDefs: Map[String, FunctionDef], node: Program) = {
        def checkExisting(s: String) = {
            if (oldDefs.contains(s)) {
                errors :+= "Duplicate definition " + s + " found"
            }
        }

        var defs = oldDefs
        node.definition foreach {
            case fd @ FunctionDef(IdCon(name), _, _, _) =>
                checkExisting(name)
                defs += (name -> fd)
            case _ => ()
        }
        defs
    }

    def showErrors() {
        if (!errors.isEmpty) {
            println("Found " + errors.size + " errors")
            errors foreach {e => println("ERROR: " + e)}
        }
    }

    def concatNodes(nodes: List[NodeSeq]) =
        nodes.foldLeft(NodeSeq.Empty)(_ ++ _)

    def evalStatements(statements: List[Statement], env: Env): NodeSeq =
        concatNodes(statements.map(evalStatement(_, env)))

    def evalStatement(statement: Statement, env: Env): NodeSeq =
        statement match {
            case EchoStatement(echoBody, _) =>
                evalExpr(echoBody, env)
            case MarkupStatement(markup, chain) =>
                val chainRet = evalMarkupChain(chain, env)
                evalMarkup(markup, chainRet, env)
            case LetStatement(bindings, statements, _) =>
                val newEnv = applyBindings(bindings, env)
                evalStatements(statements, newEnv)
            case BlockStatement(statements) =>
                evalStatements(statements, env)
            case EachStatement(id, exp, statement) =>
                processEachStatement(id, exp, statement, env)
            case IfStatement(Predicate(pArgs, pOp), ifStat, elseStat) =>
                if (resolvePredicateChain(pArgs, pOp, env))
                    evalStatement(ifStat, env)
                else // case where elseStat eq null is handled in evalStatement
                    evalStatement(elseStat, env)
            case YieldStatement(_) =>
                env.resolveYield
            case CommentStatement(_, strCon, _) =>
                new Comment(stripEdges(strCon.text))
            case _ =>
                NodeSeq.Empty
        }

    def processEachStatement(id: IdCon, exp: Expression,
                             statement: Statement, env: Env) = {
        def evalStmt(item: NodeSeq) =
            evalStatement(statement, env.varExpand(Map(id.text -> item)))

        def processList(lst: List[NodeSeq]) =
            concatNodes(lst.map(evalStmt))

        evalExpr(exp, env) match {
            case ListNodeSeq(list) =>
                processList(list)
            case RecordNodeSeq(records) =>
                processList(records)
            case _ =>
                NodeSeq.Empty
        }
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
            val newEnv = bindParameters(fun.args, fun.env, markup, env)
            newEnv.yieldValue = body
            evalStatements(fun.statements, newEnv)
        } else {
            try {
                XHTMLTags.withName(desText toUpperCase)
            } catch {
                case e: java.util.NoSuchElementException =>
                    errors :+= "Function " + desText + " not defined nor it is a XHTML tag"
            }
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
            case ListExpression(items) =>
                ListNodeSeq(items map(evalExpr(_, env))  toList)
            case RecordExpression(items) =>
                D.ebug("RecordExpression found " + items)
                RecordNodeSeq(
                    (for (f <- items)
                    yield evalExpr(f.asInstanceOf[KeyValuePair], env)
                                .asInstanceOf[KeyValueNodeSeq]
                            ) toList)
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

    private def applyBindings(assignments: List[Assignment], env: Env): Env = {
        //let's process assignments in the order and expand the env for each assignment
        var newEnv: Env = env
        for (a <- assignments) {
            if (a.isInstanceOf[VarBinding]) {
                newEnv = newEnv.varExpand(
                    Map(a.asInstanceOf[VarBinding].idCon.text -> evalExpr(a.asInstanceOf[VarBinding].expression, env)))
            } else {
                val funcName = a.asInstanceOf[FuncBinding].name.text
                if (newEnv.resolveFunction(funcName) ne null) {
                    errors :+= "Overdefined function " + funcName
                }
                newEnv = newEnv.expand(Map(funcName -> a.asInstanceOf[FuncBinding]),
                    Map.empty)
            }
        }
        return newEnv
        /*// variable assignments are by default scoped lexically as they are evaluated during the env expand
        val aMap = assignments filter { a => a.isInstanceOf[VarBinding] } map {
            a => (a.asInstanceOf[VarBinding].idCon.text, evalExpr(a.asInstanceOf[VarBinding].expression, env))} toMap;
        val newEnv: Env = env.expand((Map.empty, null), collection.mutable.Map(aMap.toSeq: _*))
        // lexical scoping is needed for function as it is not evaluated during the env expand
        val fMap = assignments filter { mapping => mapping.isInstanceOf[FuncBinding] } map {
            mapping => (mapping.asInstanceOf[FuncBinding].func.text, mapping.asInstanceOf[FuncBinding]) } toMap;
        D.ebug("aMap: " + aMap)
        D.ebug("fMap: " + fMap)
        return newEnv.expand((collection.mutable.Map(fMap.toSeq: _*), newEnv), Map.empty)
        */

    }

    private def getMarkupArgs(markup: Markup) =
        if (markup.args eq null)
            List.empty
        else
            markup.args

    //funArgs contains the env for the function
    private def bindParameters(funArgs: List[IdCon], funEnv: Env, markup: Markup, env: Env): Env = {
        var markupArgs: List[Argument] = getMarkupArgs(markup)
        // check whether number of arguments and number of parameters match
        if (markupArgs.size < funArgs.size) {
            errors :+= "Wrong number of arguments(" + markupArgs.size +
                    ") for function " + markup.designator.idCon.text + ". Expected: " + funArgs.size
            while (markupArgs.size < funArgs.size)
                markupArgs :+= Txt("\"undef\"")
        }

        val bindMap = funArgs zip markupArgs map { f => (f._1.text, f._2 match {
            case AttrArg(_, exp) => evalExpr(exp, env)
            case _ => evalExpr(f._2, env)})} toMap;
        // Use the function environment for the expansion
        return funEnv.varExpand(Map(bindMap.toSeq: _*))

    }

    private def addXHTMLAttributes(elem: Elem, markup: Markup, env: Env): NodeSeq = {
        val map = collection.mutable.Map.empty[String, NodeSeq]

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
        //markupArgs filter { mapping => mapping.isInstanceOf[AttrArg]} map {
        // or not:
        getMarkupArgs(markup) map {
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
}

object ImportLoader extends MainBase {
    def load(path: String): Program = {
        val grammar = new WaebricSimplGrammar
        grammar.parseFile(path)
        checkErrors(grammar.errors)
        grammar.tree
    }
}

object WaebricGenerator extends MainBase {
    def main(arg: Array[String]) {
        parseOptions(arg)
        val grammar = new WaebricSimplGrammar
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            val gen = new Generator(grammar.tree);
            gen.generate()
        }
    }
}

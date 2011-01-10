package ee.cyber.waebric.lexer

import ee.cyber.simplicitas.{TerminalNode, CommonNode}

case class CatExpression(left: Expression, right: Expression) extends Expression {
    def childrenNames = Array("left", "right")
}

case class AndOrPredicate(left: Predicate, right: Predicate, op: String) extends Predicate {
	def childrenNames = Array("left", "right", "op")
}

case class NotPredicate(predicate: Predicate) extends Predicate {
	def childrenNames = Array("predicate")
}

case class TypeCheckPredicate(exp: Expression, t: Types) extends Predicate {
	def childrenNames = Array("exp", "t")
}

case class FieldExpression(left: Expression, field: IdCon) extends Expression {
	def childrenNames = Array("left", "field")
}


object HelperMethods {
    
    def makeCat(args: List[Expression]) = {
        def loop(left: Expression, right: List[Expression]): Expression =
            (right) match {
                case (rh :: rt) =>
                    loop(CatExpression(left, rh).setStart(left).setEnd(rh), rt)
                case (Nil) =>
                    left
            }

        loop(args.head, args.tail)
    }
    
    def getAndOrPredicate(op: String, args: List[Predicate]) = {
    	def loop(left: Predicate, right: List[Predicate], op: String): Predicate =
            (right) match {
                case (rh :: rt) =>
                    loop(AndOrPredicate(left, rh, op).setStart(left).setEnd(rh), rt, op)
                case (Nil) =>
                    left
            }

        loop(args.head, args.tail, op)
    }
}
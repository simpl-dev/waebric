package ee.cyber.waebric.lexer

import ee.cyber.simplicitas.{TerminalNode, CommonNode}

/*
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
*/

object HelperMethods {
    def listEmpty(arg: List[CommonNode]): Boolean = {
      arg != null && arg.size > 0;
    }

  def makeArgumentList(first: Argument, rest: List[Argument]): ArgumentRest = {
    new ArgumentRest(rest)
  }

}

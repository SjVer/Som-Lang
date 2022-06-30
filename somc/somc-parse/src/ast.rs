use somc_lex::token::Token;

// ================= Expr =================

#[derive(Clone, Debug)]
pub struct ExprNode {
	pub token: Token,
	pub item: ExprItem,
}

#[derive(Clone, Debug)]
pub enum ExprItem {
	Equality	{ lhs: Box<ExprNode>, rhs: Box<ExprNode> },
	Term 		{ lhs: Box<ExprNode>, rhs: Box<ExprNode> },
	Factor 		{ lhs: Box<ExprNode>, rhs: Box<ExprNode> },
	Unary		(Box<ExprNode>),
	Power 		{ base: Box<ExprNode>, power: Box<ExprNode> },
	Grouping	(Box<ExprNode>),
	Variable	{path: String, expr: Box<ExprNode>},
	Literal		(Literal),
}

#[derive(Clone, Debug)]
pub enum Literal {
	Integer(u64),
	Float(f64),
}

pub trait ExprVisitor<T> {
	fn visit(&mut self, node: &ExprNode) -> T {
		match &node.item {
			ExprItem::Equality	{ lhs, rhs } 	=> self.visit_equality(node, lhs.as_ref(), rhs.as_ref()),
			ExprItem::Term		{ lhs, rhs } 	=> self.visit_term(node, lhs.as_ref(), rhs.as_ref()),
			ExprItem::Factor	{ lhs, rhs } 	=> self.visit_factor(node, lhs.as_ref(), rhs.as_ref()),
			ExprItem::Unary		( expr ) 	 	=> self.visit_unary(node, expr.as_ref()),
			ExprItem::Power		{ base, power} 	=> self.visit_power(node, base.as_ref(), power.as_ref()),
			ExprItem::Grouping	( expr ) 		=> self.visit_grouping(node, expr.as_ref()),
			ExprItem::Variable	{ path, expr } 	=> self.visit_variable(node, &path, expr.as_ref()),
			ExprItem::Literal	( literal ) 	=> self.visit_literal(node, &literal),
		}
	}
	
	fn visit_equality(&mut self, node: &ExprNode, lhs: &ExprNode, rhs: &ExprNode) -> T;
	fn visit_term(&mut self, node: &ExprNode, lhs: &ExprNode, rhs: &ExprNode) -> T;
	fn visit_factor(&mut self, node: &ExprNode, lhs: &ExprNode, rhs: &ExprNode) -> T;
	fn visit_unary(&mut self, node: &ExprNode, expr: &ExprNode) -> T;
	fn visit_power(&mut self, node: &ExprNode, base: &ExprNode, power: &ExprNode) -> T;
	fn visit_grouping(&mut self, node: &ExprNode, expr: &ExprNode) -> T;
	fn visit_variable(&mut self, node: &ExprNode, path: &String, expr: &ExprNode) -> T;
	fn visit_literal(&mut self, node: &ExprNode, literal: &Literal) -> T;
}
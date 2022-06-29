use somc_lex::token::Token;

// ================ Theory ================

#[derive(Clone, Debug)]
pub struct TheoryNode {
	pub token: Token,
	pub item: TheoryItem
}

#[derive(Clone, Debug)]
pub enum TheoryItem {
	Logical     { lhs: Box<TheoryNode>, rhs: Box<TheoryNode> },
	Unary     	(Box<TheoryNode>),
	Implies     { lhs: Box<TheoryNode>, rhs: Box<TheoryNode> },
	Comparison  { lhs: Box<TheoryNode>, rhs: Box<TheoryNode> },
	Divisible	{ expr: Box<TheoryNode>, divisor: Box<TheoryNode> },
	Exists  	(Box<TheoryNode>),
	Grouping  	(Box<TheoryNode>),
	Expression 	(ExprNode),
}

pub trait TheoryVisitor<T> {
	fn visit(&mut self, node: &TheoryNode) -> T {
		match &node.item {
			TheoryItem::Logical		{ lhs, rhs } 	=> self.visit_logical(node, lhs.as_ref(), rhs.as_ref()),
			TheoryItem::Unary		( expr ) 		=> self.visit_unary(node, expr.as_ref()),
			TheoryItem::Implies		{ lhs, rhs } 	=> self.visit_implies(node, lhs.as_ref(), rhs.as_ref()),
			TheoryItem::Comparison	{ lhs, rhs } 	=> self.visit_comparison(node, lhs.as_ref(), rhs.as_ref()),
			TheoryItem::Divisible	{ expr, divisor } => self.visit_divisible(node, expr.as_ref(), divisor.as_ref()),
			TheoryItem::Exists		( expr ) 		=> self.visit_exists(node, expr.as_ref()),
			TheoryItem::Grouping	( expr ) 		=> self.visit_grouping(node, expr.as_ref()),
			TheoryItem::Expression	( expr ) 		=> self.visit_expression(node, &expr),
		}
	}
	
	fn visit_logical(&mut self, node: &TheoryNode, lhs: &TheoryNode, rhs: &TheoryNode) -> T;
	fn visit_unary(&mut self, node: &TheoryNode, expr: &TheoryNode) -> T;
	fn visit_implies(&mut self, node: &TheoryNode, lhs: &TheoryNode, rhs: &TheoryNode) -> T;
	fn visit_comparison(&mut self, node: &TheoryNode, lhs: &TheoryNode, rhs: &TheoryNode) -> T;
	fn visit_divisible(&mut self, node: &TheoryNode, expr: &TheoryNode, divisor: &TheoryNode) -> T;
	fn visit_exists(&mut self, node: &TheoryNode, expr: &TheoryNode) -> T;
	fn visit_grouping(&mut self, node: &TheoryNode, expr: &TheoryNode) -> T;
	fn visit_expression(&mut self, node: &TheoryNode, expr: &ExprNode) -> T;
}

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
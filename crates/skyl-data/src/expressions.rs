use std::{
    fmt::{self, Display},
    rc::Rc,
};

use crate::{Span, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Token, Span),
    Unary(Token, Rc<Expression>, Span),
    PostFix(Token, Rc<Expression>, Span),
    Arithmetic(Rc<Expression>, Token, Rc<Expression>, Span),
    Logical(Rc<Expression>, Token, Rc<Expression>, Span),
    Ternary(Rc<Expression>, Rc<Expression>, Rc<Expression>, Span),
    Assign(Token, Rc<Expression>, Span),
    Lambda,
    Get(Rc<Expression>, Token, Span),
    Variable(Token, Span),
    Set(Rc<Expression>, Token, Rc<Expression>, Span),
    Call(Rc<Expression>, Token, Vec<Expression>, Span),
    Tuple(Vec<Rc<Expression>>, Span),
    List(Vec<Rc<Expression>>, Span),
    TypeComposition(Vec<Token>, Span),
    Attribute(Token, Vec<Rc<Expression>>, Span),
    Group(Rc<Expression>, Span),
    Void,
    ListGet(Box<Expression>, Box<Expression>, Span),
    ListSet(Box<Expression>, Box<Expression>, Rc<Expression>, Span),
}

impl Expression {
    pub fn line(&self) -> usize {
        match self {
            Expression::Literal(token, _) => token.line,
            Expression::Unary(token, _, _) => token.line,
            Expression::PostFix(token, _, _) => token.line,
            Expression::Arithmetic(left, _, _, _) => left.line(),
            Expression::Logical(left, _, _, _) => left.line(),
            Expression::Ternary(cond, _, _, _) => cond.line(),
            Expression::Assign(token, _, _) => token.line,
            Expression::Lambda => panic!("Lambda expression has no line information"),
            Expression::Get(expr, _, _) => expr.line(),
            Expression::Variable(token, _) => token.line,
            Expression::Set(expr, _, _, _) => expr.line(),
            Expression::Call(expr, _, _, _) => expr.line(),
            Expression::Tuple(values, _) => values
                .first()
                .expect("Empty tuple expression has no elements")
                .line(),
            Expression::List(values, _) => values
                .first()
                .expect("Empty list expression has no elements")
                .line(),
            Expression::TypeComposition(tokens, _) => {
                tokens
                    .first()
                    .expect("Empty type composition has no tokens")
                    .line
            }
            Expression::Attribute(token, _, _) => token.line,
            Expression::Group(expr, _) => expr.line(),
            Expression::Void => 0,
            Expression::ListGet(expr, _, _) => expr.line(),
            Expression::ListSet(expr, _, _, _) => expr.line(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(_, span)
            | Expression::Unary(_, _, span)
            | Expression::PostFix(_, _, span)
            | Expression::Arithmetic(_, _, _, span)
            | Expression::Logical(_, _, _, span)
            | Expression::Ternary(_, _, _, span)
            | Expression::Assign(_, _, span)
            | Expression::Get(_, _, span)
            | Expression::Variable(_, span)
            | Expression::Set(_, _, _, span)
            | Expression::Call(_, _, _, span)
            | Expression::Tuple(_, span)
            | Expression::List(_, span)
            | Expression::TypeComposition(_, span)
            | Expression::Attribute(_, _, span)
            | Expression::Group(_, span)
            | Expression::ListGet(_, _, span)
            | Expression::ListSet(_, _, _, span) => span.clone(),

            Expression::Lambda => {
                unimplemented!("")
            }
            Expression::Void => Span { end: 0, start: 1 },
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::ListSet(list, index, value, _) => {
                write!(f, "ListSet({}[{}] = {})", list, index, value)
            }
            Expression::Literal(token, _) => write!(f, "{}", token),
            Expression::Unary(op, expr, _) => write!(f, "({} {})", op, expr),
            Expression::PostFix(op, var, _) => write!(f, "({} {})", op.lexeme, var),
            Expression::Arithmetic(left, op, right, _) => write!(f, "({} {} {})", left, op, right),
            Expression::Logical(left, op, right, _) => write!(f, "({} {} {})", left, op, right),
            Expression::Ternary(cond, then_expr, else_expr, _) => {
                write!(f, "Ternary({} ? {} : {})", cond, then_expr, else_expr)
            }
            Expression::Assign(var, expr, _) => write!(f, "({} = {})", var, expr),
            Expression::Lambda => write!(f, "(lambda)"),
            Expression::Get(object, field, _) => write!(f, "Get({}.{})", object, field),
            Expression::ListGet(expression, index, _) => {
                write!(f, "ListGet({}.{})", expression, index)
            }
            Expression::Set(object, field, value, _) => {
                write!(f, "Set({}.{} = {})", object, field, value)
            }
            Expression::Variable(name, _) => write!(f, "Variable({})", name),
            Expression::Call(callee, _, args, _) => write!(f, "Call({:?}, {:?})", callee, args),
            Expression::Tuple(values, _) => write!(f, "Tuple({:?})", values),
            Expression::List(values, _) => write!(f, "List({:?})", values),
            Expression::Group(expr, _) => write!(f, "Group({:?})", expr),
            Expression::Attribute(name, args, _) => write!(f, "Attribute({:?}, {:?})", name, args),
            Expression::Void => write!(f, "Void()"),
            Expression::TypeComposition(_, _) => write!(f, "TypeComposition"),
        }
    }
}

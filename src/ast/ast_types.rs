//! This module defines the AST types and simple methods on those types.

use crate::instructions::Comparison;
use crate::token::Span;

/* AST Definitions */

/// Represents an abstract syntax tree for a single file.
///
/// The AST owns all of the data of all of the nodes underneath it. This top level
/// node is really just a list of [`DeclarationASTs`](DeclarationAST), which are
/// the declarations in that file (at time of writing, should be all functions).
///
/// As with the other AST types, this node has an [`ASTNodeData`] field.
#[derive(Debug)]
pub struct AST {
    /// The declarations in the file.
    pub declarations: Vec<DeclarationAST>,
    /// Metadata, which is stored in every AST type.
    pub node_data: ASTNodeData,
}

/// Metadata associated with each and every AST node (all AST node types).
///
/// Instead of repeating ourselves across every enum variant, we place a single
/// [`ASTNodeData`] in each and allow that to hold common information. With a constructor,
/// it is easy to initialize this struct without getting too distracted from the
/// main logic.
///
/// Actually, much of the data associated with a node is only associated with the
/// `id` field.
#[derive(Debug)]
pub struct ASTNodeData {
    /// A unique id. The only gaurantee is that these are unique, there is no promise
    /// that a given number is somewhere in the tree. Useful for associating additional
    /// information (like type info) outside of the tree, which is slightly better
    /// for borrowing / const correctness purposes.
    pub id: u32,
    /// A [`Span`] representing the part of the file that corresponds to this language
    /// construct. Used in error messages.
    pub span: Span,
}

impl ASTNodeData {
    /// Initializes an [`ASTNodeData`].
    ///
    /// The span is provided as an argument. The id is generated such that it will
    /// never be used again (unless perhaps we overflow, which is extremely unlikely).
    pub fn new(span: Span) -> ASTNodeData {
        ASTNodeData { id: crate::util::next_id(), span }
    }

    /// Clones the [`ASTNodeData`], but gives it a new id.
    ///
    /// Notice that [`ASTNodeData`] lacks a Clone implentation. This is the analog.
    /// While clone would imply that the type is fully copied (more or less), this
    /// function is not a true copy. All fields are cloned, except for `id`, which
    /// receives a new id. This way, the cloned subtree can be added to the AST
    /// without causing issues involving, say, the type index.
    pub fn relabel(&self) -> ASTNodeData {
        ASTNodeData { id: crate::util::next_id(), span: self.span.clone() }
    }
}

/// Represents a declaration in the Nom program.
///
/// This AST type handles declarations, which include functions, variables, and
/// types.
///
/// Note that this type is explicitely not [`Clone`]. Instead, you should use
/// [`duplicate()`](DeclarationAST::duplicate), which acts like clone except the
/// [`ASTNodeData`] is provided a new id.
#[derive(Debug)]
pub enum DeclarationAST {
    /// A function declaration in the Nom program. Note that function calls are
    /// different.
    Function {
        /// The name of the function.
        name: String,
        /// A list of parameters, each represented by a (name, type) pair.
        params: Vec<(String, TypeAST)>,
        /// The code of the function. This [`ExprAST`] should always be a block.
        block: ExprAST,
        /// The type the function returns
        return_type: TypeAST,
        /// Metadata.
        node_data: ASTNodeData,
    },
    /// A variable declaration within a function. Global variables are not yet
    /// supported.
    Variable {
        /// Represents whether or not the variable is mutable or not.
        mutability: Mutability,
        /// The name of the variables.
        name: String,
        /// The expression that defines the variable. It is not permitted to have
        /// a variable without an initial value.
        expr: ExprAST,
        /// The type associated with the variable. This is optional, so that someday
        /// we might allow type inference to determine the type (not yet implemented).
        type_ascription: Option<TypeAST>,
        /// Metadata.
        node_data: ASTNodeData,
    },
    /// A type declaration for a struct.
    Struct {
        /// The name of the struct.
        name: String,
        /// Members of the struct. The string is the name and the type is the member's type.
        members: Vec<(String, TypeAST)>,
        /// Metadata.
        node_data: ASTNodeData,
    },
}

impl DeclarationAST {
    /// Creates an identical copy, except for the `node_data` which is intended to be unique.
    ///
    /// The `node_data` field recieves a value which is gauranteed to be completely new.
    pub fn duplicate(&self) -> DeclarationAST {
        match self {
            DeclarationAST::Function { name, params, block, return_type, node_data } => DeclarationAST::Function {
                name: name.clone(),
                params: params.iter().map(|(name, type_ast)| (name.clone(), type_ast.duplicate())).collect(),
                block: block.duplicate(),
                return_type: return_type.duplicate(),
                node_data: node_data.relabel(),
            },
            DeclarationAST::Variable { mutability, name, expr, type_ascription, node_data } => {
                DeclarationAST::Variable {
                    mutability: *mutability,
                    name: name.clone(),
                    expr: expr.duplicate(),
                    type_ascription: type_ascription.as_ref().map(TypeAST::duplicate),
                    node_data: node_data.relabel(),
                }
            }
            DeclarationAST::Struct { name, members, node_data } => DeclarationAST::Struct {
                name: name.clone(),
                members: members.iter().map(|(name, type_ast)| (name.clone(), type_ast.duplicate())).collect(),
                node_data: node_data.relabel(),
            },
        }
    }

    /// Retrieves the node data, regardless of which enum variant is present.
    pub fn get_node_data(&self) -> &ASTNodeData {
        match self {
            DeclarationAST::Function { node_data, .. }
            | DeclarationAST::Variable { node_data, .. }
            | DeclarationAST::Struct { node_data, .. } => node_data,
        }
    }
}

/// Represents a statements in the Nom program.
///
/// Assignments, including compound assignments, are the main form of statements.
/// Declarations (which do require assignment), are another one. Finally, it is
/// permitted for the statement to simply evaluate an expression.
///
/// Almost all constructs in Nom are expressions, so there are few enum variants
/// here.
#[derive(Debug)]
pub enum StatementAST {
    /// A expression executed for its side effects.
    ExpressionStatement(ExprAST, ASTNodeData),
    /// Represents an assignment of the left [`ExprAST`] from the value generated
    /// from the right [`ExprAST`]. The left expression has some restrictions to
    /// ensure it is actually assignable (a so called lvalue).
    Assignment(ExprAST, ExprAST, ASTNodeData),
    /// Represents a compound assignment. The left [`ExprAST`] is the assignee, and
    /// the right one is the value. Eventually desugared, so that `a += b` becomes
    /// `a = a + b`.
    CompoundAssignment(ExprAST, ExprAST, MathOperation, ASTNodeData),
    /// A declaration. Currently, only variable declarations are permitted within
    /// functions.
    Declaration(DeclarationAST, ASTNodeData), // Any declaration will be allowed, but for now only variable declarations work.
}

impl StatementAST {
    /// Creates an identical copy, except for the `node_data` which is intended to be unique.
    ///
    /// The `node_data` field recieves a value which is gauranteed to be completely new.
    pub fn duplicate(&self) -> StatementAST {
        match self {
            StatementAST::ExpressionStatement(expr, node_data) => {
                StatementAST::ExpressionStatement(expr.duplicate(), node_data.relabel())
            }
            StatementAST::Assignment(left, right, node_data) => {
                StatementAST::Assignment(left.duplicate(), right.duplicate(), node_data.relabel())
            }
            StatementAST::CompoundAssignment(left, right, op, node_data) => {
                StatementAST::CompoundAssignment(left.duplicate(), right.duplicate(), *op, node_data.relabel())
            }
            StatementAST::Declaration(decl, node_data) => {
                StatementAST::Declaration(decl.duplicate(), node_data.relabel())
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeAST {
    NamedType(String, ASTNodeData),
    Pointer(Box<TypeAST>, ASTNodeData),
}

impl TypeAST {
    /// Creates an identical copy, except for the `node_data` which will have a new unique identifier.
    pub fn duplicate(&self) -> TypeAST {
        match self {
            TypeAST::NamedType(name, node_data) => TypeAST::NamedType(name.clone(), node_data.relabel()),
            TypeAST::Pointer(type_ast, node_data) => {
                TypeAST::Pointer(Box::new(type_ast.duplicate()), node_data.relabel())
            }
        }
    }

    pub fn get_node_data(&self) -> &ASTNodeData {
        match self {
            TypeAST::NamedType(_, ast_node_data) | TypeAST::Pointer(_, ast_node_data) => ast_node_data,
        }
    }
}

#[derive(Debug, Default)]
pub enum ExprAST {
    Add(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Subtract(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Multiply(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Divide(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Modulus(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Comparison(Box<ExprAST>, Box<ExprAST>, Comparison, ASTNodeData),
    Or(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    And(Box<ExprAST>, Box<ExprAST>, ASTNodeData),
    Not(Box<ExprAST>, ASTNodeData),
    MemberAccess(Box<ExprAST>, String, ASTNodeData),
    // i128 can fit all of our literals, up to u64 and i64. Whether a literal fits in a specific type is decided later.
    IntegerLiteral(i128, ASTNodeData),
    BooleanLiteral(bool, ASTNodeData),
    Variable(String, ASTNodeData),
    // The vec contains the arguments.
    FunctionCall(String, Vec<ExprAST>, ASTNodeData),
    Block(Vec<StatementAST>, Option<Box<ExprAST>>, ASTNodeData),
    If {
        condition: Box<ExprAST>,
        block: Box<ExprAST>,
        else_branch: Option<Box<ExprAST>>,
        data: ASTNodeData,
    },
    While {
        condition: Box<ExprAST>,
        block: Box<ExprAST>,
        data: ASTNodeData,
    },
    Return(Option<Box<ExprAST>>, ASTNodeData),
    StructExpression {
        name: String,
        members: Vec<(String, ExprAST)>,
        data: ASTNodeData,
    },
    Free {
        subexpr: Box<ExprAST>,
        data: ASTNodeData,
    },
    // This is a hack that allows us to remove an AST, operate on it, and put it back. (Blame the borrow checker for this.)
    // (See std::mem::take usage for why we need this...)
    #[default]
    Moved,
}

impl ExprAST {
    pub fn get_node_data(&self) -> &ASTNodeData {
        match self {
            ExprAST::Add(_, _, data)
            | ExprAST::Subtract(_, _, data)
            | ExprAST::Multiply(_, _, data)
            | ExprAST::Divide(_, _, data)
            | ExprAST::Modulus(_, _, data)
            | ExprAST::Comparison(_, _, _, data)
            | ExprAST::Or(_, _, data)
            | ExprAST::And(_, _, data)
            | ExprAST::Not(_, data)
            | ExprAST::IntegerLiteral(_, data)
            | ExprAST::BooleanLiteral(_, data)
            | ExprAST::Variable(_, data)
            | ExprAST::FunctionCall(_, _, data)
            | ExprAST::Block(_, _, data)
            | ExprAST::If { data, .. }
            | ExprAST::While { data, .. }
            | ExprAST::Return(_, data)
            | ExprAST::MemberAccess(_, _, data)
            | ExprAST::StructExpression { data, .. }
            | ExprAST::Free { data, .. } => data,
            ExprAST::Moved => panic!("ExprAST was moved"),
        }
    }

    // Creates an identical copy, except for the node_data which is intended to be unique.
    pub fn duplicate(&self) -> ExprAST {
        match self {
            ExprAST::Add(left, right, node_data) => {
                ExprAST::Add(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::Subtract(left, right, node_data) => {
                ExprAST::Subtract(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::Multiply(left, right, node_data) => {
                ExprAST::Multiply(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::Divide(left, right, node_data) => {
                ExprAST::Divide(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::Modulus(left, right, node_data) => {
                ExprAST::Modulus(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::Comparison(left, right, comp, node_data) => {
                ExprAST::Comparison(Box::new(left.duplicate()), Box::new(right.duplicate()), *comp, node_data.relabel())
            }
            ExprAST::Or(left, right, node_data) => {
                ExprAST::Or(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::And(left, right, node_data) => {
                ExprAST::And(Box::new(left.duplicate()), Box::new(right.duplicate()), node_data.relabel())
            }
            ExprAST::MemberAccess(expr, name, node_data) => {
                ExprAST::MemberAccess(Box::new(expr.duplicate()), name.clone(), node_data.relabel())
            }
            ExprAST::Not(inner, node_data) => ExprAST::Not(Box::new(inner.duplicate()), node_data.relabel()),
            ExprAST::IntegerLiteral(num, node_data) => ExprAST::IntegerLiteral(*num, node_data.relabel()),
            ExprAST::BooleanLiteral(bool, node_data) => ExprAST::BooleanLiteral(*bool, node_data.relabel()),
            ExprAST::Variable(name, node_data) => ExprAST::Variable(name.clone(), node_data.relabel()),
            ExprAST::FunctionCall(name, exprs, node_data) => {
                ExprAST::FunctionCall(name.clone(), exprs.iter().map(ExprAST::duplicate).collect(), node_data.relabel())
            }
            ExprAST::Block(statements, final_expr, node_data) => ExprAST::Block(
                statements.iter().map(StatementAST::duplicate).collect(),
                final_expr.as_ref().map(|expr| Box::new(ExprAST::duplicate(expr.as_ref()))),
                node_data.relabel(),
            ),
            ExprAST::If { condition, block, else_branch, data } => ExprAST::If {
                condition: Box::new(condition.as_ref().duplicate()),
                block: Box::new(block.as_ref().duplicate()),
                else_branch: else_branch.as_ref().map(|expr| Box::new(ExprAST::duplicate(expr.as_ref()))),
                data: data.relabel(),
            },
            ExprAST::While { condition, block, data } => ExprAST::While {
                condition: Box::new(condition.as_ref().duplicate()),
                block: Box::new(block.as_ref().duplicate()),
                data: data.relabel(),
            },
            ExprAST::Return(expr, node_data) => {
                if let Some(expr) = expr {
                    ExprAST::Return(Some(Box::new(expr.duplicate())), node_data.relabel())
                } else {
                    ExprAST::Return(None, node_data.relabel())
                }
            }
            ExprAST::StructExpression { name, members, data } => ExprAST::StructExpression {
                name: name.clone(),
                members: members
                    .iter()
                    .map(|(field_name, field_expr)| (field_name.clone(), field_expr.duplicate()))
                    .collect(),
                data: data.relabel(),
            },
            ExprAST::Free { subexpr, data } => {
                ExprAST::Free { subexpr: Box::new(subexpr.duplicate()), data: data.relabel() }
            }
            ExprAST::Moved => panic!("ExprAST moved"),
        }
    }
}

/* Visitor Pattern */

#[derive(Debug)]
pub enum AnyAST<'a> {
    File(&'a mut AST),
    Declaration(&'a mut DeclarationAST),
    Statement(&'a mut StatementAST),
    Expression(&'a mut ExprAST),
}

impl<'a> AnyAST<'a> {
    /// This function permits recursion over the tree without inspecting the structure.
    /// Most methods that want to crawl the whole tree will use this, check to see if
    /// it is at a specific type of node, then recurse in both cases. I anticipate this
    /// being useful for applying optimizations / desugaring stages.
    #[allow(clippy::match_same_arms)] // I prefer my ordering
    pub fn children(&'a mut self) -> Vec<AnyAST<'a>> {
        use AnyAST as A;
        use DeclarationAST as D;
        use ExprAST as E;
        use StatementAST as S;

        // TODO: do we need to think about types? Perhaps yes if we get something like `Array<T, u64>`, though we could
        // perhaps skip this for a while if we demand that the u64 is a literal (not even an expression like 2 * 8).

        match self {
            A::File(AST { declarations, .. }) => declarations.iter_mut().map(A::Declaration).collect(),
            A::Declaration(D::Function { block: ref mut expr, .. } | D::Variable { ref mut expr, .. }) => {
                vec![A::Expression(expr)]
            }
            A::Declaration(D::Struct { .. }) => vec![],
            A::Statement(
                S::Assignment(ref mut expr_1, ref mut expr_2, ..)
                | S::CompoundAssignment(ref mut expr_1, ref mut expr_2, ..),
            ) => vec![A::Expression(expr_1), A::Expression(expr_2)],
            A::Statement(S::Declaration(ref mut dec, ..)) => vec![A::Declaration(dec)],
            A::Statement(S::ExpressionStatement(ref mut expr, ..)) => vec![A::Expression(expr)],
            A::Expression(E::IntegerLiteral(..) | E::BooleanLiteral(..) | E::Variable(..) | E::Return(None, ..)) => {
                vec![]
            }
            A::Expression(E::Not(expr, ..) | E::Return(Some(expr), ..) | E::MemberAccess(expr, ..)) => {
                vec![A::Expression(expr.as_mut())]
            }
            A::Expression(
                E::Add(expr_1, expr_2, ..)
                | E::Subtract(expr_1, expr_2, ..)
                | E::Multiply(expr_1, expr_2, ..)
                | E::Divide(expr_1, expr_2, ..)
                | E::Modulus(expr_1, expr_2, ..)
                | E::Comparison(expr_1, expr_2, ..)
                | E::Or(expr_1, expr_2, ..)
                | E::And(expr_1, expr_2, ..)
                | E::If { condition: expr_1, block: expr_2, else_branch: None, .. }
                | E::While { condition: expr_1, block: expr_2, .. },
            ) => vec![A::Expression(expr_1.as_mut()), A::Expression(expr_2.as_mut())],
            A::Expression(E::If { condition: expr_1, block: expr_2, else_branch: Some(expr_3), .. }) => {
                vec![A::Expression(expr_1.as_mut()), A::Expression(expr_2.as_mut()), A::Expression(expr_3.as_mut())]
            }
            A::Expression(E::FunctionCall(_, exprs, _)) => exprs.iter_mut().map(A::Expression).collect(),
            A::Expression(E::Block(stmts, maybe_expr, ..)) => {
                let mut vec: Vec<_> = stmts.iter_mut().map(A::Statement).collect();

                if let Some(expr) = maybe_expr {
                    vec.push(A::Expression(expr.as_mut()));
                }

                vec
            }
            A::Expression(E::StructExpression { members, .. }) => {
                members.iter_mut().map(|(_, field_expr)| A::Expression(field_expr)).collect()
            }
            A::Expression(E::Free { subexpr, .. }) => {
                vec![A::Expression(subexpr)]
            }
            A::Expression(E::Moved) => panic!("Expected Unmoved Value"),
        }
    }

    #[allow(unused)]
    pub fn get_node_data(&self) -> &ASTNodeData {
        use AnyAST as A;
        use DeclarationAST as D;
        use ExprAST as E;
        use StatementAST as S;

        match self {
            A::File(AST { node_data, .. })
            | A::Declaration(
                D::Function { node_data, .. } | D::Variable { node_data, .. } | D::Struct { node_data, .. },
            )
            | A::Statement(
                S::Assignment(_, _, node_data)
                | S::CompoundAssignment(_, _, _, node_data)
                | S::Declaration(_, node_data)
                | S::ExpressionStatement(_, node_data),
            ) => node_data,

            A::Expression(E::Moved) => panic!("Expected Unmoved Value"),

            A::Expression(e) => e.get_node_data(),
        }
    }
}

/* Related Types */

#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Var,
    Val,
}

#[derive(Debug, Clone, Copy)]
pub enum MathOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
}

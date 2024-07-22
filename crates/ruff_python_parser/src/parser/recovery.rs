use ruff_allocator::{Allocator, CloneIn, CollectIn};
use ruff_python_ast::{self as ast, Expr, ExprContext, Pattern};
use ruff_text_size::{Ranged, TextLen, TextRange};

/// Convert the given [`Pattern`] to an [`Expr`].
///
/// This is used to convert an invalid use of pattern to their equivalent expression
/// to preserve the structure of the pattern.
///
/// The conversion is done as follows:
/// - `PatternMatchSingleton`: Boolean and None literals
/// - `PatternMatchValue`: The value itself
/// - `PatternMatchSequence`: List literal
/// - `PatternMatchMapping`: Dictionary literal
/// - `PatternMatchClass`: Call expression
/// - `PatternMatchStar`: Starred expression
/// - `PatternMatchAs`: The pattern itself or the name
/// - `PatternMatchOr`: Binary expression with `|` operator
///
/// Note that the sequence pattern is always converted to a list literal even
/// if it was surrounded by parentheses.
///
/// # Note
///
/// This function returns an invalid [`ast::ExprName`] if the given pattern is a [`Pattern::MatchAs`]
/// with both the pattern and name present. This is because it cannot be converted to an expression
/// without dropping one of them as there's no way to represent `x as y` as a valid expression.
pub(super) fn pattern_to_expr<'ast>(
    pattern: &Pattern<'ast>,
    allocator: &'ast Allocator,
) -> Expr<'ast> {
    match pattern {
        Pattern::MatchSingleton(ast::PatternMatchSingleton { range, value }) => match value {
            ast::Singleton::True => Expr::BooleanLiteral(ast::ExprBooleanLiteral {
                value: true,
                range: *range,
            }),
            ast::Singleton::False => Expr::BooleanLiteral(ast::ExprBooleanLiteral {
                value: false,
                range: *range,
            }),
            ast::Singleton::None => Expr::NoneLiteral(ast::ExprNoneLiteral { range: *range }),
        },
        Pattern::MatchValue(ast::PatternMatchValue { value, .. }) => (*value).clone_in(allocator),
        // We don't know which kind of sequence this is: `case [1, 2]:` or `case (1, 2):`.
        Pattern::MatchSequence(ast::PatternMatchSequence { range, patterns }) => {
            let mut elements = ruff_allocator::Vec::with_capacity_in(patterns.len(), allocator);
            elements.extend(
                (**patterns)
                    .iter()
                    .map(|pattern| pattern_to_expr(pattern, allocator)),
            );

            Expr::List(ast::ExprList {
                elts: elements,
                ctx: ExprContext::Store,
                range: *range,
            })
        }
        Pattern::MatchMapping(ast::PatternMatchMapping {
            range,
            keys,
            patterns,
            rest,
        }) => {
            let mut items =
                ruff_allocator::Vec::with_capacity_in(keys.len() + patterns.len(), allocator);
            items.extend(
                (**keys)
                    .iter()
                    .zip(patterns.iter())
                    .map(|(key, pattern)| ast::DictItem {
                        key: Some(key.clone_in(allocator)),
                        value: pattern_to_expr(pattern, allocator),
                    }),
            );
            if let Some(rest) = rest {
                let value = Expr::Name(ast::ExprName {
                    range: rest.range,
                    id: rest.id,
                    ctx: ExprContext::Store,
                });
                items.push(ast::DictItem { key: None, value });
            }
            Expr::Dict(ast::ExprDict {
                range: *range,
                items,
            })
        }
        Pattern::MatchClass(ast::PatternMatchClass {
            range,
            cls,
            arguments,
        }) => Expr::Call(ast::ExprCall {
            range: *range,
            func: allocator.alloc((*cls).clone_in(allocator)),
            arguments: ast::Arguments {
                range: arguments.range,
                args: arguments
                    .patterns
                    .iter()
                    .map(|pattern| pattern_to_expr(pattern, allocator))
                    .collect_in(allocator),
                keywords: arguments
                    .keywords
                    .iter()
                    .map(|keyword_pattern| ast::Keyword {
                        range: keyword_pattern.range,
                        arg: Some(keyword_pattern.attr.clone_in(allocator)),
                        value: pattern_to_expr(&keyword_pattern.pattern, allocator),
                    })
                    .collect_in(allocator),
            },
        }),
        Pattern::MatchStar(ast::PatternMatchStar { range, name }) => {
            if let Some(name) = name {
                Expr::Starred(ast::ExprStarred {
                    range: *range,
                    value: allocator.alloc(Expr::Name(ast::ExprName {
                        range: name.range,
                        id: name.id,
                        ctx: ExprContext::Store,
                    })),
                    ctx: ExprContext::Store,
                })
            } else {
                Expr::Starred(ast::ExprStarred {
                    range: *range,
                    value: allocator.alloc(Expr::Name(ast::ExprName {
                        range: TextRange::new(range.end() - "_".text_len(), range.end()),
                        id: "_",
                        ctx: ExprContext::Store,
                    })),
                    ctx: ExprContext::Store,
                })
            }
        }
        Pattern::MatchAs(ast::PatternMatchAs {
            range,
            pattern,
            name,
        }) => match (pattern, name) {
            (Some(_), Some(_)) => Expr::Name(ast::ExprName {
                range: *range,
                id: "",
                ctx: ExprContext::Invalid,
            }),
            (Some(pattern), None) => pattern_to_expr(pattern, allocator),
            (None, Some(name)) => Expr::Name(ast::ExprName {
                range: name.range,
                id: name.id,
                ctx: ExprContext::Store,
            }),
            (None, None) => Expr::Name(ast::ExprName {
                range: *range,
                id: "_",
                ctx: ExprContext::Store,
            }),
        },
        Pattern::MatchOr(ast::PatternMatchOr { patterns, .. }) => {
            let to_bin_expr = |left: &Pattern<'ast>, right: &Pattern<'ast>| ast::ExprBinOp {
                range: TextRange::new(left.start(), right.end()),
                left: allocator.alloc(pattern_to_expr(left, allocator)),
                op: ast::Operator::BitOr,
                right: allocator.alloc(pattern_to_expr(right, allocator)),
            };

            let mut iter = (**patterns).iter();

            match (iter.next(), iter.next()) {
                (Some(left), Some(right)) => {
                    Expr::BinOp(iter.fold(to_bin_expr(left, right), |expr_bin_op, pattern| {
                        ast::ExprBinOp {
                            range: TextRange::new(expr_bin_op.start(), pattern.end()),
                            left: allocator.alloc(Expr::BinOp(expr_bin_op)),
                            op: ast::Operator::BitOr,
                            right: allocator.alloc(pattern_to_expr(pattern, allocator)),
                        }
                    }))
                }
                _ => unreachable!("Or patterns can only be formed with at least two patterns."),
            }
        }
    }
}

extern crate peg;

use std::{collections::HashMap, hash::Hash};
peg::parser! {
    grammar expr_parser() for str {
        rule whitespace() = [' ' | '\t' | '\n' | '\r']*

        rule symbol() -> Expr
            = s:$(['a'..='z' | 'A'..='Z' | '?' | '$'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' ]* ) { Expr::Sym(s.into()) }

        rule list() -> Expr
            = "(" l:Expr() ** whitespace() ")" { Expr::List(l) }

        pub rule Expr() -> Expr
            = symbol() / list()
    }
}

fn expr(s: &str) -> Expr {
    expr_parser::Expr(s).unwrap()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Sym(String),
    List(Vec<Expr>),
}

fn sym(s: &str) -> Expr {
    Expr::Sym(s.to_string())
}

fn list(strs: Vec<&str>) -> Expr {
    Expr::List(strs.iter().map(|s| sym(s)).collect::<Vec<_>>())
}

fn liste(es: Vec<Expr>) -> Expr {
    Expr::List(es)
}

fn head(e: Expr) -> Expr {
    match e {
        Expr::Sym(_) => sym("Sym"),
        Expr::List(es) => es[0].clone(),
    }
}

fn my_match(expr: Expr, pattern: Expr) -> bool {
    match (expr.clone(), pattern.clone()) {
        (_, Expr::Sym(p)) => {
            if p == "blank" {
                true
            } else {
                expr == pattern
            }
        }
        (Expr::List(es), Expr::List(ps)) => {
            if es.len() != ps.len() {
                false
            } else {
                for (e, p) in es.iter().zip(ps) {
                    if !my_match(e.clone(), p.clone()) {
                        return false;
                    }
                }
                true
            }
        }
        _ => false,
    }
}

fn is_blank_match(e: Expr, p: Expr) -> bool {
    if let Expr::List(ps) = p {
        if ps.len() == 2 {
            let p_head = &ps[1];
            if p_head == &head(e) {
                true
            } else {
                false
            }
        } else {
            true
        }
    } else {
        panic!("is_blank_match needs a list for p")
    }
}

// traverses the expression, and whenever it hits a position in the tree that exists in pos map,
// replace
// fn pos_map_replace

/// in this case our wildcard is no longer just a symbol "blank"
/// now we have blank is a zero argument function so list(vec!["blank"])
fn my_match2(
    pos: Vec<usize>,
    expr: Expr,
    pattern: Expr,
    pos_map: &mut HashMap<Vec<usize>, Expr>,
    named_map: &mut HashMap<Expr, Expr>,
) -> bool {
    println!("{pos:?} | {expr:?} | {pattern:?}");
    match (expr.clone(), pattern.clone()) {
        (Expr::Sym(e), Expr::Sym(p)) => e == p,
        (Expr::Sym(e), Expr::List(ps)) => {
            // f | (blank)
            // f | (blank Sym)
            if ps[0] == sym("blank") {
                if is_blank_match(expr.clone(), pattern) {
                    pos_map.insert(pos, expr.clone());
                    true
                } else {
                    false
                }
            } else if ps[0] == sym("pattern") {
                let p_name = &ps[1];
                let b = &ps[2];
                if is_blank_match(expr.clone(), b.clone()) {
                    named_map.insert(pattern, expr.clone());
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
        // (f (a b)) | (f (blank))
        (Expr::List(es), Expr::List(ps)) => {
            if ps[0] == sym("blank") {
                if is_blank_match(expr.clone(), pattern) {
                    pos_map.insert(pos, expr.clone());
                    true
                } else {
                    false
                }
            } else if ps[0] == sym("pattern") {
                let p_name = &ps[1];
                let b = &ps[2];
                if is_blank_match(expr.clone(), b.clone()) {
                    named_map.insert(pattern, expr.clone());
                    true
                } else {
                    false
                }
            } else {
                if es.len() != ps.len() {
                    return false;
                }
                for (i, (e, p)) in es.iter().zip(ps).enumerate() {
                    let mut new_pos = pos.clone();
                    new_pos.push(i);
                    if !my_match2(new_pos, e.clone(), p.clone(), pos_map, named_map) {
                        return false;
                    }
                }
                true
            }
            // }
        }
        _ => false,
    }
}

fn main() {
    let fx = list(vec!["f", "x"]);
    // println!("{fx:?}");

    let blank_sym = sym("blank");
    let blank = list(vec!["blank"]);

    let lhs = list(vec!["f", "a", "b"]);
    let rhs = list(vec!["f", "blank"]);

    let test_cases = vec![
        (sym("1"), sym("1"), true),      // goes to "1" == "1" Sym, Sym arm
        (sym("1"), blank.clone(), true), // Sym Sym arm with blank
        (sym("1"), Expr::List(vec![sym("1")]), false), // Sym List -> false
        (Expr::List(vec![sym("1")]), sym("1"), false), // List Sym
        // (1) | (blank)
        (Expr::List(vec![sym("1")]), blank.clone(), true), // List, sym, with blank
        (lhs.clone(), rhs.clone(), false),                 // List, sym, with blank
        // (lhs.clone(), list(vec!["f", "blank", "blank"]), true), // List, sym, with blank
        (
            lhs.clone(),
            liste(vec![sym("f"), blank.clone(), blank.clone()]),
            true,
        ), // List, sym, with blank
        (sym("f"), list(vec!["blank", "Sym"]), true),
        (sym("f"), list(vec!["blank", "f"]), false),
        (list(vec!["f", "x"]), list(vec!["blank", "f"]), true),
        (list(vec!["f", "x"]), list(vec!["blank", "g"]), false),
        (expr("(f (a b))"), expr("(f (blank))"), true),
        (expr("(f (a b))"), expr("(f (blank a))"), true),
        (expr("(f x)"), expr("((blank) (blank))"), true),
        (expr("f"), expr("(pattern x (blank))"), true),
        (expr("(f)"), expr("(pattern x (blank))"), true),
    ];

    // list(vec!["f", "a", "b", "c"]), list(vec!["f", sym("blank_sequence")])
    for (i, (ex, pat, expected)) in test_cases.iter().enumerate() {
        println!("testing case {i}: {ex:?} | {pat:?} ");
        let pos = vec![];
        let mut pos_map = HashMap::new();
        let mut named_map = HashMap::new();

        assert_eq!(
            my_match2(pos, ex.clone(), pat.clone(), &mut pos_map, &mut named_map),
            *expected
        );
        println!("pos:\n{pos_map:?}\nnamed:\n{named_map:?}\n\n");
    }

    // what were doing now, is creating a map from the positions of the blanks in pattern
    // to the expressions they matched to.
}

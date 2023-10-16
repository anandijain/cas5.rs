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

fn parse(s: &str) -> Expr {
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

// this is the simplest pattern matcher, where blank is a Symbol, not a List and cannot have a head
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
fn pos_map_rebuild(pos: Vec<usize>, pat: Expr, pos_map: &HashMap<Vec<usize>, Expr>) -> Expr {
    if let Some(replacement) = pos_map.get(&pos) {
        return replacement.clone();
    }

    match pat {
        Expr::Sym(_) => pat,
        Expr::List(es) => {
            let mut new_es = vec![];
            for (i, e) in es.iter().enumerate() {
                let mut new_pos = pos.clone();
                new_pos.push(i);
                let new_e = pos_map_rebuild(new_pos, e.clone(), pos_map);
                new_es.push(new_e);
            }
            Expr::List(new_es)
        }
    }
}

pub fn named_rebuild_all(expr: Expr, map: &HashMap<Expr, Expr>) -> Expr {
    // First, check if the entire expression exists in the map and replace it if it does
    if let Some(replacement) = map.get(&expr) {
        return replacement.clone();
    }

    // If the expression is not in the map, proceed with the recursion
    match expr {
        Expr::Sym(_) => expr,
        Expr::List(list) => {
            // Recursively rebuild all sub-expressions in the list
            let new_list: Vec<Expr> = list
                .into_iter()
                .map(|e| named_rebuild_all(e, map))
                .collect();
            Expr::List(new_list)
        }
    }
}

fn rebuild_all(
    pat: Expr,
    named_map: &HashMap<Expr, Expr>,
    pos_map: &HashMap<Vec<usize>, Expr>,
) -> Expr {
    let new_ex = named_rebuild_all(pat, named_map);
    pos_map_rebuild(vec![], new_ex, pos_map)
}

/// in this case our wildcard is no longer just a symbol "blank"
/// now we have blank is a zero argument function so list(vec!["blank"]), with an optional first argument to blank for head matching
/// i dont think that we need to do a position lookup for unnamed patterns because they are all implicitly unique
/// and since we are traversing we shouldn't ever see our current position
/// this also supports named blanks ie (pattern x (blank))
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
                // todo: do lookup first
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
        (parse("(f (a b))"), parse("(f (blank))"), true),
        (parse("(f (a b))"), parse("(f (blank a))"), true),
        (parse("(f x)"), parse("((blank) (blank))"), true),
        (parse("f"), parse("(pattern x (blank))"), true),
        (parse("(f)"), parse("(pattern x (blank))"), true),
        (parse("(f x)"), parse("((pattern x (blank)) (blank))"), true),
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
        if *expected {
            let rebuilt_ex = rebuild_all(pat.clone(), &named_map, &pos_map);
            assert_eq!(rebuilt_ex, ex.clone());
        }
        println!("pos:\n{pos_map:?}\nnamed:\n{named_map:?}\n\n");
    }
}

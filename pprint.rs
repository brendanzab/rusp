use super::*;


impl ToStr for Value {
    fn to_str(&self) -> ~str {
        match *self {
            Empty => ~"()",
            Boolean(b) => b.to_str(),
            Integer(i) => i.to_str(),
            Float(f)   => f.to_str(),
            String(ref s) => fmt!("\"%?\"", str::escape_default(*s)),
            Quote(expr) => fmt!("(quote %s)", expr.to_str()),
            Lambda(ref ids, expr, _env) => {
                fmt!("|%s| %s",
                     str::connect(ids.map(|id| id.to_str()), " "),
                     expr.to_str())
            }
        }
    }
}

impl ToStr for Expr {
    fn to_str(&self) -> ~str {
        match *self {
            Symbol(ref id) => id.to_str(),
            Literal(ref val) => val.to_str(),
            QuoteExpr(ref expr) => fmt!("(quote %s)", expr.to_str()),
            IfExpr(ref pred, ref then, ref els) => fmt!("(if %s %s %s)",
                                            pred.to_str(), then.to_str(), els.to_str()),
            LambdaExpr(ref ids, ref expr) => {
                fmt!("|%s| %s",
                     str::connect(ids.map(|id| id.to_str()), " "),
                     expr.to_str())
            }
            LetExpr(ref id, ref expr) => {
                fmt!("(let %s %s)", id.to_str(), expr.to_str())
            }
            CallExpr(ref expr, ref exprs) => {
                fmt!("(%s %s)",
                     expr.to_str(),
                     str::connect(exprs.map(|e| e.to_str()), " "))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::*;
    fn test_pprint() {
        assert_eq!(IfExpr(
            @Literal(Boolean(true)),
            @CallExpr(@LambdaExpr(~[], @QuoteExpr(@Symbol(Ident(~"a")))), ~[]),
            @Literal(Boolean(true))
        ).to_str(), ~"(if true (|| (quote a)) true)")
    }
}
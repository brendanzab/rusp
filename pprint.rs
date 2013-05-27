use super::*;

impl ToStr for Ident {
    fn to_str(&self) -> ~str {
        (**self).clone()
    }
}

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
                fmt!("(fn (%s) %s)",
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
            If(ref pred, ref then, ref els) => fmt!("(if %s %s %s)",
                                            pred.to_str(), then.to_str(), els.to_str()),
            Let(ref id, ref expr) => {
                fmt!("(let %s %s)", id.to_str(), expr.to_str())
            }
            Call(ref expr, ref exprs) => {
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
        assert_eq!(If(
            @Literal(Boolean(true)),
            @Call(@Literal(Lambda(~[], @Literal(Quote(@Symbol(Ident(~"a")))), Env::empty())), ~[]),
            @Literal(Boolean(true))
        ).to_str(), ~"(if true ((fn () (quote a))) true)")
    }
}
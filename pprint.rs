use super::*;

impl ToStr for Ident {
    fn to_str(&self) -> ~str {
        (**self).clone()
    }
}

impl ToStr for Value {
    fn to_str(&self) -> ~str {
        match *self {
            Nil => ~"()",
            Bool(b) => b.to_str(),
            Int(i) => i.to_str(),
            Float(f)   => f.to_str(),
            Str(ref s) => fmt!("\"%s\"", str::escape_default(*s)),
            Quote(ref expr) => fmt!("(quote %s)", expr.to_str()),
            Fn(ref ids, ref expr) => {
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
            If(ref pred, ref conseq, ref alt) => {
                fmt!("(if %s %s %s)",
                     pred.to_str(), conseq.to_str(), alt.to_str())
            }
            Let(ref id, ref expr) => {
                fmt!("(let %s %s)", id.to_str(), expr.to_str())
            }
            Do(ref exprs) => {
                fmt!("(do %s)",
                     str::connect(exprs.map(|e| e.to_str()), " "))
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
            ~Literal(Bool(true)),
            ~Call(~Literal(Fn(~[], ~Literal(Quote(~Symbol(Ident(~"a")))))), ~[]),
            ~Literal(Bool(true))
        ).to_str(), ~"(if true ((fn () (quote a))) true)")
    }
}
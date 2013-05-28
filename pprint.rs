use super::*;

impl ToStr for Value {
    fn to_str(&self) -> ~str {
        match *self {
            Unit => ~"()",
            Bool(b) => b.to_str(),
            Int(i) => i.to_str(),
            Float(f) => f.to_str(),
            Str(ref s) => fmt!("\"%s\"", str::escape_default(*s)),
            Symbol(ref id) => id.to_str(),
            If(ref pred, ref conseq, ref alt) => {
                fmt!("(if %s %s %s)",
                     pred.to_str(), conseq.to_str(), alt.to_str())
            }
            Def(ref id, ref expr) => {
                fmt!("(def %s %s)", id.to_str(), expr.to_str())
            }
            Do(ref exprs) => {
                fmt!("(do %s)",
                     str::connect(exprs.map(|e| e.to_str()), " "))
            }
            Quote(ref expr) => fmt!("(quote %s)", expr.to_str()),
            Rust(_) => ~"(extern)", // Not sure about this?
            Lambda(ref ids, ref expr) => {
                fmt!("(fn (%s) %s)",
                     str::connect(ids.map(|id| id.to_str()), " "),
                     expr.to_str())
            }
            Apply(ref expr, ref exprs) => {
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
            ~Bool(true),
            ~Apply(~Lambda(~[], ~Quote(~Symbol(~"a"))), ~[]),
            ~Bool(true)
        ).to_str(), ~"(if true ((fn () (quote a))) true)")
    }
}
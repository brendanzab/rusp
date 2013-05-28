use super::*;

impl ToStr for Value {
    fn to_str(&self) -> ~str {
        match *self {
            Bool(b) => b.to_str(),
            Int(i) => i.to_str(),
            Float(f) => f.to_str(),
            List(ref vals) => {
                fmt!("(list%s)", vals.foldl(~"", |&x, val| fmt!("%s %s", x, val.to_str())))
            }
            Str(ref s) => fmt!("\"%s\"", str::escape_default(*s)),
            Symbol(ref id) => id.to_str(),
            If(ref pred, ref conseq, ref alt) => {
                fmt!("(if %s %s %s)",
                     pred.to_str(), conseq.to_str(), alt.to_str())
            }
            Def(ref id, ref val) => {
                fmt!("(def %s %s)", id.to_str(), val.to_str())
            }
            Do(ref vals) => {
                fmt!("(do %s)",
                     str::connect(vals.map(|val| val.to_str()), " "))
            }
            Quote(ref val) => fmt!("(quote %s)", val.to_str()),
            Rust(_) => ~"(extern)", // Not sure about this?
            Lambda(ref ids, ref val) => {
                fmt!("(fn (%s) %s)",
                     str::connect(ids.map(|id| id.to_str()), " "),
                     val.to_str())
            }
            Apply(ref proc, ref vals) => {
                fmt!("(%s %s)",
                     proc.to_str(),
                     str::connect(vals.map(|val| val.to_str()), " "))
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
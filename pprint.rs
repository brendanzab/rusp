use super::*;

impl ToStr for Value {
    fn to_str(&self) -> ~str {
        match *self {
            Bool(b) => b.to_str(),
            Int(i) => i.to_str(),
            Float(f) => f.to_str(),
            List(ref vals) => fmt!("(%s)", str::connect(vals.map(|val| val.to_str()), " ")),
            Str(ref s) => fmt!("\"%s\"", str::escape_default(*s)),
            Symbol(ref id) => id.to_str(),
            Rust(_, special) => fmt!("(extern %s)", // Not sure about this?
                                     if special {"macro"} else {"fn"}),
            Lambda(ref ids, ref val, special) => {
                fmt!("(%s (%s) %s)",
                     if special {"macro"} else {"fn"},
                     str::connect(ids.map(|id| id.to_str()), " "),
                     val.to_str())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::*;
    fn test_pprint() {
        assert_eq!(List(~[
            @Symbol(~"if"),
            @Bool(true),
            @List(~[@Lambda(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")]), false)]),
            @Bool(true)
        ]).to_str(), ~"(if true ((fn () (quote a))) true)")
    }
}

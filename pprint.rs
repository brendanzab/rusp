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
            Fn(ref ids, ref val) => {
                fmt!("(fn (%s) %s)",
                     str::connect(ids.map(|id| id.to_str()), " "),
                     val.to_str())
            }
            Macro(ref ids, ref val) => {
                fmt!("(macro (%s) %s)",
                     str::connect(ids.map(|id| id.to_str()), " "),
                     val.to_str())
            }
            ExternFn(_) => ~"(extern fn)",
            ExternMacro(_) => ~"(extern macro)",
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
            @List(~[@Fn(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")]))]),
            @Macro(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")])),
            @Bool(true)
        ]).to_str(), ~"(if true ((fn () (quote a))) (macro () (quote a) true)")
    }
}

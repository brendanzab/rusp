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
            FnEnv(ref func, _) => {
                match *func {
                    RuspFn(ref ids, ref val, ref fn_type) => {
                        fmt!("(%s (%s) %s)",
                             fn_type.to_str(),
                             str::connect(ids.map(|id| id.to_str()), " "),
                             val.to_str())
                    }
                    ExternFn(_, ref fn_type) => {
                        fmt!("(extern %s)", fn_type.to_str())
                    }
                }
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
            @List(~[@FnEnv(RuspFn(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")]), Fn), None)]),
            @FnEnv(RuspFn(~[], @List(~[@Symbol(~"quote"), @Symbol(~"a")]), Macro), None),
            @Bool(true)
        ]).to_str(), ~"(if true ((fn () (quote a))) (macro () (quote a) true)")
    }
}

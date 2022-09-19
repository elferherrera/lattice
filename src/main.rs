use lattice_lang::{lex, parse};
use lattice_protocol::report_error;

fn main() {
    // let args: Vec<_> = env::args().collect();
    let source = br#"command a --flag | command b -g"#;

    let res = lex(source);

    match res {
        Ok(tokens) => {
            println!("{:?}", tokens);
            let res = parse(tokens);
            println!("{:#?}", res);

            if !res.errors.is_empty() {
                for err in res.errors {
                    report_error(source, &err)
                }
            }
        }
        Err(error) => report_error(source, &error),
    }
}

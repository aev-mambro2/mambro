#[cfg(test)]
mod tests {
    #[test]
    fn testParse() {
        extern crate sxd_document;
        extern crate sxd_xpath;

        use sxd_document::parser;
        use sxd_xpath::{evaluate_xpath, Value};

        let package = parser::parse("<root>hello</root>").expect("failed to parse XML");
        let document = package.as_document();

        let value = evaluate_xpath(&document, "/root").expect("XPath evaluation failed");

        assert_eq!("hello", value.string());
    }
}
mod xml {
    extern crate sxd_document;
    extern crate sxd_xpath;

    use sxd_document::parser;
    use sxd_xpath::{evaluate_xpath, Value};

    fn parse() {
        let package = parser::parse("<root>hello</root>").expect("failed to parse XML");
        let document = package.as_document();

        let value = evaluate_xpath(&document, "/root").expect("XPath evaluation failed");

        assert_eq!("hello", value.string());
    }
}

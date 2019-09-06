#[cfg(test)]
mod tests {
    #[test]
    fn test_parse() {
        extern crate sxd_document;
        extern crate sxd_xpath;

        use super::xpath as xpath;
        use sxd_document::parser;
        use sxd_xpath::evaluate_xpath;

        let input = "<root>hello</root>";
        let package = parser::parse(input).expect("failed to parse XML");
        let document = package.as_document();

        let statement: &str = "/root";
        let value = evaluate_xpath(&document, statement).expect("XPath evaluation failed");

        assert_eq!("hello", value.string());

        let extracted: String = xpath::extract(input, statement);

        assert_eq!("hello", extracted.as_str());
    }
}
mod xpath {
    extern crate sxd_document;
    extern crate sxd_xpath;

    use sxd_document::parser;
    use sxd_xpath::evaluate_xpath;

    pub fn extract(d: &str, s: &str) -> String {
        let package = parser::parse(d).expect("failed to parse XML");
        let document = package.as_document();
        let extracted = evaluate_xpath(&document, s).expect("XPath evaluation failed");
        let output = extracted.string();
        output
    }
}

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

// Contains convenience methods for extracting
// information from XML documents using XPath
// statements.
mod xpath {
    extern crate sxd_document;
    extern crate sxd_xpath;

    use sxd_document::parser;
    use sxd_xpath::evaluate_xpath;

    // Extracts the passed-in XPath statement
    // from the passed-in document as a String.
    // Use this if you have a single XPath
    // statement to resolve. If you have 
    // several, use extract(&str, Vec<&str>)
    // instead. 
    //
    // # Parameters
    // - d: a referenced string slice containing 
    //   an XML-formated document. 
    // - s: an XPath statement.
    //   
    // # Returns
    // Either an empty string, or the value 
    // found by evaluating the XPath statement
    // against the document.
    //
    /// # Example
    ///    use crate::xpath as xpath;
    ///    let input = "<root>hello</root>";
    ///    let statement: &str = "/root";
    ///    let extracted: String = xpath::extract(input, statement);
    ///    assert_eq!("hello", extracted.as_str());
    ///
    pub fn extract(d: &str, s: &str) -> String {
        let package = parser::parse(d).expect("failed to parse XML");
        let document = package.as_document();
        let extracted = evaluate_xpath(&document, s).expect("XPath evaluation failed");
        let output = extracted.string();
        output
    }
}

#[cfg(test)]
/// Tests
mod tests {
    #[test]
    /// Our xpath convenience module must be 
    /// able to take text contents as xml and
    /// extract element values based on an xpath
    /// statement.
    fn test_parse_one() {
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

        let extracted: String = xpath::one(input, statement);

        assert_eq!("hello", extracted.as_str());
    }

    #[test]
    /// Our xpath convenience module must be 
    /// able to take text contents as xml and
    /// extract element values based on xpath
    /// statements.
    fn test_parse_more() {
        extern crate sxd_document;
        extern crate sxd_xpath;

        use super::xpath as xpath;
        use sxd_document::parser;
        use sxd_xpath::evaluate_xpath;

        let input = "<root><hello>world</hello><world>planet</world></root>";
        let package = parser::parse(input).expect("failed to parse XML");
        let doc = package.as_document();

        let s1 = "/root/hello/text()";
        let v1 = evaluate_xpath(&doc, s1).expect("XPath evaluation failed");
        let o1 = v1.string();
        assert_eq!("world", o1);
        
        let s2 = "/root/world/text()";
        let v2 = evaluate_xpath(&doc, s2).expect("XPath evaluation failed");
        let o2 = v2.string();
        assert_eq!("planet", o2);

        let more = xpath::more(input, vec![s1, s2]);

        assert!(more.contains(&o1));
        assert!(more.contains(&o2));
    }
}

/// Contains convenience methods for extracting
/// information from XML documents using XPath
/// statements.
pub mod xpath {
    extern crate sxd_document;
    extern crate sxd_xpath;

    use sxd_document::parser;
    use sxd_xpath::evaluate_xpath;

    /// Extracts the passed-in XPath statement
    /// from the passed-in document as a String.
    /// Use this if you have a single XPath
    /// statement to resolve. If you have 
    /// several, use extract(&str, Vec<&str>)
    /// instead. 
    ///
    /// 
    /// # Parameters
    /// 
    /// - d: a referenced string slice containing 
    ///   an XML-formated document. 
    /// - s: an XPath statement.
    /// 
    ///
    /// # Returns
    /// 
    /// Either an empty string, or the value 
    /// found by evaluating the XPath statement
    /// against the document.
    ///
    /// 
    /// # Examples
    ///
    /// ```rust
    ///    use apiconsumer::xpath as xpath;
    ///    let input = "<root>hello</root>";
    ///    let statement: &str = "/root";
    ///    let extracted: String = xpath::one(input, statement);
    ///    assert_eq!("hello", extracted.as_str());
    /// ```
    ///
    pub fn one(d: &str, s: &str) -> String {
        let package = parser::parse(d).expect("failed to parse XML");
        let document = package.as_document();
        let extracted = evaluate_xpath(&document, s).expect("XPath evaluation failed");
        let output = extracted.string();
        output
    }
    
    /// Extracts the passed-in XPath statements
    /// from the passed-in document as Strings.
    /// Use this if you have a multiple XPath
    /// statements to resolve. If you have just
    /// one, use one(&str, &str) instead. 
    ///
    /// 
    /// # Parameters
    /// 
    /// - d: a referenced string slice containing 
    ///   an XML-formated document. 
    /// - s: a set of XPath statements.
    ///   
    /// 
    /// # Returns
    /// 
    /// Either an empty list, or the values found
    /// by evaluating the XPath statements
    /// against the document.
    ///
    /// 
    /// # Examples
    ///
    /// ```rust
    ///    use apiconsumer::xpath as xpath;
    ///    let input = "<root><hello>world</hello><world>planet</world></root>";
    ///    let statements: Vec<&str> = vec!["/root/hello/text()", "/root/world/text()"];
    ///    let extracted: Vec<String> = xpath::more(input, statements);
    ///    assert_eq!(2, extracted.len());
    /// ```
    ///
    pub fn more(d: &str, s: Vec<&str>) -> Vec<String> {
        let package = parser::parse(d).expect("failed to parse XML");
        let document = package.as_document();
        let mut buffer: Vec<String> = Vec::new();
        for e in s {
            let extracted = evaluate_xpath(&document, e).expect("XPath evaluation failed");
            let output = extracted.string();
            buffer.push(output);
        }
        buffer
    }
}

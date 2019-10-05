#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

///! Fluent TSQL Statement Builder
///!
///! @author A.E.Veltstra
///! @since 2.19.1003.2150
///! @version 2,19.1003.2150
pub struct SqlBuilder {
    buffer: String,
}

/// Wraps the input in prefix and suffix if it isn't already.
///
/// # Examples
///
/// ```rust
/// use tsql_fluent::*;
/// let s = "verb".to_string();
/// let w = wrap(s, "f", "5");
/// assert_eq!("fverb5".to_string(), w);
/// ```
pub fn wrap(input: String, prefix: &str, suffix: &str) -> String {
    if input.is_empty() {
        if "" == prefix && "" == suffix {
            return input;
        }
        let b1 = String::new() + prefix + suffix;
        return b1;
    }
    if "" == prefix && "" == suffix {
        return input;
    }
    if input.starts_with(prefix) && input.ends_with(suffix) {
        return input;
    }
    let b2 = prefix.to_owned() + &input.clone();
    let b3 = b2 + suffix;
    return b3;
}
pub trait WrapInBrackets {
    /// Wraps the input in brackets if it isn't already.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let s = "verb".to_string();
    /// let w = s.wrap_in_brackets();
    /// assert_eq!("[verb]".to_string(), w);
    /// ```
    fn wrap_in_brackets(self) -> String;
}

const BRACKET_OPEN: &'static str = "[";
const BRACKET_CLOSE: &'static str = "]";
impl WrapInBrackets for String {
    fn wrap_in_brackets(self) -> String {
        wrap(self, BRACKET_OPEN, BRACKET_CLOSE)
    }
}

const SELECT: &'static str = "select ";
const TOP: &'static str = "top ";

/// Starts a new TSQL statement builder.
///
/// # Examples
/// ```rust
/// use tsql_fluent::*;
/// let s = select();
/// assert_eq!(s.to_string(), "select ".to_string());
/// ```
pub fn select() -> SqlBuilder {
    SqlBuilder { buffer: SELECT.to_owned() }
}

/// Starts a new SQL statement builder with 
/// a single field name. If you want to use 
/// a name that requires escaping due to data 
/// store rules, do escape it first.
///
/// # Examples
/// ```rust
/// use tsql_fluent::*;
/// let n = "id".to_string();
/// let s = select_n(n);
/// assert_eq!(s.to_string(), "select [id] ".to_string());
/// ```
pub fn select_n(n: String) -> SqlBuilder {
    let b = format!("{}{} ", SELECT, n.wrap_in_brackets().as_str());
    SqlBuilder { buffer: b }
}

/// Starts a new SQL statement builder with 
/// a single field name. If you want to use 
/// a name that requires escaping due to data 
/// store rules, do escape it first.
///
/// # Examples
/// ```rust
/// use tsql_fluent::*;
/// let a = "a".to_string();
/// let b = "b".to_string();
/// let c = "c".to_string();
/// let m = vec![a, b, c];
/// let s = select_m(m);
/// assert_eq!(s.to_string(), "select [a], [b], [c] ".to_string());
/// ```
pub fn select_m(m: Vec<String>) -> SqlBuilder {
    let mut b = SELECT.to_owned();
    if let Some((last, more)) = m.split_last() {
        for f in more {
            let f1 = f.clone().wrap_in_brackets();
            let f2 = f1.as_str();
            b = b + f2;
            b = b + ", ";
        }
        let f3 = last.clone().wrap_in_brackets();
        let f4 = f3.as_str();
        b = b + f4;
        b = b + " ";
    }

    let c = b;
    SqlBuilder { buffer: c }
}

impl SqlBuilder {
    /// Finalize the statement and 
    /// output it as a String.
    pub fn to_string (&self) -> String {
        self.buffer.to_owned()
    }

    /// Add the start of the TOP clause 
    /// to the existing statement.
    ///
    /// 
    /// # Examples
    /// ```rust
    /// use tsql_fluent::*;
    /// let t = select().top();
    /// assert_eq!(t.to_string(), "select top ".to_string());
    /// ```
    pub fn top (&self) -> Self {
        let n = format!("{}{}", self.buffer, TOP);
        SqlBuilder { buffer: n }
    }

    /// Add the start of the TOP clause 
    /// followed by the max amount of records 
    /// to return, to the existing statement.
    ///
    /// 
    /// # Examples
    /// ```rust
    /// use tsql_fluent::*;
    /// let t = select().top_x(5);
    /// assert_eq!(t.to_string(), "select top 5 ".to_string());
    /// ```
    pub fn top_x (&self, x: i64) -> Self {
        let n = format!("{}{}{} ", self.buffer, TOP, x);
        SqlBuilder { buffer: n }
    }

}

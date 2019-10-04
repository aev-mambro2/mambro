#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

///! Fluent SQL Statement Builder
///!
///! @author A.E.Veltstra
///! @since 2.19.1003.2150
///! @version 2,19.1003.2150
pub struct SqlBuilder {
    buffer: String,
}

const SELECT: &'static str = "select ";
const TOP: &'static str = "top ";

/// Starts a new SQL statement builder.
///
/// # Examples
/// ```rust
/// use sql::*;
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
/// use sql::*;
/// let n = "id".to_string();
/// let s = select_n(n);
/// assert_eq!(s.to_string(), "select id ".to_string());
/// ```
pub fn select_n(n: String) -> SqlBuilder {
    let b = format!("{}{} ", SELECT, n);
    SqlBuilder { buffer: b }
}

/// Starts a new SQL statement builder with 
/// a single field name. If you want to use 
/// a name that requires escaping due to data 
/// store rules, do escape it first.
///
/// # Examples
/// ```rust
/// use sql::*;
/// let a = "a".to_string();
/// let b = "b".to_string();
/// let c = "c".to_string();
/// let m = vec![a, b, c];
/// let s = select_m(m);
/// assert_eq!(s.to_string(), "select a, b, c ".to_string());
/// ```
pub fn select_m(m: Vec<String>) -> SqlBuilder {
    let mut b = SELECT.to_owned();
    if let Some((last, more)) = m.split_last() {
        for f in more {
            b.push_str(f);
            b.push_str(", ");
        }
        b.push_str(last);
        b.push_str(" ");
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
    /// use sql::*;
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
    /// use sql::*;
    /// let t = select().top_x(5);
    /// assert_eq!(t.to_string(), "select top 5 ".to_string());
    /// ```
    pub fn top_x (&self, x: i64) -> Self {
        let n = format!("{}{}{} ", self.buffer, TOP, x);
        SqlBuilder { buffer: n }
    }

}

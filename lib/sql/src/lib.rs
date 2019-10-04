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

}

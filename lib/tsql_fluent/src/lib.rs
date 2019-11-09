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
///! @version 2,19.1108.725
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

/// Starts a new TSQL statement builder.
///
/// # Examples
///
/// ```rust
/// use tsql_fluent::*;
/// let s = select();
/// assert_eq!(s.as_str(), "select ");
/// ```
pub fn select() -> SqlBuilder {
    SqlBuilder { buffer: SELECT.to_owned(), }
}

/// Starts a data retrieval query with 
/// instead of fields, selecting a static 
/// amount, no matter how many rows are found.
/// This is used in EXISTS sub statements, 
/// and other places. 
///
/// # Examples
///
/// ```rust
/// use tsql_fluent::*;
/// let a = select_i(7);
/// assert_eq!("select 7 ", a.as_str());
/// ```
///
pub fn select_i(amount: i8) -> SqlBuilder {
    let s = SELECT.to_owned();
    let b = format!("{}{} ", s, amount);
    SqlBuilder { buffer: b, }
}


/// Starts a new SQL statement builder with 
/// a single field name. If you want to use 
/// a name that requires escaping due to data 
/// store rules, do escape it first.
///
///
/// # Examples
///
/// ```rust
/// use tsql_fluent::*;
/// let a = "a".to_string();
/// let b = "b".to_string();
/// let c = "c".to_string();
/// let m = vec![a, b, c];
/// let s = select_m(m);
/// assert_eq!(
///     s.as_str(), 
///     "select [a], [b], [c] "
/// );
/// ```
pub fn select_m(m: Vec<String>) -> SqlBuilder {
    let b = SELECT.to_owned();
    let f = list_fields(m);
    let c  = b + f.as_str();
    SqlBuilder { buffer: c, }
}

// Considers passed-in elements are db field names.
// Wraps them in square brackets if needed,
// Concatenates them using a comma.
//
// # Parameters
// Vec<String>: a list of field names.
//
// This is a private function used in eg.
// select_m/1 and top_x/2., 
fn list_fields(m: Vec<String>) -> String {
    let mut b = "".to_string();
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
    c
}

impl ToString for SqlBuilder {
    /// Finalize the statement and 
    /// output it as a String.
    fn to_string (&self) -> String {
        self.buffer.to_owned()
    }
}

impl AsRef<SqlBuilder> for SqlBuilder {
    fn as_ref (&self) -> &SqlBuilder {
        self
    }
}

const TOP: &'static str = "top ";
const DISTINCT: &'static str = "distinct ";

impl SqlBuilder {

    pub fn as_str (&self) -> &str {
        self.buffer.as_str()
    }

    /// Add the DISTINCT clause 
    /// to the existing statement.
    /// This clause will make the DB attempt 
    /// to remove any duplicate rows from 
    /// the final query result prior to 
    /// returning the result.
    ///
    /// 
    /// # Examples
    /// ```rust
    /// use tsql_fluent::*;
    /// let t = select()
    ///             .distinct();
    /// assert_eq!(
    ///     t.as_str(), 
    ///     "select distinct "
    /// );
    /// ```
    pub fn distinct (&self) -> Self {
        let n = format!("{}{}", self.buffer, DISTINCT);
        SqlBuilder { buffer: n }
    }

    /// Add the DISTINCT clause with fields
    /// to the existing statement.
    /// This clause will make the DB attempt 
    /// to remove any duplicate rows from 
    /// the final query result prior to 
    /// returning the result.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let a = "a".to_string();
    /// let b = "b".to_string();
    /// let c = "c".to_string();
    /// let m = vec![a, b, c];
    /// let s = select()
    ///             .distinct_m(m);
    /// assert_eq!(
    ///     s.as_str(), 
    ///     "select distinct [a], [b], [c] "
    /// );
    /// ```
    pub fn distinct_m (&self, m: Vec<String>) -> Self {
        let n = format!("{}{}", self.buffer, DISTINCT);
        let f = list_fields(m);
        let c  = n + f.as_str();
        SqlBuilder { buffer: c, }
    }

    /// Add the start of the TOP clause 
    /// to the existing statement.
    /// This clause lets you specify an 
    /// amount of rows to return. 
    ///
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// use tsql_fluent::*;
    /// let t = select().top();
    /// assert_eq!(t.to_string(), "select top ");
    /// ```
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let t = select()
    ///     .top()
    ///     .distinct();
    /// assert_eq!(
    ///     t.as_str(), 
    ///     "select top distinct "
    /// );
    /// ```
    pub fn top (&self) -> Self {
        let n = format!("{}{}", self.buffer, TOP);
        SqlBuilder { buffer: n }
    }

    /// Add the start of the TOP clause 
    /// followed by the max amount of records 
    /// to return, and which fields to return,
    /// to the existing statement. A TOP clause
    /// causes the query executor to evaluate 
    /// the records and rank them, and then 
    /// returning only those highest in rank.
    ///
    /// 
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let a = "a".to_string();
    /// let b = "b".to_string();
    /// let c = "c".to_string();
    /// let m = vec![a, b, c];
    /// let t = select()
    ///             .top_x(5, m);
    /// assert_eq!(
    ///     t.as_str(), 
    ///     "select top 5 [a], [b], [c] "
    /// );
    /// ```
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let a = "a".to_string();
    /// let b = "b".to_string();
    /// let c = "c".to_string();
    /// let m = vec![a, b, c];
    /// let t = select()
    ///     .distinct()
    ///     .top_x(5, m);
    /// assert_eq!(
    ///     t.as_str(), 
    ///     "select distinct top 5 [a], [b], [c] "
    /// );
    /// ```
    pub fn top_x (&self, x: i64, m: Vec<String>) -> Self {
        let t = self.top().buffer;
        let f = list_fields(m);
        let t_x_m  = format!("{}{} {}", t, x, f.as_str());
        SqlBuilder { buffer: t_x_m, }
    }

    /// Adds the AS clause to the SQL statement.
    /// This is used to assign an alias to a 
    /// field value, field name, or object name.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///     .from("card_holders".to_string())
    ///     .as("grantees".to_string());
    /// assert_eq!(
    ///     "select 1 from [card_holders] as [grantees] ", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn r#as (&self, alias: String) -> Self {
        let b = format!("{}as {} ", self.buffer, alias.wrap_in_brackets());
        SqlBuilder { buffer: b, }
}

    /// Adds the FROM clause to the SQL statement.
    /// This is used to indicate data storage
    /// objects (tables, etc.) or other data row 
    /// yielding objects (views, procedures, 
    /// functions, etc.).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///         .from("employees".to_string());
    /// assert_eq!(
    ///     "select 1 from [employees] ", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn from (&self, store: String) -> Self {
        let b = format!("{}{} ", self.buffer, store.wrap_in_brackets());
        SqlBuilder { buffer: b, }
    }

    /// Adds the WHERE clause to the SQL statement.
    /// This is used to start specifying criteria
    /// that rows must meet in order to get
    /// included in the returned result.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///         .from("employees".to_string())
    ///         .where("name".to_string());
    /// assert_eq!(
    ///     "select 1 from [employees] where [name] ", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn r#where (&self, field: String) -> Self {
        let b = format!("{}{} ", self.buffer, field.wrap_in_brackets());
        SqlBuilder { buffer: b, }
    }

    /// Adds the EQUALS clause to the SQL statement.
    /// This is used to specify criteria values
    /// that row columnss must match in order to 
    /// get included in the returned result. You
    /// must apply proper quoting and escaping. 
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///         .from("employees".to_string());
    ///         .where("name".to_string())
    ///         .equals("'Pratchett'".to_string());
    /// assert_eq!(
    ///     "select 1 from [employees] where [name] = 'Pratchett'", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn equals (&self, value: String) -> Self {
        let b = format!("{}{} ", self.buffer, value);
        SqlBuilder { buffer: b, }
    }

    /// This is used to add specifying criteria
    /// that rows must meet in order to get
    /// included in the returned result.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///         .from("employees".to_string())
    ///         .where("name".to_string())
    ///         .equals("'Pratchett'".to_string())
    ///         .and("first".to_string())
    ///         .is_not_null();
    /// assert_eq!(
    ///     "select 1 from [employees] where [name] = 'Pratchett' and [first] is not null", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn and (&self, field: String) -> Self {
        let b = format!("{}{} ", self.buffer, field.wrap_in_brackets());
        SqlBuilder { buffer: b, }
    }

    /// This is used to add the IS NULL match
    /// to compare the last row field's value,
    /// that rows must meet in order to get
    /// included in the returned result.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///         .from("employees".to_string());
    ///         .where("first".to_string())
    ///         .is_null();
    /// assert_eq!(
    ///     "select 1 from [employees] where [first]  is null ", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn is_null (&self) -> Self {
        let b = format!("{} is null ", self.buffer);
        SqlBuilder { buffer: b, }
    }

    /// This is used to add the IS NOT NULL match
    /// to compare the last row field's value,
    /// that rows must meet in order to get
    /// included in the returned result.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = select_i(1)
    ///         .from("employers".to_string());
    ///         .where("benefits".to_string())
    ///         .is_not_null();
    /// assert_eq!(
    ///     "select 1 from [employers] where [benefits]  is not null ", 
    ///     f.as_str()
    /// );
    /// ```
    pub fn is_not_null (&self) -> Self {
        let b = format!("{} is not null ", self.buffer);
        SqlBuilder { buffer: b, }
    }

}



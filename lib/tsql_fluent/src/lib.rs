///! Fluent TSQL Statement Builder
///!
///! @author A.E.Veltstra
///! @since 2.19.1003.2150
///! @version 2.20.226.2215
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
    let b2 = prefix.to_owned() + &input;
    b2 + suffix
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

const BRACKET_OPEN: &str = "[";
const BRACKET_CLOSE: &str = "]";
impl WrapInBrackets for String {
    fn wrap_in_brackets(self) -> String {
        wrap(self, BRACKET_OPEN, BRACKET_CLOSE)
    }
}

const SELECT: &str = "select ";

pub trait Select {
    fn select(self) -> SqlBuilder;
}

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
    SqlBuilder {
        buffer: SELECT.to_owned(),
    }
}

impl Select for i8 {
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
    /// let a = 7.select();
    /// assert_eq!("select 7 ", a.as_str());
    /// ```
    ///
    fn select(self) -> SqlBuilder {
        let s = SELECT.to_owned();
        let b = format!("{}{} ", s, &self);
        SqlBuilder { buffer: b }
    }
}

impl Select for String {
    /// Starts a data retrieval query with
    /// a single field.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let a = "42".to_string().select();
    /// assert_eq!("select [42] ", a.as_str());
    /// ```
    ///
    fn select(self) -> SqlBuilder {
        let s = SELECT.to_owned();
        let b = format!("{}{} ", s, &self.wrap_in_brackets());
        SqlBuilder { buffer: b }
    }
}

impl Select for Vec<String> {
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
    /// let s = m.select();
    /// assert_eq!(
    ///     s.as_str(),
    ///     "select [a], [b], [c] "
    /// );
    /// ```
    fn select(self) -> SqlBuilder {
        let b = SELECT.to_owned();
        let f = wrap_and_list_fields(self);
        let c = b + f.as_str();
        SqlBuilder { buffer: c }
    }
}

// Considers passed-in elements are db field names.
// Wraps them in square brackets if needed,
// Concatenates them using a comma.
//
// # Parameters
// Vec<String>: a list of field names.
//
// This is a private function used in eg.
// select/1 and top_x/2.,
fn wrap_and_list_fields(m: Vec<String>) -> String {
    let mut b = "".to_string();
    if let Some((last, more)) = m.split_last() {
        for f in more {
            let f1 = f.clone().wrap_in_brackets();
            let f2 = f1.as_str();
            b += f2;
            b += ", ";
        }
        let f3 = last.clone().wrap_in_brackets();
        let f4 = f3.as_str();
        b += f4;
        b += " ";
    }
    b
}

impl ToString for SqlBuilder {
    /// Finalize the statement and
    /// output it as a String.
    fn to_string(&self) -> String {
        self.buffer.to_owned()
    }
}

impl AsRef<SqlBuilder> for SqlBuilder {
    fn as_ref(&self) -> &SqlBuilder {
        self
    }
}

const TOP: &str = "top ";
const COUNT_ALL: &str = "count (*) ";
const DISTINCT: &str = "distinct ";
const FROM: &str = "from ";
const ALIAS: &str = "as ";
const WHERE: &str = "where ";
const EQUALS: &str = "= ";
const NOT_EQUALS: &str = "!= ";
const AND: &str = "and ";
const IS_NULL: &str = "is null ";
const IS_NOT_NULL: &str = "is not null ";

impl SqlBuilder {
    pub fn as_str(&self) -> &str {
        self.buffer.as_str()
    }

    /// Add the COUNT * clause
    /// to the existing statement.
    /// This clause will make the DB count
    /// the number of records in a ledger.
    ///
    ///
    /// # Examples
    /// ```rust
    /// use tsql_fluent::*;
    /// let a = select()
    ///          .count_all();
    /// assert_eq!(
    ///     a.as_str(),
    ///     "select count (*) "
    /// );
    /// ```
    pub fn count_all(&self) -> Self {
        let a = format!("{}{}", self.buffer, COUNT_ALL);
        SqlBuilder { buffer: a }
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
    ///         .distinct();
    /// assert_eq!(
    ///     t.as_str(),
    ///     "select distinct "
    /// );
    /// ```
    pub fn distinct(&self) -> Self {
        let n = format!("{}{}", self.buffer, DISTINCT);
        SqlBuilder { buffer: n }
    }

    /// Add the start of the TOP clause
    /// to the existing statement.
    /// This clause lets you specify an
    /// amount of rows to return.
    ///
    fn top(&self) -> Self {
        let n = format!("{}{}", self.buffer, TOP);
        SqlBuilder { buffer: n }
    }

    /// Starts a data of the TOP clause
    /// followed by the max amount of records
    /// to return. A TOP clause
    /// causes the query executor to evaluate
    /// the records and rank them, and then
    /// returning only those highest in rank.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let a = select().top_i(7);
    /// assert_eq!("select top 7 ", a.as_str());
    /// ```
    ///
    pub fn top_i(&self, i: i64) -> Self {
        let t = self.top().buffer;
        let b = format!("{}{} ", t, i);
        SqlBuilder { buffer: b }
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
    ///         .top_i(5)
    ///         .fields(m);
    /// assert_eq!(
    ///     t.as_str(),
    ///     "select top 5 [a], [b], [c] "
    /// );
    /// ```
    ///
    pub fn fields(&self, m: Vec<String>) -> Self {
        let f = wrap_and_list_fields(m);
        let t_f = format!("{}{}", self.buffer, f.as_str());
        SqlBuilder { buffer: t_f }
    }

    /// Adds the AS clause to the SQL statement.
    /// This is used to assign an alias to a
    /// field value, field name, or object name.
    ///
    /// The function is called 'alias' rather
    /// than 'as', because the keyword 'as'
    /// is a reserved word in the rust language,
    /// and the rust compiler is not smart
    /// enough to understand the difference.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = 1.select()
    ///     .from("card_holders".to_string())
    ///     .alias("grantees".to_string());
    /// assert_eq!(
    ///     "select 1 from [card_holders] as [grantees] ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn alias(&self, alias: String) -> Self {
        let b = format!("{}{}{} ", self.buffer, ALIAS, alias.wrap_in_brackets());
        SqlBuilder { buffer: b }
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
    /// let f = 1.select()
    ///         .from("employees".to_string());
    /// assert_eq!(
    ///     "select 1 from [employees] ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn from(&self, store: String) -> Self {
        let b = format!("{}{}{} ", self.buffer, FROM, store.wrap_in_brackets());
        SqlBuilder { buffer: b }
    }

    /// Adds the WHERE clause to the SQL statement.
    /// This is used to start specifying criteria
    /// that rows must meet in order to get
    /// included in the returned result.
    ///   
    /// This method is called "wher" instead of "where",
    /// because the compiler is not smart enough to tell
    /// apart keywords from functions.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = 1.select()
    ///         .from("employees".to_string())
    ///         .wher()
    ///         .field("name".to_string());
    /// assert_eq!(
    ///     "select 1 from [employees] where [name] ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn wher(&self) -> Self {
        let b = format!("{}{}", self.buffer, WHERE);
        SqlBuilder { buffer: b }
    }

    /// Adds a field (column) to the SQL statement.
    /// This can be used to specify criteria
    /// that rows must meet in order to get
    /// included in the returned result.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = 1.select()
    ///         .from("employees".to_string())
    ///         .wher()
    ///         .field("name".to_string());
    /// assert_eq!(
    ///     "select 1 from [employees] where [name] ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn field(&self, field: String) -> Self {
        let b = format!("{}{} ", self.buffer, field.wrap_in_brackets());
        SqlBuilder { buffer: b }
    }

    /// Adds the EQUALS clause to the SQL statement,
    /// adding a parameter question mark
    /// that allows you to later add a value that
    /// row columns must match in order to
    /// get included in the returned result. You
    /// must apply proper quoting and escaping.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = 1.select()
    ///         .from("authors".to_string())
    ///         .wher()
    ///         .field("book".to_string())
    ///         .equals_param();
    /// assert_eq!(
    ///     "select 1 from [authors] where [book] = ? ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn equals_param(&self) -> Self {
        let b = format!("{}{}? ", self.buffer, EQUALS);
        SqlBuilder { buffer: b }
    }

    /// Adds the NOT EQUALS clause to the SQL statement,
    /// adding a parameter question mark
    /// that allows you to later add a value that
    /// row columns must not match in order to
    /// get included in the returned result. You
    /// must apply proper quoting and escaping.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = 1.select()
    ///         .from("authors".to_string())
    ///         .wher()
    ///         .field("book".to_string())
    ///         .not_equals_param();
    /// assert_eq!(
    ///     "select 1 from [authors] where [book] != ? ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn not_equals_param(&self) -> Self {
        let b = format!("{}{}? ", self.buffer, NOT_EQUALS);
        SqlBuilder { buffer: b }
    }

    /// Adds the EQUALS clause to the SQL statement,
    /// allowing to enter the value
    /// that row columns must match in order to
    /// get included in the returned result. You
    /// must apply proper quoting and escaping.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = "address".to_string()
    ///         .select()
    ///         .from("employees".to_string())
    ///         .wher()
    ///         .field("name".to_string())
    ///         .equals_value("'Pratchett'".to_string());
    /// assert_eq!(
    ///     "select [address] from [employees] where [name] = 'Pratchett' ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn equals_value(&self, value: String) -> Self {
        let b = format!("{}{}{} ", self.buffer, EQUALS, value);
        SqlBuilder { buffer: b }
    }

    /// Adds the NOT EQUALS clause to the SQL statement,
    /// allowing to enter the value
    /// that row columns must not match in order to
    /// get included in the returned result. You
    /// must apply proper quoting and escaping.
    ///
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tsql_fluent::*;
    /// let f = "address".to_string()
    ///         .select()
    ///         .from("employees".to_string())
    ///         .wher()
    ///         .field("name".to_string())
    ///         .not_equals_value("'Pratchett'".to_string());
    /// assert_eq!(
    ///     "select [address] from [employees] where [name] != 'Pratchett' ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn not_equals_value(&self, value: String) -> Self {
        let b = format!("{}{}{} ", self.buffer, NOT_EQUALS, value);
        SqlBuilder { buffer: b }
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
    /// let f = "code".to_string()
    ///         .select()
    ///         .from("employees".to_string())
    ///         .alias("workers".to_string())
    ///         .wher()
    ///         .field("name".to_string())
    ///         .equals_value("'Pratchett'".to_string())
    ///         .and()
    ///         .field("first".to_string())
    ///         .is_not_null();
    /// assert_eq!(
    ///     "select [code] from [employees] as [workers] where [name] = 'Pratchett' and [first] is not null ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn and(&self) -> Self {
        let b = format!("{}{}", self.buffer, AND);
        SqlBuilder { buffer: b }
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
    /// let f = 1.select()
    ///         .from("employees".to_string())
    ///         .wher()
    ///         .field("first".to_string())
    ///         .is_null();
    /// assert_eq!(
    ///     "select 1 from [employees] where [first] is null ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn is_null(&self) -> Self {
        let b = format!("{}{}", self.buffer, IS_NULL);
        SqlBuilder { buffer: b }
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
    /// let f = select()
    ///         .top_i(1)
    ///         .field("name".to_string())
    ///         .from("employers".to_string())
    ///         .wher()
    ///         .field("benefits".to_string())
    ///         .is_not_null();
    /// assert_eq!(
    ///     "select top 1 [name] from [employers] where [benefits] is not null ",
    ///     f.as_str()
    /// );
    /// ```
    pub fn is_not_null(&self) -> Self {
        let b = format!("{}{}", self.buffer, IS_NOT_NULL);
        SqlBuilder { buffer: b }
    }
}

/// Handles data input and output to our
/// data store.
///
/// Author: A.E.Veltstra
/// Since: 2.19.501.900
/// Version: 2.20.226.1945
///
//The 1 method in this module returns a Result.
use Result;

//The sqlite crate lets us talk to an sqlite data store.
extern crate sqlite;
//The Result return by the 1 method in thid module
//contains an Sqlite Connection.
//But in case of failure, we might get an Sqlite
//Error, instead.

/// Creates a connection to our data store.
///
/// # Examples
/// ```rust
/// extern crate db;
/// use db;
///
/// let maybe_connection = db::try_connect();
/// assert!(maybe_connection.is_ok());
///
/// ```
pub fn try_connect() -> Result<sqlite::Connection, sqlite::Error> {
    //fetch the database locator
    let database_url = "/home/dave/dev/mambro/data/db.sqlite3";

    //attempt to create and return the db connection
    sqlite::open(&database_url)
}

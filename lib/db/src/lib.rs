#![recursion_limit = "128"]
/// Handles data input and output to our
/// data store.
///
/// Author: A.E.Veltstra
/// Since: 2.19.501.900
/// Version: 2.19.924.2148

// For compiling and debugging diesel, we are 
// required to increase the recursion limit.

// Prelude contains things that should have
// been included in the main diesel crate,
// but whatever.
extern crate diesel;
use diesel::prelude::*;

// DotEnv lets us read configurations from
// a local file.
extern crate dotenv;
use dotenv::dotenv;

// Env gives access to the world outside
// of rust and the app.
use std::env;

/// Creates a connection to our data store.
///
/// # Examples
/// ```rust
/// extern crate db;
/// use db;
///
/// let connection = db::connect();
/// ```
pub fn connect() -> SqliteConnection {
    //initialize dotenv: we use that to read the 
    //local db url, to avoid hard-coding.
    dotenv().ok();

    //fetch the database locator
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set. Create a file named '.env' at the root of the project folder. Add a line starting with 'DATABASE_URL', add an = sign, and then add the absolute path to the sqlite3 database file.");

    //create and return the db connection
    SqliteConnection::establish(&database_url).expect(&format!("Error connecting to db. Attempting to connect to {}.", &database_url))
}

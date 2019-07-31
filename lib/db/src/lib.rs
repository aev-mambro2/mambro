// Handles data input and output to our
// data store.
//
// Author: A.E.Veltstra
// Since: 2.19.501.900
// Version: 2.19.730.730

// For compiling and debugging diesel,
// we are required to increase the
// recursion limit.
#![recursion_limit = "128"]

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

// Creates a connection to our data store.
//
// # Examples
// ```
// extern crate db;
// use db;
//
// let connection = db::connect();
// ```
pub fn connect() -> SqliteConnection {
    //initialize the component
    dotenv().ok();

    //fetch the database locator
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    //create and return the db connection
    SqliteConnection::establish(&database_url).expect("Error connecting to db.")
}

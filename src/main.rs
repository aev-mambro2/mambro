mod com;
#[macro_use]
extern crate diesel;
extern crate dotenv;

use crate::com::mambro::db as db;
use crate::com::mambro::models as models;
use crate::com::mambro::schema as schema;
use db::*;
use models::*;
use schema::*;
use self::diesel::prelude::*;

fn main() {
    use schema::accounts::dsl::*;

    let connection = db::establish_connection();
    let results = accounts.filter(id.eq("1"))
        .limit(5)
        .load::<models::Accounts>(&connection)
        .expect("Error loading accounts");

    println!("Displaying {} accounts", results.len());
    for acc in results {
        println!("{}", acc.id);
        println!("----------\n");
        println!("{}", acc.third_party);
    }
}

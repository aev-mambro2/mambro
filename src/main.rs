mod com;
#[macro_use]
extern crate diesel;
extern crate dotenv;

use crate::com::mambro::db as db;
use db::models as models;
use db::schema as schema;
use self::diesel::prelude::*;

fn main() {
    use schema::accounts::dsl::*;

    let connection = db::connect();
    let results = accounts.filter(thirdParty.eq("Joor"))
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

extern crate diesel;

use crate::com::mambro::*;
use crate::com::mambro::models::*;
use self::diesel::prelude::*;

fn main() {
    use crate::com::mambro::schema::accounts::dsl::*;

    let connection = establish_connection();
    let results = accounts.filter(id.eq("1"))
        .limit(5)
        .load::<accounts>(&connection)
        .expect("Error loading accounts");

    println!("Displaying {} accounts", results.len());
    for acc in results {
        println!("{}", acc.id);
        println!("----------\n");
        println!("{}", acc.thirdParty);
    }
}

#![recursion_limit = "128"]
#[macro_use]
extern crate diesel;
extern crate dotenv;

mod com;
use crate::com::mambro::domain;

fn nope_main() {
    match domain::attempt_load_account(String::from("one"), String::from("two")) {
        None => {}
        Some(ref account) => {
            println!("3rd party: {:?}", account.config_id.third_party_id);
        }
    }
}

fn main() {
    let a = domain::AccountId("one".to_string());
    let t = domain::ThirdPartyId("two".to_string());
    println!("{:?}", a);
    println!("{:?}", t);
}

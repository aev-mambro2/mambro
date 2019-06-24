#![recursion_limit = "128"]
#[macro_use]
extern crate diesel;
extern crate dotenv;

mod com;
use crate::com::mambro::domain;

fn nope_main() {
    match domain::attempt_load_account("one", "two") {
        None => {}
        Some(ref account) => {
            println!("3rd party: {:?}", account.config_id.third_party_id);
        }
    }
}

fn main() {
    let a = domain::AccountId::from("one");
    let t = domain::ThirdPartyId::from("two");
    println!("{:?}", a);
    println!("{:?}", t);
}

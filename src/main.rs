#![recursion_limit = "128"]
#[macro_use]
extern crate diesel;
extern crate dotenv;

mod com;
use crate::com::mambro::domain;
use std::env;
use std::time::SystemTime;

fn main() {
    let start = SystemTime::now();
    let args: Vec<String> = env::args().collect();
    let args_amount = args.len();
    let app_name = &args[0];
    if 2 < args_amount {
        let account_id = &args[1].as_str();
        let third_party_id = &args[2].as_str();
        println!("Running for {:?}@{:?}.", account_id, third_party_id);
    match domain::attempt_load_account(account_id, third_party_id) {
        None => {}
        Some(ref account) => {
            println!("Account: {:?}", account.config_id.to_string());
        }
    }
        println!("Usage: {:?} account thirdparty\nin which blah blah", app_name);
    }
    let end = SystemTime::now();
    let duration = end.duration_since(start).expect("Time went backwards?");
    println!("App ran for {:#?}, from {:#?} to {:#?}.", duration, start, end);
}


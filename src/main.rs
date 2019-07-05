#![recursion_limit = "128"]
#[macro_use]
extern crate diesel;
extern crate dotenv;

#[cfg(test)]
mod tests {
    #[test]
    fn can_create_file_location() {
        use crate::com::mambro::domain;
        let p = String::from("orders");
        let f = String::from("/usr/dave/orders");
        let n = String::from("order-{id}-dd-{dt}.xml");
        let fl = domain::FileLocation::new(&p, &f, &n);
        assert_eq!(fl.name.to_string(), n);
    }
}

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
            None => {
                println!("No account found for id {:?} at third party {:?}.", account_id, third_party_id);
            }
            Some(ref account) => {
                println!("Account found: {:?}.", account.config_id.to_string());
            }
        }
    } else {
        println!(
            "Usage: {:?} account thirdparty\nin which account should be the identifier of a record in the configurations data store, \nand thirdparty should be the identifier of that record's third party. \nTogether they form a unique identification we like to call a ConfigId.",
            app_name
        );
    }
    let end = SystemTime::now();
    let duration = end.duration_since(start).expect("Time went backwards?");
    println!(
        "App ran for {:#?}, from {:#?} to {:#?}.",
        duration, start, end
    );
}

// For compiling and debugging diesel,
// we are required to increase the 
// recursion limit.
#![recursion_limit = "128"]

// Diesel requires the use of macros.
#[macro_use]

// Diesel handles our data store.
extern crate diesel;

// DotEnv handles a local configuration file.
extern crate dotenv;

///! Downloads new orders from Joor for 
///! accounts like Habitual Denim.
///! For installation instructions, refer to
///! installing.md.
///! 
///! Author: A.E. Veltstra
///! Since: 2.19.501.900
///! Version: 2.19.726.1804

/// Unit tests for this module.
#[cfg(test)]
mod tests {
    // The domain crate must let us create 
    // a file location instance.
    #[test]
    fn can_create_file_location() {
        use crate::com::mambro::domain;
        let p = "orders";
        let f = "/usr/dave/orders";
        let n = "order-{id}-dd-{dt}.xml";
        let fl = domain::FileLocation::new(p, f, n);
        // We can't compare name against &str (yet).
        assert_eq!(fl.name.to_string(), n.to_string());
    }
}

mod com;
use crate::com::mambro::domain;
use std::env;
use std::time::SystemTime;
//use glob::glob;

/// Execution entry point.
/// Resolves command-line arguments,
/// fetches a matching account, and 
/// downloads its orders. Orders 
/// get saved in the location set in
/// the data store for the account.
/// Warnings and errors get logged.
///
/// # Example run invocation: 
/// ```
/// > com/mambro/joor/download/orders --account HaCoSandbox
/// ```
/// 
/// # Panics
/// If we failed to connect to the data store.
///
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
            None => println!(
                "No account found for id {:?} at third party {:?}.",
                account_id, third_party_id
            ),
            Some(ref account) => {
                println!(
                    "File locations for account {:?}:",
                    account.config_id.to_string()
                );
                let mut iter = account.locations.iter();
                while let Some(loc) = iter.next() {
                    println!("- {:?}: {:?}", loc.purpose, loc.to_path())
                }
                let o = "orders";
                match account
                    .locations
                    .iter()
                    .find(|&it| it.purpose.eq(&o))
                {
                    None => println!(
                        "No location for purpose {} for account {:?}.",
                        o,
                        account.config_id.to_string()
                    ),
                    Some(ref there) => println!(
                        "Location for purpose {} for account {:?}: {:?}.",
                        o,
                        account.config_id.to_string(),
                        there.to_path()
                    ),
                }
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

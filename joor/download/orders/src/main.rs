// Downloads new orders from Joor for
// accounts like Habitual Denim.
// For installation instructions, refer to
// installing.md.
//
// Author: A.E. Veltstra
// Since: 2.19.501.900
// Version: 2.19.801.1236

// For compiling and debugging diesel,
// we are required to increase the
// recursion limit.
#![recursion_limit = "128"]

// Domain holds semantically significant
// data structures and maps them to
// technical data store structures.
extern crate domain;

// Unit tests for this module.
#[cfg(test)]
mod tests {
    // The domain crate must let us create
    // a file location instance.
    #[test]
    fn can_create_file_location() {
        use domain;
        use std::path::Path;
        let p = "orders";
        let f = "/usr/dave/orders";
        let n = "order-{id}-dd-{dt}.xml";
        let fl = domain::FileLocation::new(p, f, n);
        // We can't compare path against &str (yet).
        assert_eq!(fl.to_path(), Path::new(f).join(n));
    }

    // The function find_account_id must find
    // the expected array element based on
    // its sort order immediately following
    // an element containing "--account".
    #[test]
    fn can_locate_account_id() {
        let mut args: Vec<String> = Vec::new();
        args.push(String::from("nothing"));
        args.push(String::from("nothing"));
        args.push(String::from("--account"));
        args.push(String::from("HaCoSandbox"));
        args.push(String::from("nothing"));
        let found = crate::find_account_id(&args);
        assert_eq!(true, found.is_some());
        assert_eq!(Some("HaCoSandbox"), found);
    }

    // The function find_third_party_id must
    // find the expected array element based
    // on its sort order immediately aftee
    // an element containing "--thirdparty".
    #[test]
    fn can_locate_third_party_id() {
        let mut args: Vec<String> = Vec::new();
        args.push(String::from("--account"));
        args.push(String::from("HaCoSandbox"));
        args.push(String::from("--thirdparty"));
        args.push(String::from("Joor"));
        let found = crate::find_third_party_id(&args);
        assert_eq!(true, found.is_some());
        assert_eq!(Some("Joor"), found);
    }
}

use std::env;
use std::time::SystemTime;
//use glob::glob;

// Rummages through the input to locate and
// return something that might be an account
// id. This assumes input is command-line
// arguments, with one argument equaling
// `--account`, followed by the identifier.
//
// # Examples
// ```
// let mut args: Vec<String> = Vec::new();
// args.push(String::from("nothing"));
// args.push(String::from("nothing"));
// args.push(String::from("--account"));
// args.push(String::from("HaCoSandbox"));
// args.push(String::from("nothing"));
// let found = find_account_id(&args);
// assert_eq!(true, found.is_some());
// assert_eq!(Some("HaCoSandbox"), found);
// ```
fn find_account_id(args: &Vec<String>) -> Option<&str> {
    let mut i = args.len() - 1;
while 1 < i {
    i = i - 1;
    let y: &str = &*args[i];
    if y.eq("--account") {
        let x = i + 1;
        return Some(&*args[x]);
    }
}
None
}

// Rummages through the input to locate and
// return something that might be a third-
// party id. This assumes input is command-
// line arguments, with one argument equaling
// `--thirdparty`, followed by the identifier.
//
// # Examples
// ```
// let mut args: Vec<String> = Vec::new();
// args.push(String::from("nothing"));
// args.push(String::from("--thirdparty"));
// args.push(String::from("Joor"));
// args.push(String::from("nothing"));
// args.push(String::from("nothing"));
// let found = find_third_party_id(&args);
// assert_eq!(true, found.is_some());
// assert_eq!(Some("Joor"), found);
// ```
fn find_third_party_id(args: &Vec<String>) -> Option<&str> {
    let mut i = args.len() - 1;
    while 1 < i {
        i = i - 1;
        let y: &str = &*args[i];
        if y.eq("--thirdparty") {
            let x = i + 1;
            return Some(&*args[x]);
        }
    }
    None
}

// Execution entry point.
// Resolves command-line arguments,
// fetches a matching account, and
// downloads its orders. Orders
// get saved in the location set in
// the data store for the account.
// Warnings and errors get logged.
//
// # Example run invocation:
// ```
// > mambro/joor/download/orders --account HaCoSandbox --thirdparty Joor
// ```
//
// # Panics
// If we failed to connect to the data store.
//
fn main() {
    let start = SystemTime::now();
    let args: Vec<String> = env::args().collect();
    let maybe_account_id = find_account_id(&args);
    let maybe_third_party_id = find_third_party_id(&args);
    let mut account_id = "";
    if maybe_account_id.is_none() {
        println!("Missing CL parameter `--account` followed by an account identifier.");
    } else {
        account_id = maybe_account_id.unwrap_or_default();
    }
    let mut third_party_id = "";
    if maybe_third_party_id.is_none() {
        println!("Missing CL parameter `--thirparty` followed by a third party name.");
    } else {
        third_party_id = maybe_third_party_id.unwrap_or_default();
    }
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
            match account.locations.iter().find(|&it| it.purpose.eq(&o)) {
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
    let end = SystemTime::now();
    let duration = end.duration_since(start).expect("Time went backwards?");
    println!("App ran for {:#?}, from {:#?}", duration, start);
}

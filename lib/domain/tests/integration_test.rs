//! Tests the domain crate in ways that depend on context and time.
//!
//! Author: A.E.Veltstra
//! Since: 2.19.501.900
//! Version: 2.20.228.2012

#[cfg(test)]
//The crate under test:
extern crate domain;
use domain::*;

/// The function domain::fetch_account should be able to
/// access the data store and fetch a known account
/// without panicking.
#[test]
pub fn test_fetch_known_account() {
    let connection = db::try_connect().unwrap();
    let maybe_account = fetch_account(&connection, "HaCo", "Joor");
    match maybe_account {
        Some(account) => assert!(!account.is_empty()),
        None => panic!("Expected to have found the known account."),
    }
}

/// The function domain::fetch_account should be able to
/// access the data store and not fetch an unknown account
/// without panicking.
#[test]
pub fn test_fetch_unknown_account() {
    let connection = db::try_connect().unwrap();
    let maybe_account = fetch_account(&connection, "Unknown", "Unknown");
    match maybe_account {
        Some(_) => panic!("Expected to have found no account."),
        None => assert!(true),
    }
}

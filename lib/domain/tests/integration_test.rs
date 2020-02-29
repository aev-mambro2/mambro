//! Tests the domain crate in ways that depend on context and time.
//!
//! Author: A.E.Veltstra
//! Since: 2.19.501.900
//! Version: 2.20.228.2238

#[cfg(test)]
//The crate under test:
extern crate domain;
use domain::*;

/// The function domain::check_config_id should be able to
/// access the data store and fetch a known config id
/// without panicking.
#[test]
pub fn test_fetch_known_config_id() {
    let connection = db::try_connect().unwrap();
    let maybe_config_id = check_config_id(
        &connection, 
        "HaCo", 
        "Joor"
    );
    match maybe_config_id {
        Some(config_id) => {
                assert!(!config_id.is_empty());
                let account_id = config_id.account_id;
                let third_party_id = config_id.third_party_id;
                assert_eq!(
                    "HaCo", 
                    &account_id.to_string())
                ;
                assert_eq!(
                    "Joor", 
                    &third_party_id.to_string()
                );
            },
        None => panic!("Expected to have found the known config id."),
    }
}

/// The function domain::check_config_id should be able to
/// access the data store and not fetch an unknown config id
/// without panicking.
#[test]
pub fn test_fetch_unknown_config_id() {
    let connection = db::try_connect().unwrap();
    let maybe_config_id = check_config_id(
          &connection, 
          "Unknown", 
          "Unknown"
    );
    match maybe_config_id {
        Some(_) => panic!("Expected to have found no config id."),
        None => assert!(true),
    }
}

/// The function domain::attempt_load_account() 
/// should be able to load common account assets for 
/// a known config id without panicking.
#[test]
pub fn test_fetch_known_account() {
    let account_id = "HaCo";
    let third_party_id = "Joor";
    let attempt = attempt_load_account(
        account_id, 
        third_party_id
    );
    match attempt {
        None => panic!("Expected to have loaded known account."),
        Some(account) => {
            let config_id = account.config_id;
            assert_eq!(
                account_id, 
                &config_id.account_id.to_string()
            );
            assert_eq!(
                third_party_id, 
                &config_id.third_party_id.to_string()
            );
        }
    }
}

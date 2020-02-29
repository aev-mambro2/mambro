/// Integration tests for the db crate.
///
/// Author: A.E.Veltstra
/// Since: 2.19.501.900
/// Version: 2.20.226.1945
///
use db::try_connect;

/// Attempts to set up a connection to the data store.
/// If this test fails, check the database_url in db::try_connect().
#[test]
fn can_connect () {
    let maybe = try_connect();
    assert!(maybe.is_ok());
}



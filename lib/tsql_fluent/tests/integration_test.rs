///! Integration tests for the crate tsql_fluent.
///!
///! Author: A.E.Veltstra
///! Since: 2.20.226.1900
///! Version: 2.20.226.2215
///!

#[cfg(test)]

/// The data store should hold a ledger named "accounts".
/// Each of its records is an account. The tested crate
/// together with the db crate (which connects to the
/// data store) should be able to count how many records
/// the accounts ledger contains. The amount can be 0
/// (zero) or more. The methods should not panic.
#[test]
fn can_count_accounts() {
    let mut found = -1;
    assert!(-1 == found);
    match db::try_connect() {
        Ok(conn) => {
            let mut statement = conn
                .prepare(
                    tsql_fluent::select()
                        .count_all()
                        .alias("amount".to_string())
                        .from("accounts".to_string())
                        .to_string(),
                )
                .unwrap();
            statement.next().unwrap();
            found = statement.read::<i64>(0).ok().unwrap();
        }
        Err(e) => {
            let msg = format!("{}", &e);
            panic!(msg);
        }
    }
    assert!(-1 < found);
}

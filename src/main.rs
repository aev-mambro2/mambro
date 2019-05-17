mod com;
use crate::com::mambro::domain::{
    Account, AccountId, ConfigId, Credentials, FileLocation, FileName, FolderName, IMaybeEmpty,
    Key, Secret, ThirdPartyId, Token, URI,
};
use std::collections::HashMap;
use std::error::Error;
use std::result;

fn build(map: HashMap<String, String>) -> result::Result<Account, Box<Error>> {
    let account = Account {
        config_id: ConfigId {
            account_id: AccountId(String::from("a")),
            third_party_id: ThirdPartyId(String::from("b")),
        },
        credentials: Credentials {
            uri: URI(String::from("https://here/")),
            app: Token {
                key: Key(String::from("a")),
                secret: Secret(String::from("b")),
            },
            user: Token {
                key: Key(String::from("a")),
                secret: Secret(String::from("b")),
            },
        },
        location: FileLocation {
            folder: FolderName(String::from("1")),
            name: FileName(String::from("2")),
        },
    };
    if account.is_empty() {
        return Err(Box::from("Account is empty."));
    }
    return Ok(account);
}

fn main() {
    let maybe_account = build(HashMap::new());
    match maybe_account {
        Ok(account) => {
            panic_if_empty(&account);
            //println!("{:#?}", account);
            let account_id = account.config_id.account_id;
            println!("{:#?}", account_id.0);
        }
        Err(e) => println!("{:#?}", e),
    };
    fn panic_if_empty(account: &Account) {
        if account.is_empty() {
            panic!("Stopping: account is empty.{}", "");
        }
    }
}

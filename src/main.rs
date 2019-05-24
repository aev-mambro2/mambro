mod com;
extern crate secstr;
use crate::com::mambro::domain::{
    Account, AccountId, ConfigId, Credentials, FileLocation, FileLocationIntentions, FileName,
    FolderName, IMaybeEmpty, Key, Secret, ThirdPartyId, Token, URI,
};
use secstr::*;
use std::collections::HashMap;
use std::error::Error;
use std::result;

fn build(data: HashMap<&str, &str>) -> result::Result<Account, Box<Error>> {
    let account = Account {
        config_id: ConfigId {
            account_id: AccountId(data.get("id").unwrap_or(&"").to_string()),
            third_party_id: ThirdPartyId(data.get("third_party_id").unwrap_or(&"").to_string()),
        },
        credentials: Credentials {
            uri: URI(data.get("uri").unwrap_or(&"").to_string()),
            app: Token {
                key: Key(SecStr::from(data.get("app_key").unwrap_or(&"").to_string())),
                secret: Secret(SecStr::from(data.get("app_secret").unwrap_or(&"").to_string())),
            },
            user: Token {
                key: Key(SecStr::from(data.get("user_key").unwrap_or(&"").to_string())),
                secret: Secret(SecStr::from(data.get("user_token").unwrap_or(&"").to_string())),
            },
        },
        location: FileLocation {
            intent: FileLocationIntentions::ReadFrom,
            folder: FolderName(data.get("readfrom_folder").unwrap_or(&"").to_string()),
            name: FileName(data.get("readfrom_name").unwrap_or(&"").to_string()),
        },
    };
    if account.is_empty() {
        return Err(Box::from("Account is empty."));
    }
    Ok(account)
}

fn main() {
    let mut account_data = HashMap::new();
    account_data.insert("id", "a");
    account_data.insert("third_party_id", "b");
    account_data.insert("uri", "https://here/");
    account_data.insert("app_key", "a");
    account_data.insert("app_secret", "b");
    account_data.insert("user_key", "c");
    account_data.insert("user_token", "d");
    account_data.insert("readfrom_folder", "1");
    account_data.insert("readfrom_name", "2");

    let maybe_account = build(account_data);
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

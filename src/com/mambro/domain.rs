extern crate diesel;
extern crate secstr;
use crate::com::mambro::db;
use crate::com::mambro::domain;
use db::models;
use secstr::*;

#[derive(Debug)]
pub struct AccountId(pub Vec<u8>);
impl From<&str> for AccountId {
    fn from(s: &str) -> Self {
        AccountId(s.as_bytes().to_vec())
    }
}
impl AccountId {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}
impl From<&String> for AccountId {
    fn from(s: &String) -> Self {
        AccountId(s.into_bytes().clone())
    }
}

#[derive(Debug)]
pub struct FileExtension(pub Vec<u8>);
impl From<&str> for FileExtension {
    fn from(s: &str) -> Self {
        FileExtension(s.as_bytes().to_vec())
    }
}
impl From<&String> for FileExtension {
    fn from(s: &String) -> Self {
        FileExtension(s.into_bytes().clone())
    }
}
impl FileExtension {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

#[derive(Debug)]
pub struct FileName(pub Vec<u8>);
impl From<&str> for FileName{
    fn from(s: &str) -> Self {
        FileName(s.as_bytes().to_vec())
    }
}
impl From<&String> for FileName {
    fn from(s: &String) -> Self {
        FileName(s.into_bytes().clone())
    }
}
impl FileName {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

#[derive(Debug)]
pub struct FileLocationPurpose(pub Vec<u8>);
impl From<&str> for FileLocationPurpose{
    fn from(s: &str) -> Self {
        FileLocationPurpose(s.as_bytes().to_vec())
    }
}
impl From<&String> for FileLocationPurpose {
    fn from(s: &String) -> Self {
        FileLocationPurpose(s.into_bytes().clone())
    }
}
impl FileLocationPurpose {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

#[derive(Debug)]
pub struct FolderName(pub Vec<u8>);
impl From<&str> for FolderName {
    fn from(s: &str) -> Self {
        FolderName (s.as_bytes().to_vec())
    }
}
impl From<&String> for FolderName {
    fn from(s: &String) -> Self {
        FolderName(s.into_bytes().clone())
    }
}
impl FolderName {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

pub type Key = SecStr;

pub type Secret = SecStr;

#[derive(Debug)]
pub struct ThirdPartyId(pub Vec<u8>);
impl From<&str> for ThirdPartyId {
    fn from(s: &str) -> Self {
        ThirdPartyId(s.as_bytes().to_vec())
    }
}
impl From<&String> for ThirdPartyId {
    fn from(s: &String) -> Self {
        ThirdPartyId(s.into_bytes().clone())
    }
}
impl ThirdPartyId {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

#[derive(Debug)]
pub struct URI(pub Vec<u8>);
impl From<&str> for URI {
    fn from(s: &str) -> Self {
        URI(s.as_bytes().to_vec())
    }
}
impl From<&String> for URI {
    fn from(s: &String) -> Self {
        URI(s.into_bytes().clone())
    }
}
impl URI {
    fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

pub trait IMaybeEmpty {
    fn is_empty(&self) -> bool;
}

impl IMaybeEmpty for AccountId {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for FileName {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for FileExtension {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for FileLocationPurpose {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for FolderName {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for SecStr {
    fn is_empty(&self) -> bool {
        self.unsecure().len() == 0
    }
}

impl IMaybeEmpty for ThirdPartyId {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for URI {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug)]
pub struct ConfigId {
    pub account_id: AccountId,
    pub third_party_id: ThirdPartyId,
}
impl IMaybeEmpty for ConfigId {
    fn is_empty(&self) -> bool {
        self.account_id.is_empty() && self.third_party_id.is_empty()
    }
}
impl From<&models::Accounts> for ConfigId {
    fn from(acc: &models::Accounts) -> Self {
        ConfigId {
            account_id: AccountId::from(&acc.id),
            third_party_id: ThirdPartyId::from(&acc.third_party),
        }
    }
}
impl ConfigId {
    fn to_string(&self) -> String {
        [self.account_id.to_string(), self.third_party_id.to_string()].join("@")
    }
}

#[derive(Debug)]
pub struct Token {
    pub key: Key,
    pub secret: Secret,
}
impl IMaybeEmpty for Token {
    fn is_empty(&self) -> bool {
        self.key.is_empty() && self.secret.is_empty()
    }
}
impl Token {
    fn from(key: &String, secret: &String) -> Self {
        Token {
            key: Key::from(key),
            secret: Secret::from(secret),
        }
    }
}

#[derive(Debug)]
pub struct Credentials {
    pub uri: URI,
    pub app: Token,
    pub user: Token,
}
impl IMaybeEmpty for domain::Credentials {
    fn is_empty(&self) -> bool {
        self.uri.is_empty() && self.app.is_empty() && self.user.is_empty()
    }
}
impl From<&models::Credentials> for domain::Credentials {
    fn from(creds: &models::Credentials) -> Self {
        domain::Credentials {
            uri: URI::from(&creds.uri),
            app: Token::from(&creds.app_key, &creds.app_secret),
            user: Token::from(&creds.user_key, &creds.user_secret),
        }
    }
}

#[derive(Debug)]
pub struct FileLocation {
    pub purpose: FileLocationPurpose,
    pub folder: FolderName,
    pub name: FileName,
}
impl IMaybeEmpty for FileLocation {
    fn is_empty(&self) -> bool {
        self.folder.is_empty() && self.name.is_empty()
    }
}
impl FileLocation {
    fn extension(&self) -> FileExtension {
        FileExtension::from("")
    }
    fn to_string(&self) -> String {
        [self.folder.to_string(), self.name.to_string()].join("/")
    }
}
impl From<&models::FileLocations> for domain::FileLocation {
    fn from(it: &models::FileLocations) -> Self {
        domain::FileLocation {
            purpose: FileLocationPurpose::from(&it.purpose),
            folder: FolderName::from(&it.folder),
            name: FileName::from(&it.name),
        }
    }
}
impl domain::FileLocation {
    fn fromAll(them: &Vec<models::FileLocations>) -> Vec<domain::FileLocation> {
        let mut buffer: Vec<domain::FileLocation> = Vec::with_capacity(them.len());
        for x in 0..them.len() {
            buffer.push(domain::FileLocation::from(&them[x]));
        }
        return buffer;
    }
    fn new(p: &String, f: &String, n: &String) -> domain::FileLocation {
        FileLocation {
            purpose: FileLocationPurpose::from(p),
            folder: FolderName::from(f),
            name: FileName::from(n),
        }
    }
}

#[derive(Debug)]
pub struct Account {
    pub config_id: domain::ConfigId,
    pub credentials: domain::Credentials,
    pub locations: Vec<domain::FileLocation>,
}
impl IMaybeEmpty for Account {
    fn is_empty(&self) -> bool {
        self.config_id.is_empty() && self.credentials.is_empty() && self.locations.is_empty()
    }
}

fn fetch_account(account_id: &str, third_party: &str) -> Option<ConfigId> {
    use self::diesel::prelude::*;
    let connection = db::connect();
    use db::schema;
    use schema::accounts::dsl::*;
    let results = accounts
        .filter(thirdParty.eq(&third_party))
        .filter(id.eq(&account_id))
        .limit(1)
        .load::<models::Accounts>(&connection)
        .expect("Error loading accounts");
    if results.is_empty() {
        return None;
    }
    return Some(ConfigId::from(&results[0]));
}

fn fetch_credentials(config_id: &ConfigId) -> Option<domain::Credentials> {
    use self::diesel::prelude::*;
    let connection = db::connect();
    use db::schema;
    use schema::credentials::dsl::*;
    let results = credentials
        .filter(thirdParty.eq(&config_id.third_party_id.to_string()))
        .filter(account.eq(&config_id.account_id.to_string()))
        .limit(1)
        .load::<models::Credentials>(&connection)
        .expect("Error loading credentials");
    if results.is_empty() {
        return None;
    } else {
        return Some(domain::Credentials::from(&results[0]));
    }
}

fn fetch_file_locations(config_id: &ConfigId) -> Vec<domain::FileLocation> {
    use self::diesel::prelude::*;
    let connection = db::connect();
    use db::schema;
    use schema::fileLocation::dsl::*;
    let results = fileLocation
        .filter(thirdParty.eq(&config_id.third_party_id.to_string()))
        .filter(account.eq(&config_id.account_id.to_string()))
        .load::<models::FileLocations>(&connection)
        .expect("Error loading file locations");
    if results.is_empty() {
        return vec![];
    } else {
        return domain::FileLocation::fromAll(&results);
    }
}

pub fn attempt_load_account(id: &str, third_party: &str) -> Option<Account> {
    match fetch_account(&id, &third_party) {
        None => None,
        Some(ref config_id) => match fetch_credentials(config_id) {
            None => None,
            Some(ref creds) => Some(domain::Account {
                config_id: *config_id,
                credentials: *creds,
                locations: fetch_file_locations(config_id),
            }),
        },
    }
}

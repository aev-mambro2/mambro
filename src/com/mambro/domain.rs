///! Domain contains data types that are
///! used in data integration applications
///! that connect local ERPs to remote 
///! channels, a.k.a. third parties.
///!
///! Those often require account inform-
///! ation and credentials for authoriz-
///! ing communications. 
///!
///! Each account tends to read the info
///! it needs to send from file locations
///! that are marked for a particular
///! communication purpose.
///!
///! When communications have completed,
///! users and managers expect some notific-
///! ation. Thus, each account owns a list
///! of email addresses, each of which is 
///! marked for a purpose, and sometimes 
///! exclusive to an application.

/// Diesel accesses data stores.
extern crate diesel;
/// SecStr destroys sensitive data from
/// memory once done.
extern crate secstr;
/// Mambro db knows the layout of the 
/// data store.
use crate::com::mambro::db;
/// Hey, look, that's me!
use crate::com::mambro::domain;
/// Db models contain rust-based data 
/// wrappers around the data store's 
/// structures.
use db::models;
use secstr::*;
/// OsStr is used for O.S.-specific texts
/// (like file paths and names), that can
/// contain characters which don't fit into
/// utf-8.
use std::ffi::OsStr;
/// Fmt helps us share human-readable views 
/// on the data retained by the data types 
/// in this module.
use std::fmt;
/// Path and PathBuf handle file locations.
use std::path::{Path, PathBuf};

/// Identifies an account. Gets combined 
/// with ThirdPartyId in ConfigId, which
/// must be unique.
///
/// Assets like email addresses and file
/// locations get assigned to ConfigIds.
#[derive(Clone, Debug, PartialEq)]
pub struct AccountId(Vec<u8>);
impl From<&str> for AccountId {
    fn from(s: &str) -> Self {
        AccountId::from(s)
    }
}
impl From<String> for AccountId {
    fn from(s: String) -> Self {
        AccountId::from(s.as_str())
    }
}
impl AccountId {
    fn from(s: &str) -> AccountId {
        AccountId(s.as_bytes().to_vec())
    }
    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

/// The intended use of a file location. 
/// Gets combined with a folder path
/// and file name in a FileLocation.
#[derive(Clone, Debug, PartialEq)]
pub struct FileLocationPurpose(Vec<u8>);
impl PartialEq<&str> for FileLocationPurpose {
    fn eq(&self, s: &&str) -> bool {
        let other = FileLocationPurpose::from(*s);
        self.eq(&other)
    }
}
/// To make the purpose display as a word,
/// rather than a byte array. Failed.
impl fmt::Display for FileLocationPurpose {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.to_string(), f)
    }
}
impl From<&str> for FileLocationPurpose {
    fn from(s: &str) -> Self {
        FileLocationPurpose::from(s)
    }
}
impl From<String> for FileLocationPurpose {
    fn from(s: String) -> Self {
        FileLocationPurpose::from(s.as_str())
    }
}
impl FileLocationPurpose {
    fn from(s: &str) -> FileLocationPurpose {
        FileLocationPurpose(s.as_bytes().to_vec())
    }
    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

/// Identifies a remote party. Gets combined
/// with an AccountId in a ConfigId, which
/// must be unique.
///
/// Assets like file locations and email 
/// addresses get assigned to ConfigIds.
#[derive(Clone, Debug, PartialEq)]
pub struct ThirdPartyId(Vec<u8>);
impl From<&str> for ThirdPartyId {
    fn from(s: &str) -> Self {
        ThirdPartyId::from(s)
    }
}
impl From<String> for ThirdPartyId {
    fn from(s: String) -> Self {
        ThirdPartyId::from(s.as_str())
    }
}
impl ThirdPartyId {
    fn from(s: &str) -> ThirdPartyId {
        ThirdPartyId(s.as_bytes().to_vec())
    }
    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

/// The address via which communication 
/// should happen for an account at a 
/// third party. Gets combined with 
/// authentication tokens in Credentials.
///
/// This may be replaced with a standard 
/// data type.
#[derive(Clone, Debug, PartialEq)]
pub struct URI(Vec<u8>);
impl From<&str> for URI {
    fn from(s: &str) -> Self {
        URI::from(s)
    }
}
impl From<String> for URI {
    fn from(s: String) -> Self {
        URI::from(s.as_str())
    }
}
impl URI {
    fn from(s: &str) -> URI {
        URI(s.as_bytes().to_vec())
    }
    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

/// Indicates that a data type instance 
/// may contain no data at all.
pub trait IMaybeEmpty {
    /// Whether the instance contains anything.
    fn is_empty(&self) -> bool;
}

impl IMaybeEmpty for AccountId {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for FileLocationPurpose {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for PathBuf {
    fn is_empty(&self) -> bool {
        self.as_os_str().is_empty()
    }
}

impl IMaybeEmpty for SecUtf8 {
    fn is_empty(&self) -> bool {
        self.unsecure().is_empty()
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

/// A unique combination of AccountId and 
/// ThirdPartyId. Owns assets like email
/// addresses, file locations, credentials,
/// and communication addresses.
#[derive(Clone, Debug, PartialEq)]
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
            account_id: AccountId::from(&acc.id.as_str()),
            third_party_id: ThirdPartyId::from(&acc.third_party.as_str()),
        }
    }
}
impl ConfigId {
    pub fn to_string(&self) -> String {
        [self.account_id.to_string(), self.third_party_id.to_string()].join("@")
    }
}

/// Combines a key and a secret used for
/// authentication. Both are destroyed from 
/// memory after use.
///
/// In all cases encountered, 2 tokens and 
/// a URI suffice to make a Credential.
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub key: SecUtf8,
    pub secret: SecUtf8,
}
impl IMaybeEmpty for Token {
    fn is_empty(&self) -> bool {
        self.key.is_empty() && self.secret.is_empty()
    }
}
impl Token {
    fn from(k: &str, s: &str) -> Self {
        Token {
            key: SecUtf8::from(k.as_bytes()),
            secret: SecUtf8::from(s.as_bytes()),
        }
    }
}

/// All that is needed to authenticate against
/// a communication address. An account tends
/// to have exactly 1 instance of Credentials.
#[derive(Clone, Debug, PartialEq)]
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
            uri: URI::from(&creds.uri.as_str()),
            app: Token::from(&creds.app_key.as_str(), &creds.app_secret.as_str()),
            user: Token::from(&creds.user_key.as_str(), &creds.user_secret.as_str()),
        }
    }
}

/// Determines where files can be located
/// and how they are named, what purpose 
/// they serve, and whether they are used 
/// for reading, writing, or both.
///
/// An account tends to own multiple file
/// locations for multiple purposes. An 
/// application tends to be built for a 
/// single such purpose.
#[derive(Clone, Debug, PartialEq)]
pub struct FileLocation {
    pub purpose: FileLocationPurpose,
    pub path: PathBuf,
}
impl IMaybeEmpty for FileLocation {
    fn is_empty(&self) -> bool {
        self.path.is_empty()
    }
}
impl From<&models::FileLocations> for domain::FileLocation {
    fn from(it: &models::FileLocations) -> Self {
        domain::FileLocation {
            purpose: FileLocationPurpose::from(&it.purpose.as_str()),
            path: Path::new(&it.folder).join(&it.name),
        }
    }
}
impl domain::FileLocation {
    pub fn extension(&self) -> Option<&OsStr> {
        self.path.extension()
    }
    fn from_all(them: &[models::FileLocations]) -> Vec<domain::FileLocation> {
        let mut buffer: Vec<domain::FileLocation> = Vec::with_capacity(them.len());
        for item in them {
            buffer.push(domain::FileLocation::from(item));
        }
        buffer.dedup();
        buffer
    }
    pub fn new(p: &str, f: &str, n: &str) -> domain::FileLocation {
        FileLocation {
            purpose: FileLocationPurpose::from(p),
            path: Path::new(f).join(n),
        }
    }
    pub fn to_path(&self) -> &Path {
        self.path.as_path()
    }
}

/// Combines the ConfigId, Credentials, 
/// FileLocations, and EmailAddresses 
/// owned by a single account.
#[derive(Clone, Debug, PartialEq)]
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

/// Attempts to fetch that ConfigId from the 
/// data store which matches the passed-in 
/// account id and third party id.
///
/// If not found, returns None.
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

/// Attempts to load from the data store those
/// Credentials that belong to the passed-in
/// ConfigId.
///
/// If not found, returns None.
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
        None
    } else {
        Some(domain::Credentials::from(&results[0]))
    }
}

/// Attempts to load from the data store those
/// file locations that belong to the passed-in
/// ConfigId.
/// 
/// If not found, returns None.
fn fetch_file_locations(config_id: &ConfigId) -> Vec<domain::FileLocation> {
    use self::diesel::prelude::*;
    let connection = db::connect();
    use db::schema;
    use schema::fileLocations::dsl::*;
    let results = fileLocations
        .filter(thirdParty.eq(&config_id.third_party_id.to_string()))
        .filter(account.eq(&config_id.account_id.to_string()))
        .load::<models::FileLocations>(&connection)
        .expect("Error loading file locations");
    if results.is_empty() {
        vec![]
    } else {
        domain::FileLocation::from_all(&results)
    }
}

pub fn attempt_load_account(id: &str, third_party: &str) -> Option<Account> {
    match fetch_account(&id, &third_party) {
        None => None,
        Some(config_id) => match fetch_credentials(&config_id) {
            None => None,
            Some(creds) => Some(domain::Account {
                config_id: config_id.clone(),
                credentials: creds.clone(),
                locations: fetch_file_locations(&config_id),
            }),
        },
    }
}

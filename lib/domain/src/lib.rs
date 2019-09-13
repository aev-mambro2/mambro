//! Domain contains data types that are used 
//! in data integration applications that 
//! connect local ERPs to remote channels, 
//! a.k.a. third parties.
//!
//! Those often require account information 
//! and credentials for authorizing 
//! communications.
//!
//! Each account tends to read the info it 
//! needs to send from file locations that 
//! are marked for a particular communication 
//! purpose.
//!
//! When communications have completed, users 
//! and managers expect some notification. 
//! Thus, each account owns a list of email 
//! addresses, each of which is marked for a 
//! purpose, and sometimes exclusive to an 
//! application.  
//!
//! Author: A.E.Veltstra  
//! Since: 2.19.501.900  
//! Version: 2.19.906.2131  

// For compiling and debugging diesel,
// we are required to increase the
// recursion limit.
#![recursion_limit = "128"]

/// Prelude contains things that should have
/// been included in the main diesel crate,
/// but whatever.
extern crate diesel;
use diesel::prelude::*;

/// SecStr destroys sensitive data from
/// memory once done.
extern crate secstr;
use secstr::*;

/// Mambro db knows the layout of the
/// data store.
extern crate db;

/// Db models contain rust-based data
/// wrappers around the data store's
/// structures.
extern crate models;

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
///
///
/// # Example
///
/// ```rust
/// use crate::domain::AccountId;  
/// let id = AccountId::from("Hot Topic");  
/// assert_eq!("Hot Topic".to_string(), id.to_string());  
/// ```
///
#[derive(Clone, Debug, PartialEq)]
pub struct AccountId(Vec<u8>);

/// Convert from a common string slice.
impl From<&str> for AccountId {
    /// # Arguments
    ///
    /// - &str: a name that identifies an
    ///         account. Should match a name
    ///         in the data store. The name
    ///         gets copied and owned by the
    ///         instance.
    ///
    fn from(s: &str) -> Self {
        AccountId::from(s)
    }
}

/// Converts from a common string.
impl From<String> for AccountId {
    /// # Arguments
    ///
    /// - String: a name that identifies an
    ///         account. Should match a name
    ///         in the data store. The name
    ///         gets copied and owned by the
    ///         instance.
    ///
    fn from(s: String) -> Self {
        AccountId::from(s.as_str())
    }
}


impl AccountId {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a name that identifies an
    ///         account. Should match a name
    ///         in the data store. The name
    ///         gets copied and owned by the
    ///         instance.
    ///
    fn from(s: &str) -> AccountId {
        AccountId(s.as_bytes().to_vec())
    }

    /// Returns an owned, immutable copy of
    /// the name retained in this instance.
    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

/// The intended use of a file location.
/// Gets combined with a folder path
/// and file name in a FileLocation.
///
///
/// # Example
///
/// ```rust
/// use crate::domain::FileLocationPurpose;  
/// let p = FileLocationPurpose::from("orders");  
/// assert_eq!("orders".to_string(), p.to_string());  
/// ```
///
#[derive(Clone, Debug, PartialEq)]
pub struct FileLocationPurpose(Vec<u8>);

// Documenting this is useless: 
// it gets documented using standard text
// yanked from elsewhere. 
impl PartialEq<&str> for FileLocationPurpose {
    fn eq(&self, s: &&str) -> bool {
        let other = FileLocationPurpose::from(*s);
        self.eq(&other)
    }
}
/// To make the purpose display as a word,
/// rather than a byte array. 
// Failed.
impl fmt::Display for FileLocationPurpose {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.to_string(), f)
    }
}
/// Convert from a common string slice.
impl From<&str> for FileLocationPurpose {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a name that identifies why a
    ///         location is used. Should 
    ///         match a purpose in the data 
    ///         store. The name gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: &str) -> Self {
        FileLocationPurpose::from(s)
    }
}

/// Convert from a common string.
impl From<String> for FileLocationPurpose {
    /// Converts from a common string.
    ///
    ///
    /// # Arguments
    ///
    /// - String: a name that identifies why a
    ///         location is used. Should 
    ///         match a purpose in the data 
    ///         store. The name gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: String) -> Self {
        FileLocationPurpose::from(s.as_str())
    }
}

impl FileLocationPurpose {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a name that identifies why a
    ///         location is used. Should 
    ///         match a purpose in the data 
    ///         store. The name gets copied 
    ///         and owned by the instance.
    ///
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
///
///
/// # Example
///
/// ```rust
/// use crate::domain::ThirdPartyId;  
/// let id = ThirdPartyId::from("Spencers");  
/// assert_eq!("Spencers".to_string(), id.to_string());  
/// ```
///
#[derive(Clone, Debug, PartialEq)]
pub struct ThirdPartyId(Vec<u8>);

/// Convert from a common string slice.
impl From<&str> for ThirdPartyId {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a name that identifies a
    ///         remote partner. Should match
    ///         a third party in the data 
    ///         store. The name gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: &str) -> Self {
        ThirdPartyId::from(s)
    }
}

/// Convert from a common string.
impl From<String> for ThirdPartyId {
    /// Converts from a common string.
    ///
    ///
    /// # Arguments
    ///
    /// - String: a name that identifies a
    ///         remote partner. Should match
    ///         a third party in the data 
    ///         store. The name gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: String) -> Self {
        ThirdPartyId::from(s.as_str())
    }
}

impl ThirdPartyId {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a name that identifies a
    ///         remote partner. Should match
    ///         a third party in the data 
    ///         store. The name gets copied 
    ///         and owned by the instance.
    ///
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
///
///
/// # Example
///
/// ```rust
/// use crate::domain::URI;  
/// let uri = URI::from("how://there.that/");  
/// assert_eq!("how://there.that/".to_string(), uri.to_string());  
/// ```
///
#[derive(Clone, Debug, PartialEq)]
pub struct URI(Vec<u8>);

/// Convert from a common string slice.
impl From<&str> for URI {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a web address to use for 
    ///         reaching out to the remote 
    ///         partner's web / REST servers. 
    ///         Suffix with / if the last 
    ///         part is a TLD or a folder. 
    ///         Omit the end / if the last
    ///         part is a document or query 
    ///         value.
    ///         The address gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: &str) -> Self {
        URI::from(s)
    }
}

/// Convert from a common string slice.
impl From<String> for URI {
    /// Converts from a common string..
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a web address to use for 
    ///         reaching out to the remote 
    ///         partner's web / REST servers. 
    ///         Suffix with / if the last 
    ///         part is a TLD or a folder. 
    ///         Omit the end / if the last
    ///         part is a document or query 
    ///         value.
    ///         The address gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: String) -> Self {
        URI::from(s.as_str())
    }
}

impl URI {
    /// Converts from a common string slice.
    ///
    ///
    /// # Arguments
    ///
    /// - &str: a web address to use for 
    ///         reaching out to the remote 
    ///         partner's web / REST servers. 
    ///         Suffix with / if the last 
    ///         part is a TLD or a folder. 
    ///         Omit the end / if the last
    ///         part is a document or query 
    ///         value.
    ///         The address gets copied 
    ///         and owned by the instance.
    ///
    fn from(s: &str) -> URI {
        URI(s.as_bytes().to_vec())
    }
    pub fn to_string(&self) -> String {
        String::from_utf8(self.0.as_slice().to_vec()).unwrap()
    }
}

// Indicates that a data type instance
// may contain no data at all.
pub trait IMaybeEmpty {
    // Whether the instance contains anything.
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

// A unique combination of AccountId and
// ThirdPartyId. Owns assets like email
// addresses, file locations, credentials,
// and communication addresses.
///
///
/// # Example
///
/// ```
/// use crate::domain::{AccountId, ConfigId, ThirdPartyId};
/// let config = ConfigId {
///   account_id: AccountId::from("hello"),
///   third_party_id: ThirdPartyId::from("world")
/// };
/// assert_eq!("hello@world".to_string(), config.to_string());
/// ```
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

// Combines a key and a secret used for
// authentication. Both are destroyed from
// memory after use.
//
// In all cases encountered, 2 tokens and
// a URI suffice to make a Credential.
///
///
/// # Example
///
/// ```
/// use crate::domain::Token;
/// extern crate secstr;
/// use secstr::*;
/// let app1 = Token {
///     key: SecUtf8::from("qwerty".as_bytes()),
///     secret: SecUtf8::from("7#6$5".as_bytes()),
/// };
/// let app2 = Token::new("qwerty", "7#6$5");
/// assert_eq!("***SECRET***:***SECRET***".to_string(), app1.to_string());
/// assert_eq!(app1.to_string(), app2.to_string());
/// ```
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
    /// Create from 2 common string slices.
    /// Internally uses SecStr for holding 
    /// each passed-in value. SecStr not 
    /// only masks the value when displaying,
    /// it also destroys the value from 
    /// memory after use.
    pub fn new(k: &str, s: &str) -> Self {
        Token {
            key: SecUtf8::from(k.as_bytes()),
            secret: SecUtf8::from(s.as_bytes()),
        }
    }
    pub fn to_string(&self) -> String {
        [self.key.to_string(), self.secret.to_string()].join(":")
    }
}

// All that is needed to authenticate against
// a communication address. An account tends
// to have exactly 1 instance of Credentials.
///
///
/// # Example
///
/// ```
/// use crate::domain::{URI, Token, Credentials};
/// let creds = Credentials {
///     uri: URI::from("how://there.that/"),
///     app: Token::new("oknbgr", "8-'$31?"),
///     user: Token::new("HublOprc", "(2$05+@)")
/// };
/// assert_eq!("***SECRET***:***SECRET***".to_string(), creds.app.to_string());
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Credentials {
    pub uri: URI,
    pub app: Token,
    pub user: Token,
}
impl IMaybeEmpty for crate::Credentials {
    fn is_empty(&self) -> bool {
        self.uri.is_empty() && self.app.is_empty() && self.user.is_empty()
    }
}
impl From<&models::Credentials> for crate::Credentials {
    fn from(creds: &models::Credentials) -> Self {
        crate::Credentials {
            uri: URI::from(&creds.uri.as_str()),
            app: Token::new(&creds.app_key.as_str(), &creds.app_secret.as_str()),
            user: Token::new(&creds.user_key.as_str(), &creds.user_secret.as_str()),
        }
    }
}

// Determines where files can be located
// and how they are named, what purpose
// they serve, and whether they are used
// for reading, writing, or both.
//
// An account tends to own multiple file
// locations for multiple purposes. An
// application tends to be built for a
// single such purpose.
///
///
/// # Example
///
/// ```
/// use crate::domain::{FileLocation, FileLocationPurpose};;
/// use std::path::Path;
/// let there = FileLocation {
///     purpose: FileLocationPurpose::from("inventoryRequests"),
///     path: Path::new("/there/").join("that.kind")
/// };
/// assert_eq!("inventoryRequests".to_string(), there.purpose.to_string());
/// ```
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
impl From<&models::FileLocations> for crate::FileLocation {
    fn from(it: &models::FileLocations) -> Self {
        crate::FileLocation {
            purpose: FileLocationPurpose::from(&it.purpose.as_str()),
            path: Path::new(&it.folder).join(&it.name),
        }
    }
}
impl crate::FileLocation {
    pub fn extension(&self) -> Option<&OsStr> {
        self.path.extension()
    }
    fn from_all(them: &[models::FileLocations]) -> Vec<crate::FileLocation> {
        let mut buffer: Vec<crate::FileLocation> = Vec::with_capacity(them.len());
        for item in them {
            buffer.push(crate::FileLocation::from(item));
        }
        buffer.dedup();
        buffer
    }

    /// Create from 3 common string slices.
    /// The new instance winds up owning the 
    /// passed-in contents.
    pub fn new(p: &str, f: &str, n: &str) -> crate::FileLocation {
        FileLocation {
            purpose: FileLocationPurpose::from(p),
            path: Path::new(f).join(n),
        }
    }
    pub fn to_path(&self) -> &Path {
        self.path.as_path()
    }
}

// Combines the ConfigId, Credentials,
// FileLocations, and EmailAddresses
// owned by a single account.
#[derive(Clone, Debug, PartialEq)]
pub struct Account {
    pub config_id: crate::ConfigId,
    pub credentials: crate::Credentials,
    pub locations: Vec<crate::FileLocation>,
}
impl IMaybeEmpty for Account {
    fn is_empty(&self) -> bool {
        self.config_id.is_empty() && self.credentials.is_empty() && self.locations.is_empty()
    }
}

// Attempts to fetch that ConfigId from the
// data store which matches the passed-in
// account id and third party id.
//
// If not found, returns None.
fn fetch_account(
    connection: &SqliteConnection,
    account_id: &str,
    third_party: &str,
) -> Option<ConfigId> {
    use diesel::prelude::*;
    use schema::accounts::dsl::*;
    let results = accounts
        .filter(thirdParty.eq(&third_party))
        .filter(id.eq(&account_id))
        .limit(1)
        .load::<models::Accounts>(connection)
        .expect("Error loading accounts");
    if results.is_empty() {
        return None;
    }
    return Some(ConfigId::from(&results[0]));
}

// Attempts to load from the data store those
// Credentials that belong to the passed-in
// ConfigId.
//
// If not found, returns None.
fn fetch_credentials(
    connection: &SqliteConnection,
    config_id: &ConfigId,
) -> Option<crate::Credentials> {
    use diesel::prelude::*;
    use schema::credentials::dsl::*;
    let results = credentials
        .filter(thirdParty.eq(&config_id.third_party_id.to_string()))
        .filter(account.eq(&config_id.account_id.to_string()))
        .limit(1)
        .load::<models::Credentials>(connection)
        .expect("Error loading credentials");
    if results.is_empty() {
        None
    } else {
        Some(crate::Credentials::from(&results[0]))
    }
}

// Attempts to load from the data store those
// file locations that belong to the passed-in
// ConfigId.
//
// If not found, returns None.
fn fetch_file_locations(
    connection: &SqliteConnection,
    config_id: &ConfigId,
) -> Vec<crate::FileLocation> {
    use diesel::prelude::*;
    use schema::fileLocations::dsl::*;
    let results = fileLocations
        .filter(thirdParty.eq(&config_id.third_party_id.to_string()))
        .filter(account.eq(&config_id.account_id.to_string()))
        .load::<models::FileLocations>(connection)
        .expect("Error loading file locations");
    if results.is_empty() {
        vec![]
    } else {
        crate::FileLocation::from_all(&results)
    }
}

// Attempts to load an Account with Credentials
// and FileLocations that match the passed-in
// id and third party.
///
/// # Example
///
/// let id = "Levi's";
/// let third_party = "Macy's":
/// match attempt_load_account(id, third_party) {
///   Some(account): assert_true!(true),
///   None: assert_true!(true)
/// }
pub fn attempt_load_account(id: &str, third_party: &str) -> Option<Account> {
    let connection = db::connect();
    match fetch_account(&connection, &id, &third_party) {
        None => None,
        Some(config_id) => match fetch_credentials(&connection, &config_id) {
            None => None,
            Some(creds) => Some(crate::Account {
                config_id: config_id.clone(),
                credentials: creds.clone(),
                locations: fetch_file_locations(&connection, &config_id),
            }),
        },
    }
}

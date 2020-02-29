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
//! Version: 2.20.228.2227

//  Crate sqlite gives us low-level access to the
//  data store without getting in our way (like
//  diesel and other binding modules do).
extern crate sqlite;

/// SecStr destroys sensitive data from
/// memory once done.
extern crate secstr;
use secstr::*;

/// Mambro db knows the layout of the
/// data store.
extern crate db;

/// Tsql_fluent helps build SQL statements.
extern crate tsql_fluent;

/// OsStr is used for O.S.-specific texts
/// (like file paths and names), that can
/// contain characters which don't fit into
/// utf-8.
use std::ffi::OsStr;

/// Fmt helps us share human-readable views
/// on the data retained by the data types
/// in this module.
/// use std::fmt;

/// Path and PathBuf handle file locations.
use std::path::{Path, PathBuf};

/// We have multiple structs that need this trait.
use std::string::{String, ToString};

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
        AccountId {
            0: s.as_bytes().to_vec(),
        }
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

impl ToString for AccountId {
    /// Returns an owned, immutable copy of
    /// the name retained in this instance.
    fn to_string(&self) -> String {
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
        FileLocationPurpose {
            0: s.as_bytes().to_vec(),
        }
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

impl ToString for FileLocationPurpose {
    fn to_string(&self) -> String {
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
        ThirdPartyId {
            0: s.as_bytes().to_vec(),
        }
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

impl ToString for ThirdPartyId {
    fn to_string(&self) -> String {
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

impl URI {
    pub fn empty() -> Self {
        URI { 0: vec![] }
    }
}

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
        URI {
            0: s.as_bytes().to_vec(),
        }
    }
}

/// Convert from a standard string.
impl From<String> for URI {
    /// Converts from a standard string.
    ///
    ///
    /// # Arguments
    ///
    /// - String: a web address to use for
    ///           reaching out to the remote
    ///           partner's web / REST servers.
    ///           Suffix with / if the last
    ///           part is a TLD or a folder.
    ///           Omit the end / if the last
    ///           part is a document or query
    ///           value.
    ///           The address gets copied
    ///           and owned by the instance.
    ///
    fn from(s: String) -> Self {
        URI::from(s.as_str())
    }
}

/// Convert from a standard Option that holds a String.
impl From<Option<String>> for URI {
    /// Converts from a standard Option that
    /// holds a String.
    ///
    ///
    /// # Arguments
    ///
    /// - Option<String>:  
    ///         a web address to use for
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
    fn from(s: Option<String>) -> Self {
        match s {
            None => URI::empty(),
            Some(t) => URI::from(t.as_str()),
        }
    }
}

impl ToString for URI {
    fn to_string(&self) -> String {
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

// Now we're combining things we need a New trait
// that can accept multiple type parameters.
pub trait New<R, S> {
    fn new(r: R, s: S) -> Self;
}

impl New<String, String> for ConfigId {
    fn new(a: String, b: String) -> Self {
        ConfigId {
            account_id: AccountId::from(a),
            third_party_id: ThirdPartyId::from(b),
        }
    }
}

impl New<&str, &str> for ConfigId {
    fn new(a: &str, b: &str) -> Self {
        ConfigId {
            account_id: AccountId::from(a),
            third_party_id: ThirdPartyId::from(b),
        }
    }
}

impl ToString for ConfigId {
    fn to_string(&self) -> String {
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
/// Create a new token from string slices.
/// When turning them into a trivial string,
/// they must hide their true value.
///
/// ```
/// use crate::domain::Token;
/// use crate::domain::New;
/// let app1 = Token::new("qwerty", "7#6$5");
/// assert_eq!("***SECRET***:***SECRET***".to_string(), app1.to_string());
/// ```
///
/// # Example
///   
/// Create 2 different tokens in differing ways.
/// If their contents differs, then a trivial
/// comparison of their to_string() must think
/// they are the same.
///
/// ```
/// use crate::domain::Token;
/// use crate::domain::New;
/// extern crate secstr;
/// use secstr::*;
/// let apples = Token {
///     key: SecUtf8::from("apples".as_bytes()),
///     secret: SecUtf8::from("7#6$5".as_bytes()),
/// };
/// let pears = Token::new("pears", "7#6$5");
/// assert_eq!(apples.to_string(), pears.to_string());
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

impl New<&str, &str> for Token {
    /// Create from 2 common string slices.
    /// Internally uses SecStr for holding
    /// each passed-in value. SecStr not
    /// only masks the value when displaying,
    /// it also destroys the value from
    /// memory after use.
    fn new(k: &str, s: &str) -> Self {
        Token {
            key: SecUtf8::from(k.as_bytes()),
            secret: SecUtf8::from(s.as_bytes()),
        }
    }
}

impl New<Option<String>, Option<String>> for Token {
    /// Create from 2 standard Options holding a String.
    /// Internally uses SecStr for holding
    /// each passed-in value. SecStr not
    /// only masks the value when displaying,
    /// it also destroys the value from
    /// memory after use.
    fn new(k: Option<String>, s: Option<String>) -> Self {
        match k {
            None => Token::empty(),
            Some(k2) => match s {
                None => Token::empty(),
                Some(s2) => Token {
                    key: SecUtf8::from(k2.as_bytes()),
                    secret: SecUtf8::from(s2.as_bytes()),
                },
            },
        }
    }
}

// Use this method to generate a trivial copy of
// the values in this token. The values will not
// be revealed. Instead they are substituted by
// a constant expression '***SECRET***'.
// This is done to prevent accidental reveals
// via (for instance) debug, display, or format.
// To generate a revealing copy, use unsecure_to_string().
impl ToString for Token {
    fn to_string(&self) -> String {
        [self.key.to_string(), self.secret.to_string()].join(":")
    }
}

// Use this method to generate a revealing copy
// of the values contained in this token.
//
///
/// # Example
///
/// Create a new token from string slices.
/// When turning them into an unsecure string,
/// they must reveal their true value.
///
/// ```
/// use crate::domain::Token;
/// use crate::domain::New;
/// let app1 = Token::new("qwerty", "7#6$5");
/// assert_eq!("qwerty:7#6$5".to_string(), app1.unsecure_to_string());
/// ```
///
/// # Example
///   
/// Create 2 tokens in differing ways, and
/// if their contents is different, then they are
/// different.
///
/// ```
/// use crate::domain::Token;
/// use crate::domain::New;
/// extern crate secstr;
/// use secstr::*;
/// let apples = Token {
///     key: SecUtf8::from("apples".as_bytes()),
///     secret: SecUtf8::from("7#6$5".as_bytes()),
/// };
/// let pears = Token::new("pears", "7#6$5");
/// assert_ne!(apples.unsecure_to_string(), pears.unsecure_to_string());
/// ```
impl Token {
    pub fn unsecure_to_string(&self) -> String {
        [
            self.key.unsecure().to_string(),
            self.secret.unsecure().to_string(),
        ]
        .join(":")
    }
    pub fn empty() -> Token {
        Token {
            key: SecUtf8::from(""),
            secret: SecUtf8::from(""),
        }
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
/// use crate::domain::{New, URI, Token, Credentials};
/// let creds = Credentials {
///     uri: URI::from("how://there.that/"),
///     app: Token::new("oknbgr", "8-'$31?"),
///     user: Token::new("HublOprc", "(2$05+@)")
/// };
/// assert_eq!("***SECRET***:***SECRET***".to_string(), creds.app.to_string());
/// assert_eq!("HublOprc:(2$05+@)".to_string(), creds.user.unsecure_to_string());
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Credentials {
    pub uri: URI,
    pub app: Token,
    pub user: Token,
}
impl IMaybeEmpty for Credentials {
    fn is_empty(&self) -> bool {
        self.uri.is_empty() && self.app.is_empty() && self.user.is_empty()
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
///     path: Path::new("/there/").join("that.kind"),
///     for_reading: true,
///     for_writing: false
/// };
/// assert_eq!("inventoryRequests".to_string(), there.purpose.to_string());
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct FileLocation {
    pub purpose: FileLocationPurpose,
    pub path: PathBuf,
    pub for_reading: bool,
    pub for_writing: bool,
}

impl IMaybeEmpty for FileLocation {
    fn is_empty(&self) -> bool {
        self.path.is_empty()
    }
}

impl From<&(&str, &str, &str, bool, bool)> for FileLocation {
    fn from(that: &(&str, &str, &str, bool, bool)) -> Self {
        let (how, folder, name, read, write) = that;
        FileLocation {
            purpose: FileLocationPurpose::from(*how),
            path: Path::new(*folder).join(*name),
            for_reading: *read,
            for_writing: *write,
        }
    }
}

impl
    From<(
        Option<String>,
        Option<String>,
        Option<String>,
        Option<i64>,
        Option<i64>,
    )> for FileLocation
{
    fn from<'a>(
        that: (
            Option<String>,
            Option<String>,
            Option<String>,
            Option<i64>,
            Option<i64>,
        ),
    ) -> Self {
        let (maybe_how, maybe_folder, maybe_name, maybe_read, maybe_write) = that;
        let how = match maybe_how {
            None => "".to_string(),
            Some(h) => h,
        };
        let folder = match maybe_folder {
            None => "".to_string(),
            Some(f) => f,
        };
        let name = match maybe_name {
            None => "".to_string(),
            Some(n) => n,
        };
        let read = match maybe_read {
            None => false,
            Some(r) => r != 0,
        };
        let write = match maybe_write {
            None => false,
            Some(w) => w != 0,
        };
        FileLocation {
            purpose: FileLocationPurpose::from(how),
            path: Path::new(&folder).join(&name),
            for_reading: read,
            for_writing: write,
        }
    }
}

impl FileLocation {
    pub fn extension(&self) -> Option<&OsStr> {
        self.path.extension()
    }
}

// Let's ensure that we can make paths in a
// language-safe way.
pub trait ToPath {
    fn to_path(&self) -> &Path;
}
impl ToPath for FileLocation {
    fn to_path(&self) -> &Path {
        self.path.as_path()
    }
}

// Combines the ConfigId, Credentials,
// FileLocations, and EmailAddresses
// owned by a single account.
#[derive(Clone, Debug, PartialEq)]
pub struct Account {
    pub config_id: ConfigId,
    pub credentials: Credentials,
    pub locations: Vec<FileLocation>,
}
impl IMaybeEmpty for Account {
    fn is_empty(&self) -> bool {
        self.config_id.is_empty() && self.credentials.is_empty() && self.locations.is_empty()
    }
}

/// Attempts to check that any ConfigId in the
/// data store matches the passed-in account id 
/// and third party id.
///
/// If found, returns the ConfigId. Otherwise None.
///
///
/// # Parameters
///
/// 1. SqliteConnection should supply access
///    to the data store.
/// 2. Account identifier as a string slice.
///    Should specify which account to find.
/// 3. Third-party identifier as string slice.
///    Should specify
///
///
/// # Panics
///
/// Panics when the SQL statement parameters failed
/// to bind to their values.
///
pub fn check_config_id(
    connection: &sqlite::Connection,
    account_id: &str,
    third_party: &str,
) -> Option<ConfigId> {
    use tsql_fluent::*;
    use sqlite::State::*;
    let mut statement = connection
        .prepare(
            1.select()
                .from("accounts".to_string())
                .wher()
                .field("id".to_string())
                .equals_param()
                .and()
                .field("thirdParty".to_string())
                .equals_param()
                .to_string(),
        )
        .unwrap();
    statement.bind(1, account_id).unwrap();
    statement.bind(2, third_party).unwrap();
    //There should be only 1 row at most,
    //and if that exists it should contain an
    //unnamed column with the number 1.
    match statement.next() {
        Ok(state) => match state {
                Row => Some(ConfigId::new(account_id, third_party)),
                Done => None,
            },
        Err(_) => None,
    }
}

/// Attempts to load from the data store those
/// Credentials that belong to the passed-in
/// ConfigId.
///
///
/// # Parameters
///
/// 1. SqliteConnection should supply access
///    to the data store.
/// 2. Configuration identifier should specify
///    which account to find at which thurd party.
///
///
/// # Panics
///
/// Panics when the SQL parameters failed to
/// bind to their values.
///
/// If not found, returns None.
fn fetch_credentials(connection: &sqlite::Connection, config_id: &ConfigId) -> Option<Credentials> {
    use tsql_fluent::*;
    let mut statement = connection
        .prepare(
            vec![
                "uri".to_string(),
                "app_key".to_string(),
                "app_secret".to_string(),
                "user_key".to_string(),
                "user_secret".to_string(),
            ]
            .select()
            .from("credentials".to_string())
            .wher()
            .field("account".to_string())
            .equals_param()
            .and()
            .field("thirdParty".to_string())
            .equals_param()
            .to_string(),
        ).unwrap();
    statement.bind(1, config_id.account_id.to_string().as_str()).unwrap();
    statement.bind(2, config_id.third_party_id.to_string().as_str()).unwrap();
    match statement.next() {
        Ok(state) => match state {
            sqlite::State::Row => Some(Credentials {
                uri: URI::from(statement.read::<String>(0).ok()),
                app: Token::new(
                    statement.read::<String>(1).ok(),
                    statement.read::<String>(2).ok(),
                ),
                user: Token::new(
                    statement.read::<String>(3).ok(),
                    statement.read::<String>(4).ok(),
                ),
              }),
            sqlite::State::Done => None
        },
        Err(e) => None,
    }
}

/// Attempts to load from the data store those
/// file locations that belong to the passed-in
/// ConfigId.
///
/// If not found, returns None.
fn fetch_file_locations(
    connection: &sqlite::Connection,
    config_id: &ConfigId,
) -> Vec<FileLocation> {
    use tsql_fluent::*;
    let maybe_vec = connection
        .prepare(
            vec![
                "purpose".to_string(),
                "folder".to_string(),
                "fileName".to_string(),
                "for_reading".to_string(),
                "for_writing".to_string(),
            ]
            .select()
            .from("fileLocations".to_string())
            .wher()
            .field("account".to_string())
            .equals_param()
            .and()
            .field("thirdParty".to_string())
            .equals_param()
            .to_string(),
        )
        .ok()
        .and_then(|mut statement| {
            statement
                .bind(1, config_id.account_id.to_string().as_str())
                .unwrap();
            statement
                .bind(2, config_id.third_party_id.to_string().as_str())
                .unwrap();
            let mut them: Vec<FileLocation> = Vec::new();
            loop {
                if statement.next().is_ok() {
                    them.push(FileLocation::from((
                        statement.read::<String>(0).ok(),
                        statement.read::<String>(1).ok(),
                        statement.read::<String>(2).ok(),
                        statement.read::<i64>(3).ok(),
                        statement.read::<i64>(4).ok(),
                    )));
                } else {
                    break;
                }
            }
            let immu = them;
            Some(immu)
        });
    if maybe_vec.is_some() {
        return maybe_vec.unwrap();
    }
    return vec![];
}

/// Attempts to load an account's configuration, with
/// credentials and fileLocations, for that account
/// that matches the passed-in id and third party.
///
/// Returns None if all is well, and no known account
/// matches the provided account id and third-party id.
///
///
/// # Panics
///
/// Panics when the database connection could not be
/// established.
///
///
/// # Parameters
///
/// 1. Account ID, &str: identifies the account for
///    which to fetch additional configuration.
/// 2. Third Party ID, &str: identifies which
///    remote service provided the account.
///
///
/// # Examples
///
/// ```rust
/// use domain::attempt_load_account;
/// use domain::Account;
/// use domain::IMaybeEmpty;
/// let id = "Levi's";
/// let third_party = "Macy's";
/// match attempt_load_account(id, third_party) {
///   Some(account) => assert!(!account.is_empty()),
///   None => assert!(true)
/// }
/// ```
///
pub fn attempt_load_account(id: &str, third_party: &str) -> Option<Account> {
    match db::try_connect() {
        Ok(connection) => match check_config_id(&connection, &id, &third_party) {
            None => None,
            Some(config_id) => match fetch_credentials(&connection, &config_id) {
                None => None,
                Some(creds) => Some(Account {
                    config_id: config_id.clone(),
                    credentials: creds,
                    locations: fetch_file_locations(&connection, &config_id),
                }),
            },
        },
        Err(e) => {
            let msg = format!("Error connecting to db. Additional error message: {}.", &e);
            panic!(msg);
        }
    }
}

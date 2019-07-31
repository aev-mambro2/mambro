// Models translates our data store structures
// into rust structures. These have fields in
// rust primitive and library data types that
// have no semantic significance.

// Diesel requires the use of macros.
#[macro_use]

// Prelude contains things that should have
// been included in the main diesel crate,
// but whatever.
extern crate diesel;

// ThirdParties stores identifiers for trading
// partners. Together with Account IDs these
// create unique Accounts.
#[derive(Queryable, Debug)]
pub struct ThirdParties {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Accounts own file locations, email addresses,
// and credentials to remote locations, that
// all work together to configure from where
// an application needs to read, to where it
// needs to write, with what remote party to
// communicate, and whom to send error reports.
#[derive(Queryable, Debug)]
pub struct Accounts {
    pub id: String,
    pub third_party: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Whom to contact.
#[derive(Queryable)]
pub struct EmailAddresses {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Why an email gets sent.
#[derive(Queryable, Debug)]
pub struct EmailReports {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Identifiers for known programs.
#[derive(Queryable, Debug)]
pub struct Applications {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Why a file gets used.
#[derive(Queryable, Debug)]
pub struct FileLocationPurposes {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// File-system-dependent path to a document
// and some meta-info to identify it and its
// owner.
#[derive(Queryable, Debug)]
pub struct FileLocations {
    pub purpose: String,
    pub account: String,
    pub third_party: String,
    pub for_reading: i32,
    pub for_writing: i32,
    pub folder: String,
    pub name: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Who should receive which report via email.
#[derive(Queryable, Debug)]
pub struct EmailReportSubscriptions {
    pub report: String,
    pub account: String,
    pub third_party: String,
    pub application: String,
    pub address: String,
    pub name: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

// Which remote communication address can get
// accessed using which authentication keys,
// and who owns that set. Typically, a single
// account owns a single set of credentials.
#[derive(Queryable, Debug)]
pub struct Credentials {
    pub account: String,
    pub third_party: String,
    pub uri: String,
    pub app_key: String,
    pub app_secret: String,
    pub user_key: String,
    pub user_secret: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

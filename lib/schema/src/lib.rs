// Technical data structures, generated by
// Diesel.
// That probably means we'll lose these
// comments.

// For compiling and debugging diesel,
// we are required to increase the
// recursion limit.
#![recursion_limit = "128"]
// Table names do not follow rust naming
// conventions, so both the schema and
// the models will contain objects that
// have odd names.
#![allow(non_snake_case)]

// Diesel requires the use of macros.
#[macro_use]

// Prelude contains things that should have
// been included in the main diesel crate,
// but whatever.
extern crate diesel;

table! {
    /// Representation of the `accounts` table.
    ///
    /// (Automatically generated by Diesel.)
    accounts (id, thirdParty) {
        /// The `id` column of the `accounts` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        id -> Text,
        /// The `thirdParty` column of the `accounts` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        thirdParty -> Text,
        /// The `insertedAt` column of the `accounts` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `accounts` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `accounts` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `applications` table.
    ///
    /// (Automatically generated by Diesel.)
    applications (id) {
        /// The `id` column of the `applications` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        id -> Text,
        /// The `description` column of the `applications` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        description -> Text,
        /// The `insertedAt` column of the `applications` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `applications` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `applications` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `credentials` table.
    ///
    /// (Automatically generated by Diesel.)
    credentials (account, thirdParty, uri) {
        /// The `account` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        account -> Text,
        /// The `thirdParty` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        thirdParty -> Text,
        /// The `uri` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        uri -> Text,
        /// The `app_key` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        app_key -> Text,
        /// The `app_secret` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        app_secret -> Text,
        /// The `user_key` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        user_key -> Text,
        /// The `user_secret` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        user_secret -> Text,
        /// The `insertedAt` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `credentials` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `credentials` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `emailAddresses` table.
    ///
    /// (Automatically generated by Diesel.)
    emailAddresses (id) {
        /// The `id` column of the `emailAddresses` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        id -> Text,
        /// The `insertedAt` column of the `emailAddresses` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `emailAddresses` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `emailAddresses` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `emailReportSubscriptions` table.
    ///
    /// (Automatically generated by Diesel.)
    emailReportSubscriptions (report, account, thirdParty, application) {
        /// The `report` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        report -> Text,
        /// The `account` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        account -> Text,
        /// The `thirdParty` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        thirdParty -> Text,
        /// The `application` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        application -> Text,
        /// The `address` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        address -> Text,
        /// The `insertedAt` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `emailReportSubscriptions` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `emailReports` table.
    ///
    /// (Automatically generated by Diesel.)
    emailReports (id) {
        /// The `id` column of the `emailReports` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        id -> Text,
        /// The `description` column of the `emailReports` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        description -> Text,
        /// The `insertedAt` column of the `emailReports` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `emailReports` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `emailReports` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `fileLocationPurposes` table.
    ///
    /// (Automatically generated by Diesel.)
    fileLocationPurposes (id) {
        /// The `id` column of the `fileLocationPurposes` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        id -> Text,
        /// The `insertedAt` column of the `fileLocationPurposes` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `fileLocationPurposes` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `fileLocationPurposes` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `fileLocations` table.
    ///
    /// (Automatically generated by Diesel.)
    fileLocations (purpose, account, thirdParty, forReading, forWriting) {
        /// The `purpose` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        purpose -> Text,
        /// The `account` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        account -> Text,
        /// The `thirdParty` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        thirdParty -> Text,
        /// The `forReading` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        forReading -> Integer,
        /// The `forWriting` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        forWriting -> Integer,
        /// The `folder` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        folder -> Text,
        /// The `fileName` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        fileName -> Text,
        /// The `insertedAt` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `fileLocations` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

table! {
    /// Representation of the `thirdParties` table.
    ///
    /// (Automatically generated by Diesel.)
    thirdParties (id) {
        /// The `id` column of the `thirdParties` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        id -> Text,
        /// The `insertedAt` column of the `thirdParties` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        insertedAt -> Text,
        /// The `modifiedAt` column of the `thirdParties` table.
        ///
        /// Its SQL type is `Text`.
        ///
        /// (Automatically generated by Diesel.)
        modifiedAt -> Text,
        /// The `version` column of the `thirdParties` table.
        ///
        /// Its SQL type is `Integer`.
        ///
        /// (Automatically generated by Diesel.)
        version -> Integer,
    }
}

joinable!(accounts -> thirdParties (thirdParty));
joinable!(credentials -> thirdParties (thirdParty));
joinable!(emailReportSubscriptions -> applications (application));
joinable!(emailReportSubscriptions -> emailAddresses (address));
joinable!(emailReportSubscriptions -> emailReports (report));
joinable!(emailReportSubscriptions -> thirdParties (thirdParty));
joinable!(fileLocations -> fileLocationPurposes (purpose));
joinable!(fileLocations -> thirdParties (thirdParty));

allow_tables_to_appear_in_same_query!(
    accounts,
    applications,
    credentials,
    emailAddresses,
    emailReportSubscriptions,
    emailReports,
    fileLocationPurposes,
    fileLocations,
    thirdParties,
);

#[derive(Queryable, Debug)]
pub struct ThirdParties {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

#[derive(Queryable, Debug)]
pub struct Accounts {
    pub id: String,
    pub third_party: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

#[derive(Queryable)]
pub struct EmailAddresses {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

#[derive(Queryable, Debug)]
pub struct EmailReports {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

#[derive(Queryable, Debug)]
pub struct Applications {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

#[derive(Queryable, Debug)]
pub struct FileLocationPurposes {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

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

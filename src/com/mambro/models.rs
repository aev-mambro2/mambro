#[derive(Queryable)]
pub struct ThirdParties {
    pub id: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

#[derive(Queryable)]
pub struct Accounts {
    pub id: String,
    pub third_party: String,
    pub inserted_at: String,
    pub modified_at: String,
    pub version: i32,
}

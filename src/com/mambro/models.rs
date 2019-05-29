#[derive(Queryable)]

pub struct thirdParties {
    pub id: String,
    pub insertedAt: String,
    pub modifiedAt: String,
    pub version: usize,
}

pub struct accounts {
    pub id: String,
    pub thirdParty: String,
    pub insertedAt: String,
    pub modifiedAt: String,
    pub version: usize,
}

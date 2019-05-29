table! {
    accounts (id, thirdParty) {
        id -> Text,
        thirdParty -> Text,
        insertedAt -> Text,
        modifiedAt -> Text,
        version -> Integer,
    }
}

table! {
    thirdParties (id) {
        id -> Text,
        insertedAt -> Text,
        modifiedAt -> Text,
        version -> Integer,
    }
}

joinable!(accounts -> thirdParties (thirdParty));

allow_tables_to_appear_in_same_query!(
    accounts,
    thirdParties,
);

#[derive(Debug)]
pub struct AccountId(pub String);
#[derive(Debug)]
pub struct FileExtension(pub String);
#[derive(Debug)]
pub struct FileName(pub String);
#[derive(Debug)]
pub struct FolderName(pub String);
#[derive(Debug)]
pub struct Key(pub String);
#[derive(Debug)]
pub struct Secret(pub String);
#[derive(Debug)]
pub struct ThirdPartyId(pub String);
#[derive(Debug)]
pub struct URI(pub String);

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

impl IMaybeEmpty for FolderName {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for Key {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IMaybeEmpty for Secret {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum FileLocationIntentions {
    None,
    ReadFrom,
    WriteTo,
    ForwardTo,
    LogRequestsTo,
    LogResponsesTo,
}

#[derive(Debug)]
pub struct FileLocation {
    pub intent: FileLocationIntentions,
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
        FileExtension(String::from(""))
    }
}

#[derive(Debug)]
pub struct Account {
    pub config_id: ConfigId,
    pub credentials: Credentials,
    pub location: FileLocation,
}
impl IMaybeEmpty for Account {
    fn is_empty(&self) -> bool {
        self.config_id.is_empty() && self.credentials.is_empty() && self.location.is_empty()
    }
}

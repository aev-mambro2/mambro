CREATE TABLE credentials (
  account TEXT NOT NULL REFERENCES accounts (id) ON DELETE RESTRICT ON UPDATE CASCADE,
  thirdParty TEXT NOT NULL REFERENCES thirdParties (id) ON DELETE RESTRICT ON UPDATE CASCADE,
  uri TEXT NOT NULL default "",
  app_key TEXT NOT NULL default "",
  app_secret TEXT NOT NULL default "",
  user_key TEXT NOT NULL default "",
  user_secret TEXT NOT NULL default "",
  insertedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modifiedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  version INTEGER NOT NULL DEFAULT 0,
  CONSTRAINT e1 PRIMARY KEY (account, thirdParty, uri)
);

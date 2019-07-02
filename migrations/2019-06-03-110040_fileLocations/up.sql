CREATE TABLE fileLocationPurposes (
  id TEXT NOT NULL PRIMARY KEY ASC UNIQUE,
  insertedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modifiedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  version INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE fileLocations (
  purpose TEXT NOT NULL REFERENCES fileLocationPurposes (id) ON DELETE RESTRICT ON UPDATE CASCADE,
  account TEXT NOT NULL REFERENCES accounts (id) ON DELETE RESTRICT ON UPDATE CASCADE,
  thirdParty TEXT NOT NULL REFERENCES thirdParties (id) ON DELETE RESTRICT ON UPDATE CASCADE,
  forReading INTEGER NOT NULL default 1,
  forWriting INTEGER NOT NULL default 0,
  folder TEXT NOT NULL default "~",
  fileName TEXT NOT NULL,
  insertedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modifiedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  version INTEGER NOT NULL DEFAULT 0,
  CONSTRAINT a2 PRIMARY KEY (purpose, account, thirdParty, forReading, forWriting)
);
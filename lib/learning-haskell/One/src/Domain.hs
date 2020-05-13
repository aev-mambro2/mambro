module Domain (
  findAccount,
  findFileLocations,
  findFileLocationForPurpose,
  AccountID,
  ThirdPartyID,
  FileLocationPurpose,
  EmailPurpose,
  UserToken,
  AppToken,
  URL,
  Credential,
  FileAccess(None, Read, Write),
  Path,
  FileLocation, 
  FileLocations,
  Account,
  Accounts
) where

  {-| 
   - Definitions and functions pertaining to the e-commerce 
   - and logistics domain.
   - @author A.E.Veltstra
   - @since 2020-05-01T14:18:00.000EDT
   - @version 2020-05-12T17:16:00.000EDT
   -}

  type Key = String
  type AccountID = Key
  type ThirdPartyID = Key
  type Purpose = Key
  type FileLocationPurpose = Purpose
  type EmailPurpose = Purpose
  type Secret = String
  type Token = (Key, Secret)
  type UserToken = Token
  type AppToken = Token
  type URL = String
  type Credential = (URL, AppToken, UserToken)
  data FileAccess = None | Read | Write deriving (Eq,Ord,Enum,Show)
  type Path = String
  type FileLocation = (FileLocationPurpose, Path, FileAccess)
  type FileLocations = [ FileLocation ]
  type Account = (AccountID, ThirdPartyID, Credential, FileLocations)
  type Accounts = [ Account ]


  {-|
    Finds all accounts that match the passed-in account id and 
    third-party id.
    Returns: either the empty list, or a list with matching accounts.
  -}
  findAccount :: Accounts -> (AccountID, ThirdPartyID) -> Accounts
  findAccount them (a', t') 
    | length them == 0  = error "No accounts available."
    | otherwise = [ (a,t,c,f) | (a,t,c,f) <- them, a == a', t == t']

  {-|
    Finds all file locations for the passed-in account.
    Returns: either the empty list, or a list of FileLocation instances.
  -}
  findFileLocations :: Account -> FileLocations
  findFileLocations (a,t,c,f) = f

  {-|
    Finds all file locations for the passed-in account,
    purpose, and access type.
    Returns: either the empty list, or a list of Path instances.
  -}
  findFileLocationForPurpose :: Account -> (FileLocationPurpose, FileAccess) -> [Path]
  findFileLocationForPurpose x (p', a') = [ l | (p,l,a) <- findFileLocations x, p == p', a == a']


----------


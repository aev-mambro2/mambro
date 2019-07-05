-- This file should undo anything in `up.sql`
delete from credentials where thirdParty = 'Joor';

-- This file should undo anything in `up.sql`
delete from accounts where thirdParty = 'Joor';

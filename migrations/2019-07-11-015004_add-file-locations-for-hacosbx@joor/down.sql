-- This file should undo anything in `up.sql`
delete from fileLocations where account = 'HaCoSandbox' and thirdParty = 'Joor';


-- Your SQL goes here
insert into fileLocations 
(purpose, account, thirdParty, forReading,
 forWriting, folder, fileName) values 
('orders', 'HaCoSandbox', 'Joor', 0, 1, 
 '/mnt/e/Joor/HacoSandbox/Inbound/Orders', 
 '%orderid%-%datetime%.xml'),
('orderRequests', 'HaCoSandbox', 'Joor', 0, 1, 
 '/mnt/e/Joor/HacoSandbox/Outbound/Orders/logs', 
 'requests-%datetime%.log'),
('orderResponses', 'HaCoSandbox', 'Joor', 0, 1, 
 '/mnt/e/Joor/HacoSandbox/Inbound/Orders/logs', 
 'responses-%datetime%.log');



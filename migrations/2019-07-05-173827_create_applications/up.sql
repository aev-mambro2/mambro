-- Your SQL goes here
insert into applications (id, description) values 
("com.mambro.joor.alertstragglers", "Alerts about files for / from Joor that should have been handled by now, but haven't."),
("com.mambro.joor.bulkcreatestyles", "Sends style files to Joor to create products for sale."),
("com.mambro.joor.bulkupdatecustomers", "Sends customer files to Joor to create customers to sell to."),
("com.mambro.joor.bulkupdateinventory", "Sends inventory files to Joor to indicate how much product is left to sell."),
("com.mambro.joor.bulkupdatestyles", "Sends style files to Joor to update products for sale."),
("com.mambro.joor.downloadcustomerfeedstatus", "Requests a status about Joor's progress in processing the customer update job."),
("com.mambro.joor.downloadstylefeedstatus", "Requests a status about Joor's progress in processing the style feed."),
("com.mambro.joor.downloadstyles", "Receives all style information Joor has about our products.");

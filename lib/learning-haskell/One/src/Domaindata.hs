module Domaindata where 

  import qualified Domain ( Accounts, Account, FileAccess(None, Read, Write))

  fb1 :: Domain.Account
  fb1 = (
          "One", 
          "Facebook", 
          (
            "https://fb.com/", 
            ("app key", "app token"),
            ("user key", "user token")
          ),
          [
            (
              "UploadInventory",
              "//mnt/e/Facebook/Outbound/One/Inventory/fb1-inventory-*.xml",
              Domain.Read
            ),
            (
              "DownloadOrders",
              "//mnt/e/Facebook/Inbound/One/Orders/fb1-order-%orderid%-dd-%datetime%.json",
              Domain.Write 
            )
          ]
        )

  gpn1 :: Domain.Account
  gpn1 = (
          "One", 
          "Groupon", 
          (
            "https://groupon.com/", 
            ("app key", "app token"),
            ("user key", "user token")
          ),
          [
            (
              "UploadInventory",
              "//mnt/e/GPN/Outbound/One/Inventory/gpn1-inventory-*.xml",
              Domain.Read
            ),
            (
              "DownloadOrders",
              "//mnt/e/GPN/Inbound/One/Orders/gpn1-order-%orderid%-dd-%datetime%.json",
              Domain.Write
            )
          ]
        )

  accounts :: Domain.Accounts
  accounts = [ fb1, gpn1 ]


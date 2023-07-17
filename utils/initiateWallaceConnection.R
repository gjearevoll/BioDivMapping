# Connect to the NOFA database
library(pool)
library(raster)
library(DBI)

pg_user <- rstudioapi::askForPassword("Wallace username")
pg_password <- rstudioapi::askForPassword(paste0("Password for ", pg_user))

optionsText <- paste0("-c search_path=", targetDatabase)

pool <- dbPool(drv = RPostgreSQL::PostgreSQL(), dbname = 'biodiv',
               host = 'vm-srv-wallace.vm.ntnu.no', user = pg_user, password = pg_password,
               options = optionsText,
               idleTimeout = 36000000
)
con <- poolCheckout(pool)

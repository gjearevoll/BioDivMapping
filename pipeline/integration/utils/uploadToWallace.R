# Connect to the NOFA database
library(pool)
library(raster)
library(DBI)

dbName <- 'biodiv'
targetHost <- 'vm-srv-wallace.vm.ntnu.no'
optionsText <- paste0("-c search_path=", "species_occurrences")

pg_user <- rstudioapi::askForPassword("Wallace username")
pg_password <- rstudioapi::askForPassword(paste0("Password for ", pg_user))


pool <- dbPool(drv = RPostgreSQL::PostgreSQL(), dbname = dbName,
               host = targetHost, user = pg_user, password = pg_password,
               options = optionsText, idleTimeout = 36000000
)
con <- poolCheckout(pool)

dbAppendTable(con, name = "processed_species_data", value = processedDataDF)

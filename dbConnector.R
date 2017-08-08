library(RSQLite)
source('./properties.R')
getConnection <- function() {
  return(con <- dbConnect(RSQLite::SQLite(), dbname=dbPath))
}

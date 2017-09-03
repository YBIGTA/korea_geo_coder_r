library("RSQLite")
library(dplyr)
source('./properties.R')
con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)

source('./toSido.R')
source('./queryBuilder.R')
source('./getGeoCode.R')

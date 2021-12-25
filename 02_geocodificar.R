#install.packages("httr")
#install.packages("jsonlite")

library(httr)
library(jsonlite)

url <- "http://ws.usig.buenosaires.gob.ar/geocoder/2.2"
geocoding <- GET(url)

geocoding

geocoding <- fromJSON(content(geocoding, type = "text"))
geocoding[["date"]]


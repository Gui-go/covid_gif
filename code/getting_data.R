rm(list = ls())
gc()

# Packages ----------------------------------------------------------------
if(!require(readr)) install.packages("readr")
if(!require(httr)) install.packages("httr")
if(!require(jsonlite)) install.packages("jsonlite")
if(!require(RCurl)) install.packages("RCurl")
if(!require(rio)) install.packages("rio")

# Getting the data --------------------------------------------------------
# https://brasil.io/api/dataset/covid19
covid19data <- data.frame()
for (i in 1:100) {
  newpage <- fromJSON(paste0("https://brasil.io/api/dataset/covid19/caso/data?page=", as.character(i)), flatten = T)$results
  covid19data <- rbind(covid19data, newpage)
  print(paste0("Downloading page ", i))
  if(!url.exists(paste0("https://brasil.io/api/dataset/covid19/caso/data?page=", as.character(i+1)))){
    print("Esgotaram-se os dados. Por hoje é só..")
    break
  }
}
str(covid19data)
# View(covid19data)
# rio::export(covid19data, "covid19data.csv")

# Libraries and Functions -------------------------------------------------

library(mongolite)
library(dplyr)
library(stringr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(readxl)
library(tidytext)
library(topicmodels)
library(SnowballC)
library(tm)
library(snow)


cleanText <- function(text){
  text %>%
    tolower() %>%
    str_replace_all("[\r\n]"," ") %>%
    str_replace_all("#[a-z,A-Z]*","") %>%
    str_replace_all(" http.*","") %>%
    str_replace_all(" https.*","") %>%
    str_replace_all("[[:punct:]]"," ") %>%
    str_replace_all("ä","ae") %>%
    str_replace_all("ü","ue") %>%
    str_replace_all("ö","oe") %>%
    str_replace_all("ß","ss") %>%
    str_replace_all("[[:digit:]]","") %>%
    iconv(from = "UTF-8", to = "ASCII", sub = "") %>%
    str_replace_all("\\s+"," ") %>%
    str_trim
}

mongoIn <- function(var, vals){
  query <- paste0(vals,collapse = '","')
  query <- paste0('{ "',var,'": { "$in": ["', query, '"] } }')
  return(query)
}

getPartyFacebookIds <- function(URI){
  org <- mongo("track_organizations", "facebook", URI)$find()
  org <- org[!org$group_or_party %in% c("media", "Pegida"), ] %>%
    select(facebook_link, facebook_id, group_or_party) %>%
    rename(link = facebook_link, party = group_or_party, facebookId = facebook_id)
  
  pol <- mongo("track_politicians", "facebook", URI)$find() %>%
    select(cleanLink, facebookId, partyName) %>%
    filter(!is.na(facebookId)) %>%
    rename(link = cleanLink, party = partyName)
  
  rbind(org, pol) %>%
    filter(!duplicated(.))
}



# Options and Constants ---------------------------------------------------

mongo_options(bigint_as_char = FALSE)
options(stringsAsFactors = FALSE, scipen = 999)
DB <- "facebook"
URI <- readLines("./data/URI")
PARTIES <- c("#000000", "#EB001F", "#BE3075", "#64A12D", "#005e87", "#FFED00", "#009EE0")
names(PARTIES) <- c("CDU", "SPD", "Linke", "Grüne", "CSU", "FDP", "AfD")


# Candidates List ---------------------------------------------------------
pol <- mongo("track_politicians", "facebook", URI)$find()
org <- mongo("track_organizations", "facebook", URI)$find() %>%
  filter(!group_or_party %in% c("media", "Pegida"))


ids <- getPartyFacebookIds(URI) %>%
  mutate(name = str_replace(link, "https://www.facebook.com/", ""), link = NULL)


# Get Queries -------------------------------------------------------------

q <- mongoIn("fromId", ids$facebookId)
posts_pol <- mongo("politicians.posts", DB, URI)$find(
  query = q,
  fields = '{"createdTime":1, "fromId":1, "likes":1, "reactions":1, "shares":1, "comments":1, "_id":1, "text":1, "type":1}'
)

posts_org <- mongo("organizations.posts", DB, URI)$find(
  query = q, 
  fields = '{"createdTime":1, "fromId":1, "likes":1, "reactions":1, "shares":1, "comments":1, "_id":1, "text":1, "type":1}'
)

posts <- rbind(posts_org, posts_pol) %>%
  rename(postId = `_id`) %>%
  as_tibble %>%
  left_join(ids, by = c("fromId" = "facebookId")) %>%
  mutate(createdDate = as.Date(createdTime)) %>%
  mutate(cleanText = cleanText(text)) %>%
  filter(!duplicated(postId))

saveRDS(posts, file = "./data/posts.rds")

rm(posts_pol, posts_org)


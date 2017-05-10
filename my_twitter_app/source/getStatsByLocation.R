get.stats.by.location<-function(lat = 51.5074, long = 0.1278, r = 15, 
                                radius.measure = "mi", 
                                n.sample = 1000,
                                yes.tags = c("#wengerin", "#WengerStay", "#WengerStays", "#InArseneWeTrust", "#onlyonearsenewenger"),
                                no.tags=c("#wengerout"))
{
  geocode <- toString(c(lat, long, paste0(r, radius.measure)))
  geocode <- gsub(" ","", geocode, fixed=TRUE)
  search.text <- paste(c(yes.tags, no.tags), sep = " || ", collapse = " || ")
  data <- twitteR::searchTwitter(searchString=search.text, n=n.sample,resultType="recent",
                         geocode=geocode)
  # remove truncated
  texts <- lapply(data, FUN = function(x) ifelse(x$truncated, NA, x$text))
  trunc.percent <- length(which(is.na(texts))) / length(texts)
  texts <- texts[!is.na(texts)]
  reactions <- lapply(texts, FUN = function(x) classify.tweet(x, yes.tags, no.tags))
  yes.percent <- length(reactions[(reactions == 1)])/length(reactions)
  no.percent <- length(reactions[(reactions == 0)])/length(reactions)
  neutral.percent <- length(reactions[(reactions == -1)])/length(reactions)
  list(yes.percent, no.percent, neutral.percent)
}

classify.tweet<-function(t, yes.tags, no.tags)
{
  no.number <- sum(stringr::str_count(t, paste0("(?i)", no.tags)))
  yes.number <- sum(stringr::str_count(t, paste0("(?i)", yes.tags)))
  classes <- ifelse(yes.number > no.number, 1, 0)
  classes[yes.number == no.number] <- -1
  classes
}

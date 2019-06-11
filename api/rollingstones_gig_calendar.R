library(RCurl)
library(XML)

gig_calendar <- function(){
  url <- "https://rollingstoneindia.com/gigscalendar/"
  src <-  getURL(url,encoding="UTF-8")
  src_prs <- htmlParse(src)
  
  dates <- xpathSApply(src_prs, "//td[@class='gigpress-date']",xmlValue)
  artist <- xpathSApply(src_prs, "//td[@class='gigpress-artist']",xmlValue)
  city <- xpathSApply(src_prs, "//td[@class='gigpress-city']",xmlValue)
  venue <- xpathSApply(src_prs, "//td[@class='gigpress-venue']",xmlValue)
  link <- xpathSApply(src_prs, "//*[@class='gigpress-calendar-links-inner']/span/a",xmlGetAttr,'href')
  #links <- links[!links %in% "http://r<!DOCTYPE HTML PUBLIC "]
  
  
  gig_calendar <- data.frame("dates"=dates,
                             "city"=city,
                             "venue"=venue,
                             "link"=links,
                             "add_to_calendar"=linkz)
  return(gig_calendar)
  
}


news_updates <- function(x){
  url <- "https://rollingstoneindia.com/category/news-updates/"
  url <- ifelse(x>1,
                url <- paste0(url,"page/",x,"/"),
                url)
  
  src <-  getURL(url,encoding="UTF-8")
  src_prs <- htmlParse(src)
  
  tag <- xpathSApply(src_prs, "//div[@class='tt-post-cat']",xmlValue)
  title <- xpathSApply(src_prs, "//*[@class='tt-post-title c-h5']",xmlValue)
  author <- xpathSApply(src_prs, "//*[@class='tt-post-label']/span[1]",xmlValue)
  date <- xpathSApply(src_prs, "//*[@class='tt-post-label']/span[2]",xmlValue)
  description <- xpathSApply(src_prs, "//*[@class='simple-text']/p",xmlValue)
  image_url <- xpathSApply(src_prs, "//*[@class='img-responsive wp-post-image']",xmlGetAttr,'src')
  post_url <- xpathSApply(src_prs, "//*[@class='tt-post-title c-h5']",xmlGetAttr,'href')
  
  updates <- data.frame("dates"=date,
                             "author"=author,
                             "title"=title,
                             "description"=description,
                             "tag"=tag,
                              "image_url"=image_url,
                        "post_url"=post_url)
  return(gig_calendar)
  
}

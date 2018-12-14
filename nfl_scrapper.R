library(dplyr)
library(rvest)
library(stringr)


passing_home <- "http://www.espn.com/nfl/weekly/leaders/_/type/passing"

passing_urls <- (passing_home %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="content"]/div[2]/div/div/div[2]/form/select/option') %>%
  html_attr("value"))[-1]

passing_regular_season_urls <- unname(sapply(passing_urls[!str_detect(passing_urls, "seasontype")], function(url) {
  str_c("http:", url)
}))
passing_post_season_urls <- passing_urls[str_detect(passing_urls, "seasontype")]

passing_regular_season_all_urls <- unlist(lapply(passing_regular_season_urls, function(url) {
  weeks <- str_split(url %>%
                       read_html() %>%
                       html_nodes(xpath='//*[@id="content"]/div[2]/div/div/div[2]') %>%
                       html_text(), ":")
  weeks <- str_split(str_sub(weeks[[1]][3], 2, (str_length(weeks[[1]][3])-6)), ' ')[[1]]
  weeks <- as.numeric(weeks[!(weeks == "|")])
  
  urls <- unname(sapply(weeks, function(week){
    str_c(url, "/week/", week)
  }))
}))

passing_data <- lapply(passing_regular_season_all_urls, function(url) {
  frame <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="my-players-table"]/div[1]/div[1]/table[2]') %>%
    html_table() %>%
    as.data.frame
  names(frame) <- as.vector(frame[2,])
  frame[-c(1,2),]
})
passing_data <- do.call(rbind, passing_data)
passing_data$WINLOSS <- unname(sapply(passing_data$RESULT, function(x){
  str_sub(x, 1,1)
}))
passing_data$WINSCORE <- unname(sapply(passing_data$RESULT, function(res){
  unlist(str_split(str_split(res, "-")[[1]], " "))[2]
}))
passing_data$LOSSSCORE <- unname(sapply(passing_data$RESULT, function(res){
  unlist(str_split(str_split(res, "-")[[1]], " "))[3]
}))
passing_data$OPPONENT <- unname(sapply(passing_data$RESULT, function(res){
  unlist(str_split(str_split(res, "-")[[1]], " "))[6]
}))
passing_data$NAME <- unname(sapply(passing_data$PLAYER, function(p) {
  str_split(p, ", ")[[1]][1]
}))
passing_data$POS <- unname(sapply(passing_data$PLAYER, function(p) {
  str_split(p, ", ")[[1]][2]
}))
passing_data$PLAYER <- NULL


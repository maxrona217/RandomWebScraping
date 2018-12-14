library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(plyr)
library(BRRR)

base_url <- "http://www.espn.com/golf/player/scorecards/_/id/462/tiger-woods"

urls.players <- "http://espn.com/golf/players" %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="my-players-table"]/div/div/table/tr/td/a') %>%
  html_attr("href")

all_player_data <- function(url) {
  urls.year <- url_convert(url) %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="content"]/div[5]/div[1]/div/div[2]/div[1]/form/select[1]/option') %>%
    html_attr("value")
  
  url.tourns <- str_c("http:",unlist(lapply(urls.year[-1], function(url){
    (str_c("http:/", str_sub(url, 2)) %>%  
       read_html() %>%
       html_nodes(xpath='//*[@id="content"]/div[5]/div[1]/div/div[2]/div[1]/form/select[2]/option') %>%
       html_attr("value"))[-1]
  })), sep="")
  
  # lengths <- unname(sapply(url.tourns, length))
  # url.tourns <- url.tourns[lengths>6]
  
  all_scores <- lapply(url.tourns, get_scores)
  data <- do.call(rbind.fill, all_scores)
  
  if(is.null(data)) {
    return()
  }
  
  data <- data[c("name", "year", as.character(1:(ncol(data)-2)))]
  names(data) <- c("Name", "Year", str_c("hole", 1:(ncol(data)-2)))
  return(data)
}

get_scores <- function(url) {
  html <- read_html(url)
  
  tag <- html %>%
    html_nodes(xpath='//*[@id="content"]/div[5]/div[1]/div/div[2]/div[3]/div/p') %>%
    html_text
  
  scores <- html %>%
    html_nodes(xpath='//*[@class="roundSwap active"]/table') %>%
    html_table(fill=T)
  
  if(length(scores) == 0 | identical(tag, "*Withdrew from tournament.")) {
    return()
  }else if(length(scores) > 4){
    return()
  } else if(identical(tag, character(0))) {
    scores[[1]] <- scores[[1]][-2,]
    rownames(scores[[1]]) <- 1:5
  }

  scores <- do.call(cbind, lapply(scores, function(frame){cbind(frame[3,c(-1, -11,-12)], frame[5,c(-1, -11,-12)])}))
  names(scores) <- seq(from = ncol(scores), to = 1)
  scores <- scores[as.character(1:ncol(scores))]
  
  scores$year <- html %>%
    html_nodes(xpath='//*[@id="content"]/div[5]/div[1]/div/div[2]/div[1]/form/select[1]/option[@selected="selected"]') %>%
    html_text()
  
  name <- html %>%
    html_nodes(xpath='//*[@id="content"]/div[5]/div[1]/div/div[2]/div[1]/form/select[2]/option[@selected="selected"]') %>%
    html_text()
  name <- str_sub(name, 0, length(name)-18)
  scores$name <- name
  
  return(scores)
}

find_player_url <- function(player) {
  player <- unlist(str_split(player, pattern = " "))
  tag <- str_c(str_to_lower(player[1]), "-", str_to_lower(player[2]))
  return(urls.players[str_detect(urls.players, pattern = tag)])
}

url_to_name <- function(url) {
  pieces <- str_split(url, '/')[[1]]
  names <- str_split(pieces[length(pieces)], '-')[[1]]
  return(str_c(str_to_title(names[1]), ' ', str_to_title(names[2])))
}

url_convert <- function(url) {
  split <- str_split(url, '/')[[1]]
  new_split <- c(split[1:5], "scorecards", split[6:length(split)])
  return(paste(new_split, collapse='/'))
}


all_data <- data.frame(t(data.frame(rep('', 75))))
names(all_data) <- c("Name", "Year", str_c("hole", 1:72), "Player")
all_data <- all_data[-1,]
for(url in urls.players[102:length(urls.players)]) {
  print(url_to_name(url))
  player_data <- all_player_data(url)
  if(is.na(player_data)) {
    print("Break: ", url_to_name(url))
    break
  }
  player_data <- cbind(player_data, Player=rep(url_to_name(url), nrow(player_data)))
  all_data <- rbind.fill(all_data, player_data)
}
skrrrahh(27)

urls.players


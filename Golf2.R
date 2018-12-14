library(dplyr)
library(stringr)
library(rvest)
library(rlist)
library(plyr)

base_url <- "http://www.espn.com/golf//leaderboard"


tourn_values <- lapply(base_url %>%
                         read_html() %>%
                         html_nodes(xpath='//*[@id="prev-years"]/option') %>%
                         html_attr('value') %>%
                         sapply(function(value) {
                           str_c(base_url, "?tournamentId=", value)
                         }) %>%
                         unname, function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="all-tournaments"]/option') %>%
    html_attr('value')
}) %>%
  unlist %>%
  sapply(function(value) {
    str_c(base_url, "?tournamentId=", value)
  }) %>%
  unname

all_data <- lapply(tourn_values, function(url) {
  print(which(tourn_values==url))
  html <- read_html(url)
  
  list(html %>%
         html_nodes(xpath='//*[@id="content"]/div[2]/div[2]/div/div[1]/h1') %>%
         html_text(),
       html %>%
         html_nodes(xpath='//*[@id="content"]/div[2]/div[2]/div/div[1]/h3[1]') %>%
         html_text(),
       html %>%
         html_nodes(xpath='//*[@id="regular-leaderboard"]/table') %>%
         html_table(fill=T))
})

clean_data <- lapply(all_data, function(tourn) {
  if(identical(tourn[[1]], character(0)) | identical(tourn[[3]], list())) {
    return(NA)
  }
  
  name <- tourn[[1]]
  
  date_str <- tourn[[2]]
  year <- as.numeric(str_sub(date_str, length(date_str)-5))
  
  scores <- tourn[[3]][[1]]
  
  if(nrow(scores) == 0) {
    return(NA)
  }
  
  scores$name <- name
  scores$year <- year
  
  scores[seq(from=1, to=nrow(scores), by=2),]
})

clean_data <- clean_data[!is.na(clean_data)]
clean_data <- clean_data[which(sapply(clean_data, function(x){!("TEE TIME" %in% names(x))}))]
clean_data <- clean_data[which(sapply(sapply(clean_data, names), length) < 1000)]

data <- do.call(rbind.fill, clean_data)[c(-2, -14, -15, -16)]
data$R1 <- as.numeric(data$R1)
data$R2 <- as.numeric(data$R2)
data$R3 <- as.numeric(data$R3)
data$R4 <- as.numeric(data$R4)
data$`FEDEX PTS` <- as.numeric(data$`FEDEX PTS`)

fedex_cup_points <- data %>%
  filter(year == "2018") %>%
  group_by(PLAYER) %>%
  dplyr::summarise(PTS = sum(`FEDEX PTS`, na.rm=T)) %>%
  arrange(desc(PTS)) %>%
  as.data.frame




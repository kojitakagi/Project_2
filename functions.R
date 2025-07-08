library(httr)
library(jsonlite)
library(dplyr)

# 月別打撃成績を取得する関数
get_monthly_stats <- function(player_id = 660271, season = 2023) {
  url <- paste0(
    "https://statsapi.mlb.com/api/v1/people/", player_id,
    "/stats?stats=monthByMonth&group=hitting&season=", season
  )
  
  res <- GET(url)
  if (status_code(res) != 200) {
    stop("API request failed.")
  }
  
  data <- fromJSON(content(res, as = "text"), flatten = TRUE)
  df <- data$stats$splits[[1]] # 月別の打撃記録
  
  return(df)
}

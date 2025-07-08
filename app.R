# app.R
library(shiny)
library(bslib)

source("functions.R")  # APIの関数をあとで作る予定

ui <- navbarPage(
  title = "Shohei Ohtani Stats App",
  tabPanel("About", "ここにAboutページ内容"),
  tabPanel("Data Download", "ここにデータ取得ページ"),
  tabPanel("Data Exploration", "ここにグラフと集計ページ")
)

server <- function(input, output, session) {
  # サーバー処理は後ほど追加
}

shinyApp(ui = ui, server = server)

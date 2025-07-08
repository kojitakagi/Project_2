library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

source("functions.R")

stats <- get_monthly_stats()

ui <- fluidPage(
  titlePanel("Shohei Ohtani - 2024 Monthly Batting Stats"),
  tabsetPanel(
    tabPanel("Table", DT::DTOutput("stats_table")),
    tabPanel("Plot", plotlyOutput("stats_plot"))
  )
)

server <- function(input, output) {
  output$stats_table <- DT::renderDT({
    stats %>%
      mutate(month_name = factor(month_name, levels = month.abb)) %>%  # 月順に並べる準備
      arrange(month_name) %>%                                          # 月順で並べる
      select(month_name, stat.gamesPlayed, stat.avg, stat.obp, stat.slg, stat.ops, stat.homeRuns, stat.stolenBases)
  })
  
  output$stats_plot <- renderPlotly({
    stats$month_name <- factor(stats$month_name, levels = month.abb)
    
    ggplot(stats, aes(x = month_name, y = stat.homeRuns)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Home Runs by Month", x = "Month", y = "Home Runs")
  })
  
}

shinyApp(ui = ui, server = server)

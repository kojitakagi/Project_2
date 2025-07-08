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
    tabPanel("Plot", plotlyOutput("stats_plot")),
    tabPanel("Stolen Bases vs AVG", plotlyOutput("stats_plot2"))  
  )
)


server <- function(input, output) {
  output$stats_table <- DT::renderDT({
    stats %>%
      mutate(month_name = factor(month_name, levels = month.abb)) %>%  
      arrange(month_name) %>%                                          
      select(month_name, stat.gamesPlayed, stat.avg, stat.obp, stat.slg, stat.ops, stat.homeRuns, stat.stolenBases)
  })
  
  output$stats_plot <- renderPlotly({
    stats$month_name <- factor(stats$month_name, levels = month.abb)
    
    ggplot(stats, aes(x = month_name, y = stat.homeRuns)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Home Runs by Month", x = "Month", y = "Home Runs")
  })
  
  output$stats_plot2 <- renderPlotly({
    ggplot(stats, aes(
      x = factor(month_name, levels = month.abb),
      y = as.numeric(stat.stolenBases),
      fill = as.numeric(stat.avg)
    )) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(
        title = "Stolen Bases by Month (Colored by AVG)",
        x = "Month", y = "Stolen Bases",
        fill = "AVG"
      )
  })
  
}

shinyApp(ui = ui, server = server)

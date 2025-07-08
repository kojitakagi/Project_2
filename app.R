# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)
library(DT)

# ---- Famous MLB players ----
famous_players <- tibble::tibble(
  name = c("Shohei Ohtani", "Aaron Judge", "Ronald Acuña Jr.", "Juan Soto", "Mookie Betts"),
  id = c(660271, 592450, 660670, 665742, 605141)
)

# ---- Get Home vs Away stats ----
get_home_away_stats <- function(player_id, season = 2024) {
  url <- paste0("https://statsapi.mlb.com/api/v1/people/", player_id,
                "/stats?stats=homeAndAway&group=hitting&season=", season)
  json <- fromJSON(url, flatten = TRUE)
  
  if (length(json$stats$splits[[1]]) == 0) return(NULL)
  
  splits <- json$stats$splits[[1]]
  df <- tibble::as_tibble(splits) %>%
    mutate(
      Location = ifelse(isHome, "Home", "Away"),
      BattingAverage = as.numeric(stat.avg),
      HomeRuns = as.numeric(stat.homeRuns),
      StolenBases = as.numeric(stat.stolenBases)
    ) %>%
    select(Location, BattingAverage, HomeRuns, StolenBases)
  return(df)
}

# ---- Get monthly stats ----
get_monthly_stats <- function(player_id = 660271, season = 2024) {
  url <- paste0("https://statsapi.mlb.com/api/v1/people/", player_id,
                "/stats?stats=byMonth&group=hitting&season=", season)
  json <- fromJSON(url, flatten = TRUE)
  splits <- json$stats$splits[[1]]
  
  df <- tibble::as_tibble(splits) %>%
    transmute(
      month = factor(month.name[as.integer(month)], levels = month.name),
      HR = as.numeric(stat.homeRuns),
      SB = as.numeric(stat.stolenBases),
      AVG = as.numeric(stat.avg)
    ) %>%
    pivot_longer(cols = c(HR, SB, AVG), names_to = "Metric", values_to = "Value") %>%
    arrange(month)
  return(df)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("MLB Star Player Stats (2024)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_id", "Select a player:",
                  choices = setNames(famous_players$id, famous_players$name),
                  selected = 660271),
      selectInput("metric", "Select metric to compare:",
                  choices = c("BattingAverage", "HomeRuns", "StolenBases"),
                  selected = "HomeRuns"),
      checkboxGroupInput("metrics", "Select Metrics to Display:",
                         choices = c("HR", "SB", "AVG"),
                         selected = c("HR", "SB", "AVG")),
      radioButtons("plot_type", "Plot Type:",
                   choices = c("Facet per Metric" = "facet", "Combined (HR/SB bar + AVG line)" = "combined"),
                   selected = "facet")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Home vs Away", plotOutput("barPlot")),
        tabPanel("Monthly Stats", plotOutput("monthlyPlot", height = "600px")),
        tabPanel("Raw Data", DT::dataTableOutput("rawData"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Home vs Away Data
  player_data <- reactive({
    get_home_away_stats(player_id = input$selected_id)
  })
  
  # Monthly Data
  data <- reactive({
    df <- get_monthly_stats(player_id = input$selected_id)
    df %>% filter(Metric %in% input$metrics)
  })
  
  # ---- Home vs Away Plot ----
  output$barPlot <- renderPlot({
    df <- player_data()
    req(df)
    
    player_name <- famous_players$name[famous_players$id == input$selected_id]
    y_limits <- switch(input$metric,
                       "BattingAverage" = c(0, 0.4),
                       "HomeRuns" = c(0, 40),
                       "StolenBases" = c(0, 40))
    
    ggplot(df, aes(x = Location, y = .data[[input$metric]], fill = Location)) +
      geom_col(width = 0.6) +
      coord_cartesian(ylim = y_limits) +
      labs(
        title = paste0("2024 ", player_name, ": ", input$metric),
        x = "Location",
        y = input$metric
      ) +
      theme_minimal(base_size = 16) +
      scale_fill_manual(values = c("Home" = "skyblue", "Away" = "tomato"))
  })
  
  # ---- Monthly Plot ----
  output$monthlyPlot <- renderPlot({
    df <- data()
    player_name <- famous_players$name[famous_players$id == input$selected_id]
    
    if (input$plot_type == "facet") {
      ggplot(df, aes(x = month, y = Value, fill = Metric)) +
        geom_col(show.legend = FALSE, width = 0.6) +
        facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
        labs(title = paste("Monthly Stats for", player_name), x = "Month", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      df_wide <- df %>% pivot_wider(names_from = Metric, values_from = Value)
      ggplot(df_wide, aes(x = month)) +
        geom_col(aes(y = HR), fill = "steelblue", width = 0.4, position = position_nudge(x = -0.2)) +
        geom_col(aes(y = SB), fill = "tomato", width = 0.4, position = position_nudge(x = 0.2)) +
        geom_line(aes(y = AVG * 50, group = 1), color = "darkgreen", size = 1.2) +
        geom_point(aes(y = AVG * 50), color = "darkgreen", size = 2) +
        scale_y_continuous(
          name = "HR / SB",
          sec.axis = sec_axis(~./50, name = "AVG")
        ) +
        labs(title = paste("Monthly Stats for", player_name), x = "Month") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # ---- Raw Data ----
  output$rawData <- DT::renderDataTable({
    df <- data() %>%
      pivot_wider(names_from = Metric, values_from = Value)
    DT::datatable(df, options = list(pageLength = 12))
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)

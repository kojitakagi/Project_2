# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)
library(DT)
library(GGally)

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
        tabPanel("Raw Data", DT::dataTableOutput("rawData")),
        tabPanel("Cumulative", plotOutput("cumulativePlot", height = "500px")),
        tabPanel("Contingency Table",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("hrLevel", "HR Level", choices = c("Low", "Mid", "High")),
                     selectInput("sbLevel", "SB Level", choices = c("Low", "Mid", "High"))
                   ),
                   mainPanel(
                     tableOutput("contingencyTable")
                   )
                 )
        ),
        
        tabPanel("Data Exploration",
                 fluidRow(
                   column(4,
                          selectInput("xvar", "X Variable:", choices = c("month", "Metric")),
                          selectInput("yvar", "Y Variable:", choices = c("Value", "Cumulative")),
                          selectInput("facet_var", "Facet by:", choices = c("Metric", "month", "None")),
                          radioButtons("plot_type_explore", "Plot Type:",
                                       choices = c("Bar", "Boxplot", "Scatter"), selected = "Bar"),
                          br(),
                          downloadButton("downloadData", "Download CSV")
                   ),
                   column(8,
                          plotOutput("explorePlot", height = "500px")
                   )
                 )
        ),
        tabPanel("About",
                 h3("MLB Star Player Stats App"),
                 p("This app provides interactive visualizations of 2024 MLB batting statistics for top players including Shohei Ohtani, Ronald Acuña Jr., Aaron Judge, and others."),
                 p("Users can explore home vs away performance, monthly trends, raw data, and contingency tables."),
                 p("A highlight is the cumulative plot of home runs and stolen bases. This feature allows users to discover gameplay shifts across the season for each player — such as sudden surges in performance or plateaus."),
                 p("Data Source: ",
                   a("MLB Stats API", href = "https://statsapi.mlb.com/api/", target = "_blank")),
                 p("GitHub Repository: ",
                   a("Project_2 on GitHub", href = "https://github.com/kojitakagi/Project_2", target = "_blank")),
                 img(src = "Shohei.jpg", height = "300px", alt = "Shohei Ohtani"),
                 br(),
                 h4("Tab Descriptions:"),
                 tags$ul(
                   tags$li(strong("Home vs Away"), ": Compare player's performance at home vs away games."),
                   tags$li(strong("Monthly Stats"), ": Visualize monthly performance in HR, SB, and AVG."),
                   tags$li(strong("Raw Data"), ": View the raw data behind the visualizations."),
                   tags$li(strong("Cumulative"), ": Track cumulative home runs and stolen bases over the season.")
                 )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  player_data <- reactive({
    get_home_away_stats(player_id = input$selected_id)
  })
  
  data <- reactive({
    df <- get_monthly_stats(player_id = input$selected_id)
    df %>% filter(Metric %in% input$metrics)
  })
  
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
  
  output$cumulativePlot <- renderPlot({
    df <- get_monthly_stats(player_id = input$selected_id) %>%
      filter(Metric %in% c("HR", "SB")) %>%
      arrange(month) %>%
      group_by(Metric) %>%
      mutate(Cumulative = cumsum(Value))
    
    player_name <- famous_players$name[famous_players$id == input$selected_id]
    
    ggplot(df, aes(x = month, y = Cumulative, color = Metric, group = Metric)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = paste("Cumulative HR & SB for", player_name),
        x = "Month",
        y = "Cumulative Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$contingencyTable <- renderTable({
    monthly_data <- get_monthly_stats(player_id = input$selected_id) %>%
      pivot_wider(names_from = Metric, values_from = Value)
    
    monthly_data$HR_cat <- cut(monthly_data$HR,
                               breaks = c(-1, 5, 10, Inf),
                               labels = c("Low", "Mid", "High"))
    monthly_data$SB_cat <- cut(monthly_data$SB,
                               breaks = c(-1, 5, 10, Inf),
                               labels = c("Low", "Mid", "High"))
    
    tab <- table(monthly_data$HR_cat, monthly_data$SB_cat)
    filtered_count <- tab[input$hrLevel, input$sbLevel]
    
    data.frame(
      `HR Level` = input$hrLevel,
      `SB Level` = input$sbLevel,
      Count = filtered_count
    )
  })
  
  
  explore_data <- reactive({
    df <- get_monthly_stats(player_id = input$selected_id) %>%
      arrange(month)
    df_cum <- df %>%
      filter(Metric %in% c("HR", "SB")) %>%
      group_by(Metric) %>%
      mutate(Cumulative = cumsum(Value))
    df_full <- full_join(df, df_cum, by = c("month", "Metric", "Value")) %>%
      mutate(Cumulative = ifelse(is.na(Cumulative), NA, Cumulative))
    return(df_full)
  })
  
  output$explorePlot <- renderPlot({
    df <- explore_data()
    req(input$xvar, input$yvar)
    
    p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar))
    
    if (input$plot_type_explore == "Bar") {
      p <- p + geom_bar(stat = "identity", fill = "steelblue")
    } else if (input$plot_type_explore == "Boxplot") {
      p <- p + geom_boxplot()
    } else {
      p <- p + geom_point(size = 3)
    }
    
    if (input$facet_var != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    p + theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$rawData <- DT::renderDataTable({
    df <- data() %>%
      pivot_wider(names_from = Metric, values_from = Value)
    DT::datatable(df, options = list(pageLength = 12))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("player_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- explore_data()
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ---- Run App ----
shinyApp(ui = ui, server = server)

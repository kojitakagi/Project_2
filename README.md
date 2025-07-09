# MLB Star Player Stats App (Project 2)
This Shiny app allows users to explore the 2024 batting stats of famous MLB players such as Shohei Ohtani and others. It uses real-time data from the MLB Stats API and provides:

## Features
- **Player Selection:** Choose from five popular MLB players.
- **Home vs Away Stats:** View batting average (AVG), home runs (HR), and stolen bases (SB).
- **Monthly Stats:** Select multiple metrics (HR, SB, AVG) and visualize them either in separate facets or combined view.
- **Cumulative Plot:** Track season-long trends of HR and SB by player.
- **Raw Data Tab:** View the raw data used for the visualizations.

## How to Run
1. Install required packages (if not already installed):
```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "jsonlite", "DT"))
```
2. Launch the app using this command:
```r
shiny::runGitHub("Project_2", "kojitakagi")
```

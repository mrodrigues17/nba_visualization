library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
player_data <- read.csv("player_data.csv")
Players <- read.csv("Players.csv")
season_stats <- read.csv("Seasons_Stats.csv")

season_stats <- left_join(season_stats, Players, by = "Player")

season_stats$Player <- as.factor(season_stats$Player)


shinyUI(dashboardPage(skin="black",
                      dashboardHeader(title = "The NBA's GOAT"),
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Statistics by Player", tabName = "player_stat", icon = icon("star-o")),
                          menuItem("About", tabName = "about", icon = icon("question-circle")),
                          menuItem(
                            list(selectInput("type", label = h5("View player statistics over entire NBA history or compare individual player careers:"),
                                            choices = list("Historical", "By Player Career"),
                                                   selected = "Historical"),
                                 
                              selectInput("stat", label = h5("Choose a statistic"),
                                             list("Average Player Weight" = "weight",
                                                  "Average Player Height" = "height",
                                                  "Points Per Game" = "PTS",
                                                  "Blocks Per Game" = "BLK",
                                                  "Steals Per Game" = "STL",
                                                  "Assists Per Game" = "AST",
                                                  "Rebounds Per Game" = "TRB",
                                                  "Free Throw Percentage" = "FT",
                                                  "Three Point Percentage" = "X3P",
                                                  "Field Goal Percentage" = "FG"), selected = "PTS"),

                              
                              conditionalPanel(condition = "input.type == 'Historical'",
                              selectInput("percentile", label = h5("Choose NBA percentile to compare to:"),
                                          list("25th Percentile" = "NBA 25th Percentile",
                                               "50th Percentile" = "NBA 50th Percentile",
                                               "75th Percentile" = "NBA 75th Percentile",
                                               "90th Percentile" = "NBA 90th Percentile",
                                               "95th Percentile" = "NBA 95th Percentile",
                                               "99th Percentile" = "NBA 99th Percentile"), selected = "NBA_50th")),
                                 
                                 selectizeInput("player",
                                                label = h5("Type the name of the NBA players you wish to compare:"),
                                                choices = levels(season_stats$Player),
                                                selected = "Michael Jordan*", multiple = T, options = list(maxItems = 5))
                            
                            )
                          )
                        )
                        
                      ),
                      
                      
                      dashboardBody(
                        tags$head(
                          tags$style(type="text/css", "select { max-width: 360px; }"),
                          tags$style(type="text/css", ".span4 { max-width: 360px; }"),
                          tags$style(type="text/css",  ".well { max-width: 360px; }")
                        ),
                        
                        tabItems(  
                          tabItem(tabName = "about",
                                  h2("About this App"),
                                  
                                  HTML('<br/>'),
                                  
                                  fluidRow(
                                    box(title = "Author: Max Rodrigues", background = "black", width=7, collapsible = F,
                                        
                                        helpText(p(strong("This application is meant to compare the statistics for all NBA players between 1950 and 2018 as well as view differences in player careers"))),
                                        
                                        helpText(p("Please contact me at my email mrod1791@gmail.com or visit my",
                                                   a(href ="https://mrodrigues17.github.io./", "personal page", target = "_blank"),
                                                   "for more information, to suggest improvements or report errors."))
  
                                    )
                                  )
                          ),
                          

                          tabItem(tabName = "player_stat",
                          box(plotOutput("plot"), width=11, collapsible = TRUE))

                          



                      )

                          )
 )  )   
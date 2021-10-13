library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(reshape2)

shinyServer(function(input, output) {

  

    
player_data <- read.csv("player_data.csv")
Players <- read.csv("Players.csv")
season_stats <- read.csv("Seasons_Stats.csv")
    
season_stats <- dplyr::left_join(season_stats, Players, by = "Player")
    
season_stats$Player <- as.factor(season_stats$Player)
    
season_stats$id <- 0
    
x <- levels(season_stats$Player)
y <- x[-1]
    
   withProgress(message = 'Making plot', value = 0, {
      
    incProgress(3, detail = paste("Doing part 1"))
      
      l = 0
      play = list()
      for(i in y){
        l = l + 1
        z = season_stats[season_stats$Player == i, ]
        play[[l]] <- z
        
      }
      
      SeasonNum <- function(dataframe) {
        x <- seq(nrow(dataframe))
        return(x)
      }
      
      incProgress(2, detail = paste("Doing part 2"))
      
      
      season_list <- list()
      i = 0
      for(p in play) {
        i = i + 1
        x <- SeasonNum(p)
        season_list[[i]] <- x
      }
      
      incProgress(2, detail = paste("Doing part 3"))
      
      
      ind = 0
      players_list <- list()
      for(i in season_list) {
        ind = ind + 1
        x <- as.data.frame(play[ind])
        x$id <- season_list[[ind]]
        players_list[[ind]] <- x
      }
      
      incProgress(3, detail = paste("Doing part 4"))
      
      final_players <- do.call("rbind", players_list)
      final_players$Player <- factor(final_players$Player)
      
      color_palette <- c("red", "blue", "black", "green", "orange")
      
    })
    

    
    
output$plot <- renderPlot({
    
  
  

  if(input$type == "Historical") {
    

  

  if (input$stat == "weight") {

    


weight_q <- season_stats %>%
      group_by(Year) %>%
      summarise(NBA_25th = quantile(weight, .25, na.rm = T),
                NBA_50th = quantile(weight, .5, na.rm = T),
                NBA_75th = quantile(weight, .75, na.rm = T),
                NBA_90th = quantile(weight, .9, na.rm = T),
                NBA_95th = quantile(weight, .95, na.rm = T),
                NBA_99th = quantile(weight, .99, na.rm = T))
    
weight_p <- season_stats[season_stats$Player %in% input$player, ] %>%
      group_by(Player, Year) %>%
      summarise(p_weight = mean(weight))
    
new_players <- left_join(weight_q, weight_p, by = "Year")
    
player_new <- spread(new_players, key = Player, value = p_weight)
    
x <- player_new[,-length(player_new)]
    
player_long <- melt(x, id.vars = names(x)[1])


levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"



ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value*2.20462, color = variable)) +
  geom_line(size = 1) +
  ylab("Weight in Pounds") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Weight of Players Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) +
  ylim(100,350)
    


} else if (input$stat == "height") {

  
season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(height, .25, na.rm = T),
              NBA_50th = quantile(height, .5, na.rm = T),
              NBA_75th = quantile(height, .75, na.rm = T),
              NBA_90th = quantile(height, .9, na.rm = T),
              NBA_95th = quantile(height, .95, na.rm = T),
              NBA_99th = quantile(height, .99, na.rm = T))
  
season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(height))
  
new_players <- left_join(season_new, season_new2, by = "Year")
  
player_new <- spread(new_players, key = Player, value = p_stat)
  
x <- player_new[,-length(player_new)]
  
player_long <- melt(x, id.vars = names(x)[1])
  

levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"


ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value*.0328084, color = variable)) +
    geom_line(size = 1) +
    ylab("Height in Feet") +
    theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Height of Players Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) +
  ylim(5,8)

  
        
} else if (input$stat == "PTS") {
  
season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(PTS/G, .25, na.rm = T),
              NBA_50th = quantile(PTS/G, .5, na.rm = T),
              NBA_75th = quantile(PTS/G, .75, na.rm = T),
              NBA_90th = quantile(PTS/G, .9, na.rm = T),
              NBA_95th = quantile(PTS/G, .95, na.rm = T),
              NBA_99th = quantile(PTS/G, .99, na.rm = T))
  
season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(PTS/G))
  
new_players <- left_join(season_new, season_new2, by = "Year")
  
player_new <- spread(new_players, key = Player, value = p_stat)
  
x <- player_new[,-length(player_new)]
  
player_long <- melt(x, id.vars = names(x)[1])


levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"

 
ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value, color = variable)) +
    geom_line(size = 1) +
    ylab("Points Per Game") +
    theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Points Per Game Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) + 
  ylim(0,51) 

  

  
} else if (input$stat == "BLK") {
 
season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(BLK/G, .25, na.rm = T),
              NBA_50th = quantile(BLK/G, .5, na.rm = T),
              NBA_75th = quantile(BLK/G, .75, na.rm = T),
              NBA_90th = quantile(BLK/G, .9, na.rm = T),
              NBA_95th = quantile(BLK/G, .95, na.rm = T),
              NBA_99th = quantile(BLK/G, .99, na.rm = T))
  
season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(BLK/G))
  
new_players <- left_join(season_new, season_new2, by = "Year")
  
player_new <- spread(new_players, key = Player, value = p_stat)
  
x <- player_new[,-length(player_new)]
  
player_long <- melt(x, id.vars = names(x)[1])


levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"


ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value, color = variable)) +
    geom_line(size = 1) +
    ylab("Blocks Per Game") +
    theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Blocks Per Game Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) +
  ylim(0,6)

  
} else if (input$stat == "STL") {
  
season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(STL/G, .25, na.rm = T),
              NBA_50th = quantile(STL/G, .5, na.rm = T),
              NBA_75th = quantile(STL/G, .75, na.rm = T),
              NBA_90th = quantile(STL/G, .9, na.rm = T),
              NBA_95th = quantile(STL/G, .95, na.rm = T),
              NBA_99th = quantile(STL/G, .99, na.rm = T))
  
season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(STL/G))
  
new_players <- left_join(season_new, season_new2, by = "Year")
  
player_new <- spread(new_players, key = Player, value = p_stat)
  
x <- player_new[,-length(player_new)]
  
player_long <- melt(x, id.vars = names(x)[1])


levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"


ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value, color = variable)) +
    geom_line(size = 1) +
    ylab("Steals Per Game") +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
    ggtitle("Steals Per Game Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) +
  ylim(0,4)

  
} else if (input$stat == "AST") {
  
season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(AST/G, .25, na.rm = T),
              NBA_50th = quantile(AST/G, .5, na.rm = T),
              NBA_75th = quantile(AST/G, .75, na.rm = T),
              NBA_90th = quantile(AST/G, .9, na.rm = T),
              NBA_95th = quantile(AST/G, .95, na.rm = T),
              NBA_99th = quantile(AST/G, .99, na.rm = T))
  
season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(AST/G))
  
new_players <- left_join(season_new, season_new2, by = "Year")
  
player_new <- spread(new_players, key = Player, value = p_stat)
  
x <- player_new[,-length(player_new)]
  
player_long <- melt(x, id.vars = names(x)[1])
 

levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"

 
ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value, color = variable)) +
    geom_line(size = 1) +
    ylab("Assists Per Game") +
    theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Assists Per Game Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) + 
  ylim(0,15)

  
} else if (input$stat == "TRB") {
  
season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(TRB/G, .25, na.rm = T),
              NBA_50th = quantile(TRB/G, .5, na.rm = T),
              NBA_75th = quantile(TRB/G, .75, na.rm = T),
              NBA_90th = quantile(TRB/G, .9, na.rm = T),
              NBA_95th = quantile(TRB/G, .95, na.rm = T),
              NBA_99th = quantile(TRB/G, .99, na.rm = T))
  
season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(TRB/G))
  
new_players <- left_join(season_new, season_new2, by = "Year")
  
player_new <- spread(new_players, key = Player, value = p_stat)
  
x <- player_new[,-length(player_new)]
  
player_long <- melt(x, id.vars = names(x)[1])
  

levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"


ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value, color = variable)) +
    geom_line(size = 1) +
    ylab("Rebounds Per Game") +
    theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Rebounds Per Game Over NBA History") +
  scale_color_manual(name="Percentile/Player Name",
                     values = c("black", "blue", "red", "green", "orange", "tan3")) +
  ylim(0,30)

} else if (input$stat == "FT") {
  
  season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(FT/FTA, .25, na.rm = T),
              NBA_50th = quantile(FT/FTA, .5, na.rm = T),
              NBA_75th = quantile(FT/FTA, .75, na.rm = T),
              NBA_90th = quantile(FT/FTA, .9, na.rm = T),
              NBA_95th = quantile(FT/FTA, .95, na.rm = T),
              NBA_99th = quantile(FT/FTA, .99, na.rm = T))
  
  season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(FT/FTA))
  
  new_players <- left_join(season_new, season_new2, by = "Year")
  
  player_new <- spread(new_players, key = Player, value = p_stat)
  
  x <- player_new[,-length(player_new)]
  
  player_long <- melt(x, id.vars = names(x)[1])
  
  
  levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"
  
  
  ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value * 100, color = variable)) +
    geom_line(size = 1) +
    ylab("Free Throw %") +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16)) +
    ggtitle("Free Throw Percentage Over NBA History") +
    scale_color_manual(name="Percentile/Player Name",
                       values = c("black", "blue", "red", "green", "orange", "tan3")) + 
    ylim(0,100)
  
} else if (input$stat == "X3P") {
  
  season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(X3P/X3PA, .25, na.rm = T),
              NBA_50th = quantile(X3P/X3PA, .5, na.rm = T),
              NBA_75th = quantile(X3P/X3PA, .75, na.rm = T),
              NBA_90th = quantile(X3P/X3PA, .9, na.rm = T),
              NBA_95th = quantile(X3P/X3PA, .95, na.rm = T),
              NBA_99th = quantile(X3P/X3PA, .99, na.rm = T))
  
  season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(X3P/X3PA))
  
  new_players <- left_join(season_new, season_new2, by = "Year")
  
  player_new <- spread(new_players, key = Player, value = p_stat)
  
  x <- player_new[,-length(player_new)]
  
  player_long <- melt(x, id.vars = names(x)[1])
  
  
  levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"
  
  
  ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value * 100, color = variable)) +
    geom_line(size = 1) +
    ylab("Three Point %") +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16)) +
    ggtitle("Three Point Percentage Over NBA History") +
    scale_color_manual(name="Percentile/Player Name",
                       values = c("black", "blue", "red", "green", "orange", "tan3")) +
    ylim(0,100)
  
} else if (input$stat == "FG") {
  
  season_new <- season_stats %>%
    group_by(Year) %>%
    summarise(NBA_25th = quantile(FG/FGA, .25, na.rm = T),
              NBA_50th = quantile(FG/FGA, .5, na.rm = T),
              NBA_75th = quantile(FG/FGA, .75, na.rm = T),
              NBA_90th = quantile(FG/FGA, .9, na.rm = T),
              NBA_95th = quantile(FG/FGA, .95, na.rm = T),
              NBA_99th = quantile(FG/FGA, .99, na.rm = T))
  
  season_new2 <- season_stats[season_stats$Player %in% input$player, ] %>%
    group_by(Player, Year) %>%
    summarise(p_stat = mean(FG/FGA))
  
  new_players <- left_join(season_new, season_new2, by = "Year")
  
  player_new <- spread(new_players, key = Player, value = p_stat)
  
  x <- player_new[,-length(player_new)]
  
  player_long <- melt(x, id.vars = names(x)[1])
  
  
  levels(player_long$variable)[levels(player_long$variable)=="NBA_25th"] <- "NBA 25th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_50th"] <- "NBA 50th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_75th"] <- "NBA 75th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_90th"] <- "NBA 90th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_95th"] <- "NBA 95th Percentile"
  levels(player_long$variable)[levels(player_long$variable)=="NBA_99th"] <- "NBA 99th Percentile"
  
  
  ggplot(data = player_long[player_long$variable %in% input$player | player_long$variable %in% input$percentile,], aes(x = Year, y = value * 100, color = variable)) +
    geom_line(size = 1) +
    ylab("Field Goal %") +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16)) +
    ggtitle("Field Goal Percentage Over NBA History") +
    scale_color_manual(name="Percentile/Player Name",
                       values = c("black", "blue", "red", "green", "orange", "tan3")) +
    ylim(0,100)
  
} 
  
  

} else {



if (input$stat == "PTS") {
ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= PTS/G, color = Player)) +
        geom_line(size = 1) +
        theme_bw() +
        xlab("Player Season Number") +
        ylab("Points Per Game") +
        scale_color_manual(values = color_palette) +
        theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16)) +
    ggtitle("Player Points Per Game Over Career") + 
    ylim(0,55)
    } else if (input$stat == "BLK") {
          
ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= BLK/G, color = Player)) +
            geom_line(size = 1.5) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Blocks Per Game") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16, face = "bold"),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 14),
              plot.title = element_text(size = 16)) +
            ggtitle("Player Blocks Per Game Over Career") +
            ylim(0,6)
            
          
        } else if (input$stat == "STL") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= STL/G, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Steals Per Game") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Steals Per Game Over Career") +
            ylim(0,4)
          
          
        } else if (input$stat == "AST") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= AST/G, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Assists Per Game") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Assists Per Game Over Career") +
            ylim(0,15)
          
          
        } else if (input$stat == "TRB") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= TRB/G, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Rebounds Per Game") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Rebounds Per Game Over Career") +
            ylim(0,30)
          
          
        } else if (input$stat == "X3P") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= (X3P/X3PA) * 100, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Three Point %") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Three Point Percentage Over Career") +
           ylim(0,100)
          
          
        } else if (input$stat == "FT") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= (FT/FTA)*100, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Free Throw %") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Free Throw Percentage Over Career") +
            ylim(0,100)
          
          
        } else if (input$stat == "weight") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= weight*2.20462, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Weight in Pounds") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Weight", "Does not change over time") +
            ylim(100,350)
          
        } else if (input$stat == "height") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= height*.0328084, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Height in Feet") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) + 
            ggtitle("Player Height", "Does not change over time") +
            ylim(5,8)
          
        } else if (input$stat == "FG") {
          
          ggplot(final_players[final_players$Player %in% input$player, ], aes(x = id, y= (FG/FGA)*100, color = Player)) +
            geom_line(size = 1) +
            theme_bw() +
            xlab("Player Season Number") +
            ylab("Field Goal Percentage") +
            scale_color_manual(values = color_palette) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16, face = "bold"),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  plot.title = element_text(size = 16)) +
            ggtitle("Player Field Goal Percentage Over Career") +
            ylim(0,100)
          
        
 
  }
  
}

  
  
  
  
})


})
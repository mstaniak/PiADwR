library("data.table")

dt <- fread("Joined_Data_mod.csv", drop = 1)
# FTHG and HG = Full Time Home Team Goals
# FTAG and AG = Full-Time Away Team Goals
# FTR and Res = Full-Time Result (H=Home Win, D=Draw, A=Away Win)
# HTHG = Half Time Home Team Goals
# HTAG = Half Time Away Team Goals
# HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)
# Referee = Match Referee
# HS = Home Team Shots
# AS = Away Team Shots
# HST = Home Team Shots on Target
# AST = Away Team Shots on Target
# HF = Home Team Fouls Committed
# AF = Away Team Fouls Committed
# HC = Home Team Corners
# AC = Away Team Corners
# HY = Home Team Yellow Cards
# AY = Away Team Yellow Cards
# HR = Home Team Red Cards
# AR = Away Team Red Cards
library("ggplot2")


statystyki_sedziego <- function(data, sedzia, grid, tlo, tekst)
{
  data <- data[data$Referee == sedzia]
  length(data$Referee)
  
  wykres <- ggplot(data, aes(1:length(data$Referee), HF+AF)) +
    geom_line() +
    labs(y="ilosc fauli", x = "mecz")+
    geom_point()
  
  
  wykres +
    theme(
      panel.grid.major = element_line(colour = grid),
      panel.background = element_rect(fill = tlo),
      axis.text = element_text(colour = tekst)
    )
  
}

statystyki_sedziego(dt, "C Foy", "red", "white", "pink")
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# LIBRARIES 

library(shiny)
library(data.table)
library(bigchess)
library(chess)
library(rchess)
library(stringr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(shinyalert)
library(httr)
library(plyr)
library(dplyr)
library(shinythemes)


####################################
##### STATISTICS TAB FUNCTIONS #####
####################################


get_link_stats <- function(username, start, end){
  
  username <- tolower(username)
  
  start <- as.numeric(as.POSIXct(start, format="%Y-%m-%d")) * 1000
  end   <- as.numeric(as.POSIXct(end, format="%Y-%m-%d")) * 1000
  
  base_link <- "https://lichess.org/api/games/user/"
  weblink <- paste0(base_link, username, "?since=", start, "&until=", end)

}

get_games_stats_df <- function(weblink){
  
  games <- read.pgn(weblink,
                    add.tags = c("WhiteElo", "BlackElo"),
                    n.moves = F,
                    extract.moves = -1,
                    last.move = F,
                    stat.moves = F,
                    big.mode = F,
                    quiet = T,
                    ignore.other.games = F,
                    source.movetext = F)
}

get_game_info_df <- function(weblink, n_moves = 1){
  
  game_info_df <- read.pgn(weblink,
                           add.tags = c("WhiteElo", "BlackElo"),
                           n.moves = F,
                           extract.moves = n_moves,
                           last.move = F,
                           stat.moves = F,
                           big.mode = F,
                           quiet = T,
                           ignore.other.games = F,
                           source.movetext = F)
}

get_opening_coords <- function(opening_move){
  
  # opening move can only be done with a pawn or a Knight
  coords <- str_split(opening_move , "")
  if (coords[[1]][1] == "N"){
    
    x <- coords[[1]][2]
    y <- coords[[1]][3]
    
  } else {
    
    x <- which(letters == coords[[1]][1])
    y <- coords[[1]][2]
    
  }
  
  return(list(x,y))
}

get_heatmap_dt <- function(game_df, username){
  
  username <- tolower(username)
  
  game_dt = data.table(game_df)[, c("Date", "White", 'Black', 'Result', 'WhiteElo', 'BlackElo', 'W1')]
  game_dt[, `:=` (White = tolower(White), Black = tolower(Black))]
  game_dt[White == username, c("X", "Y") := get_opening_coords(W1)]
  game_dt[, N := .N, by = c("X", "Y")]
  game_dt <- game_dt[White == username, ]  
  
}

get_wins_dt <- function(game_df, username){
  
  player <- isolate(username)
  downloaded_games <- data.table(game_df)
  # deleting records with NAs
  downloaded_games <-  downloaded_games[!is.na(WhiteElo) & !is.na(BlackElo), ]
  
  # Result column is factor
  
  downloaded_games[, Result := lapply(.SD, toString), .SDcols = c("Result")] 
  downloaded_games[, `:=` (white_win = substr(Result, 1, 1),
                           black_win = substr(Result, 3, 3))] 
  
  downloaded_games$player_rating <- 0
  downloaded_games[Black == player]$player_rating <- downloaded_games[Black == player]$BlackElo
  downloaded_games[White == player]$player_rating <- downloaded_games[White == player]$WhiteElo
  
  downloaded_games$oponent_rating <- 0
  downloaded_games[Black == player]$oponent_rating <- downloaded_games[Black == player]$WhiteElo
  downloaded_games[White == player]$oponent_rating <- downloaded_games[White == player]$BlackElo
  
  downloaded_games[, rating_diff := round_any(player_rating - oponent_rating, 50)]
  downloaded_games[, player_win := ifelse((Black == player) & (black_win == 1), 1 ,
                                          ifelse((White == player) & (white_win == 1), 1 , 0))]
  
  downloaded_games[, `:=` (win_perc = round(sum(player_win)/.N * 100, 0)), by = rating_diff]
  downloaded_games[, NMoves := round_any(NMoves, 10)]
  downloaded_games[, `:=` (count_all = .N), by = NMoves]
  downloaded_games[, `:=` (count_wins = sum(player_win)), by = NMoves]
  downloaded_games[, `:=` (count_loss = count_all - count_wins), by = NMoves]
  
}

plot_win_by_moves <- function(game_dt){
  
  ggplot(game_dt) +
    geom_line(aes(NMoves, count_wins, colour='Wins')) +
    geom_line(aes(NMoves, count_loss, colour='Losses')) +
    theme_cowplot() +
    scale_colour_manual(values=c("red","green")) +
    labs(title = 'Wins/Loss by Number of Moves per games', x = '# Moves', y = '# Games') +
    scale_x_continuous(breaks = unique(game_dt[,NMoves]))
  
}

plot_win_ratio <- function(game_dt){
  
  ggplot(game_dt, aes(rating_diff, win_perc, label = win_perc)) +
    geom_point(size = 2) +
    geom_text(aes(label=win_perc), hjust=0.5, vjust=-1) +
    geom_line(size = 1.0) +
    theme_cowplot() +
    labs(title = 'Wins by rating difference', x = 'Rating difference', y = 'Winnings Percentage') +
    ylim(c(-5, 105)) + 
    scale_x_continuous(breaks = unique(game_dt[, rating_diff]))
  
}

plot_opening_heatmap <- function(game_dt){
  
  ggplot(game_dt, aes(X, Y, fill = N)) +
           geom_tile() + 
           scale_y_discrete(limits = factor(1:8)) + 
           geom_hline(yintercept= seq(0.5, 8.5, 1), size = 1) + 
           geom_vline(xintercept= seq(0.5, 8.5, 1), size = 1) +
           xlim(letters[1:8]) + 
           theme_cowplot() +
           theme(text = element_text(size=25, face = "bold"),
                 panel.border = element_rect(colour = "black", fill=NA, size=7)) + 
           labs(title = "Most played openings", x = NULL, y = NULL) + 
           coord_equal()
}

plot_elo_history <- function(game_dt){
  
  ggplot(game_dt[, max(player_rating), by = c("Date")], aes(x=Date, y = V1)) +
    geom_line(aes(group=1)) +
    labs(title = 'ELO history (showing maximum achieved ELO on a given day)', x = 'Date', y = 'Rating') + 
    scale_x_discrete(breaks = unique(game_dt[, Date])) + 
    theme_cowplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0))

}


####################################
####### REPLAY TAB FUNCTIONS #######
####################################


get_link_replay <- function(username){
  
  ### Creates a LICHESS API link to download the most
  ### recently finished match played by a given player
  
    base_link <- "https://lichess.org/api/games/user/"
    username_link <- paste0(username, "?max=1")
    link_string <- paste0(base_link, username_link)
    
}

get_game_replay <- function(weblink){
  
  ### Loads a .pgn game from a provided LICHESS API link
  
  loaded_games <- read_game(weblink, n_max = 1)
  
}

get_n_moves <- function(weblink){
  
  ### Extracts the total number of moves for a slider in Shiny App
  
  pgn <- readLines(weblink, warn = FALSE)
  pgn <- paste(pgn, collapse = "\n")
  chsspgn <- Chess$new()
  chsspgn$load_pgn(pgn)
  max_moves <- length(chsspgn$history())
  
}

plot_board_replay <- function(game_node, moves_in = 0){
  
  ### Plots the chessboard after a specified number of moves
  
  plot(game_node %>%
         forward(moves_in),
       type = "ggplot"
  )
}


####################################
###### PRACTICE TAB FUNCTIONS ######
####################################

new_game <- function(){
  new_match <- Chess$new()
}

computer_make_move <- function(game_node){
  
  if (!game_node$in_checkmate()){
    
    next_comp_move <- sample(game_node$moves(), size = 1)
    game_node$move(next_comp_move)
    game_node
    
  }
}

player_make_move <- function(game_node, input_move){
  
  if (!game_node$in_checkmate()){
    
    if (is.element(input_move, new_match$moves())){
      
      game_node$move(input_move)
      game_node
      
    }
  }
}



####################################
################ UI ################
####################################

ui <- fluidPage(
  
    theme = shinytheme('sandstone'),

    useShinyalert(),
  
    # Application title
    titlePanel("LICHESS Game Analysis App"),
    
    tabsetPanel(id = "tabs",
                
      tabPanel("Statistics", fluid = TRUE, 
                         
        titlePanel("Get insight into the statistics of your previous matches!"),
       
        sidebarLayout(
          sidebarPanel(
          
            textInput("user_name", "Provide the LICHESS username and time period below:"),
            
            dateRangeInput('dateRange',
                           label = 'Date range input: yyyy-mm-dd',
                           start = "2013-01-01", end = "2021-01-31"
            ),
            
            
          
            actionButton("download", "Download")),
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel('ELO History', plotOutput("elo_history")),
            
            tabPanel('W/L by number of moves', plotOutput("win_loss_per_moves")),
            
            tabPanel('Wins by rating difference', plotOutput("wins_ratio")),
            
            tabPanel('Most played openings', plotOutput("heatmap"))
            
          )
        )
      )
    ),
                
                
      tabPanel("Replay", fluid = TRUE, 
        
        titlePanel("Replay your last match!"),
        
        sidebarLayout(
          sidebarPanel(

            textInput("source", "Provide the LICHESS username below:"),
                   
            actionButton("ok", "Download"),
          
            uiOutput("moves"),
          
            tableOutput("game_info")),
                 
          mainPanel(
            
              plotOutput("board")
                  
          )
        )
      ),
      
      tabPanel("Practice", fluid = TRUE, 
               
        titlePanel("Play Against Artificial 'Intelligence'!"),
               
        sidebarLayout(
           sidebarPanel(
             
             actionButton("start", "New Game")
             
           ),
                 
          mainPanel(
          
               plotOutput("match"),
               textOutput("comp_move"))
                 
          )
        )
      )
)


####################################
############## SERVER ##############
####################################


server <- function(input, output, session) {
  
  
    ######################################################
    #################### STATS TAB #######################
    ######################################################
  
  
    observeEvent(input$download, {
    
      req(input$user_name)
      
      new_link <- get_link_stats(input$user_name,
                                 input$dateRange[1],
                                 input$dateRange[2])
      
      
      showNotification(
        paste('Downloading data, please wait'), type = "message", duration = 5)
      
      # link may have status 200 but possible that no games were played 
      # and an empty file is downloaded - need to take care of that
      
      len_check <- length(readLines(new_link, warn = FALSE))
      

      if (GET(new_link)$status == 200 & len_check != 0){
        
        showNotification(
          paste('The data is being processed, this may take a while'), type = "message", duration = 10)
        
        game_stats <- get_games_stats_df(new_link)
        rval_player_games <- get_wins_dt(game_stats, input$user_name)
        
        
        output$elo_history <- renderPlot({
          
          plot_elo_history(rval_player_games)
          
        })
        

        output$win_loss_per_moves <- renderPlot({
          
          plot_win_by_moves(rval_player_games)
          
        })
        
        
        output$wins_ratio <- renderPlot({
          
          plot_win_ratio(rval_player_games)
          
        })
        
        
        output$heatmap <- renderPlot({
          
          plot_opening_heatmap(get_heatmap_dt(game_df = game_stats,
                                              username = isolate(input$user_name)))
        })
        
        
      } else if (len_check == 0) {
        
        ## get a message saying that there are no games from that period
        shinyalert("OOF!", "This user did not play any matches between those dates!", type = "error")
        
      } else {
        
        ## get a message saying that there is no such user
        shinyalert("OOF!", "This user does not exist!", type = "error")
        
      }
    })
  
  
  
    ######################################################
    #################### REPLAY TAB ######################
    ######################################################
  

    ######## board generator from username input #########
  
    observeEvent(input$ok, {
      
        req(input$source)

        new_link <- get_link_replay(username = input$source)
        
        showNotification(
          paste('Downloading data, please wait'), type = "message", duration = 5)
        
        if (GET(new_link)$status == 200){
          
          downloaded_game <- get_game_replay(new_link)
          moves <- get_n_moves(new_link)
          summary_table <- data.table(get_game_info_df(new_link))[, c("White", "Black", "Date", "Result")]
          
          # plot board
          output$board <- renderPlot({
            
            plot_board_replay(downloaded_game,
                              moves_in = ifelse(is.null(input$moves), 0, input$moves))
            
          })
          
          
          output$game_info <- renderTable(align = "c",{
            
            summary_table
            
            
          })
          
          
          output$moves <- renderUI({
            div(sliderInput("moves",
                            "Moves since the beginning:",
                            min = 0,
                            max = moves,
                            value = 0,
                            step = 1))
          })
          
        } else {
          
          ## get a message saying that there is no such user
          shinyalert("OOF!", "This user does not exist!", type = "error")
          
        }
    })
  
  
  
  #######################################################
  ################### PRACTICE TAB ######################
  #######################################################

  
  
  ############ create board for a new game #############
  
  observeEvent(input$start, {
    
    new_match <<- new_game()
    
    output$match <- renderPlot({
      
      plot(new_match, type = "ggplot")
      
    })
    
    insertUI(
      selector = "#start",
      where = "afterEnd",
      immediate = TRUE,
      ui = tagList(textInput("move", "Type your move here!"),
                   actionButton("make_move", "Move"),
                   actionButton("replay", "Retry"))
    )
    
    removeUI(
      selector = "#start",
      immediate = TRUE
    )
  })
  

  #### create board for a new game if replay clicked ####
  
  observeEvent(input$replay, {
    
    new_match <<- new_game()
    
    output$match <- renderPlot({
      
      plot(new_match, type = "ggplot")
      
    })
  })
  
  ############# Player move on the board ################
  
  # this is updated when a player makes a move
  # causing the computer to make a move as well 
  computer_trigger <- reactiveVal(0)
  
  observeEvent(input$make_move, {
    
    req(input$move)
    
    
    if (is.element(isolate(input$move), new_match$moves())) {
      
      player_move <- isolate(input$move)
      
      output$match <- renderPlot({
      
        plot(player_make_move(new_match, player_move), type = "ggplot")
      
      })
      
      nice_comment <- sample(1:4, size = 1)
      
      if (nice_comment == 4){
        
        showNotification(
          paste('Outstanding move!'), type = "message", duration = 3)
        
      }
      
      updateTextInput(session, "move", value="")
      
      computer_trigger(computer_trigger() + 1)
      
    } else {
      
      shinyalert("OOF!", "That's not a valid move!", type = "error")

      output$match <- renderPlot({
        
        plot(new_match, type = "ggplot")
        
      })
    }
  })
  
  ################ Computer reaction #################
  
  observe({
    
    if (computer_trigger() > 0){
      
      output$match <- renderPlot({
        
        plot(computer_make_move(new_match), type = "ggplot")
        
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

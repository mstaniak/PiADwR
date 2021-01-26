library(readr)
library(dplyr)
library(data.table)
library(lubridate)

# import data
szachy <- fread("games.csv")
summary(szachy)
head(szachy)



### dealing with the ID column
n_rows <- dim(szachy)[1]
n_rows
szachy[, uniqueN(id)]
# czyli sa powtorzone ID - zobaczmy czy cale rzedy sa identyczne
n_occur <- data.table(table(szachy$id))
n_occur[n_occur$N > 1,]

# get sample rows with repeated id
szachy[id == n_occur[n_occur$N > 1,]$V1[1],]
# we can see that the whole rows are identical - just get rid of them
szachy <- unique(szachy)
# now we can drop the ID column - no need to keep it
szachy[, id := NULL]


### drop the opening_ply column - it is not very informative in the context of our analysis
#szachy[, opening_ply := NULL]


### Convert column "Rated" from char to BOOL:
szachy[, rated := (as.logical(rated))]
summary(szachy)
head(szachy)

### columns 'opening_eco' and 'opening_name' denote the same thing, 
### but the first one is encoded and the other is a full proper name

### could convert column 'moves' to a more appropriate form - maybe a string with moves separated by a coma?


### update the 'winner' column to explicitly state the winner (username) instead of the winning colour
winning_user <- function(white_id, black_id, winner){
  white_win <- winner == "white"
  black_win <- winner == "black"
  winner[white_win] <- white_id[white_win]
  winner[black_win] <- black_id[black_win]
  return(winner)
}
szachy[, winner := winning_user(white_id, black_id, winner)]



### Converting times to suitable format
# in order to get duration of games we need to get rows where start_time != end_time 
# sadly almost 10 000 rows have identical start and end times
szachy_duration <- szachy[created_at != last_move_at]
# times are encoded in the UNIX format
szachy_duration[, c("created_at", "last_move_at") := .(as.POSIXct((created_at/1000), origin="1970-01-01", tz="GMT"),
                                                            as.POSIXct((last_move_at/1000), origin="1970-01-01", tz="GMT"))]
# create new column with duration (in minutes) of each match
szachy_duration[, "Match_duration" := .(difftime(last_move_at, created_at, units = "mins"))]
# quite a big number of games seem to last for exactly the same amount of time -
# possibly faulty data?
n_occur <- data.table(table(szachy_duration$Match_duration))
n_occur[n_occur$N > 1,]




### Get unique player names
# method 1
all_players <- union(szachy[, black_id], szachy[, white_id])
# method 2
black_players <- szachy[, unique(black_id)]
white_players <- szachy[, unique(white_id)]
all_players <- union(black_players,  white_players)



# saving szachy_duration with fwrite
fwrite(szachy_duration, file = "szachy_cleaned.csv", sep = ",")

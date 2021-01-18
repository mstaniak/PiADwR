library(data.table)
library(tidyverse)
library(microbenchmark)

#group
tidyverse_grouping = function(table)
{
  statistics = table%>% 
                    group_by(table$group)%>% 
                    summarise(
                       mean_W = mean(W),
                       mean_Y = mean(Y),
                       mean_Z = mean(Z))
  statistics
}
basic_grouping = function(table)
{
  statistics = aggregate( .~ group, data = frame, FUN = mean)
  statistics
}

data_table_grouping = function(table)
{
  statistics = table[,list(mean_W=mean(W),mean_Y=mean(Y),mean_Z = mean(Z)),by=group]
  statistics
}


# microbench  
N = c(100,1000,10000,100000) # number of rows
par(mfrow = c(1,4))
box_plot_group = function(g)
{
  for(i in N)
  {
    # Input data
    group = rep(1:G, each = i/G)
    group = as.character(group)
    W = runif(i)
    Y = rnorm(i)
    Z = 10 * runif(i)
  
    # frames, tables and others
    frame = data.frame(group,W,Y,Z)
    table = data.table(group,W,Y,Z)
    tibble_table = tibble(group,W,Y,Z)
    # time 
  
    time = microbenchmark(
      m_1 = basic_grouping(frame),
      m_2 = data_table_grouping(table),
      m_3 = tidyverse_grouping(tibble_table),
      times = 40)
    # Box plot
    boxplot(time,
           main = paste("N =", as.character(i),"G = ", as.character(g)),
           xlab = "model",
           ylab = "czas trwania",
           col = "orange",
           border = "brown"
  )
}
}

# Przykłady dla różnej ilości grup 
#G=20 
box_plot_group(10)
#G=25
box_plot_group(25)
#G=50
box_plot_group(50)

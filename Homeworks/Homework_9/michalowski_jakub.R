# Zadanie domowe: napisać funkcję, która dla danych z Waszego projektu (ew. danych z zajęć)
# narysuje wykres zadanego typu (parametr)
# dla zadanej zmiennej (parametr, napis) i pozwoli dodać dowolne parametry graficzne dla theme(),

# typ wykresu powinien być do wyboru z 2 lub 3 rodzajów, które ustalicie
# jeszcze jedno uściślenie: dane mają być parametrem, ale zilustrujcie funkcję na swoich danych

library('ggplot2')
library('data.table')

szachy <- fread(file = 'szachy_cleaned.csv')
szachy <- szachy[szachy$Match_duration<150]
szachy <- szachy[1300:1800]

# args-in to pass: g_type, x_var, y_var, ...
plot_with_ggplot2 <- function(dframe, g_type = 'blank', x_var = NA, y_var = NA, title = "", x_lab="", y_lab="", colr=NULL, ...) {

  if((is.na(x_var) == FALSE) & (is.na(y_var) == FALSE)) {
    #if two vars are passed
    ggplt_obj <- ggplot(dframe, aes_string(x = x_var, y = y_var, color=colr))
    if(g_type == 'points'){
      ggplt_obj <- ggplt_obj + geom_point()

    } else if (g_type == 'smooth') {
      ggplt_obj <- ggplt_obj + geom_smooth()

    } else if  (g_type == 'jitter') {
      ggplt_obj <- ggplt_obj + geom_jitter()

    } else {
      ggplt_obj <- ggplt_obj + geom_blank()
    }

  } else if (is.na(x_var) | is.na(y_var)) {
    if (is.na(x_var)){
      x_var <- y_var
    }
    ggplt_obj <- ggplot(dframe, aes_string(x = x_var))
    if(g_type == 'bar'){
      ggplt_obj <- ggplt_obj + geom_bar()

    } else if (g_type == 'hist') {
      ggplt_obj <- ggplt_obj + geom_histogram(stat="count")

    } else if  (g_type == 'dotplot') {
      ggplt_obj <- ggplt_obj + geom_dotplot()

    } else {
      ggplt_obj <- ggplt_obj + geom_blank()
    }

  }

  ggplt_obj <- ggplt_obj + labs(title = title, x=x_lab, y = y_lab)
  return(ggplt_obj+theme(...))
}



plot_with_ggplot2(dframe = szachy, g_type = 'points',
                  x_var = 'turns', y_var = 'Match_duration', title = "My chess plot",
                  y_lab="match duration", x_lab = "turns'")

#Title manipulation with theme()
plot_with_ggplot2(dframe = szachy, g_type = 'points',
                  x_var = 'turns', y_var = 'Match_duration', title = "My chess plot",
                  y_lab="match duration", x_lab = "turns", colr = 'victory_status',
                  plot.title = element_text(color="red", size=16, face="bold.italic"),
                  plot.title.position = 'plot')
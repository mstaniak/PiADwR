data()
data('ChickWeight')
head(ChickWeight)
weights <- ChickWeight[["weight"]]
diets <- ChickWeight[["Diet"]]
time <- ChickWeight[["Time"]]

summary(weights)
hist(weights)

weights[1:10]

TRUE, FALSE

as.numeric(as.character(diets))
# Equality
diets[diets == 1]
weights[diets == 1]
# AND
weights[diets == 1 & time == 0]
weights[diets == 1 && time == 0]

# Names
names(weights) <- paste("My_chicken",
                        1:length(weights),
                        sep = "_")
names(weights)
weights

grep
grepl
gsub

class(ChickWeight)
ChickWeight[[1]]
ChickWeight[["Time"]]

ChickWeight[ , "Time", drop = FALSE]
ChickWeight[["Time"]][ChickWeight[["Diet"]] == 1]
ChickWeight[["new_col"]] <- 1
weight[]

tapply(ChickWeight[["weight"]],
       ChickWeight[["Diet"]],
        mean)

sapply(ChickWeight, is.numeric)

ChickWeightNumeric <- ChickWeight[, sapply(ChickWeight, 
                                           is.numeric)]

sort(weights)

weights[order(weights)]

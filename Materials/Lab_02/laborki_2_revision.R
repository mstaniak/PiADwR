auta <- cars
View(auta)
class(auta)
auta$speed

auta[["dist"]]
auta$sp

auta$spee <- auta$dist

auta[["sp"]]

head(auta,10)
colnames(auta)
rownames(auta)

str(auta)
summary(auta)
summary(auta$speed)

head(auta)
help(cars)
auta[which(auta$speed < 12), ]
auta[auta$speed < 12, ]
auta[as.character(1:12), ]

auta[auta$speed < 12 | auta$speed > 20, ]

auta[auta$speed < 12 | auta$speed > 20, ]$dist

class(auta[auta$speed < 12 | auta$speed > 20, 2, drop = FALSE])

auta[speed < 12, ]
with(auta, auta[speed < 12, ])
subset(auta, speed < 12)
colMeans(auta)
apply(auta, 2, mean)
lapply(auta, mean)
sapply(auta, mean)

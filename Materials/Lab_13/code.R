# Plots -----

library(ggplot2)
library(data.table)

some_stats = data.table(
    year = as.character(2000:2020),
    stat = runif(21)
)
some_stats[, lower_ci := stat - 1.96*0.5/sqrt(21)]
some_stats[, upper_ci := stat + 1.96*0.5/sqrt(21)]

head(some_stats)


ggplot(some_stats,
       aes(x = year, y = stat, group = 1)) +
    geom_point(size = 1.5) +
    geom_line(size = 0.5, linetype = 2) +
    geom_linerange(aes(ymin = lower_ci, ymax = upper_ci)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270))

ggplot(some_stats,
       aes(x = year, y = stat, group = 1)) +
    geom_point(size = 1.5) +
    geom_line(size = 0.5, linetype = 2) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270))

overall_mean = mean(some_stats$stat)

ggplot(some_stats,
       aes(x = year, y = stat, group = 1)) +
    geom_point(size = 1.5) +
    geom_line(size = 0.5, linetype = 2) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
    geom_hline(yintercept = overall_mean, color = "blue") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270))

library(lubridate)

new_data = data.table(
    date = seq(ymd("2016-01-01"),
               ymd("2020-12-31"),
               by = "1 month"),
    value = rnorm(60, mean = rep(c(11:15), each = 12))
)
new_data[, average := mean(value),
         by = floor_date(date, "years")]

new_data

ggplot(new_data, aes(x = date, y = value)) +
    geom_point(size = 1.5) +
    geom_line(size = 0.5, color = "grey") +
    geom_line(aes(x = date, y = average,
                  group = year(date)), data = new_data) +
    theme_bw()

# Divorce data ----
## Divorce prediction from kaggle
library(mlr)
library(data.table)

divorces = fread("divorce_data.csv")
q_annotation = fread("reference.tsv")

head(divorces)
head(q_annotation)

class(divorces$Q1)
class(divorces$Divorce)

unique(divorces$Q1)

divorces[, lapply(.SD, function(x) sum(is.na(x)))]
divorces[, lapply(.SD, function(x) sum(x < 0))]

divorces$Divorce

ggplot(divorces, aes(x = Divorce)) +
    geom_bar() +
    theme_bw()


q_annotation[, Q_ID := paste0("Q", atribute_id)]
divorces[, ParticipantID := 1:nrow(divorces)]
div_long = melt(divorces, id.vars = c("ParticipantID", "Divorce"),
                variable.name = "Q_ID", value.name = "Answer", variable.factor = FALSE)
div_long = merge(div_long, q_annotation, by = "Q_ID", all.x = TRUE)
div_summary[, Divorce := as.factor(as.character(Divorce))]
div_summary = div_long[, .(NumAnswers = .N), by = c("Divorce", "atribute_id", "description", "Answer")]
div_summary

ggplot(div_summary[atribute_id %in% 1:10],
       aes(x = Answer, y = NumAnswers, fill = Divorce)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~description) +
    theme_bw()



divorces2 = divorces[, 1:55, with = F]
divorces2[, 1:54] = divorces2[, lapply(.SD, function(x) as.factor(ifelse(x %in% c(0, 1), "0-1",
                                           ifelse(x == 2, "2", "3-4")))),
          .SDcols = setdiff(colnames(divorces2), "Divorce")]
divorces2

model_glm = glm(Divorce ~ ., family = "binomial", data = divorces2,
                control = list(maxit = 50))
summary(model_glm)




library(mlr)

train_set = sample(1:nrow(divorces2), round(0.8*nrow(divorces2)))
test_set = setdiff(1:nrow(divorces2), train_set)

div_task = makeClassifTask(id = "divorces",
                           data = as.data.frame(divorces2),
                           target = "Divorce")
div_task

rf = makeLearner("classif.randomForest")
rf_trained = train(rf, div_task, train_set)
performance(predict(object = rf_trained, task = div_task, subset = test_set), measures = acc)
fi_data = generateFeatureImportanceData(div_task, learner = rf, measure = acc)
fi_data

q_annotation[c(26, 40)]


?benchmark


benchmark(
    learners,
    tasks,
    resamplings,
    measures,
    keep.pred = TRUE,
    keep.extract = FALSE,
    models = FALSE,
    show.info = getMlrOption("show.info")
)



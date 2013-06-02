#!/usr/bin/Rscript
library('SortableHTMLTables')

#source('example_model_2.R')

training.data$LogitProbabilities <- 1 / (1 + exp(-predict(logit.fit)))

predicted.probabilities <- ddply(training.data,
                                 'Package',
                                 function (d) {with(d, LogitProbabilities[1])})
names(predicted.probabilities) <- c('Package', 'PredictedProbability')

empirical.probabilities <- ddply(training.data,
                                 'Package',
                                 function (d) {nrow(subset(d, Installed == 1)) / nrow(d)})
names(empirical.probabilities) <- c('Package', 'EmpiricalProbability')

probabilities <- merge(predicted.probabilities,
                       empirical.probabilities,
                       by = 'Package')

probabilities$ProbabilityError <- abs(probabilities$PredictedProbability - probabilities$EmpiricalProbability)

mean.absolute.error <- with(probabilities,
                            mean(abs(PredictedProbability - EmpiricalProbability)))
worst.case.absolute.error <- with(probabilities,
                                  max(abs(PredictedProbability - EmpiricalProbability)))

auc(outcome=probabilities$EmpiricalProbability, proba=probabilities$PredictedProbability)


#sortable.html.table(probabilities, 'probabilities.html','reports')
#qplot(data=probabilities[order(-probabilities$EmpiricalProbability),], x=EmpiricalProbability, y=abs(EmpiricalProbability-PredictedProbability))
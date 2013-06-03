#devn data investigation
library(pROC)

#!/usr/bin/Rscript

training.data.old <- training.data
#training.data <- training.data.old
training.data <- merge(training.data, topics, by = 'Package', all.x = TRUE)
training.data$Topic[is.na(training.data$Topic) == TRUE] <- 0
training.data <- transform(training.data, Topic = factor(Topic))
training.data <- merge(training.data, stack.overflow, by = 'Package', all.x = TRUE)
training.data$Topic.y <- NULL
training.data$LogViewsIncluding <- training.data$LogViewsIncluding + training.data$LogViews

logit.fit <- glm(Installed ~ LogDependencyCount +
                   LogSuggestionCount +
                   LogImportCount +
                   LogViewsIncluding +
                   LogPackagesMaintaining +
                   LogMedianReputation +
                   LogMedianScore +
                   LogMedianViews +
                   CorePackage +
                   RecommendedPackage +
                   factor(User) +
                   Topic, #adds topic as a factor
                 data = training.data,
                 family = binomial(link = 'logit'))

# summary(logit.fit)
# length(predict(logit.fit))
training.data$LogitProbabilities <- NULL
training.data$LogitProbabilities <- predict(logit.fit,type="response")
training.data$LogitProbabilities[training.data$Package == 'R'] <- 0
training.data$LogitProbabilities[training.data$Package == 'base'] <- 1


logit.roc <- roc(training.data$Installed, training.data$LogitProbabilities)
logit.roc$auc
plot(logit.roc)





qplot(data=training.data, x=Questions, geom="density") #weird looking
qplot(data=training.data, x=Score, geom="density") #big spike, potentially useful
qplot(data=training.data, x=Reputation, geom="density") #big spike
qplot(data=training.data, x=Answers, geom="density") #big spike
qplot(data=training.data, x=Views, geom="density") #big spike
qplot(data=training.data, x=MedianScore, geom="density") #big spike
qplot(data=training.data, x=MedianAnswers, geom="density") #several spikes. This has some signal
qplot(data=training.data, x=MedianViews, geom="density") #big spike
qplot(data=training.data, x=MedianReputations, geom="density") #big spike
qplot(data=training.data, x=AverageScore, geom="density") #big spike
qplot(data=training.data, x=AverageAnswers, geom="density") #several spikes. This has a lot of signal
qplot(data=training.data, x=AverageViews, geom="density") #big spike, but messy
qplot(data=training.data, x=AverageReputation, geom="density") #big spike



qplot(data=training.data, x=LogViewsIncluding, y=log(Views)) #straight line.
qplot(data=training.data, x=LogViewsIncluding, y=LogQuestions) #Still noisy
qplot(data=training.data, x=LogViewsIncluding, y=LogReputation) #a trend, but a lot of signal there
qplot(data=training.data, x=LogViewsIncluding, y=LogAnswers) #another trend, with a lot of signal
qplot(data=training.data, x=LogViewsIncluding, y=LogScore) #a clear trend. Good idea.



qplot(data=training.data, x=as.factor(Installed), y=LogQuestions, geom="boxplot") #huge difference
qplot(data=training.data, x=as.factor(Installed), y=LogScore, geom="boxplot") #still a big difference
qplot(data=training.data, x=as.factor(Installed), y=LogAnswers, geom="boxplot") #huge difference
qplot(data=training.data, x=as.factor(Installed), y=LogReputation, geom="boxplot") #less difference
qplot(data=training.data, x=as.factor(Installed), y=LogViewsIncluding, geom="boxplot") #this doesn't have as much variation as I thought.


qplot(data=training.data, x=as.factor(Installed), y=MedianScore, geom="boxplot") #not that useful
qplot(data=training.data, x=as.factor(Installed), y=AverageScore, geom="boxplot") #not that useful.
qplot(data=training.data, x=as.factor(Installed), y=LogMedianScore, geom="boxplot") #potentially useful.
qplot(data=training.data, x=as.factor(Installed), y=LogAverageScore, geom="boxplot") #potentially useful.

qplot(data=training.data, x=as.factor(Installed), y=MedianAnswers, geom="boxplot") #not very different
qplot(data=training.data, x=as.factor(Installed), y=LogMedianAnswers, geom="boxplot") #not very different
qplot(data=training.data, x=as.factor(Installed), y=AverageAnswers, geom="boxplot") #not very different
qplot(data=training.data, x=as.factor(Installed), y=LogAverageAnswers, geom="boxplot") #not very different

qplot(data=training.data, x=as.factor(Installed), y=MedianViews, geom="boxplot") #somewhat different
qplot(data=training.data, x=as.factor(Installed), y=AverageViews, geom="boxplot") #hugely different
qplot(data=training.data, x=as.factor(Installed), y=LogMedianViews, geom="boxplot") #somewhat different
qplot(data=training.data, x=as.factor(Installed), y=LogAverageViews, geom="boxplot") #whoa! different

qplot(data=training.data, x=as.factor(Installed), y=AverageReputation, geom="boxplot") #useless
qplot(data=training.data, x=as.factor(Installed), y=MedianReputations, geom="boxplot") #useless
qplot(data=training.data, x=as.factor(Installed), y=LogAverageReputation, geom="boxplot") #different
qplot(data=training.data, x=as.factor(Installed), y=LogMedianReputation, geom="boxplot") #not very different

qplot(data=training.data, x=as.factor(Installed), y=AverageScore, geom="boxplot") #not very difference
qplot(data=training.data, x=as.factor(Installed), y=MedianScore, geom="boxplot") #big difference
qplot(data=training.data, x=as.factor(Installed), y=LogAverageScore, geom="boxplot") #not very difference
qplot(data=training.data, x=as.factor(Installed), y=LogMedianScore, geom="boxplot") #big difference




#first try, using LogQuestions & LogAnswers
#> mean.absolute.error - 0.06906694
#> worst.case.absolute.error - 0.9748928

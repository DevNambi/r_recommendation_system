#devn data investigation

#!/usr/bin/Rscript

training.data.old <- training.data
#training.data <- training.data.old
training.data <- merge(training.data, topics, by = 'Package', all.x = TRUE)
training.data <- transform(training.data, Topic = factor(Topic))
training.data <- merge(training.data, stack.overflow, by = 'Package', all.x = TRUE)
training.data$Topic.y <- NULL
training.data$LogViewsIncluding <- training.data$LogViewsIncluding + training.data$LogViews

logit.fit <- glm(Installed ~ LogDependencyCount +
                   LogSuggestionCount +
                   LogImportCount +
                   LogViewsIncluding +
                   LogPackagesMaintaining +
                   #LogScore + 
                   LogAnswers +
                   LogQuestions + 
                   LogReputation +
                   CorePackage +
                   RecommendedPackage +
                   factor(User),
                 #remove topic because of NAs?
                 data = training.data,
                 family = binomial(link = 'logit'))

# summary(logit.fit)
# length(predict(logit.fit))

qplot(data=training.data, x=Questions, geom="density") #weird looking
qplot(data=training.data, x=Score, geom="density") #big spike, potentially useful
qplot(data=training.data, x=Reputation, geom="density") #big spike
qplot(data=training.data, x=Answers, geom="density") #big spike
qplot(data=training.data, x=Views, geom="density") #big spike

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


#first try, using LogQuestions & LogAnswers
#> mean.absolute.error - 0.06906694
#> worst.case.absolute.error - 0.9748928

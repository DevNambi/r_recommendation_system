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
                   LogScore + 
                   LogAnswers +
                   LogReputation +
                   CorePackage +
                   RecommendedPackage +
                   factor(User),
                  #remove topic because of NAs?
                 data = training.data,
                 family = binomial(link = 'logit'))

# summary(logit.fit)
# length(predict(logit.fit))

#first try, using LogQuestions & LogAnswers
#> mean.absolute.error - 0.06906694
#> worst.case.absolute.error - 0.9748928

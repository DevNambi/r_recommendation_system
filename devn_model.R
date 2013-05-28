#!/usr/bin/Rscript

training.data <- merge(training.data, topics, by = 'Package', all.x = TRUE)
training.data <- transform(training.data, Topic = factor(Topic))
training.data <- merge(training.data, stack.overflow, by = 'Package', all.x = TRUE)
training.data$Topic.y <- NULL

training.data$LogViewsIncluding <- training.data$LogViewsIncluding + training.data$LogViews

logit.fit <- glm(Installed ~ LogDependencyCount +
                    LogSuggestionCount +
                   LogImportCount +
                   LogViewsIncluding +
                   #LogPackagesMaintaining +
                   LogQuestions + 
                   LogAnswers + 
                   CorePackage +
                   RecommendedPackage +
                   factor(User) +
                   Topic.x, #adds topic as a factor
                 data = training.data,
                 family = binomial(link = 'logit'))

summary(logit.fit)
length(predict(logit.fit))


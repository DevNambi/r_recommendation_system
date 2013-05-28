#!/usr/bin/Rscript

training.data <- merge(training.data, topics, by = 'Package', all.x = TRUE)
training.data <- transform(training.data, Topic = factor(Topic))

logit.fit <- glm(Installed ~ LogDependencyCount +
                             LogSuggestionCount +
                             LogImportCount +
                             LogViewsIncluding +
                             LogPackagesMaintaining +
                             CorePackage +
                             RecommendedPackage +
                             factor(User) +
                             Topic, #adds topic as a factor
                 data = training.data,
                 family = binomial(link = 'logit'))

summary(logit.fit)

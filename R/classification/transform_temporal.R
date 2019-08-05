short_term <- validationData[, c("event", "day", "week")]
short_term <- apply(short_term, 1, function(x) any(x == TRUE))
year <- validationData[, "year"]
long_term <- validationData[, c("years_10", "years_100")]
long_term <- apply(long_term, 1, function(x) any(x == TRUE))

very_long_term <- validationData[, c("years_1000", "years_10000", "years_100000")]
very_long_term <- apply(very_long_term, 1, function(x) any(x == TRUE))

validationTemporal <- cbind(short_term, long_term, very_long_term)

trainingLabels <- as.data.frame(validationTemporal)
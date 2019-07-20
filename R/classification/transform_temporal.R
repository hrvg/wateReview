less_than_year <- validationData[, c("event", "day", "week")]
less_than_year <- apply(less_than_year, 1, function(x) any(x == TRUE))
year <- validationData[, "year"]
more_than_year <- validationData[, c("years_10", "years_100", "years_1000", "years_10000", "years_100000")]
more_than_year <- apply(more_than_year, 1, function(x) any(x == TRUE))
# validationTemporal <- cbind(less_than_year, year, more_than_year)
validationTemporal <- cbind(less_than_year, more_than_year)

trainingLabels <- as.data.frame(validationTemporal)
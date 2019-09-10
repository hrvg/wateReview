predMatching <- pred$data[1:nrow(predFilter$data), ]
responseAll <- as.character(predMatching$response)
responseAll[which(responseAll != "Irrelevant")] <- "Relevant"
responseIrr <- predFilter$data$response
table(responseAll == responseIrr)



p_AggrPerformances <- AggrPerformances[, -1]
p_AggrPerformances$learner.id <- c("RFSRC", "rFern", "BR")
colnames(p_AggrPerformances) <- c("learner.id", "Hamming.Loss", "Subset.0_1", "Acc", "Recall", "Precision", "F1")
library(reshape2)
library(ggplot2)

mp_AggrPerformances <- melt(p_AggrPerformances)

ggplot(mp_AggrPerformances, aes(x = learner.id, y = value, group = learner.id, color = learner.id, fill = learner.id)) +
	geom_bar(stat="identity")+
	facet_wrap(variable ~ .) +
	cowplot::theme_cowplot() +
	labs(title = "Learner Multilabel Performance")


mp_MultilabelBinaryPerformance <- melt(MultilabelBinaryPerformance)
colnames(mp_MultilabelBinaryPerformance) <- c("label", "learner.id", "value")

ggplot(mp_MultilabelBinaryPerformance, aes(x = learner.id, y = value, group = learner.id, color = learner.id, fill = learner.id)) +
	geom_bar(stat="identity")+
	facet_wrap(label ~ .) +
	cowplot::theme_cowplot() +
	labs(title = "Learner Multilabel Performance")
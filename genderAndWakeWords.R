# Analysis of small poll on preferred wake word ("Alexa" or "Ok Google") by user gender

#libraries we'll need
library(ggplot2)
library(plyr)

# first set working directory to source file location
# then read in data:
genderWW <- read.csv("genderAndWakeWords.csv")

# rename factors for prettier plotting
names(genderWW) <- c("Time", "Gender", "Wake_Word")
genderWW$Wake_Word <- mapvalues(genderWW$Wake_Word, from = c('"Ok Google" or "Hey Google"'), to = c('"Ok Google"'))

# make sure file loaded in correctly
head(genderWW)

# marginal table
margTab <- table(genderWW[,2:3])
margTab

# chi-square test of significance
chisq.test(margTab)
sum(margTab)

# results are significant @ p < 0.5 (reasonable for this sample size)

# plotting data
# simple barplot (if ggplot not installed)
barplot(margTab, beside = T)

# pretty barchart
ggplot(data = genderWW, aes(genderWW$Wake_Word)) + facet_wrap(~ Gender) +
  geom_bar(position = "dodge") +
  geom_text(stat='count',aes(label=..count..), vjust = -1) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1), 
        legend.title = element_blank(), axis.title.x = element_blank(),
        text = element_text(size=20))
  


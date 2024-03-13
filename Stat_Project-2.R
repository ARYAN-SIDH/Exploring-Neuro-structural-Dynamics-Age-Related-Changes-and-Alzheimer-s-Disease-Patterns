setwd("/Users/aryan_sidh/Downloads/archive-2/")
library(tidyverse)
library(caret)
library(FSA)

# Read the oasis_cross-sectional data set
longitudinal_data <- read.csv('oasis_longitudinal.csv')
summary(longitudinal_data)

# Making a Composite Score for all the variables indicating brain Structure
longitudinal_data$eTIV_Z <- scale(longitudinal_data$eTIV)
longitudinal_data$nWBV_Z <- scale(longitudinal_data$nWBV)
longitudinal_data$ASF_Z <- scale(longitudinal_data$ASF)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

longitudinal_data$eTIV_Norm <- normalize(longitudinal_data$eTIV)
longitudinal_data$nWBV_Norm <- normalize(longitudinal_data$nWBV)
longitudinal_data$ASF_Norm <- normalize(longitudinal_data$ASF)

longitudinal_data$CompositeScore <- rowMeans(cbind(
  scale(longitudinal_data$eTIV_Norm),
  scale(longitudinal_data$nWBV_Norm),
  scale(longitudinal_data$ASF_Norm)
))

# Test Normality
shapiro.test(longitudinal_data$CompositeScore)

# As it's not normal, we do non-parametric test
# Kruskal-Wallis Test is alternative for one-way Anova
kruskal.test(CompositeScore ~ Group, data =longitudinal_data)

dunn_result <- dunnTest(x = longitudinal_data$CompositeScore, g = longitudinal_data$Group, method = "bonferroni")
print(dunn_result)

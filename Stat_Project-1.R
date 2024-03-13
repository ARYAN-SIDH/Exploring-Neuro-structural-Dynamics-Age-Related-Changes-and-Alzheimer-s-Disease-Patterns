setwd("/Users/aryan_sidh/Downloads/archive-2/")
library(tidyverse)
library(caret)
library(FSA)

# Read the oasis_cross-sectional data set
cross_sectional <- read.csv('oasis_cross-sectional.csv')
summary(cross_sectional)

cross_sectional$AgeGroup <- cut(cross_sectional$Age,
                                  breaks = c(0, 30, 60, 100),
                                  labels = c("Young", "Middle-Aged", "Older"))

#  'eTIV', 'nWBV', and 'ASF' are the variables indicating brain structure
#  'AgeGroup' is the variable indicating different age groups

# Making a Composite Score for all the variables indicating brain Structure
cross_sectional$eTIV_Z <- scale(cross_sectional$eTIV)
cross_sectional$nWBV_Z <- scale(cross_sectional$nWBV)
cross_sectional$ASF_Z <- scale(cross_sectional$ASF)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

cross_sectional$eTIV_Norm <- normalize(cross_sectional$eTIV)
cross_sectional$nWBV_Norm <- normalize(cross_sectional$nWBV)
cross_sectional$ASF_Norm <- normalize(cross_sectional$ASF)

cross_sectional$CompositeScore <- rowMeans(cbind(
  scale(cross_sectional$eTIV_Norm),
  scale(cross_sectional$nWBV_Norm),
  scale(cross_sectional$ASF_Norm)
))

# Test Normality
shapiro.test(cross_sectional$CompositeScore)

# As it's not normal, we do non-parametric test
# Kruskal-Wallis Test is alternative for one-way Anova
kruskal.test(CompositeScore ~ AgeGroup, data =cross_sectional)

dunn_result <- dunnTest(x = cross_sectional$CompositeScore, g = cross_sectional$AgeGroup, method = "bonferroni")
print(dunn_result)





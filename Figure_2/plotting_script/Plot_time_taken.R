rm(list=ls())
library(ggplot2)
library(reshape2)
library(wesanderson)
library(RColorBrewer)

color_palette <- wes_palette("Royal1")[c(2,1)]
color_palette <- brewer.pal(10, "Paired")[c(1,2,9,10)]
color_palette <- brewer.pal(10, "Paired")[c(2,1,4,3)]
color_palette <- brewer.pal(10, "Paired")[c(10,9,8,7)]
# color_palette <- brewer.pal(10, "Paired")[c(8,7,10,9)]

data_dir <- "/Users/scirej/Documents/BDMM_projects/Speed_evaluation/Likelihood_Calc_Only/"

file_new_1_uncol <- "TimeLikelihoodCalculationsBDMM_NewVersion_Part_1.txt"
file_new_2_uncol <- "TimeLikelihoodCalculationsBDMM_NewVersion_Part_2.txt"
file_old_uncol <- "TimeLikelihoodCalculationsBDMM_OldVersion.txt"
file_old_col <- "TimeLikelihoodCalculationsBDMM_Coloured_OldVersion.txt"
file_new_col <- "TimeLikelihoodCalculationsBDMM_Coloured_NewVersion.txt"

data_new_1_uncol <- read.csv(paste0(data_dir, file_new_1_uncol), header=TRUE)
data_new_2_uncol <- read.csv(paste0(data_dir, file_new_2_uncol), header=TRUE)
data_old_uncol <- read.csv(paste0(data_dir, file_old_uncol), header=TRUE)
data_old_col <- read.csv(paste0(data_dir, file_old_col), header=TRUE)
data_new_col <- read.csv(paste0(data_dir, file_new_col), header=TRUE)

data_new_1_uncol$isInfLik <- rep(0, nrow(data_new_1_uncol)) 
data_new_uncol <- rbind(data_new_1_uncol, data_new_2_uncol)

data_new_uncol$Version <- rep("Updated - Sample-typed", nrow(data_new_uncol))
data_old_uncol$Version <- rep("Original - Sample-typed", nrow(data_old_uncol))
data_new_col$Version <- rep("Updated - Branch-typed", nrow(data_new_col))
data_old_col$Version <- rep("Original - Branch-typed", nrow(data_old_col))

data <- rbind(data_old_uncol, data_new_uncol, data_old_col, data_new_col)
data$version <- factor(data$Version)

meanTimeTaken <- c()
meanTimeTakenNonUnderflow <- c()
# CI.up <- c()
# CI.dn <- c()
SE.up <- c()
SE.dn <- c()
NumOfTips <- c()
version <- c()

for (numOfTips in sort(unique(data$NumOfTips))) {
  for(v in levels(data$version)) {
    NumOfTips <- c(NumOfTips, numOfTips)
    version <- c(version, v)
    currentMean <- mean(data$Time[data$NumOfTips == numOfTips & data$version == v & data$isInfLik == 0])
    meanTimeTakenNonUnderflow <- c(meanTimeTakenNonUnderflow, currentMean)
    se <- sd(data$Time[data$NumOfTips == numOfTips & data$version == v & data$isInfLik == 0])/sqrt(sum(data$NumOfTips == numOfTips & data$version == v & data$isInfLik == 0))
    # CI.up <- c(CI.up, ifelse(is.infinite(se) || is.nan(se) || is.nan(currentMean) , NaN, currentMean + 1.96*se))
    # CI.dn <- c(CI.dn, ifelse(is.infinite(se) || is.nan(se) || is.nan(currentMean), NaN, currentMean - 1.96*se))
    SE.up <- c(SE.up, ifelse(is.infinite(se) || is.nan(se) || is.nan(currentMean) , NaN, currentMean + se))
    SE.dn <- c(SE.dn, ifelse(is.infinite(se) || is.nan(se) || is.nan(currentMean), NaN, currentMean - se))
    # meanTimeTaken <- c(meanTimeTaken, mean(data$Time[data$NumOfTips == numOfTips & data$version == v]))
  }
}

data_plotting <- data.frame(mean = meanTimeTakenNonUnderflow)
# data_plotting$CI.up <- CI.up
# data_plotting$CI.dn <- CI.dn
data_plotting$SE.up <- SE.up
data_plotting$SE.dn <- SE.dn
data_plotting$NumOfTips <- NumOfTips
data_plotting$version <- version



ggplot(data_plotting, aes(x=NumOfTips, y=mean, colour=version)) + 
  theme_classic() +
  # geom_errorbar(aes(ymin=CI.up, ymax=CI.dn), width=25, size=0.7) +
  geom_errorbar(aes(ymin=SE.up, ymax=SE.dn), width=25, size=0.7) +
  geom_line(size=1.3) +
  geom_point(size=2.1) +
  scale_color_manual(values = color_palette) +
  theme(axis.title.x = element_text(size=17, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size=17, margin = unit(c(0, 4, 0, 0), "mm")),
        axis.text = element_text(size=16),
        legend.position="right",
        legend.text = element_text(size=16),
        legend.title = element_text(size=17)) + 
  labs(y="Time (s)", x="Tree leaves", colour="BDMM version")

outputFile <- paste0(data_dir, "Plot_time_full_data")
ggsave(filename = paste0(outputFile, ".png"), width=8, height=4.5, dpi=600)
ggsave(filename = paste0(outputFile, ".pdf"), width=8, height=4.5, dpi=600)

### Proportion of non-underflowing calculations

proportionNonUnderflowing <- c()
NumOfTips <- c()
version <- c()

for (numOfTips in sort(unique(data$NumOfTips))) {
  for(v in levels(data$version)) {
    NumOfTips <- c(NumOfTips, numOfTips)
    version <- c(version, v)
    proportionNonUnderflowing <- c(proportionNonUnderflowing, sum(data$isInfLik[data$NumOfTips == numOfTips & data$version == v] == 0)/length(data$isInfLik[data$NumOfTips == numOfTips & data$version == v]))
  }
}

data_plotting$proportionNonUnderflowing <- proportionNonUnderflowing
pd <- position_dodge(5)

ggplot(data_plotting, aes(x=NumOfTips, y=proportionNonUnderflowing, colour=version)) + 
  geom_line(position=pd, size = 1.3) +
  geom_point(position=pd, size = 2.1) +
  theme_classic() +
  scale_color_manual(values = color_palette) +
  theme(axis.title.x = element_text(size=17, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size=17, margin = unit(c(0, 4, 0, 0), "mm")),
        axis.text = element_text(size=16),
        legend.position="right",
        legend.text = element_text(size=16),
        legend.title = element_text(size=17)) + 
  labs(y="Fraction of successful calculations", x="Tree leaves", colour="BDMM version")

outputFile <- paste0(data_dir, "Plot_fraction_full_data")
ggsave(filename = paste0(outputFile, ".png"), width=8, height=4.5, dpi=600)
ggsave(filename = paste0(outputFile, ".pdf"), width=8, height=4.5, dpi=600)

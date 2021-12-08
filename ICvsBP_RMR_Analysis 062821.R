# clear console and global environment
rm(list = ls())
cat("\f") 
dev.off()

# load libraries
library("rmcorr")
library("psych")
library("plyr")
library("dplyr")
library("ggplot2") 
library("reshape2")
library("EcoSimR")
library("openxlsx")
library("tidyr")
library("tidyverse")
library("arsenal")
library("knitr")
library("gplots")
library("ggplot2")
library("nlme")
library("lme4")
library("lmerTest")
library("lattice")
library("GGally")
library("car")
library("afex")
library("emmeans")
library("ggpubr")
library("Metrics")
library("BlandAltmanLeh")
library("gridExtra")
library("blandr")
library("basicTrendline")
library("svglite")

################### Analysis of Predictive Accuracy of RMR Equations Compared to Indirect Calioremetry #####################

# This script is designed to compare the predictive accuracy of several RMR prediction equations to RMR as measured by IC
# data is from GMU's SMART Lab fee-for-service clinic on appointments where clients performed both the BodPod and IC to measure RMR

### Read in data ###

# set working directory
setwd("C:\\Users\\blindse3\\Dropbox\\FAFC SMART Lab\\BodPod vs RMR Paper\\Data")

# load data
RMRData = loadWorkbook("ICvsBPData.xlsx") 
RMRData = read.xlsx(RMRData, sheet = 1) # contains RMR values for all participants across all equations
attach(RMRData)

RMRANOVA = loadWorkbook("ICvsBPData.xlsx") 
RMRANOVA = read.xlsx(RMRANOVA, sheet = 2) # dataframe constructed to be appropriate for ANOVA analysis
attach(RMRANOVA)

RMRANOVA2 = loadWorkbook("ICvsBPData.xlsx") 
RMRANOVA2 = read.xlsx(RMRANOVA2, sheet = 3) # dataframe with outliers removed
attach(RMRANOVA2)

### assessing normality ###

library(ggpubr)
library(pryr)

densityplots %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(3,4))
    density_height = density(RMRData$`Height.(m)`)
    density_height_plot = plot(density_height, main = "Height", xlab = "", ylab = "")
    
    density_weight = density(RMRData$`Weight.(kg)`)
    density_weight_plot = plot(density_weight, main = "Weight", xlab = "", ylab = "")
    
    density_age = density(RMRData$Age)
    density_age_plot = plot(density_age, main = "Age", xlab = "", ylab = "")
    
    density_FM = density(RMRData$`FM.(kg)`)
    density_FM_plot = plot(density_FM, main = "Fat Mass", xlab = "", ylab = "")
    
    
    density_FFM = density(RMRData$`FFM.(kg)`)
    density_FFM_plot = plot(density_FFM, main = "Fat Mass", xlab = "")
    
    density_ICRMR = density(RMRData$IC.RMR)
    density_ICRMR_plot = plot(density_ICRMR, main = "IC RMR", xlab = "")
    
    
    density_NelsonRMR = density(RMRData$Nelson)
    density_NelsonRMR_plot = plot(density_NelsonRMR, main = "Nelson RMR", xlab = "")
    
    
    density_DoreRMR = density(RMRData$Dore)
    density_DoreRMR_plot = plot(density_DoreRMR, main = "Dore RMR", xlab = "")
    
    
    density_HRRMR = density(RMRData$`Harris-Benedict.ALL`)
    density_HRRMR_plot = plot(density_HRRMR, main = "Harris-Benedict RMR", xlab = "")
    
    
    density_MSJRMR = density(RMRData$`Mifflin-St.Jeor.ALL`)
    density_MSJRMR_plot = plot(density_MSJRMR, main = "Mifflin-St Jeor RMR", xlab = "")
    
    
    density_WHORMR = density(RMRData$WHO.ALL)
    density_WHORMR_plot = plot(density_WHORMR, main = "WHO RMR", xlab = "")

}
densityplots

qqplots %<a-%
  {par(mfrow=c(3,4))
    
    qqPlot(RMRData$`Height.(m)`, main = "Height", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$`Weight.(kg)`, main = "Height", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$Age, main = "Age", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$`FM.(kg)`, main = "Fat Mass", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$`FFM.(kg)`, main = "Fat Free Mass", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$IC.RMR, main = "IC RMR", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$Nelson, main = "Nelson RMR", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$Dore, main = "Dore RMR", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$`Harris-Benedict.ALL`, main = "Harris-Benedict RMR", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$`Mifflin-St.Jeor.ALL`, main = " Mifflin-St Jeor RMR", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(RMRData$WHO.ALL, main = "WHO RMR", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
  }
qqplots

### identification of outliers ###

library(ggstatsplot)

ggbetweenstats(data = RMRANOVA, 
               x = Measure,
               y = RMR,
               outlier.tagging = TRUE,
               outlier.label = ID)

ggbetweenstats(data = RMRANOVA2, 
               x = Measure,
               y = RMR,
               outlier.tagging = TRUE,
               outlier.label = ID)

### Descriptives ###

desc <- describe(RMRData)
desc

describeBy(RMRData, RMRData$Sex)

library(rcompanion)

Age_mean = groupwiseMean(Age ~ 1,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
Age_mean

Age_mean = groupwiseMean(Age ~ Sex,
              data   = RMRData,
              conf   = 0.95,
              digits = 5)
Age_mean

Height_mean = groupwiseMean(`Height.(m)` ~ 1,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
Height_mean

Height_mean = groupwiseMean(`Height.(m)` ~ Sex,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
Height_mean

Weight_mean = groupwiseMean(`Weight.(kg)` ~ 1,
                            data   = RMRData,
                            conf   = 0.95,
                            digits = 5)
Weight_mean

Weight_mean = groupwiseMean(`Weight.(kg)` ~ Sex,
                            data   = RMRData,
                            conf   = 0.95,
                            digits = 5)
Weight_mean

BMI_mean = groupwiseMean(BMI ~ 1,
                            data   = RMRData,
                            conf   = 0.95,
                            digits = 5)
BMI_mean

BMI_mean = groupwiseMean(BMI ~ Sex,
                            data   = RMRData,
                            conf   = 0.95,
                            digits = 5)
BMI_mean

BF_mean = groupwiseMean(BF ~ 1,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
BF_mean

BF_mean = groupwiseMean(BF ~ Sex,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
BF_mean

FFM_mean = groupwiseMean(`FFM.(kg)` ~ 1,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
FFM_mean

FFM_mean = groupwiseMean(`FFM.(kg)` ~ Sex,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
FFM_mean

FM_mean = groupwiseMean(`FM.(kg)` ~ 1,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
FM_mean

FM_mean = groupwiseMean(`FM.(kg)` ~ Sex,
                         data   = RMRData,
                         conf   = 0.95,
                         digits = 5)
FM_mean

ICRMR_mean = groupwiseMean(IC.RMR ~ 1,
                        data   = RMRData,
                        conf   = 0.95,
                        digits = 6)
ICRMR_mean

ICRMR_mean = groupwiseMean(IC.RMR ~ Sex,
                           data   = RMRData,
                           conf   = 0.95,
                           digits = 6)
ICRMR_mean


HBRMR_mean = groupwiseMean(`Harris-Benedict.ALL` ~ 1,
                           data   = RMRData,
                           conf   = 0.95,
                           digits = 6)
HBRMR_mean

HBRMR_mean = groupwiseMean(`Harris-Benedict.ALL` ~ Sex,
                           data   = RMRData,
                           conf   = 0.95,
                           digits = 6)
HBRMR_mean


MJRMR_mean = groupwiseMean(`Mifflin-St.Jeor.ALL` ~ 1,
                           data   = RMRData,
                           conf   = 0.95,
                           digits = 6)
MJRMR_mean

MJRMR_mean = groupwiseMean(`Mifflin-St.Jeor.ALL` ~ Sex,
                           data   = RMRData,
                           conf   = 0.95,
                           digits = 6)
MJRMR_mean

WHORMR_mean = groupwiseMean(WHO.ALL ~ 1,
                           data   = RMRData,
                           conf   = 0.95,
                           digits = 6)
WHORMR_mean

WHORMR_mean = groupwiseMean(WHO.ALL ~ Sex,
                            data   = RMRData,
                            conf   = 0.95,
                            digits = 6)
WHORMR_mean

NelsonRMR_mean = groupwiseMean(Nelson ~ 1,
                            data   = RMRData,
                            conf   = 0.95,
                            digits = 6)
NelsonRMR_mean

NelsonRMR_mean = groupwiseMean(Nelson ~ Sex,
                               data   = RMRData,
                               conf   = 0.95,
                               digits = 6)
NelsonRMR_mean

DoreRMR_mean = groupwiseMean(Dore ~ 1,
                               data   = RMRData,
                               conf   = 0.95,
                               digits = 6)
DoreRMR_mean

### Repeated measures ANOVA ###

# separate data frame into sexes
RMRANOVAFemale = RMRANOVA %>% filter( Sex == "female")
RMRANOVAMale = RMRANOVA %>% filter( Sex == "male")

### Repeated Measures ANOVA for RMR ###
Within.aov.RMR <- aov_car(RMR ~ Measure + Error(ID/Measure), data=RMRANOVA, fun_aggregate = mean)
Within.aov.RMR
knitr::kable(nice(Within.aov.RMR))

# Post Hoc Testing for RMR
Within_Fitted_RMR <- emmeans(Within.aov.RMR, ~ Measure)
Within_Fitted_RMR
pairsWithin_Fitted_RMR <- pairs(Within_Fitted_RMR, adjust = "bon")
pairsWithin_Fitted_RMR

# Repeated Measures ANOVA for RMR - female
Within.aov.RMR <- aov_car(RMR ~ Measure + Error(ID/Measure), data=RMRANOVAFemale, fun_aggregate = mean)
Within.aov.RMR
knitr::kable(nice(Within.aov.RMR))

# Post Hoc Testing for RMR
Within_Fitted_RMR <- emmeans(Within.aov.RMR, ~ Measure)
Within_Fitted_RMR
pairsWithin_Fitted_RMR <- pairs(Within_Fitted_RMR, adjust = "bon")
pairsWithin_Fitted_RMR

# Repeated Measures ANOVA for RMR - male
Within.aov.RMR <- aov_car(RMR ~ Measure + Error(ID/Measure), data=RMRANOVAMale, fun_aggregate = mean)
Within.aov.RMR
knitr::kable(nice(Within.aov.RMR))

# Post Hoc Testing for RMR
Within_Fitted_RMR <- emmeans(Within.aov.RMR, ~ Measure)
Within_Fitted_RMR
pairsWithin_Fitted_RMR <- pairs(Within_Fitted_RMR, adjust = "bon")
pairsWithin_Fitted_RMR

### RMSE ### 

RMRDataFemale = subset(RMRData, RMRData[ ,'Sex']=='Female')
RMRDataMale = subset(RMRData, RMRData[ ,'Sex']=='Male')
  
rmse.all.HB = rmse(RMRData$IC.RMR, RMRData$`Harris-Benedict.ALL`)
rmse.all.HB

rmse.all.HB = rmse(RMRDataFemale$IC.RMR, RMRDataFemale$`Harris-Benedict.ALL`)
rmse.all.HB

rmse.all.HB = rmse(RMRDataMale$IC.RMR, RMRDataMale$`Harris-Benedict.ALL`)
rmse.all.HB

rmse.all.MJ = rmse(RMRData$IC.RMR, RMRData$`Mifflin-St.Jeor.ALL`)
rmse.all.MJ

rmse.all.MJ = rmse(RMRDataFemale$IC.RMR, RMRDataFemale$`Mifflin-St.Jeor.ALL`)
rmse.all.MJ

rmse.all.MJ = rmse(RMRDataMale$IC.RMR, RMRDataMale$`Mifflin-St.Jeor.ALL`)
rmse.all.MJ

rmse.all.WHO = rmse(RMRData$IC.RMR, RMRData$WHO.ALL)
rmse.all.WHO

rmse.all.WHO = rmse(RMRDataFemale$IC.RMR, RMRDataFemale$WHO.ALL)
rmse.all.WHO

rmse.all.WHO = rmse(RMRDataMale$IC.RMR, RMRDataMale$WHO.ALL)
rmse.all.WHO

rmse.all.Nelson = rmse(RMRData$IC.RMR, RMRData$Nelson)
rmse.all.Nelson

rmse.all.Nelson = rmse(RMRDataFemale$IC.RMR, RMRDataFemale$Nelson)
rmse.all.Nelson

rmse.all.Nelson = rmse(RMRDataMale$IC.RMR, RMRDataMale$Nelson)
rmse.all.Nelson

rmse.all.Dore = rmse(RMRData$IC.RMR, RMRData$Dore)
rmse.all.Dore

### Bland-Altman analysis ###

ba.stats.all.HB = bland.altman.stats(RMRData$IC.RMR, RMRData$`Harris-Benedict.ALL`)
ba.stats.all.HB

ba.stats.all.HB = bland.altman.stats(RMRDataFemale$IC.RMR, RMRDataFemale$`Harris-Benedict.ALL`)
ba.stats.all.HB

ba.stats.all.HB = bland.altman.stats(RMRDataMale$IC.RMR, RMRDataMale$`Harris-Benedict.ALL`)
ba.stats.all.HB

ba.stats.all.MJ = bland.altman.stats(RMRData$IC.RMR, RMRData$`Mifflin-St.Jeor.ALL`)
ba.stats.all.MJ

ba.stats.all.MJ = bland.altman.stats(RMRDataFemale$IC.RMR, RMRDataFemale$`Mifflin-St.Jeor.ALL`)
ba.stats.all.MJ

ba.stats.all.MJ = bland.altman.stats(RMRDataMale$IC.RMR, RMRDataMale$`Mifflin-St.Jeor.ALL`)
ba.stats.all.MJ

ba.stats.all.WHO = bland.altman.stats(RMRData$IC.RMR, RMRData$WHO.ALL)
ba.stats.all.WHO

ba.stats.all.WHO = bland.altman.stats(RMRDataFemale$IC.RMR, RMRDataFemale$WHO.ALL)
ba.stats.all.WHO

ba.stats.all.WHO = bland.altman.stats(RMRDataMale$IC.RMR, RMRDataMale$WHO.ALL)
ba.stats.all.WHO

ba.stats.all.Nelson = bland.altman.stats(RMRData$IC.RMR, RMRData$Nelson)
ba.stats.all.Nelson

ba.stats.all.Nelson = bland.altman.stats(RMRDataFemale$IC.RMR, RMRDataFemale$Nelson)
ba.stats.all.Nelson

ba.stats.all.Nelson = bland.altman.stats(RMRDataMale$IC.RMR, RMRDataMale$Nelson)
ba.stats.all.Nelson

ba.stats.all.Dore = bland.altman.stats(RMRData$IC.RMR, RMRData$Dore)
ba.stats.all.Dore

### Bland-Altman plots ###

bland.plot.all.HR = blandr.draw(RMRData$IC.RMR, RMRData$`Harris-Benedict.ALL`, method1name = "Indirect Calorimetry",
                             method2name = "Harris Benedict",
                             plotTitle = "",
                             sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                             ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                             lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                             overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                             y.plot.mode = "difference", plotProportionalBias = TRUE,
                             plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                             ylim(-500, 1000) +
                             xlim(1100, 2250) +
                             theme(axis.title.x = element_blank()) +
                             #scale_x_continuous(name = "") +
  
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.HR
  
  bland.plot.female.HR = blandr.draw(RMRDataFemale$IC.RMR, RMRDataFemale$`Harris-Benedict.ALL`, method1name = "Indirect Calorimetry",
                                  method2name = "Harris Benedict",
                                  plotTitle = "Harris-Benedict",
                                  sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                  ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                  lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                  overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                  y.plot.mode = "difference", plotProportionalBias = TRUE,
                                  plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                  ylim(-500, 1000) +
                                  xlim(1100, 2250) +
                                  theme(axis.title.x = element_blank()) +
                                  ylab("") +
                                  
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  bland.plot.female.HR
  
  bland.plot.male.HR = blandr.draw(RMRDataMale$IC.RMR, RMRDataMale$`Harris-Benedict.ALL`, method1name = "Indirect Calorimetry",
                                  method2name = "Harris Benedict",
                                  plotTitle = "",
                                  sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                  ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                  lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                  overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                  y.plot.mode = "difference", plotProportionalBias = TRUE,
                                  plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                  ylim(-500, 1000) +
                                  xlim(1100, 2250) +
                                  theme(axis.title.x = element_blank()) +
                                  ylab("") +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  bland.plot.male.HR


bland.plot.all.MJ = blandr.draw(RMRData$IC.RMR, RMRData$`Mifflin-St.Jeor.ALL`, method1name = "Indirect Calorimetry",
                                method2name = "Mifflin-St. Jeor",
                                plotTitle = "",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.MJ

bland.plot.female.MJ = blandr.draw(RMRDataFemale$IC.RMR, RMRDataFemale$`Mifflin-St.Jeor.ALL`, method1name = "Indirect Calorimetry",
                                method2name = "Mifflin-St. Jeor",
                                plotTitle = "Mifflin-St. Jeor",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +
                                ylab("") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.female.MJ

bland.plot.male.MJ = blandr.draw(RMRDataMale$IC.RMR, RMRDataMale$`Mifflin-St.Jeor.ALL`, method1name = "Indirect Calorimetry",
                                method2name = "Mifflin-St. Jeor",
                                plotTitle = "",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +
                                ylab("") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.male.MJ

bland.plot.all.WHO = blandr.draw(RMRData$IC.RMR, RMRData$WHO.ALL, method1name = "Indirect Calorimetry",
                                method2name = "WHO",
                                plotTitle = "",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.WHO

bland.plot.female.WHO = blandr.draw(RMRDataFemale$IC.RMR, RMRDataFemale$WHO.ALL, method1name = "Indirect Calorimetry",
                                method2name = "WHO",
                                plotTitle = "WHO",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +
                                ylab("") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.female.WHO

bland.plot.male.WHO = blandr.draw(RMRDataMale$IC.RMR, RMRDataMale$WHO.ALL, method1name = "Indirect Calorimetry",
                                method2name = "WHO",
                                plotTitle = "",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +
                                ylab("") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.male.WHO

bland.plot.all.Nelson = blandr.draw(RMRData$IC.RMR, RMRData$Nelson, method1name = "Indirect Calorimetry",
                                method2name = "Nelson",
                                plotTitle = "",
                                sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                y.plot.mode = "difference", plotProportionalBias = TRUE,
                                plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                ylim(-500, 1000) +
                                xlim(1100, 2250) +
                                theme(axis.title.x = element_blank()) +
                                
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.Nelson

bland.plot.female.Nelson = blandr.draw(RMRDataFemale$IC.RMR, RMRDataFemale$Nelson, method1name = "Indirect Calorimetry",
                                    method2name = "Nelson",
                                    plotTitle = "Nelson",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                    ylim(-500, 1000) +
                                    ylab("") +
                                    xlim(1100, 2250) +
                                    theme(axis.title.x = element_blank()) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.female.Nelson

bland.plot.male.Nelson = blandr.draw(RMRDataMale$IC.RMR, RMRDataMale$Nelson, method1name = "Indirect Calorimetry",
                                    method2name = "Nelson",
                                    plotTitle = "",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
                                    ylim(-500, 1000) +
                                    xlim(1100, 2250) +
                                    theme(axis.title.x = element_blank()) +
                                    ylab("") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.male.Nelson

### arrange plots ###

plist = list(bland.plot.all.HR,
             bland.plot.female.HR,
             bland.plot.male.HR,
             bland.plot.all.MJ,
             bland.plot.female.MJ,
             bland.plot.male.MJ,
             bland.plot.all.WHO,
             bland.plot.female.WHO,
             bland.plot.male.WHO,
             bland.plot.all.Nelson,
             bland.plot.female.Nelson,
             bland.plot.male.Nelson
             )
subplot = do.call("grid.arrange",c(plist))
subplot
ggsave(file="subplot.svg",plot=subplot,width=8,height=10)







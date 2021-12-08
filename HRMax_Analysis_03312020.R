rm(list = ls())
cat("\f") 
dev.off()
# control + l clears console

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


####################
## Read in data ###
###################

# set working directory
setwd("C:\\Users\\blindse3\\Dropbox\\FAFC SMART Lab\\HR Max Paper\\Data")

# load data
HRData = loadWorkbook("ACSM2020HRDataV7.xlsx") 
HRData = read.xlsx(HRData, sheet = 1)
attach(HRData)

### Descriptives ###

# all participants
All_descriptives = describe(HRData)
All_descriptives

# split by sex - male
Sex_descriptives = describe.by(HRData,GENDER)
Sex_descriptives

# create male and female dataframes

HRData.female = HRData[HRData$GENDER=='female',]
HRData.male <- HRData[HRData$GENDER=='male',]

# mean and confidence intervals for max heart rate

library(rcompanion)

GXT_mean = groupwiseMean(HR.MAX ~ 1,
              data   = HRData,
              conf   = 0.95,
              digits = 5)
GXT_mean

GXT_mean_male = groupwiseMean(HR.MAX ~ 1,
                         data   = HRData.male,
                         conf   = 0.95,
                         digits = 5)
GXT_mean_male

GXT_mean_female = groupwiseMean(HR.MAX ~ 1,
                              data   = HRData.female,
                              conf   = 0.95,
                              digits = 5)
GXT_mean_female

FOX_mean = groupwiseMean(FOX ~ 1,
              data   = HRData,
              conf   = 0.95,
              digits = 5)
FOX_mean

GELLISH_mean = groupwiseMean(GELLISH ~ 1,
                         data   = HRData,
                         conf   = 0.95,
                         digits = 5)
GELLISH_mean

GULATI_mean = groupwiseMean(GULATI ~ 1,
                         data   = HRData.female,
                         conf   = 0.95,
                         digits = 5)
GULATI_mean

TANAKA_mean = groupwiseMean(TANAKA ~ 1,
                         data   = HRData,
                         conf   = 0.95,
                         digits = 5)
TANAKA_mean

ARENA_mean = groupwiseMean(ARENA ~ 1,
                         data   = HRData,
                         conf   = 0.95,
                         digits = 5)
ARENA_mean

ASTRAND_mean = groupwiseMean(ASTRAND ~ 1,
                         data   = HRData,
                         conf   = 0.95,
                         digits = 5)
ASTRAND_mean

NES_mean = groupwiseMean(NES ~ 1,
                         data   = HRData,
                         conf   = 0.95,
                         digits = 5)
NES_mean

FAIRBARN_M_mean = groupwiseMean(FAIRBARN_MALE ~ 1,
                         data   = HRData,
                         conf   = 0.95,
                         digits = 5)
FAIRBARN_M_mean

FAIRBARN_F_mean = groupwiseMean(FAIRBARN_FEMALE ~ 1,
                                data   = HRData,
                                conf   = 0.95,
                                digits = 5)
FAIRBARN_F_mean


# t-tests
t.test(HRData$HR.MAX, HRData$FOX, paired = TRUE, conf.level = 0.95)
t.test(HRData$HR.MAX, HRData$GELLISH, paired = TRUE, conf.level = 0.95)
t.test(HRData.female$HR.MAX, HRData.female$GULATI, paired = TRUE, conf.level = 0.95)
t.test(HRData$HR.MAX, HRData$TANAKA, paired = TRUE, conf.level = 0.95)
t.test(HRData$HR.MAX, HRData$ASTRAND, paired = TRUE, conf.level = 0.95)
t.test(HRData$HR.MAX, HRData$NES, paired = TRUE, conf.level = 0.95)
t.test(HRData.male$HR.MAX, HRData.male$FAIRBARN_MALE, paired = TRUE, conf.level = 0.95)
t.test(HRData.female$HR.MAX, HRData.female$FAIRBARN_FEMALE, paired = TRUE, conf.level = 0.95)


# RMSE 
rmse.all.fox = rmse(HRData$HR.MAX, HRData$FOX)
rmse.all.fox

rmse.all.gellish = rmse(HRData$HR.MAX, HRData$GELLISH)
rmse.all.gellish

rmse.female.gulati = rmse(HRData.female$HR.MAX, HRData.female$GULATI)
rmse.female.gulati

rmse.all.tanaka = rmse(HRData$HR.MAX, HRData$TANAKA)
rmse.all.tanaka

rmse.all.arena = rmse(HRData$HR.MAX, HRData$ARENA)
rmse.all.arena

rmse.all.astrand = rmse(HRData$HR.MAX, HRData$ASTRAND)
rmse.all.astrand

rmse.all.nes = rmse(HRData$HR.MAX, HRData$NES)
rmse.all.nes

rmse.male.fairbarn = rmse(HRData.male$HR.MAX, HRData.male$FAIRBARN_MALE)
rmse.male.fairbarn

rmse.female.fairbarn = rmse(HRData.female$HR.MAX, HRData.female$FAIRBARN_FEMALE)
rmse.female.fairbarn


# Bland-Altman analysis
ba.stats.all.fox = bland.altman.stats(HRData$HR.MAX, HRData$FOX)
ba.stats.all.fox

ba.stats.all.gellish = bland.altman.stats(HRData$HR.MAX, HRData$GELLISH)
ba.stats.all.gellish

ba.stats.female.gulati = bland.altman.stats(HRData.female$HR.MAX, HRData.female$GULATI)
ba.stats.female.gulati

ba.stats.all.tanaka = bland.altman.stats(HRData$HR.MAX, HRData$TANAKA)
ba.stats.all.tanaka

ba.stats.all.arena = bland.altman.stats(HRData$HR.MAX, HRData$ARENA)
ba.stats.all.arena

ba.stats.all.astrand = bland.altman.stats(HRData$HR.MAX, HRData$ASTRAND)
ba.stats.all.astrand

ba.stats.all.nes = bland.altman.stats(HRData$HR.MAX, HRData$NES)
ba.stats.all.nes

ba.stats.male.fairbarn = bland.altman.stats(HRData.male$HR.MAX, HRData.male$FAIRBARN_MALE)
ba.stats.male.fairbarn

ba.stats.female.fairbarn = bland.altman.stats(HRData.female$HR.MAX, HRData.female$FAIRBARN_FEMALE)
ba.stats.female.fairbarn

# Bland-Altman plots

bland.plot.all.fox = blandr.draw(HRData$HR.MAX, HRData$FOX, method1name = "GXT HR",
                             method2name = "FOX HR",
                             plotTitle = "Fox (All Participants)",
                             sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                             ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                             lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                             overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                             y.plot.mode = "difference", plotProportionalBias = TRUE,
                             plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.fox

bland.plot.all.gellish = blandr.draw(HRData$HR.MAX, HRData$GELLISH, method1name = "GXT HR",
                                 method2name = "GELLISH HR",
                                 plotTitle = "Gellish (All Participants)",
                                 sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                 ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                 lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                 overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                 y.plot.mode = "difference", plotProportionalBias = TRUE,
                                 plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.gellish

bland.plot.female.gulati = blandr.draw(HRData.female$HR.MAX, HRData.female$GULATI, method1name = "GXT HR",
                                     method2name = "GULATI HR",
                                     plotTitle = "Gulati (Females)",
                                     sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                     ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                     lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                     overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                     y.plot.mode = "difference", plotProportionalBias = TRUE,
                                     plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.female.gulati

bland.plot.all.tanaka = blandr.draw(HRData$HR.MAX, HRData$TANAKA, method1name = "GXT HR",
                                    method2name = "TANAKA HR",
                                    plotTitle = "Tanaka (All Participants)",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.tanaka

bland.plot.all.arena = blandr.draw(HRData$HR.MAX, HRData$ARENA, method1name = "GXT HR",
                                    method2name = "ARENA HR",
                                    plotTitle = "Arena (All Participants)",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.arena

bland.plot.all.astrand = blandr.draw(HRData$HR.MAX, HRData$ASTRAND, method1name = "GXT HR",
                                    method2name = "ASTRAND HR",
                                    plotTitle = "Astrand (All Participants)",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.astrand

bland.plot.all.nes = blandr.draw(HRData$HR.MAX, HRData$NES, method1name = "GXT HR",
                                    method2name = "NES HR",
                                    plotTitle = "Nes (All Participants)",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.all.nes

bland.plot.male.fairbarn = blandr.draw(HRData.male$HR.MAX, HRData.male$FAIRBARN_MALE, method1name = "GXT HR",
                                    method2name = "FAIRBARN HR",
                                    plotTitle = "Fairbarn (Males)",
                                    sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                    ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                    lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                    overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                    y.plot.mode = "difference", plotProportionalBias = TRUE,
                                    plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.male.fairbarn

bland.plot.female.fairbarn = blandr.draw(HRData.female$HR.MAX, HRData.female$FAIRBARN_FEMALE, method1name = "GXT HR",
                                       method2name = "FAIRBARN HR",
                                       plotTitle = "Fairbarn (Females)",
                                       sig.level = 0.95, LoA.mode = 1, annotate = FALSE, ciDisplay = TRUE,
                                       ciShading = TRUE, normalLow = FALSE, normalHigh = FALSE,
                                       lowest_y_axis = FALSE, highest_y_axis = FALSE, point_size = 0.8,
                                       overlapping = FALSE, plotter = "ggplot", x.plot.mode = "means",
                                       y.plot.mode = "difference", plotProportionalBias = TRUE,
                                       plotProportionalBias.se = FALSE, assume.differences.are.normal = TRUE) +
  ylim(-50, 50) +
  xlim(100, 240) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
bland.plot.female.fairbarn

### arrange plots ###

plist = list(bland.plot.all.fox,bland.plot.all.gellish,bland.plot.female.gulati,bland.plot.all.tanaka,bland.plot.all.arena,bland.plot.all.astrand,bland.plot.all.nes,bland.plot.male.fairbarn,bland.plot.female.fairbarn)
subplot = do.call("grid.arrange",c(plist))
ggsave(file="subplot.svg",plot=subplot,width=8,height=10)






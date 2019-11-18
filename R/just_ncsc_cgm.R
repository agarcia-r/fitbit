rm(list = ls())

library(tidyverse)

data <- read.csv("Fitbit Plus UHG Glucose Management Program 2019-11-01.csv", stringsAsFactors = F)

data$Archived.At.Date <- str_trunc(data$Archived.At.Date, 10)
data$Archived.At.Date <- as.Date(data$Archived.At.Date, format = "%Y-%m-%d")
date1 <- as.Date("11/01/2019", format = "%m/%d/%Y")
date2 <- as.Date("01/01/2019", format = "%m/%d/%Y")
data$Current <- ifelse(is.na(data$Archived.At.Date) | data$Archived.At.Date >=  date1, 1, 0)

data$CGM.1 <- ifelse(str_detect(data$Group.1, "CGM_NCSC"), 1, 0)
data$CGM.2 <- ifelse(str_detect(data$Group.2, "CGM_NCSC"), 1, 0)
data$CGM.3 <- ifelse(str_detect(data$Group.3, "CGM_NCSC"), 1, 0)
data$CGM.4 <- ifelse(str_detect(data$Group.4, "CGM_NCSC"), 1, 0)
data$CGM.5 <- ifelse(str_detect(data$Group.5, "CGM_NCSC"), 1, 0)
data$CGM.6 <- ifelse(str_detect(data$Group.6, "CGM_NCSC"), 1, 0)
data$CGM.7 <- ifelse(str_detect(data$Group.7, "CGM_NCSC"), 1, 0)

data$Any.CGM <- ifelse(data$CGM.1 == 1 | data$CGM.2 == 1 | data$CGM.3 == 1 | data$CGM.4 == 1 | data$CGM.5 == 1 | data$CGM.6 == 1 | data$CGM.7 == 1, 1, 0)

data <- data[data$Any.CGM == 1, ]

data$CGM.Eligible.1 <- ifelse(str_detect(data$Group.1, "ELIGIBLE"), 1, 0)
data$CGM.Eligible.2 <- ifelse(str_detect(data$Group.2, "ELIGIBLE"), 1, 0)
data$CGM.Eligible.3 <- ifelse(str_detect(data$Group.3, "ELIGIBLE"), 1, 0)
data$CGM.Eligible.4 <- ifelse(str_detect(data$Group.4, "ELIGIBLE"), 1, 0)
data$CGM.Eligible.5 <- ifelse(str_detect(data$Group.5, "ELIGIBLE"), 1, 0)
data$CGM.Eligible.6 <- ifelse(str_detect(data$Group.6, "ELIGIBLE"), 1, 0)
data$CGM.Eligible.7 <- ifelse(str_detect(data$Group.7, "ELIGIBLE"), 1, 0)

data$Any.Eligible <- ifelse(data$CGM.Eligible.1 == 1 | data$CGM.Eligible.2 == 1 | data$CGM.Eligible.3 == 1 | data$CGM.Eligible.4 == 1 | data$CGM.Eligible.5 == 1 | data$CGM.Eligible.6 == 1 | data$CGM.Eligible.7 == 1, 1, 0)

data$CGM.Declined.1 <- ifelse(str_detect(data$Group.1, "DECLINED"), 1, 0)
data$CGM.Declined.2 <- ifelse(str_detect(data$Group.2, "DECLINED"), 1, 0)
data$CGM.Declined.3 <- ifelse(str_detect(data$Group.3, "DECLINED"), 1, 0)
data$CGM.Declined.4 <- ifelse(str_detect(data$Group.4, "DECLINED"), 1, 0)
data$CGM.Declined.5 <- ifelse(str_detect(data$Group.5, "DECLINED"), 1, 0)
data$CGM.Declined.6 <- ifelse(str_detect(data$Group.6, "DECLINED"), 1, 0)
data$CGM.Declined.7 <- ifelse(str_detect(data$Group.7, "DECLINED"), 1, 0)

data$Any.Declined <- ifelse(data$CGM.Declined.1 == 1 | data$CGM.Declined.2 == 1 | data$CGM.Declined.3 == 1 | data$CGM.Declined.4 == 1 | data$CGM.Declined.5 == 1 | data$CGM.Declined.6 == 1 | data$CGM.Declined.7 == 1, 1, 0)

data$CGM.Dropoff.1 <- ifelse(str_detect(data$Group.1, "DROPOFF"), 1, 0)
data$CGM.Dropoff.2 <- ifelse(str_detect(data$Group.2, "DROPOFF"), 1, 0)
data$CGM.Dropoff.3 <- ifelse(str_detect(data$Group.3, "DROPOFF"), 1, 0)
data$CGM.Dropoff.4 <- ifelse(str_detect(data$Group.4, "DROPOFF"), 1, 0)
data$CGM.Dropoff.5 <- ifelse(str_detect(data$Group.5, "DROPOFF"), 1, 0)
data$CGM.Dropoff.6 <- ifelse(str_detect(data$Group.6, "DROPOFF"), 1, 0)
data$CGM.Dropoff.7 <- ifelse(str_detect(data$Group.7, "DROPOFF"), 1, 0)

data$Any.Dropoff <- ifelse(data$CGM.Dropoff.1 == 1 | data$CGM.Dropoff.2 == 1 | data$CGM.Dropoff.3 == 1 | data$CGM.Dropoff.4 == 1 | data$CGM.Dropoff.5 == 1 | data$CGM.Dropoff.6 == 1 | data$CGM.Dropoff.7 == 1, 1, 0)

data$Remove <- ifelse(data$Any.Eligible == 1 | data$Any.Declined == 1 | data$Any.Dropoff == 1, 1, 0)

data$Real.CGM <- ifelse(data$Any.CGM == 1 & data$Remove != 1, 1, 0)


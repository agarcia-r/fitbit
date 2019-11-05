#### Monthly Report Prep
rm(list = ls(all.names = TRUE))

### Load Packages
library(openxlsx)
library(tidyverse)
library(gdata)
library(zoo)
library(data.table)
library(gridExtra)
library(kableExtra)
library(reshape2)

### A helper function to assist with merging data
get_merge_cols <- function(data1, data2){
  names1 <- colnames(data1)
  names2 <- colnames(data2)
  remove <- setdiff(names1, names2)
  return(names1[! names1 %in% remove])
}

### A function to clean & merge the data
clean_merge_humana <- function(dir = directory, first.day = date.first.day, date.current = date.current.report, month = month.name){

  ## Set dir and read in files
  setwd(dir)

  ## Format the dates
  first.day      <- as.Date(first.day, format = "%m/%d/%Y")
  date.current <- as.Date(date.current, format = "%m/%d/%Y")

  ## Run this line to see all the files in the dir
  files <- list.files()

  ## Define a function to import the data
  get_billing_data <- function(str.name, file.list = files){
    data                       <- read.csv(files[str_detect(file.list, str.name)], stringsAsFactors = F)
    data$Enrolled.At.Date      <- str_trunc(data$Enrolled.At.Date, 10, ellipsis = "")
    data$Archived.At.Date      <- str_trunc(data$Archived.At.Date, 10, ellipsis = "")

    data$Enrolled.At.Date      <- as.Date(data$Enrolled.At.Date, format = "%m/%d/%Y")
    data$Archived.At.Date      <- as.Date(data$Archived.At.Date, format = "%m/%d/%Y")
    return(data)
  }

  data_all                       <- get_billing_data(month.name)

  # Recategorize by group & region
  data_all$Region <- "ALL"
  data_all$Group  <- "Humana"

  # Finally add a column with the labels combined
  data_all$Group.Region <- paste(data_all$Region, data_all$Group, sep = "_")
  data_all$Group.Region <- ifelse(data_all$Group.Region == "NA_NA", NA, data_all$Group.Region)

  # Tally by group
  data_all$Group_Humana   <- ifelse(data_all$Group.Region == "ALL_Humana", 1, 0)
  data_all$Group_Humana_SUM   <- sum(data_all$Group_Humana)

  # Client.Type = Care
  data_all$Client.Type <- "Care"

  ## Clean up the engagement column

  # Change to character and manipulate
  # Change back to logical
  data_all$Engaged.Status <- ifelse(data_all$Engaged.Status == "true", "TRUE", data_all$Engaged.Status)
  data_all$Engaged.Status <- ifelse(data_all$Engaged.Status == "false", "FALSE", data_all$Engaged.Status)
  data_all$Engaged.Status <- as.logical(data_all$Engaged.Status)

  # Change engagement status to T if: engaged = F, call date > 1st day of current month, call status = "completed"
  # Change engagement to "NA" if Archived.At.Date < first.day
  data_all$Engaged.Status <- NA

  data_all$Engaged.Status <- ifelse(data_all$Archived == T &
                                      data_all$Archived.At.Date < first.day,
                                    NA, data_all$Engaged.Status)

  # Change engagement to "NA" archived = T so they won't be counted towards the denominator
  data_all$Engaged.Status <- ifelse(data_all$Archived == T, NA, data_all$Engaged.Status)

  # Change engagement to "T" if archived = T but Archived.At.Date < first.day
  data_all$Engaged.Status <- ifelse(data_all$Archived == T & data_all$Archived.At.Date >= first.day, T, data_all$Engaged.Status)

  ## Add dates
  data_all$Report.Month         <- first.day
  data_all$date.current  <- date.current

  ## Now deal with enrollment

  # First clean up Archived column
  data_all$Archived <- ifelse(data_all$Archived == "true", "TRUE", data_all$Archived)
  data_all$Archived <- ifelse(data_all$Archived == "false", "FALSE", data_all$Archived)
  data_all$Archived <- ifelse(is.na(data_all$Archived), F, data_all$Archived)
  data_all$Archived <- as.logical(data_all$Archived)

  # Deal w/Enrollment column -> delete and recreate for clarity
  data_all$Enrolled <- NULL

  # Separate into YTD enrollment and current enrollment
  data_all$Enrollment.YTD <- T
  data_all$Enrollment.Current <- T

  # Change enrollment to current if (1) Not archived, or if (2) archived but date is in current month
  data_all$Enrollment.Current <- ifelse(data_all$Archived == T, F, T)
  data_all$Enrollment.Current <- ifelse(data_all$Archived == T & data_all$Archived.At.Date > first.day, T, data_all$Enrollment.Current)

  # Calculate enrollment time in days
  # Calculate avg. time enrolled
  data_all$Diff.Archived <- ifelse(data_all$Archived == T,
                                   data_all$Archived.At.Date - data_all$Enrolled.At.Date,
                                   NA)

  data_all$Diff.Current <- ifelse(data_all$Enrollment.Current == T,
                                  date.current - data_all$Enrolled.At.Date,
                                  NA)

  data_all$Diff.All     <- ifelse(data_all$Enrollment.Current == T,
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.Humana  <- ifelse(data_all$Enrollment.Current == T &
                                    data_all$Group == "Humana",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  # Return data_all
  return(data_all)
}

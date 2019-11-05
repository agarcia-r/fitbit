#### Monthly Report Prep
rm(list = ls(all.names = TRUE))

### Load Packages
library(openxlsx)
library(dplyr)
library(tidyr)
library(gdata)
library(lubridate)
library(zoo)
library(stringr)
library(forcats)
library(data.table)
library(ggplot2)
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
clean_merge_data <- function(directory, date.first.day, date.current.report){
  
  ## Set directory and read in files
  setwd(directory)
  
  ## Format the dates
  date.first.day      <- as.Date(date.first.day, format = "%m/%d/%Y")
  date.current.report <- as.Date(date.current.report, format = "%m/%d/%Y")
  
  ## Run this line to see all the files in the directory
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
  
  ## Read the files in, deal with dates here because it's easier
  data_all                       <- get_billing_data("Humana")
  
  ## Let's clean up the Group columns
  
  # Convert Group.1 & Group.2 character class -> we want to use str_detect to find matching strings
  data_all$Group.1 <- as.character(data_all$Group.1)

  # Create a new group column to collapse all of the different group tags
  # Write a function to search all colummns for a specified group tag (e.g. "TX" or "FIT")
  # Group.name refers to the part of the current group labels to look for
  # Tag indicates how it should be labeled in the new column
  data_all$Region <- NA
  data_all$Group  <- NA
  
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
  # Change engagement to "NA" if Archived.At.Date < date.first.day
  data_all$Engaged.Status <- ifelse(data_all$Archived == T &
                                      data_all$Archived.At.Date < date.first.day,
                                    NA, data_all$Engaged.Status)
  
  # Also change if they've received a completed call within the month
  
  # Change engagement to "NA" archived = T so they won't be counted towards the denominator
  data_all$Engaged.Status <- ifelse(data_all$Archived == T, NA, data_all$Engaged.Status)
  
  # Change engagement to "T" if archived = T but Archived.At.Date < date.first.day
  data_all$Engaged.Status <- ifelse(data_all$Archived == T & data_all$Archived.At.Date >= first.day, T, data_all$Engaged.Status)
  
  # Also change to T if not archived = F & they received a completed call & call was within the month
  data_all$Engaged.Status <- ifelse(data_all$Engaged.Status == F &
                                      data_all$Call.Date >= first.day  &
                                      data_all$Call.Outcome == "Call Completed",
                                    T, data_all$Engaged.Status)
  
  ## Add dates
  data_all$Report.Month         <- date.first.day
  data_all$date.current.report  <- date.current.report
  
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
  data_all$Enrollment.Current <- ifelse(data_all$Archived == T & data_all$Archived.At.Date > date.first.day, T, data_all$Enrollment.Current)
  
  # Calculate enrollment time in days
  # Calculate avg. time enrolled
  data_all$Diff.Archived <- ifelse(data_all$Archived == T,
                                   data_all$Archived.At.Date - data_all$Enrolled.At.Date,
                                   NA)
  
  data_all$Diff.Current <- ifelse(data_all$Enrollment.Current == T,
                                  date.current.report - data_all$Enrolled.At.Date,
                                  NA)
  
  data_all$Diff.All     <- ifelse(data_all$Enrollment.Current == T,
                                  date.current.report - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)
  
  data_all$Diff.Care    <- ifelse(data_all$Enrollment.Current == T &
                                    data_all$Client.Type == "Care",
                                  date.current.report - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)
  
  data_all$Diff.SaaS    <- ifelse(data_all$Enrollment.Current == T &
                                    data_all$Client.Type == "SaaS",
                                  date.current.report - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)
  
  data_all$Diff.Humana  <- ifelse(data_all$Enrollment.Current == T &
                                    data_all$Group == "Humana",
                                  date.current.report - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)
  
  data_all$Diff.UHC     <- ifelse(data_all$Enrollment.Current == T &
                                    data_all$Group == "UHC",
                                  date.current.report - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)
  
  # Return data_all
  return(data_all)
}

# Run the function - ignore the warnings it generates, these relate to NA's in the data
data_10_27 <- clean_merge_data(directory = "/Users/alex.garcia/Desktop/Data/Coaching/October/10.27",
                               date.first.day      = "10/01/2019",
                               date.current.report = "10/27/2019")

data_10_28 <- clean_merge_data(directory = "/Users/alex.garcia/Desktop/Data/Coaching/October/10.28",
                               date.first.day      = "10/01/2019",
                               date.current.report = "10/28/2019")

### Let's make some tables

## Write a function
make_stats_table <- function(directory, data = data_all, date.current.report){
  
  ## Prep an outcomes dataframe
  
  # Create it
  stats <- as.data.frame(matrix(nrow = 12, ncol = 10), stringsAsFactors = F)
  colnames(stats) <- c("Report.Date", "Client.Type", "Region", "Group", "Group.Region",
                       "Archived.Anytime", "Enrolled.Current", "Enrolled.YTD",
                       "Engaged", "Engaged.Percent")
  
  # Add report.date
  stats$Report.Date <- as.Date(date.current.report, format = "%m_%d_%Y")
  
  # Fill in the rows
  stats$Region <- as.character(c("GA", "NCSC", "NCSC", "TX", "TX", "TX", "TX", rep("ALL", 2), "WA", rep("ALL", 2)))
  
  stats$Group <- as.character(c("GMP", "CGM", "GMP", "CGM", "FIT", "GMP", "JOY",
                                "UHC", "Humana", "SEIU", "Care", "SaaS"))
  
  stats$Group.Region <- ifelse(!is.na(stats$Region),
                               as.character(paste(stats$Region, stats$Group, sep = "_")), NA)
  
  stats$Client.Type <- as.factor(c(rep("Care", 11), "SaaS"))
  
  # Calculate enrollment by program
  
  # Archived
  # By Group.Region
  table_groups                 <- table(data$Group.Region, data$Archived)
  stats$Archived.Anytime[1:7]  <- as.vector(table_groups[c(1, 3:8), 2])
  stats$Archived.Anytime[9:10] <- as.vector(table_groups[c(1, 9), 2])
  
  # Then by aggregate
  stats$Archived.Anytime[8]    <- sum(stats$Archived.Anytime[1:7])                                # All UHC
  table_orgs                   <- table(data$Organization.Name, data$Archived)
  stats$Archived.Anytime[11]   <- sum(stats$Archived.Anytime[1:10])                               # Care-All
  table_type                   <- table(data$Client.Type, data$Archived)
  stats$Archived.Anytime[12]   <- as.vector(table_type[2,2])                                      # SaaS-All
  
  # Current enrollment
  # By Group.Region
  table_groups                 <- table(data$Group.Region, data$Enrollment.Current)
  stats$Enrolled.Current[1:7]  <- as.vector(table_groups[c(1, 3:8), 2])
  stats$Enrolled.Current[9:10] <- as.vector(table_groups[c(1, 9), 2])
  
  # Then by aggregate
  stats$Enrolled.Current[8]    <- sum(stats$Enrolled.Current[1:7])                                # All UHC
  table_orgs                   <- table(data$Organization.Name, data$Enrollment.Current)
  stats$Enrolled.Current[11]   <- sum(stats$Enrolled.Current[1:10])                               # Care-All
  table_type                   <- table(data$Client.Type, data$Enrollment.Current)
  stats$Enrolled.Current[12]   <- as.vector(table_type[2, 2])                                     # SaaS-All
  
  # Next enrollment YTD
  # By Group.Region first
  table_groups                <- table(data$Group.Region, data$Enrollment.YTD)
  stats$Enrolled.YTD[1:7]     <- as.vector(table_groups[c(1, 3:8)])
  stats$Enrolled.YTD[9:10]    <- as.vector(table_groups[c(1, 9)])
  
  # Then by aggregate groups
  stats$Enrolled.YTD[8]       <- sum(stats$Enrolled.YTD[1:7])                                    # All UHC
  table_orgs                  <- table(data$Organization.Name, data$Enrollment.YTD)
  stats$Enrolled.YTD[11]      <- sum(stats$Enrolled.YTD[1:10])                                   # Care-All
  table_type                  <- table(data$Client.Type, data$Enrollment.Current)
  stats$Enrolled.YTD[12]      <- as.vector(table_type[2, 2])                                     # SaaS-All
  
  # Avg. enrollment
  # Current first
  data_current                <- data[!is.na(data$Group.Region) & data$Enrollment.Current == T, ]
  data_current                <- group_by(data_current, Group.Region)
  data_current                <- mutate(data_current, Avg.Enrolled.Time.Current = round(mean(Diff.Current), 0))
  data_current                <- data_current[ , c("Group.Region", "Avg.Enrolled.Time.Current")]
  data_current                <- distinct(data_current)
  
  stats                       <- merge(stats, data_current, by = c("Group.Region"), all = T)
  
  stats$Avg.Enrolled.Time.Current[1] <- round(mean(data$Diff.Care[data$Enrollment.Current == T], na.rm = T), 0)
  stats$Avg.Enrolled.Time.Current[3] <- round(mean(data$Diff.SaaS[data$Enrollment.Current == T], na.rm = T), 0)
  stats$Avg.Enrolled.Time.Current[4] <- round(mean(data$Diff.UHC[data$Enrollment.Current == T], na.rm = T), 0)
  
  # Then archived
  data_archived               <- data[!is.na(data$Group.Region) & data$Archived == T, ]
  data_archived               <- group_by(data_archived, Group.Region)
  data_archived               <- mutate(data_archived, Avg.Enrolled.Time.Archived = round(mean(Diff.Archived), 0))
  data_archived               <- data_archived[ , c("Group.Region", "Avg.Enrolled.Time.Archived")]
  data_archived               <- distinct(data_archived)
  
  stats                       <- merge(stats, data_archived, by = "Group.Region", all = T)
  
  stats$Avg.Enrolled.Time.Archived[1] <- round(mean(data$Diff.Care[data$Archived == T], na.rm = T), 0)
  stats$Avg.Enrolled.Time.Archived[3] <- round(mean(data$Diff.SaaS[data$Archived == T], na.rm = T), 0)
  stats$Avg.Enrolled.Time.Archived[4] <- round(mean(data$Diff.UHC[data$Archived == T], na.rm = T), 0)
  
  # Then both together
  data_both                <- data[!is.na(data$Group.Region), ]
  data_both                <- group_by(data_both, Group.Region)
  data_both                <- mutate(data_both, Avg.Enrolled.Time.All = round(mean(Diff.Care), 0))
  data_both                <- data_both[ , c("Group.Region", "Avg.Enrolled.Time.All")]
  data_both                <- distinct(data_both)
  
  stats                    <- merge(stats, data_both, by = "Group.Region", all = T)
  
  stats$Avg.Enrolled.Time.All[1] <- round(mean(data$Diff.Care, na.rm = T), 0)
  stats$Avg.Enrolled.Time.All[3] <- round(mean(data$Diff.SaaS, na.rm = T), 0)
  stats$Avg.Enrolled.Time.All[4] <- round(mean(data$Diff.UHC, na.rm = T), 0)
  
  # Calculate engagement by program - same method as above
  # By Group.Region first
  table_groups                <- table(data$Group.Region, data$Engaged.Status)
  stats$Engaged[1:7]          <- as.vector(table_groups[c(1, 3:8), 2])
  stats$Engaged[9:10]         <- as.vector(table_groups[c(1, 9)])
  
  # Then by aggregate groups
  stats$Engaged[8]            <- sum(stats$Engaged[1:7])                                         # All UHC
  table_orgs                  <- table(data$Organization.Name, data$Engaged.Status)
  stats$Engaged[11]           <- sum(stats$Engaged[1:10])                                        # Care-All
  table_type                  <- table(data$Client.Type, data$Engaged.Status)
  stats$Engaged[12]           <- as.vector(table_type[2,2])                                      # SaaS-All
  
  # Calculate engaged_percent
  stats$Engaged.Percent       <- round((stats$Engaged / stats$Enrolled.Current) * 100, 0)
  
  # Finally, drop the group_region column
  stats$Group.Region <- NULL
  
  # And write it
  write.csv(stats, file = paste(directory, "/CustomerStats_", date.current.report, ".csv", sep = ""))
  
  return(stats)
}

## Run it
stats_10_27 <- make_stats_table(directory = "/Users/alex.garcia/Desktop/Data/Coaching/October/10.27",
                                data = data_10_27,
                                date.current.report = "10_27_2019")

stats_10_28 <- make_stats_table(directory = "/Users/alex.garcia/Desktop/Data/Coaching/October/10.28",
                                data = data_10_28,
                                date.current.report = "10_27_2019")

## Coach capacity
make_coach_table <- function(directory, data = data_all, date.current.report){
  
  # Get just the rows for Care clients, where Primary.Coach has "Coach" in name
  data_coach                 <- data[data$Client.Type == "Care" &
                                       data$Enrollment.Current == T &
                                       str_detect(data$Primary.Coach, "Coach"), ]
  
  # Make a table of Group.Region x Primary.Coach, knock it sideways so it's easier to read
  table_coach                <- table(data_coach$Group.Region, data_coach$Primary.Coach)
  data_coach                 <- as.data.frame.matrix(t(table_coach))
  
  # Add row sums into a final column -> total number of participants per coach
  data_coach$Total.Per.Coach <- rowSums(data_coach)
  
  # Extract the coach names from the rownames, make it into a column, reset the rownames
  data_coach$Coach           <- rownames(data_coach)
  data_coach                 <- data_coach[ , c(11, 1:10)]
  rownames(data_coach)       <- NULL
  
  # Add row totals
  col_totals                   <- colSums(data_coach[ , 2:11])
  final_row                    <- c(0, col_totals)
  data_coach                   <- rbind(data_coach, final_row)
  data_coach[10, 1]            <- "Total.Per.Program"
  data_coach$Max.Capacity      <- NA
  data_coach$Max.Capacity[1:9] <- c(rep(300, 3), rep(150, 2), rep(300, 4))
  data_coach$Max.Capacity[10]  <- sum(data_coach$Max.Capacity[1:9])
  data_coach$Vacancy.Count     <- data_coach$Max.Capacity - data_coach$Total.Per.Coach
  data_coach$Vacancy.Percent   <- round((1 - (data_coach$Total.Per.Coach / data_coach$Max.Capacity)) * 100, 0)
  data_coach$Report.Date       <- NA
  data_coach$Report.Date       <- as.Date(date.current.report, format = "%m_%d_%Y")
  
  
  # Rearrange it
  data_coach                   <- data_coach[ , c(15, 1:14)]
  
  # Write it
  write.csv(data_coach, file = paste(directory, "/CoachCapacity_", date.current.report, ".csv", sep = ""))
  
  # Return it
  return(data_coach)
}

coach_10_27      <- make_coach_table(directory = "/Users/alex.garcia/Desktop/Data/Coaching/October/10.27",
                                     data = data_10_27,
                                     date.current.report = "10_27_2019")










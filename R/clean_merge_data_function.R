#' Clean & Merge Data Function
#'
#' This function allows you import all of the associated files for one billing report, clean them, and merge them.
#' @keywords cleaning merging
#' @export
#' @examples
#' clean_merge_data()

# For testing
rm(list = ls())
library(tidyverse)
directory = "~/Desktop/FHS/Coaching/Data/October/10.28"
date.first.day = "10_01_2019"
date.current.report = "10_28_2019"

get_merge_cols <- function(data1, data2){
  names1 <- colnames(data1)
  names2 <- colnames(data2)
  remove <- setdiff(names1, names2)
  return(names1[! names1 %in% remove])
}

clean_merge_data <- function(dir = directory, first.day = date.first.day, date.current = date.current.report){

  ## Set dir and read in files
  setwd(dir)

  ## Format the dates
  first.day    <- as.Date(first.day, format = "%m_%d_%Y")
  date.current <- as.Date(date.current, format = "%m_%d_%Y")

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

  ## Read the files in, deal with dates here because it's easier
  plus_humana                       <- get_billing_data("Humana")
  plus_gmp                          <- get_billing_data("Fitbit Plus UHG Glucose Management Program 2019")
  plus_gmp_fitbit                   <- get_billing_data("Fitbit Plus UHG Glucose Management Program  Fitbit")
  plus_seiu                         <- get_billing_data("SEIU")

  # SaaS
  saas_p1                           <- get_billing_data("Partial 1")
  saas_p2                           <- get_billing_data("Partial 2")
  saas_p3                           <- get_billing_data("Partial 3")
  saas_p4                           <- get_billing_data("Partial 4")
  saas_p5                           <- get_billing_data("Partial 5")
  saas_p6                           <- get_billing_data("Partial 6")

  # Call log
  call_log            = read.csv(files[str_detect(files, "Coach Calls")], stringsAsFactors = F)

  ## Plus data

  # Merge the data based on shared columns
  merge.columns1 <- get_merge_cols(plus_gmp, plus_gmp_fitbit)
  plus_combined <- merge(plus_gmp, plus_gmp_fitbit, by = merge.columns1, all = T)

  merge.columns2 <- get_merge_cols(plus_combined, plus_seiu)
  plus_combined <- merge(plus_combined, plus_seiu, by = merge.columns2, all = T)

  merge.columns3 <- get_merge_cols(plus_combined, plus_humana)
  plus_combined <- merge(plus_combined, plus_humana, by = merge.columns3, all = T)

  # Merge all SaaS data
  merge.columns4 <- get_merge_cols(saas_p1, saas_p2)
  saas_combined   <- merge(saas_p1, saas_p2, by = merge.columns4, all = T)

  merge.columns5 <- get_merge_cols(saas_combined, saas_p3)
  saas_combined   <- merge(saas_combined, saas_p3, by = merge.columns5, all = T)

  merge.columns6 <- get_merge_cols(saas_combined, saas_p4)
  saas_combined   <- merge(saas_combined, saas_p4, by = merge.columns6, all = T)

  merge.columns7 <- get_merge_cols(saas_combined, saas_p5)
  saas_combined   <- merge(saas_combined, saas_p5, by = merge.columns7, all = T)

  merge.columns8 <- get_merge_cols(saas_combined, saas_p6)
  saas_combined   <- merge(saas_combined, saas_p6, by = merge.columns8, all = T)

  # Merge Plus data with SaaS data
  # Generates some warnings, but this is OK -> doesn't affect us
  merge.columns9 <- get_merge_cols(plus_combined, saas_combined)
  data_all <- merge(saas_combined, plus_combined, by = merge.columns9, all = T)

  ## Now let's clean up the identifier columns - we want to extract any Salesforce IDs
  # Let's start with the identifier columns - per Tessie (10/18), just need Salesforce Lead ID, nothing else
  # Only the Id.1 and Id.2 columns contain what we want, delete the rest for clarity
  data_all[ , c("Identifier.Label.3", "Identifier.Label.4",
                "Identifier.Value.3", "Identifier.Value.4",
                "Identifier.System.1", "Identifier.System.2",
                "Identifier.System.3", "Identifier.System.4")] <- NULL

  # Some IDs are in Identifier.Value.1, some are in Identifier.Value.2
  # Not all the values are correct, some appear to not be Salesforce IDs (correct IDs start with "00")
  # Some non-Salesforce IDs also start with "00"

  # Check number of Salesforce IDs in value columns
  #summary(startsWith(data_all$Identifier.Value.1, "00") & data_all$Identifier.Label.1 == "Salesforce Lead ID")
  #summary(startsWith(data_all$Identifier.Value.2, "00") & data_all$Identifier.Label.1 == "Salesforce Lead ID")

  # Extract just the Salesforce IDs from Identifier.Value.1 by selecting only values with label "Salesforce Lead ID" that also start with "00"
  data_all$Salesforce.Lead.ID1 <- ifelse(startsWith(data_all$Identifier.Value.1, "00") & data_all$Identifier.Label.1 == "Salesforce Lead ID",
                                         data_all$Identifier.Value.1, NA)

  # Extract just the Salesforce IDs from Identifier.Value.2 by selecting only values with label "Salesforce Lead ID" that also start with "00"
  data_all$Salesforce.Lead.ID2 <- ifelse(startsWith(data_all$Identifier.Value.2, "00") & data_all$Identifier.Label.2 == "Salesforce Lead ID",
                                         data_all$Identifier.Value.2, NA)

  # Remove whitespace and erroneous characters from both Salesforce ID columns
  data_all$Salesforce.Lead.ID1 <- str_trim(data_all$Salesforce.Lead.ID1, side = "both")
  data_all$Salesforce.Lead.ID2 <- str_trim(data_all$Salesforce.Lead.ID2, side = "both")

  data_all$Salesforce.Lead.ID1 <- str_replace_all(data_all$Salesforce.Lead.ID1, "[[:punct:]]", " ")
  data_all$Salesforce.Lead.ID2 <- str_replace_all(data_all$Salesforce.Lead.ID2, "[[:punct:]]", " ")

  # Start combining the columns by adding the IDs from Salesforce.Lead.ID1
  # Then, if no ID was added in step 1 (e.g. Salesforce.Lead.ID1 is still NA), copy over Salesforce.Lead.ID2
  data_all$Salesforce.Lead.ID  <- data_all$Salesforce.Lead.ID1
  data_all$Salesforce.Lead.ID  <- ifelse(is.na(data_all$Salesforce.Lead.ID), data_all$Salesforce.Lead.ID2, data_all$Salesforce.Lead.ID1)

  # Remove Salesforce.Lead.ID1 & Salesforce.Lead.ID2 for clarity
  data_all$Salesforce.Lead.ID1 <- NULL
  data_all$Salesforce.Lead.ID2 <- NULL

  # Let's rearrange it so Salesforce Lead ID is next to Fitbit.ID
  data_all <- data_all[ , c(1:2, 36, 3:35)]

  ## Finally, let's merge the call data into the full data
  # First we have to make sure the IDs are formatted correctly

  # Make a copy of call_log and rearrange for clarity
  call_log <- call_log[ , c(1, 4:7)]

  # Remove any whitespace or special characters from the call_log IDs
  call_log$Salesforce.Lead.ID <- str_trim(call_log$Salesforce.Lead.ID, side = "both")
  call_log$Salesforce.Lead.ID <- str_replace_all(call_log$Salesforce.Lead.ID, "[[:punct:]]", " ")

  # Convert Call.Date to Date
  # Change 2-digit year, then coerce to date type format
  call_log$Call.Date <- str_replace(call_log$Call.Date, "19", "2019")
  call_log$Call.Date <- as.Date(call_log$Call.Date, format = "%m/%d/%Y")

  # Reduce to just calls in the most recent month & only completed calls
  call_log <- call_log[call_log$Call.Date > first.day, ]
  call_log <- call_log[call_log$Call.Outcome == "Call Completed", ]

  # Reduce to most recent call
  # Find IDs where there have been > 1 call within the current month
  n_occur <- data.frame(table(call_log$Salesforce.Lead.ID))

  # List IDs & records w/duplicates
  n_occur   <- n_occur[n_occur$Freq > 1, ]
  call_log <- call_log[call_log$Salesforce.Lead.ID %in% n_occur$Var1[n_occur$Freq > 1],]

  # Reduce call_log to most recent calls
  call_log <- call_log %>% group_by(Salesforce.Lead.ID) %>% arrange(Call.Date) %>% slice(n())

  # Finally merge it
  merge.columns10 <- get_merge_cols(data_all, call_log)
  data_all <- merge(data_all, call_log, by = merge.columns10, all = T)

  # This may add rows
  # This is because some participants in the call log don't have Salesforce IDs
  # e.g. These are notes from the coaches ("LA call-in")
  # Remove these
  data_all <- data_all[!is.na(data_all$Group.1), ]

  # Add columns on the end to specify SaaS or Care
  saas_combined$Organization.Name <- as.factor(saas_combined$Organization.Name)
  saas_orgs                       <- levels(saas_combined$Organization.Name)

  data_all$Client.Type <- ifelse(data_all$Organization.Name %in% saas_orgs, "SaaS", "Care")
  data_all$Client.Type <- as.factor(data_all$Client.Type)

  ## Let's clean up the Group columns

  # Convert Group.1 & Group.2 character class -> we want to use str_detect to find matching strings
  data_all$Group.1 <- as.character(data_all$Group.1)
  data_all$Group.2 <- as.character(data_all$Group.2)

  # Create a new group & region column to collapse all of the different group tags
  # There are only 3 regions - GA, TX, NCSC -- any other region gets "ALL"
  data_all$Region <- "ALL"

  # A participant's region is always part of the tag in Group.1 -> there are no NA's in this column
  # Don't need to search the other columns
  data_all$Region <- ifelse(str_detect(data_all$Group.1, "TX"), "TX", data_all$Region)
  data_all$Region <- ifelse(str_detect(data_all$Group.1, "GA"), "GA", data_all$Region)
  data_all$Region <- ifelse(str_detect(data_all$Group.1, "NCSC"), "NCSC", data_all$Region)

  # Let's dig in to groups
  # Start by giving anyone with a region tag other than all "GMP" by default
  table(data_all$Group)
  data_all$Group  <- ifelse(data_all$Region != "ALL", "GMP", NA)

  # Let's make indicators

  # If any column contains "CGM," assign indicator to "CGM"
  data_all$Group.CGM <- NA
  data_all$Group.CGM <- ifelse(str_detect(data_all$Group.1, "CGM") |
                               str_detect(data_all$Group.2, "CGM") |
                               str_detect(data_all$Group.3, "CGM") |
                               str_detect(data_all$Group.4, "CGM") |
                               str_detect(data_all$Group.5, "CGM") |
                               str_detect(data_all$Group.6, "CGM") |
                               str_detect(data_all$Group.7, "CGM"), 1, 0)

  # If any column c
  data_all$Group.CGM.Remove <- ifelse(str_detect(data_all$Group.1, "DECLINED") | str_detect(data_all$Group.1, "ELIGIBLE") | str_detect(data_all$Group.1, "DROPFF") | str_detect(data_all$Group.1, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE") |
                                      str_detect(data_all$Group.2, "DECLINED") | str_detect(data_all$Group.2, "ELIGIBLE") | str_detect(data_all$Group.2, "DROPFF") | str_detect(data_all$Group.2, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE") |
                                      str_detect(data_all$Group.3, "DECLINED") | str_detect(data_all$Group.3, "ELIGIBLE") | str_detect(data_all$Group.3, "DROPFF") | str_detect(data_all$Group.3, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE") |
                                      str_detect(data_all$Group.4, "DECLINED") | str_detect(data_all$Group.4, "ELIGIBLE") | str_detect(data_all$Group.4, "DROPFF") | str_detect(data_all$Group.4, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE") |
                                      str_detect(data_all$Group.5, "DECLINED") | str_detect(data_all$Group.5, "ELIGIBLE") | str_detect(data_all$Group.5, "DROPFF") | str_detect(data_all$Group.5, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE") |
                                      str_detect(data_all$Group.6, "DECLINED") | str_detect(data_all$Group.6, "ELIGIBLE") | str_detect(data_all$Group.6, "DROPFF") | str_detect(data_all$Group.6, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE") |
                                      str_detect(data_all$Group.7, "DECLINED") | str_detect(data_all$Group.7, "ELIGIBLE") | str_detect(data_all$Group.7, "DROPFF") | str_detect(data_all$Group.7, "ENDED") |str_detect(data_all$Group.1, "NOT_ELIGIBLE"),
                                      1, 0)

  data_all$Group.CGM <- ifelse(data_all$Group.CGM == 1 & data_all$Group.CGM.Remove == 1, 0, data_all$Group.CGM)

  data_all$Group.FIT <- NA
  data_all$Group.FIT <- ifelse(str_detect(data_all$Group.1, "FIT") |
                               str_detect(data_all$Group.2, "FIT") |
                               str_detect(data_all$Group.3, "FIT") |
                               str_detect(data_all$Group.4, "FIT") |
                               str_detect(data_all$Group.5, "FIT") |
                               str_detect(data_all$Group.6, "FIT") |
                               str_detect(data_all$Group.7, "FIT"), 1, 0)

  data_all$Group.JBT <- NA
  data_all$Group.JBT <- ifelse(str_detect(data_all$Group.1, "JBT") |
                               str_detect(data_all$Group.2, "JBT") |
                               str_detect(data_all$Group.3, "JBT") |
                               str_detect(data_all$Group.4, "JBT") |
                               str_detect(data_all$Group.5, "JBT") |
                               str_detect(data_all$Group.6, "JBT") |
                               str_detect(data_all$Group.7, "JBT"), 1, 0)

  data_all$Group.JBC <- NA
  data_all$Group.JBC <- ifelse(str_detect(data_all$Group.1, "JBC") |
                                 str_detect(data_all$Group.2, "JBC") |
                                 str_detect(data_all$Group.3, "JBC") |
                                 str_detect(data_all$Group.4, "JBC") |
                                 str_detect(data_all$Group.5, "JBC") |
                                 str_detect(data_all$Group.6, "JBC") |
                                 str_detect(data_all$Group.7, "JBC"), 1, 0)

  data_all$Group.JOY <- NA
  data_all$Group.JOY <- ifelse(data_all$Group.JBC == 1 | data_all$Group.JBT == 1, 1, 0)

  # Now overwrite if the conditions are met
  data_all$Group <- ifelse(data_all$Group.CGM == 1, "CGM", data_all$Group)
  data_all$Group <- ifelse(data_all$Group.FIT == 1, "FIT", data_all$Group)
  data_all$Group <- ifelse(data_all$Group != "FIT" & data_all$Group.JOY == 1, "JOY", data_all$Group)
  data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Organization.Name == "Humana", "Humana", data_all$Group)
  data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Organization.Name == "SEIU 775 Benefits Group", "SEIU", data_all$Group)
  data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Organization.Name == "Optum CMDM", "SaaS - Optum", data_all$Group)
  data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Client.Type == "SaaS", "SaaS - Other", data_all$Group)
  data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Client.Type == "Care", "Care", data_all$Group)

  ## Clean up the engagement column

  # Change to character and manipulate
  # Change back to logical
  data_all$Engaged.Status <- ifelse(data_all$Engaged.Status == "true", "TRUE", data_all$Engaged.Status)
  data_all$Engaged.Status <- ifelse(data_all$Engaged.Status == "false", "FALSE", data_all$Engaged.Status)
  data_all$Engaged.Status <- as.logical(data_all$Engaged.Status)

  # If they've received a completed call within the month, set engaged = T
  data_all$Engaged.Status <- ifelse(!is.na(data_all$Call.Date) &                                             # If there is a call listed, and...
                                    data_all$Call.Date >= first.day &                                        # Date of call is >= first day of month and...
                                    data_all$Call.Date <= date.current &                                     # Date of call is <= date of current report and...
                                    data_all$Call.Outcome == "Call Completed", T, data_all$Engaged.Status)  # Call outcome = "Completed", set to "T," if not, do nothing

  # If Archived.At.Date < first day of the month, set to NA
  data_all$Engaged.Status <- ifelse(!is.na(data_all$Archived.At.Date) &
                                    data_all$Archived.At.Date < first.day, NA, data_all$Engaged.Status)

  ## Add dates
  data_all$Report.Month  <- first.day
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

  data_all$Diff.Care    <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Client.Type == "Care",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.SaaS    <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Client.Type == "SaaS",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.Humana  <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Group == "Humana",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.CGM     <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Group == "CGM",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.FIT     <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Group == "FIT",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.GMP     <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Organization.Name == "UHG Glucose Management Program",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.JOY     <- ifelse(data_all$Enrollment.Current == T &
                                  data_all$Organization.Name == "JOY",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.SaaS.Optum     <- ifelse(data_all$Enrollment.Current == T &
                                    data_all$Organization.Name == "SaaS - Optum",
                                  date.current - data_all$Enrolled.At.Date,
                                  data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.SaaS.Other    <- ifelse(data_all$Enrollment.Current == T &
                                           data_all$Organization.Name == "SaaS - Other",
                                         date.current - data_all$Enrolled.At.Date,
                                         data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  data_all$Diff.SaaS.SEIU     <- ifelse(data_all$Enrollment.Current == T &
                                           data_all$Organization.Name == "SaaS - Optum",
                                         date.current - data_all$Enrolled.At.Date,
                                         data_all$Archived.At.Date - data_all$Enrolled.At.Date)

  # Return data_all
  return(data_all)
}

data_10_28 <- clean_merge_data(
)

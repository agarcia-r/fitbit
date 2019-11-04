#' Clean & Merge Data Function
#'
#' This function allows you import all of the associated files for one billing report, clean them, and merge them.
#' @keywords cleaning merging
#' @export
#' @examples
#' clean_merge_data()


clean_merge_data <- function(directory, date.first.day = date.first.day, date.current.report = date.current.report){

  ## Set directory and read in files
  setwd(directory)

  ## Format the dates
  date.first.day      <- as.Date(date.first.day, format = "%m_%d_%Y")
  date.current.report <- as.Date(date.current.report, format = "%m_%d_%Y")

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
  call_log <- call_log[call_log$Call.Date > date.first.day, ]
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

  # This merge added 3 rows (would expect no rows added)
  # This is because some participants in the call log don't have Salesforce IDs
  # e.g. These are notes from the coaches ("LA call-in")

  ## Let's clean up the Group columns

  # Convert Group.1 & Group.2 character class -> we want to use str_detect to find matching strings
  data_all$Group.1 <- as.character(data_all$Group.1)
  data_all$Group.2 <- as.character(data_all$Group.2)

  # Create a new group column to collapse all of the different group tags
  # Write a function to search all colummns for a specified group tag (e.g. "TX" or "FIT")
  # Group.name refers to the part of the current group labels to look for
  # Tag indicates how it should be labeled in the new column
  data_all$Region <- NA
  data_all$Group  <- NA

  # Make functions to search all columns and recategorize participants by region and group
  # The is.na() piece prevents it from overwriting the cell if it's already been categorized
  recat_reg <- function(region.name, tag){
    ifelse(is.na(data_all$Region) & str_detect(data_all$"Group.1", region.name) |
             is.na(data_all$Region) & str_detect(data_all$"Group.2", region.name) |
             is.na(data_all$Region) & str_detect(data_all$"Group.3", region.name) |
             is.na(data_all$Region) & str_detect(data_all$"Group.4", region.name) |
             is.na(data_all$Region) & str_detect(data_all$"Group.5", region.name) |
             is.na(data_all$Region) & str_detect(data_all$"Group.6", region.name) |
             is.na(data_all$Region) & str_detect(data_all$"Group.7", region.name), tag, data_all$Region)
  }

  recat_grp <- function(group.name, tag){
    ifelse(is.na(data_all$Group) & str_detect(data_all$"Group.1", group.name) |
             is.na(data_all$Group) & str_detect(data_all$"Group.2", group.name) |
             is.na(data_all$Group) & str_detect(data_all$"Group.3", group.name) |
             is.na(data_all$Group) & str_detect(data_all$"Group.4", group.name) |
             is.na(data_all$Group) & str_detect(data_all$"Group.5", group.name) |
             is.na(data_all$Group) & str_detect(data_all$"Group.6", group.name) |
             is.na(data_all$Group) & str_detect(data_all$"Group.7", group.name), tag, data_all$Group)
  }

  # Recategorize by group & region
  data_all$Region <- recat_reg("TX", "TX")
  data_all$Region <- recat_reg("NCSC", "NCSC")
  data_all$Region <- recat_reg("GA", "GA")
  data_all$Region <- recat_reg("SEIU", "WA")
  data_all$Region <- recat_reg("Humana", "ALL")

  data_all$Group  <- recat_grp("CGM", "CGM")
  data_all$Group  <- recat_grp("FIT", "FIT")
  data_all$Group  <- recat_grp("JBC", "JOY")
  data_all$Group  <- recat_grp("JBT", "JOY")
  data_all$Group  <- recat_grp("SEIU", "SEIU")
  data_all$Group  <- recat_grp("Humana", "Humana")

  # If none of the above apply, enter "GMP"
  data_all$Group  <- ifelse(!is.na(data_all$Region) & is.na(data_all$Group), "GMP", data_all$Group)

  # Finally add a column with the labels combined
  data_all$Group.Region <- paste(data_all$Region, data_all$Group, sep = "_")
  data_all$Group.Region <- ifelse(data_all$Group.Region == "NA_NA", NA, data_all$Group.Region)

  # Tally by group
  data_all$Group_GA_GMP   <- ifelse(data_all$Group.Region == "GA_GMP", 1, 0)
  data_all$Group_NCSC_CGM <- ifelse(data_all$Group.Region == "NCSC_CGM", 1, 0)
  data_all$Group_NCSC_GMP <- ifelse(data_all$Group.Region == "NCSC_GMP", 1, 0)
  data_all$Group_TX_CGM   <- ifelse(data_all$Group.Region == "TX_CGM", 1, 0)
  data_all$Group_TX_FIT   <- ifelse(data_all$Group.Region == "TX_FIT", 1, 0)
  data_all$Group_TX_GMP   <- ifelse(data_all$Group.Region == "TX_GMP", 1, 0)
  data_all$Group_TX_JOY   <- ifelse(data_all$Group.Region == "TX_JOY", 1, 0)
  data_all$Group_SEIU     <- ifelse(data_all$Group.Region == "WA_SEIU", 1, 0)
  data_all$Group_Humana   <- ifelse(data_all$Group.Region == "ALL_Humana", 1, 0)

  data_all$Group_GA_GMP_SUM   <- sum(data_all$Group_GA_GMP)
  data_all$Group_NCSC_CGM_SUM <- sum(data_all$Group_NCSC_CGM)
  data_all$Group_NCSC_GMP_SUM <- sum(data_all$Group_NCSC_GMP)
  data_all$Group_TX_CGM_SUM   <- sum(data_all$Group_TX_CGM)
  data_all$Group_TX_FIT_SUM   <- sum(data_all$Group_TX_FIT)
  data_all$Group_TX_GMP_SUM   <- sum(data_all$Group_TX_GMP)
  data_all$Group_TX_JOY_SUM   <- sum(data_all$Group_TX_JOY)
  data_all$Group_SEIU_SUM     <- sum(data_all$Group_SEIU)
  data_all$Group_Humana_SUM   <- sum(data_all$Group_Humana)

  # Add columns on the end to specify SaaS or Care
  saas_combined$Organization.Name <- as.factor(saas_combined$Organization.Name)
  saas_orgs                       <- levels(saas_combined$Organization.Name)

  data_all$Client.Type <- ifelse(data_all$Organization.Name %in% saas_orgs, "SaaS", "Care")
  data_all$Client.Type <- as.factor(data_all$Client.Type)

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
  data_all$Engaged.Status <- ifelse(data_all$Engaged.Status == F &
                                      data_all$Call.Date >= date.first.day  &
                                      data_all$Call.Outcome == "Call Completed",
                                    T, data_all$Engaged.Status)

  # Finally, NA's will count as FALSE here
  data_all$Engaged.Status <- ifelse(is.na(data_all$Engaged.Status), F, data_all$Engaged.Status)

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


#' Clean & Merge Data Function
#'
#' This function allows you import all of the associated files for one billing report, clean them, and merge them.
#' @keywords billing cleaning merging
#' @export clean_merge_billing
#' @examples
#' clean_merge_billing()

clean_merge_billing <- function(dir = directory, first.day = date.first.day, date.current = date.current.report, final.report = F){

  ## Set dir and read in files
  setwd(dir)

  ## Format the dates
  first.day    <- as.Date(first.day, format = "%m-%d-%Y")
  date.current <- as.Date(date.current, format = "%m-%d-%Y")

  ## Run this line to see all the files in the dir
  files <- list.files()

  ## Define a function to import the data
  get_billing_data <- function(str.name, file.list = files){
    data                       <- read.csv(files[stringr::str_detect(file.list, str.name)], stringsAsFactors = F)
    data$Enrolled.At.Date      <- str_trunc(data$Enrolled.At.Date, 10, ellipsis = "")
    data$Archived.At.Date      <- str_trunc(data$Archived.At.Date, 10, ellipsis = "")

    data$Enrolled.At.Date      <- as.Date(data$Enrolled.At.Date, format = "%m/%d/%Y")
    data$Archived.At.Date      <- as.Date(data$Archived.At.Date, format = "%m/%d/%Y")
    return(data)
  }

  ## Read the files in, deal with dates here because it's easier
  # Care
  care_humana                       <- get_billing_data("Humana")
  care_seiu                         <- get_billing_data("SEIU")

  # SaaS
  saas_p1                           <- get_billing_data("Partial 1")
  saas_p2                           <- get_billing_data("Partial 2")
  saas_p3                           <- get_billing_data("Partial 3")
  saas_p4                           <- get_billing_data("Partial 4")
  saas_p5                           <- get_billing_data("Partial 5")
  saas_p6                           <- get_billing_data("Partial 6")

  # If it's before the first of the month, then the Carenet file might not exist, we have to account for this
  carenet_exists <- ifelse(stringr::str_detect(files, "CARENET"), 1, 0)
  carenet_exists <- sum(carenet_exists)

  if(carenet_exists > 0){
    call_log <- read.csv(files[stringr::str_detect(files, "CARENET_TO_FITBIT_CONTACT")], header = T, sep = "|")
  } else {
    carenet_exists <- 0
  }

  ## Plus data

  # Define a function to merge the data based on shared columns

  # Merge the data based on shared columns
  merge.columns1 <- get_merge_cols(care_humana, care_seiu)
  care_combined <- merge(care_humana, care_seiu, by = merge.columns1, all = T)

  # Merge all SaaS data
  merge.columns2 <- get_merge_cols(saas_p1, saas_p2)
  saas_combined   <- merge(saas_p1, saas_p2, by = merge.columns2, all = T)

  merge.columns3 <- get_merge_cols(saas_combined, saas_p3)
  saas_combined   <- merge(saas_combined, saas_p3, by = merge.columns3, all = T)

  merge.columns4 <- get_merge_cols(saas_combined, saas_p4)
  saas_combined   <- merge(saas_combined, saas_p4, by = merge.columns4, all = T)

  merge.columns5 <- get_merge_cols(saas_combined, saas_p5)
  saas_combined   <- merge(saas_combined, saas_p5, by = merge.columns5, all = T)

  merge.columns6 <- get_merge_cols(saas_combined, saas_p6)
  saas_combined   <- merge(saas_combined, saas_p6, by = merge.columns6, all = T)

  # Merge Plus data with SaaS data
  # print(nrow(plus_combined) + nrow(saas_combined)) should yield same number of rows as total from individual datasets
  merge.columns7 <- get_merge_cols(care_combined, saas_combined)
  data_all <- merge(saas_combined, care_combined, by = merge.columns7, all = T)

  # Remove all the other datasets
  rm(list = c("care_combined", "care_humana", "care_seiu", "saas_p1", "saas_p2", "saas_p3", "saas_p4", "saas_p5", "saas_p6"))

  ## Now let's clean up the identifier columns - we want to extract any Salesforce IDs so we can merge this with the call sheet
  # Change all of them to character
  data_all$Identifier.Value.1 <- as.character(data_all$Identifier.Value.1)
  data_all$Identifier.Value.2 <- as.character(data_all$Identifier.Value.2)
  data_all$Identifier.Value.3 <- as.character(data_all$Identifier.Value.3)
  data_all$Identifier.Value.4 <- as.character(data_all$Identifier.Value.4)

  # There are multiple variations of how "Salesforce ID" is identified, e.g. might say "salesforce-id," "SalesforceLeadID" etc.
  # There are four rows where the actual value of the ID might live
  # Some Salesforce IDs are completely wrong, no clue why -> real IDs must have word "salesforce" in label column and start with "00" in value column
  # Extract just the Salesforce IDs from Identifier.Value.1 by selecting only values with label containing "alesforce" that also start with "00"
  data_all$Salesforce.Lead.ID1 <- ifelse(startsWith(data_all$Identifier.Value.1, "00") & stringr::str_detect(data_all$Identifier.Label.1, "alesforce"),
                                         data_all$Identifier.Value.1, NA)

  # Extract just the Salesforce IDs from Identifier.Value.2 by selecting only values with label containing "alesforce" that also start with "00"
  data_all$Salesforce.Lead.ID2 <- ifelse(startsWith(data_all$Identifier.Value.2, "00") & stringr::str_detect(data_all$Identifier.Label.2, "alesforce"),
                                         data_all$Identifier.Value.2, NA)

  # Extract just the Salesforce IDs from Identifier.Value.3 by selecting only values with label containing "alesforce" that also start with "00"
  data_all$Salesforce.Lead.ID3 <- ifelse(startsWith(data_all$Identifier.Value.3, "00") & stringr::str_detect(data_all$Identifier.Label.3, "alesforce"),
                                         data_all$Identifier.Value.3, NA)

  # Extract just the Salesforce IDs from Identifier.Value.4 by selecting only values with label containing "alesforce" that also start with "00"
  data_all$Salesforce.Lead.ID4 <- ifelse(startsWith(data_all$Identifier.Value.4, "00") & stringr::str_detect(data_all$Identifier.Label.4, "alesforce"),
                                         data_all$Identifier.Value.4, NA)

  # Some IDs may have whitespace or erroneous characters
  # Remove whitespace and erroneous characters from both Salesforce ID columns
  data_all$Salesforce.Lead.ID1 <- str_trim(data_all$Salesforce.Lead.ID1, side = "both")
  data_all$Salesforce.Lead.ID2 <- str_trim(data_all$Salesforce.Lead.ID2, side = "both")
  data_all$Salesforce.Lead.ID3 <- str_trim(data_all$Salesforce.Lead.ID3, side = "both")
  data_all$Salesforce.Lead.ID4 <- str_trim(data_all$Salesforce.Lead.ID4, side = "both")

  data_all$Salesforce.Lead.ID1 <- str_replace_all(data_all$Salesforce.Lead.ID1, "[[:punct:]]", " ")
  data_all$Salesforce.Lead.ID2 <- str_replace_all(data_all$Salesforce.Lead.ID2, "[[:punct:]]", " ")
  data_all$Salesforce.Lead.ID3 <- str_replace_all(data_all$Salesforce.Lead.ID3, "[[:punct:]]", " ")
  data_all$Salesforce.Lead.ID4 <- str_replace_all(data_all$Salesforce.Lead.ID4, "[[:punct:]]", " ")

  # Start combining the columns by adding the IDs from Salesforce.Lead.ID1
  # Then, if no ID was added in step 1 (e.g. Salesforce.Lead.ID is still NA), copy over Salesforce.Lead.ID2 (if it exists)
  # Continue for Salesforce.Lead.ID3 & 4
  data_all$Salesforce.Lead.ID  <- data_all$Salesforce.Lead.ID1
  data_all$Salesforce.Lead.ID  <- ifelse(is.na(data_all$Salesforce.Lead.ID) & !is.na(data_all$Salesforce.Lead.ID2), data_all$Salesforce.Lead.ID2, data_all$Salesforce.Lead.ID)
  data_all$Salesforce.Lead.ID  <- ifelse(is.na(data_all$Salesforce.Lead.ID) & !is.na(data_all$Salesforce.Lead.ID3), data_all$Salesforce.Lead.ID3, data_all$Salesforce.Lead.ID)
  data_all$Salesforce.Lead.ID  <- ifelse(is.na(data_all$Salesforce.Lead.ID) & !is.na(data_all$Salesforce.Lead.ID4), data_all$Salesforce.Lead.ID4, data_all$Salesforce.Lead.ID)

  ## Finally, let's merge the call data into the full data
  # Change the column names
    call_log <- call_log[ , c(1, 4, 9)]
    call_colnames <- c("Salesforce.Lead.ID", "Call.Date", "Call.Outcome")
    colnames(call_log) <- call_colnames

    # Convert dates back to character
    call_log$Call.Date <- as.character(call_log$Call.Date)

    # Change the date format
    call_log$Call.Date <- str_trunc(call_log$Call.Date, 8, ellipsis = "")
    call_log$Call.Date <- str_trim(call_log$Call.Date, side = "both")

    # Remove any whitespace or special characters from the call_log IDs
    call_log$Salesforce.Lead.ID <- str_trim(call_log$Salesforce.Lead.ID, side = "both")
    call_log$Salesforce.Lead.ID <- str_replace_all(call_log$Salesforce.Lead.ID, "[[:punct:]]", " ")

    # Limit the data to just the IDs we can match - discard the rest
    call_log <- call_log[startsWith(call_log$Salesforce.Lead.ID, "00"), ]

    # Convert Call.Date to Date
    # Change to 4-digit year, then coerce to date type format
    # Only search for "19" when it occurs at the end of the string (e.g. don't insert "2019" for the day)
    call_log$Call.Date <- str_replace(call_log$Call.Date, "19$", "2019")
    call_log$Call.Date <- as.Date(call_log$Call.Date, format = "%m/%d/%Y")

    # Reduce to just calls in the right time period (1st of month -> current data date) & only completed calls
    if(final.report == T){
      call_log <- call_log[call_log$Call.Date >= first.day & call_log$Call.Date <= date.current, ]
    } else {
      call_log <- call_log[call_log$Call.Date >= first.day & call_log$Call.Date < date.current, ]
    }

    # Add a check to see if call_log is empty
    dim.data <- dim(call_log)
    data.null <- ifelse(dim.data[1] == 0 | dim.data[2] == 0, 1, 0)

    # If call log is not null (e.g. dim.null == 0), then reduce to latest completed call for each participant
    # and merge this in to the full data. If call log is null, do nothing.
    if(data.null == 0){
      call_log <- call_log[stringr::str_detect(call_log$Call.Outcome, "ompleted"), ]

      # Reduce to most recent call (or it won't merge, but any one completed call is enough to toggle 'Engaged')
      # Find IDs where there have been > 1 call within the current month
      n_occur <- data.frame(table(call_log$Salesforce.Lead.ID))
      dim.n.occur <- dim(n_occur)

      # List IDs with duplicates
      # If there are none, skip this step
      if(dim.n.occur[1] != 0 & dim.n.occur[2] != 0){
        n_occur   <- n_occur[n_occur$Freq > 1, ]
        call_log  <- call_log[call_log$Salesforce.Lead.ID %in% n_occur$Var1[n_occur$Freq > 1], ]
      } else {
        call_log <- call_log
      }

      # Reduce call_log to most recent calls
      call_log <- call_log %>% group_by(Salesforce.Lead.ID) %>% arrange(Call.Date) %>% slice(n())

      # Finally merge it with the full billing data
      merge.columns10 <- get_merge_cols(data_all, call_log)
      data_all <- merge(data_all, call_log, by = merge.columns10, all = T)
    } else {
      data_all <- data_all
    }

  # Should have the exact same number of rows
  # Some participants in the call log don't have Salesforce IDs
  # e.g. These are notes from the coaches ("LA call-in")
  # Remove these
  data_all <- data_all[!is.na(data_all$Group.1), ]

  ## Let's clean up the Group columns

  # Add columns on the end to specify SaaS or Care
  saas_combined$Organization.Name <- as.factor(saas_combined$Organization.Name)
  saas_orgs                       <- levels(saas_combined$Organization.Name)

  data_all$Client.Type <- ifelse(data_all$Organization.Name %in% saas_orgs, "SaaS", "Care")
  data_all$Client.Type <- as.factor(data_all$Client.Type)

  # Let's dig in to groups

  # Categorize
  # First make a column that concatenates all the group tags
  data_all$Group.All <- paste(data_all$Group.1, data_all$Group.2, data_all$Group.3, data_all$Group.4, data_all$Group.5, data_all$Group.6, data_all$Group.7)

  # Then categorize
    # Start by creating blank column
      data_all$Group <- NA

    # Then categorize by subgroup
      data_all$Group <- ifelse(data_all$Organization.Name == "Humana", "Humana", data_all$Group)
      data_all$Group <- ifelse(data_all$Organization.Name == "SEIU 775 Benefits Group", "SEIU", data_all$Group)
      data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Client.Type == "SaaS", "SaaS", data_all$Group)
      data_all$Group <- ifelse(is.na(data_all$Group) & data_all$Client.Type == "Care", "Care", data_all$Group)

  ## Now deal with enrollment
  # First clean up Archived column
  data_all$Archived <- as.logical(data_all$Archived)

  # Separate out reactivations (currently shown in Archived column)
  # People who are reactivated are eligible to be currently enrolled
  data_all$Reactivated       <- ifelse(data_all$Archived.Reason == "reactivation", T, F)                                 # Create a new column indicating if the participant has been reactivated or not
  data_all$Reactived.At.Date <- ifelse(data_all$Reactivated == T, data_all$Archived.At.Date, NA)                         # Create a new column to hold reactivation date if person has been reactivated
  data_all$Archived.At.Date  <- ifelse(!is.na(data_all$Reactived.At.Date), NA, data_all$Archived.At.Date)                # If someone's been reactivated, set their Archive date to NA (not actually an archive date)
  data_all$Archived.At.Date  <- as.Date.numeric(data_all$Archived.At.Date, origin = "1970-01-01")                       # For some reason this Archive.Date to numeric, change it back

  # Deal w/Enrollment column -> delete for clarity (we'll be creating new Enrollment YTD & Enrollment Current columns)
  data_all$Enrolled <- NULL

  # Separate into YTD enrollment and current enrollment
  date.jan.1 <- as.Date(paste("01", "01", lubridate::year(date.current), sep = "_"), format = "%m_%d_%Y")                           # Get the date of the first day of the current year as reference point
  data_all$Enrollment.YTD <- ifelse(!is.na(data_all$Archived.At.Date) & data_all$Archived.At.Date < date.jan.1, F, T)    # If there's an archive date & it's before 01/01/current year, set to False

  # Change enrollment to current if (1) Not archived, or if (2) archived but date is in current month
  data_all$Enrollment.Current <- data_all$Enrollment.YTD                                                                 # Set Enrollment.Current to = Enrollment.YTD to start (less restrictive)
  data_all$Enrollment.Current <- ifelse(!is.na(data_all$Archived.At.Date) & data_all$Archived.At.Date < first.day,       # Now, if Archived.At.Date exists & is less than 1st of current month...
                                        F, data_all$Enrollment.Current)                                                 # set Enrollment.Current to True, else maintain existing value
  ## Clean up the engagement column

  # Change to logical
  data_all$Engaged.Status <- as.logical(data_all$Engaged.Status)

  # Check for anomalies -> there should be no one who's currently engaged who is not currently enrolled
  # Let's log these before we change their status
  data_all$Anomaly.Engaged <- ifelse(data_all$Enrollment.Current == F & data_all$Engaged.Status == T, 1, 0)
  engaged_anomaly <- data_all[data_all$Enrollment.Current == F & data_all$Engaged.Status == T, ]

  # If engaged_anomaly is not NULL, write it to a CSV
  if(dim(engaged_anomaly)[1] > 0){
    write.csv(engaged_anomaly, paste(paste(paste(dir,"Engaged.Anomalies", sep = "/"), date.current, sep = "_"), "csv", sep = "."))
  } else {
    rm(engaged_anomaly)
  }

  # After selecting these out, switch anyone not currently enrolled to Engaged = F
  data_all$Engaged.Status <- ifelse(data_all$Enrollment.Current == F, F, data_all$Engaged.Status)

  # Update based on Coach Calls -> everyone who is archived in the current month should be included in "Enrollment.Current," call_log1 has been reduced to only relevant calls already
  if(data.null == 0){
  data_all$Engaged.Status <- ifelse(!is.na(data_all$Call.Date), T, data_all$Engaged.Status)
  } else {
    data_all = data_all
  }

  # Make sure no one but reactivations are engaged
  #data_all$Engaged.Status <- ifelse(!is.na(data_all$Archived.Reason) & data_all$Archived.Reason != "reactivation", F, T)

  ## Add dates
  data_all$Report.Month  <- first.day
  data_all$date.current  <- date.current

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

  # Return data_all
  return(data_all)
}


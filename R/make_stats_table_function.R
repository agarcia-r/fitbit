#' Make Stats Table Function
#'
#' This function takes the output of the clean_merge_data function and turns it into the customer statistics table.
#' @keywords table coach customer
#' @export
#' @examples
#' make_stats_table()

make_stats_table <- function(dir = directory, data = ls(pos = 1), date.current = date.current.report){

  ## Prep an outcomes dataframe

  # Create it
  stats <- as.data.frame(matrix(nrow = 12, ncol = 10), stringsAsFactors = F)
  colnames(stats) <- c("Report.Date", "Client.Type", "Region", "Group", "Group.Region",
                       "Archived.Anytime", "Enrolled.Current", "Enrolled.YTD",
                       "Engaged", "Engaged.Percent")

  # Add report.date
  stats$Report.Date <- as.Date(date.current, format = "%m_%d_%Y")

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
  write.csv(stats, file = paste(dir, "/CustomerStats_", date.current, ".csv", sep = ""))

  return(stats)
}

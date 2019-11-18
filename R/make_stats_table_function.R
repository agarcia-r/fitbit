#' Make Stats Table Function
#'
#' This function takes the output of the clean_merge_data function and turns it into the customer statistics table.
#' @keywords table coach customer
#' @export
#' @examples
#' make_stats_table()

# For testing
data.name = "data_10_31"
data = get(data.name)
date.current.report = "10_31_2019"

make_stats_table <- function(dir = directory, data = get(data.name), date.current = date.current.report){

  ## Prep an outcomes dataframe

  # Create it
  stats <- as.data.frame(matrix(nrow = 13, ncol = 10), stringsAsFactors = F)
  colnames(stats) <- c("Report.Date", "Client.Type", "Region", "Group", "Group.Region",
                       "Archived.Anytime", "Enrolled.Current", "Enrolled.YTD",
                       "Engaged", "Engaged.Percent")

  # Add report.date
  stats$Report.Date <- as.Date(date.current, format = "%m_%d_%Y")

  # Fill in the rows
  stats$Region <- as.character(c("GA", "NCSC", "NCSC", rep("TX", 4), "WA", rep("ALL", 5)))

  stats$Group <- as.character(c("GMP", "CGM", "GMP", "CGM", "FIT", "GMP", "JOY",
                                "SEIU", "UHC", "Humana", "Care", "Optum", "SaaS"))

  stats$Group.Region <- ifelse(!is.na(stats$Region),
                               as.character(paste(stats$Region, stats$Group, sep = "_")), NA)

  stats$Client.Type <- as.factor(c(rep("Care", 11), "SaaS", "SaaS"))

  # Calculate enrollment by program

  # Archived
    table_groups                 <- table(data$Group.Region, data$Archived)                          # Rows in Stats Table:
    stats$Archived.Anytime[1:8]  <- as.vector(table_groups[c(5:11, 4), 2])                           # Rows 1-8: Individual UHC programs
    stats$Archived.Anytime[9]    <- sum(stats$Archived.Anytime[1:8])                                 # Row 9: Sum of UHC programs
    stats$Archived.Anytime[10]   <- as.vector(table_groups[1, 2])                                    # Row 10: All Humana
    stats$Archived.Anytime[11]   <- sum(stats$Archived.Anytime[9:10])                                # Row 11: All Care
    stats$Archived.Anytime[12]   <- as.vector(table_groups[2, 2])                                    # Row 12: Optum
    stats$Archived.Anytime[13]   <- sum(table_groups[2:3, 2])                                        # Row 13: All SaaS

  # Current enrollment
    table_groups                 <- table(data$Group.Region, data$Enrollment.Current)                # Rows in Stats Table:
    stats$Enrolled.Current[1:8]  <- as.vector(table_groups[c(5:11, 4), 2])                              # Rows 1-8: Individual UHC programs
    stats$Enrolled.Current[9]    <- sum(stats$Enrolled.Current[1:8])                                 # Row 9: Sum of UHC programs
    stats$Enrolled.Current[10]   <- as.vector(table_groups[1, 2])                                    # Row 10: All Humana
    stats$Enrolled.Current[11]   <- sum(stats$Enrolled.Current[9:10])                                # Row 11: All Care
    stats$Enrolled.Current[12]   <- as.vector(table_groups[2, 2])                                    # Row 12: Optum
    stats$Enrolled.Current[13]   <- sum(table_groups[2:3, 2])                                        # Row 13: All SaaS

  # Next enrollment YTD
    # Here there's no "False" column in table because everyone in dataset was enrolled at some point in the year
    table_groups             <- table(data$Group.Region, data$Enrollment.YTD)                       # Rows in Stats Table:
    stats$Enrolled.YTD[1:8]  <- as.vector(table_groups[c(5:11, 4)])                                 # Rows 1-8: Individual UHC programs
    stats$Enrolled.YTD[9]    <- sum(stats$Enrolled.YTD[1:8])                                        # Row 9: Sum of UHC programs
    stats$Enrolled.YTD[10]   <- as.vector(table_groups[1])                                       # Row 10: All Humana
    stats$Enrolled.YTD[11]   <- sum(stats$Enrolled.YTD[9:10])                                       # Row 11: All Care
    stats$Enrolled.YTD[12]   <- as.vector(table_groups[2])                                       # Row 12: Optum
    stats$Enrolled.YTD[13]   <- sum(table_groups[2:3])                                           # Row 13: All SaaS

  # Calculate engagement by program - same method as above
    # By Group.Region first
    table_groups             <- table(data$Group.Region, data$Engaged.Status)                       # Rows in Stats Table:
    stats$Engaged[1:8]       <- as.vector(table_groups[c(5:11, 4)])                                 # Rows 1-8: Individual UHC programs
    stats$Engaged[9]         <- sum(stats$Engaged[1:8])                                        # Row 9: Sum of UHC programs
    stats$Engaged[10]        <- as.vector(table_groups[1])                                       # Row 10: All Humana
    stats$Engaged[11]        <- sum(stats$Engaged[9:10])                                       # Row 11: All Care
    stats$Engaged[12]        <- as.vector(table_groups[2])                                       # Row 12: Optum
    stats$Engaged[13]        <- sum(table_groups[2:3])                                           # Row 13: All SaaS

  # Calculate engaged_percent
    stats$Engaged.Percent       <- round((stats$Engaged / stats$Enrolled.Current) * 100, 0)

  # Finally, drop the group_region column
    stats$Group.Region <- NULL

  # And write it
    write.csv(stats, file = paste(dir, "/CustomerStats_", date.current, ".csv", sep = ""))

  return(stats)
}

stats_11_1 <- make_stats_table()

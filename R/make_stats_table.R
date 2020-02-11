#' Make Stats Table Function
#'
#' This function takes the output of the clean_merge_data function and turns it into the customer statistics table.
#' @keywords table coach customer
#' @export make_stats_table
#' @examples
#' make_stats_table()


make_stats_table <- function(dir = directory, data, date.current = date.current.report){

  ## Prep an outcomes dataframe

  # Create it
  stats <- as.data.frame(matrix(nrow = 4, ncol = 8), stringsAsFactors = F)
  colnames(stats) <- c("Report.Date", "Client.Type", "Group",
                       "Archived.Anytime", "Enrolled.Current", "Enrolled.YTD",
                       "Engaged", "Engaged.Percent")

  # Add report.date
  stats$Report.Date <- as.Date(date.current, format = "%m-%d-%Y")

  # Fill in the rows
  stats$Group <- as.character(c("SEIU", "Humana", "Care", "SaaS"))

  stats$Client.Type <- as.factor(c(rep("Care", 3), "SaaS"))

  # Calculate enrollment by program

  # Archived
    table_groups                 <- table(data$Group, data$Archived)                          # Rows in Stats Table:
    stats$Archived.Anytime[1]    <- as.vector(table_groups[3, 2])                                    # Row 1: SEIU
    stats$Archived.Anytime[2]    <- as.vector(table_groups[1, 2])                                    # Row 2 Humana
    stats$Archived.Anytime[3]    <- sum(stats$Archived.Anytime[1:2])                                 # Row 3: All Care
    stats$Archived.Anytime[4]    <- as.vector(table_groups[2, 2])                                    # Row 4: All SaaS

  # Current enrollment
    table_groups                 <- table(data$Group, data$Enrollment.Current)                # Rows in Stats Table:
    stats$Enrolled.Current[1]    <- as.vector(table_groups[3, 2])                                    # Row 1: SEIU
    stats$Enrolled.Current[2]    <- as.vector(table_groups[1, 2])                                    # Row 2 Humana
    stats$Enrolled.Current[3]    <- sum(stats$Enrolled.Current[1:2])                                 # Row 3: All Care
    stats$Enrolled.Current[4]    <- as.vector(table_groups[2, 2])                                    # Row 4: All SaaS

  # Next enrollment YTD
    # Here there's no "False" column in table because everyone in dataset was enrolled at some point in the year
    table_groups           <- table(data$Group, data$Enrollment.YTD)                                 # Rows in Stats Table:
    stats$Enrolled.YTD[1]       <- as.vector(table_groups[3, 2])                                          # Row 1: SEIU
    stats$Enrolled.YTD[2]       <- as.vector(table_groups[1, 2])                                          # Row 2 Humana
    stats$Enrolled.YTD[3]       <- sum(stats$Enrolled.YTD[1:2])                                                # Row 3: All Care
    stats$Enrolled.YTD[4]       <- as.vector(table_groups[2, 2])                                          # Row 4: All SaaS

  # Calculate engagement by program - same method as above
    # By Group.Region first
    table_groups           <- table(data$Group, data$Engaged.Status)                       # Rows in Stats Table:
    stats$Engaged[1]       <- as.vector(table_groups[3, 2])                                          # Row 1: SEIU
    stats$Engaged[2]       <- as.vector(table_groups[1, 2])                                          # Row 2 Humana
    stats$Engaged[3]       <- sum(stats$Engaged.Status[1:2])                                         # Row 3: All Care
    stats$Engaged[4]       <- as.vector(table_groups[2, 2])                                          # Row 4: All SaaS

  # Calculate engaged_percent
    stats$Engaged.Percent       <- round((stats$Engaged / stats$Enrolled.Current) * 100, 0)

  # And write it
    write.csv(stats, file = paste(dir, "/CustomerStats_", date.current, ".csv", sep = ""))

  return(stats)
}

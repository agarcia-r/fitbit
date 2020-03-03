#' Make Stats Table Function
#'
#' This function takes the output of the clean_merge_data function and turns it into the customer statistics table.
#' @keywords table coach customer
#' @export make_stats_table
#' @examples
#' make_stats_table()


make_stats_table <- function(dir = directory, data, date.current = date.current.report, saas = F){

  ## Prep an outcomes dataframe

  # Create it
  if(saas == T){
    stats <- as.data.frame(matrix(nrow = 3, ncol = 8), stringsAsFactors = F)
    colnames(stats) <- c("Report.Date", "Client.Type", "Group",
                         "Archived.Anytime", "Enrolled.Current", "Enrolled.YTD",
                         "Engaged", "Engaged.Percent")
  } else {
    stats <- as.data.frame(matrix(nrow = 3, ncol = 7), stringsAsFactors = F)
    colnames(stats) <- c("Report.Date", "Group",
                         "Archived.Anytime", "Enrolled.Current", "Enrolled.YTD",
                         "Engaged", "Engaged.Percent")
  }

  # Add report.date
  stats$Report.Date <- as.Date(date.current, format = "%m-%d-%Y")

  # Fill in the rows
  if(saas == T){
    stats$Group <- as.character(c("SEIU", "Humana", "Care", "SaaS"))

    stats$Client.Type <- as.factor(c(rep("Care", 3), "SaaS"))
  } else {
    stats$Group <- as.character(c("SEIU", "Humana", "All Care"))
  }

  # Calculate enrollment by program
  if(saas == T){
  # Archived
    table_groups                 <- table(data$Group.1, data$Archived)                          # Rows in Stats Table:
    stats$Archived.Anytime[1]    <- as.vector(table_groups[3, 2])                                    # Row 1: SEIU
    stats$Archived.Anytime[2]    <- as.vector(table_groups[1, 2])                                    # Row 2 Humana
    stats$Archived.Anytime[3]    <- sum(stats$Archived.Anytime[1:2])                                 # Row 3: All Care
    stats$Archived.Anytime[4]    <- as.vector(table_groups[2, 2])                                    # Row 4: All SaaS

  # Current enrollment
    table_groups                 <- table(data$Group.1, data$Enrollment.Current)                # Rows in Stats Table:
    stats$Enrolled.Current[1]    <- as.vector(table_groups[3, 2])                                    # Row 1: SEIU
    stats$Enrolled.Current[2]    <- as.vector(table_groups[1, 2])                                    # Row 2 Humana
    stats$Enrolled.Current[3]    <- sum(stats$Enrolled.Current[1:2])                                 # Row 3: All Care
    stats$Enrolled.Current[4]    <- as.vector(table_groups[2, 2])                                    # Row 4: All SaaS

  # Next enrollment YTD
    # Here there's no "False" column in table because everyone in dataset was enrolled at some point in the year
    table_groups           <- table(data$Group.1, data$Enrollment.YTD)                                 # Rows in Stats Table:
    stats$Enrolled.YTD[1]       <- as.vector(table_groups[3, 2])                                          # Row 1: SEIU
    stats$Enrolled.YTD[2]       <- as.vector(table_groups[1, 2])                                          # Row 2 Humana
    stats$Enrolled.YTD[3]       <- sum(stats$Enrolled.YTD[1:2])                                                # Row 3: All Care
    stats$Enrolled.YTD[4]       <- as.vector(table_groups[2, 2])                                          # Row 4: All SaaS

  # Calculate engagement by program - same method as above
    # By Group.1.Region first
    table_groups           <- table(data$Group.1, data$Engaged.Status)                       # Rows in Stats Table:
    stats$Engaged[1]       <- as.vector(table_groups[3, 2])                                          # Row 1: SEIU
    stats$Engaged[2]       <- as.vector(table_groups[1, 2])                                          # Row 2 Humana
    stats$Engaged[3]       <- sum(stats$Engaged.Status[1:2])                                         # Row 3: All Care
    stats$Engaged[4]       <- as.vector(table_groups[2, 2])                                          # Row 4: All SaaS
  } else {
    # Archived
    table_groups                 <- table(data$Group.1, data$Archived)                          # Rows in Stats Table:
    stats$Archived.Anytime[1]    <- as.vector(table_groups[2, 2])                                    # Row 1: SEIU
    stats$Archived.Anytime[2]    <- as.vector(table_groups[1, 2])                                    # Row 2 Humana
    stats$Archived.Anytime[3]    <- sum(stats$Archived.Anytime[1:2])                                 # Row 3: All Care

    # Current enrollment
    table_groups                 <- table(data$Group.1, data$Enrollment.Current)                # Rows in Stats Table:
    stats$Enrolled.Current[1]    <- as.vector(table_groups[2, 2])                                    # Row 1: SEIU
    stats$Enrolled.Current[2]    <- as.vector(table_groups[1, 2])                                    # Row 2 Humana
    stats$Enrolled.Current[3]    <- sum(stats$Enrolled.Current[1:2])                                 # Row 3: All Care

    # Next enrollment YTD
    # Here there's no "False" column in table because everyone in dataset was enrolled at some point in the year
    table_groups           <- table(data$Group.1, data$Enrollment.YTD)                                 # Rows in Stats Table:
    stats$Enrolled.YTD[1]       <- as.vector(table_groups[2, 1])                                          # Row 1: SEIU
    stats$Enrolled.YTD[2]       <- as.vector(table_groups[1, 1])                                          # Row 2 Humana
    stats$Enrolled.YTD[3]       <- sum(stats$Enrolled.YTD[1:2])                                                # Row 3: All Care

    # Calculate engagement by program - same method as above
    # If it's early in the month there may not be any engagement
    engage.check <- ifelse(sum(data$Engaged.Status == T) > 0, 1, 0)

    if(engage.check > 0){
      # By Group.1.Region first
      table_groups           <- table(data$Group.1, data$Engaged.Status)                       # Rows in Stats Table:
      stats$Engaged[1]       <- as.vector(table_groups[2, 2])                                          # Row 1: SEIU
      stats$Engaged[2]       <- as.vector(table_groups[1, 2])                                          # Row 2 Humana
      stats$Engaged[3]       <- sum(stats$Engaged[1:2])                                         # Row 3: All Care

      # Calculate engaged_percent
      stats$Engaged.Percent       <- round((stats$Engaged / stats$Enrolled.Current) * 100, 0)

    } else {
      stats$Engaged          <- c(0, 0, 0)
      stats$Engaged.Percent  <- c(0, 0, 0)
    }
}

  # Rearrange columns
  stats <- stats[ , c("Report.Date", "Group", "Enrolled.YTD", "Enrolled.Current", "Engaged", "Engaged.Percent", "Archived.Anytime")]

  # And write it
    write.csv(stats, file = paste(dir, "/CustomerStats_", date.current, ".csv", sep = ""))

  return(stats)
}

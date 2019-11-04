#' Make Coach Table Function
#'
#' This function takes the output of the clean_merge_data function and computes coach capacity and related statistics.
#' @keywords table coach
#' @export
#' @examples
#' make_coach_t()

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

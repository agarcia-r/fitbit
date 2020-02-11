#' Make Coach Report Function
#'
#' This function takes the output of the clean_merge_data function and produces coach reports as .csv files in the working directory.
#' @keywords coach engagement report
#' @export make_coach_report
#' @examples
#' make_coach_report()

make_coach_report <- function(data, coach, dir = getwd(), date.current = date.current.report){
  data <- data[data$Primary.Coach == coach & data$Enrollment.Current == T & data$Engaged.Status == F, ]
  data <- data[ , c("Organization.Name", "Fitbit.ID", "Enrollment.Current", "Engaged.Status", "Messages.Sent.by.Member", "Last.Message.From.Member.At", "Messages.Received.from.Coaches", "Last.Message.From.Coaches.At")]
  write.csv(data, file = paste(dir, "/Coach_", coach, "_", date.current.report, ".csv", sep = ""))
  return(data)
}

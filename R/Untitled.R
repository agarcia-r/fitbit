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

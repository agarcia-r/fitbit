################ Testing ################

# Storing these as objects outside the function for clarity

# Date 11.04
directory =                          # Enter the directory where the data for a specific date is stored
date.first.day = "11_01_2019"        # Enter the date of the first day of month of the current data with format "mm_dd/_YYY"
date.current.report = "11_04_2019"   # Enter the date of the current data with format "mm_dd_YYYY"

data_11_04 <- clean_merge_data()     # Run the clean_merge_data function

table(data_11_04$Group.Region, data_11_04$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_11_04$Group.Region, data_11_04$Enrollment.Current) # Run to see Current Enrollment
table(data_11_04$Group.Region, data_11_04$Engaged.Status)     # Run to see Engagement (resets monthly)

# Date 11.11
directory =
date.first.day = "11_01_2019"
date.current.report = "11_11_2019"

data_11_11 <- clean_merge_data()

table(data_11_11$Group.Region, data_11_11$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_11_11$Group.Region, data_11_11$Enrollment.Current) # Run to see Current Enrollment
table(data_11_11$Group.Region, data_11_11$Engaged.Status)     # Run to see Engagement (resets monthly)

# Date 11.18
directory =
date.first.day = "11_01_2019"
date.current.report = "11_18_2019"

data_11_18 <- clean_merge_data()

table(data_11_18$Group.Region, data_11_18$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_11_18$Group.Region, data_11_18$Enrollment.Current) # Run to see Current Enrollment
table(data_11_18$Group.Region, data_11_18$Engaged.Status)     # Run to see Engagement (resets monthly)


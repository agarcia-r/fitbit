################ Testing ################

# Storing these as objects outside the function for clarity
rm(list = ls())

# Date 11.04
rm(list = ls())
directory = "~/Desktop/FHS/Coaching/Data/November/2019-11-04"  # Enter the directory where the data for a specific date is stored
date.first.day = "11_01_2019"                            # Enter the date of the first day of month of the current data with format "mm_dd/_YYY"
date.current.report = "11_04_2019"                       # Enter the date of the current data with format "mm_dd_YYYY"

data_11_04 <- clean_merge_data()                         # Run the clean_merge_data function

table(data_11_04$Group.Region, data_11_04$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_11_04$Group.Region, data_11_04$Enrollment.Current) # Run to see Current Enrollment
table(data_11_04$Group.Region, data_11_04$Engaged.Status)     # Run to see Engagement (resets monthly)

# Date 11.11
rm(list = ls())
directory = "~/Desktop/FHS/Coaching/Data/November/2019-11-11"
date.first.day = "11_01_2019"
date.current.report = "11_11_2019"

data_11_11 <- clean_merge_data()

table(data_11_11$Group.Region, data_11_11$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_11_11$Group.Region, data_11_11$Enrollment.Current) # Run to see Current Enrollment
table(data_11_11$Group.Region, data_11_11$Engaged.Status)     # Run to see Engagement (resets monthly)

# Date 11.18
rm(list = ls())
directory = "~/Desktop/FHS/Coaching/Data/November/2019-11-18"
date.first.day = "11_01_2019"
date.current.report = "11_18_2019"

data_11_18 <- clean_merge_data()

table(data_11_18$Group.Region, data_11_18$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_11_18$Group.Region, data_11_18$Enrollment.Current) # Run to see Current Enrollment
table(data_11_18$Group.Region, data_11_18$Engaged.Status)     # Run to see Engagement (resets monthly)

data = data_11_18


# Date 12.01
rm(list = ls())
directory = "~/Desktop/FHS/Coaching/Data/November/Billing/2019-12-01"
date.first.day = "12_01_2019"
date.current.report = "12_18_2019"

data_12_01 <- clean_merge_data()

table(data_12_01$Group.Region, data_12_01$Enrollment.YTD)     # Run to see Enrollment YTD
table(data_12_01$Group.Region, data_12_01$Enrollment.Current) # Run to see Current Enrollment
table(data_12_01$Group.Region, data_12_01$Engaged.Status)     # Run to see Engagement (resets monthly)

table(data_12_01$Engaged.Status == T & data_12_01$Group.Region == "TX_FIT") # 44

## The code is including two people with archive.reason "opted-out-of-coaching"???
table(data_12_01$Archived.Reason[data_12_01$Engaged.Status == T & data_12_01$Group.Region == "TX_FIT"])






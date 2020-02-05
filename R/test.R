rm(list = ls())

directory = setwd("~/Desktop/FHS/Coaching/Data/2020/February/Billing/2020-02-02")
date.first.day = "02-01-2020"
date.current.report = "02-02-2020"
final.report = F

engage_feb <- clean_merge_billing()

data = engage_feb

stats_02_02 <- make_stats_table(data = engage_feb, date.current = "02-02-2020")



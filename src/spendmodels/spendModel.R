# ##############################################################################
# purpose: Gamma-Gamma model evaluation with CDNOW dataset from BTYD package
# author: Sandeep Illuri
# dependencies: data.table, BTYD, ggplot2
# created: Mar 13, 2015 14:02:15 IST
# ##############################################################################

library(ggplot2)
library(data.table)
library(BTYD)

cdnow = read.table("../../data/CDNOW_sample/CDNOW_sample.txt")
names(cdnow) = c("cid", "cidrestricted", "date", "numitems", "value_usd")

# keep only one id col, date and value columns
cdd = cdnow[,c(2,3,5)]
nowd = data.table(cdd)

# changing names
setnames(nowd, c("cidrestricted", "date", "value_usd"), c("cust", "date", "sales"))
nowd[, date := as.Date(as.character(date), format = "%Y%m%d")]

# keep only one transaction per date (take average txn value)
nowd[, sales := mean(sales, na.rm=TRUE), by = list(cust, date)]
nowd = unique(nowd)

keycols = c("cust", "date")
setkeyv(nowd, keycols)

# date - period lookup table
min_date = min(nowd$date)
max_date = max(nowd$date)

# create period lookup for each transaction date
dates = seq(min_date, max_date, by = "days")
period = as.numeric(dates - min_date)/7
lookup_numeric = unique(data.table(date=dates, period=period, key="date"))

# label transactions with corresponding periods in timeline
rows = nrow(nowd)
nowd = merge(nowd, lookup_numeric, by="date", all.x=TRUE)
sum(is.na(nowd[["period"]]))  # should be zero
nrow(nowd) == rows # should be TRUE
setkeyv(nowd, c("cust", "period"))

# calculate integer period intervals
nowd[, period.int := as.integer(period) + 1]

# separate calibration and holdout periods
end.cal.period = as.Date("1997-09-30")
nowd.hold = nowd[date > end.cal.period, ]
nowd.cal = nowd[date <= end.cal.period, ]

# consider only one transaction per period
# take mean sales value
nowd.cal[, sales := mean(sales), by = list(cust, period)]
nowd.hold[, sales := mean(sales), by = list(cust, period)]
nowd.cal = unique(nowd.cal[, list(cust, sales, period, period.int)])

# consider only repeat transactions. remove txns in first period
nowd.cal[, first_period := min(period), by = cust]
nowd.cal = nowd.cal[period != first_period, ]

# spend model only need no.of txns(x) and average txn value per period
nowd.cal[, ':=' (m.x.value = mean(sales),
                 x.vector = .N), by = cust]
nowd.cal.spend = unique(nowd.cal[, list(cust, m.x.value, x.vector)])

# remove customers who have not made any repeat txns for estimation to
# avoid warnings
nowd.cal.spend = nowd.cal.spend[x.vector > 0,]
m.x.value = nowd.cal.spend[, m.x.value]
x.vector = nowd.cal.spend[, x.vector]

# estimate model parameters
spend.params = spend.EstimateParameters(m.x.value, x.vector)


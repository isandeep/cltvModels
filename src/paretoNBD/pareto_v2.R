# ##########################################################################
# Code for data manipulation with data.table package
# ##########################################################################

cdnow = read.table("../../data/CDNOW_sample/CDNOW_sample.txt")
names(cdnow) = c("cid", "cidrestricted", "date", "numitems", "value_usd")

# keep only one id col, date and value columns
cdd = cdnow[,c(2,3)]
nowd = data.table(cdd)

# changing names
setnames(nowd, c("cidrestricted", "date"), c("cust", "date"))
nowd[, date := as.Date(as.character(date), format = "%Y%m%d")]

# keep only one transaction per date
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


setkeyv(nowd, c("cust", "date", "period"))
nowd = unique(nowd, by = c("cust", "period"))

# get first and last txn period and no. of transactions
nowd[, ':='(first_period = min(period)), by=cust]

nowd[, period.int := as.integer(period) + 1]
nowd[, first_period.int := min(period.int), by = cust]
nowd[, weekly.trans := .N, by = period.int]

# Looks like a hack to get repeat transactions
# May be a "Smart person's mirage!!"
# close match to weekly repeat txns from BTYD package
nowd[period.int == first_period.int, new.trans := .N, by = period.int]
nowd[, new.trans := ifelse(is.na(new.trans), 0, new.trans)]
nowd[, repeat.trans := weekly.trans - new.trans]
w.track.data.nowd = sapply(1:max(nowd[, period.int]),
                           function(x) min(nowd[period.int == x, repeat.trans]))

# separate calibration and holdout period
end.cal.period = as.Date("1997-09-30")
nowd.hold = nowd[date > end.cal.period, ]
nowd.cal = nowd[date <= end.cal.period, ]

# remove cutomers who didn't transact in calibration period
nowd.hold = nowd.hold[period > first_period, ]

# create t.x and T.cal variables
periods = as.numeric(max(nowd.cal$date) - min(nowd.cal$date))/7
nowd.cal[, ':=' (last_period = max(period),
                 x = .N-1), by = cust]

nowd.cal[, ':=' (t.x = last_period - first_period,
             n.cal = periods - first_period)]

# Create RF matrix from the data
cal.cbs.nowd = unique(nowd.cal[,list(cust, x, t.x, n.cal)])
cal.cbs.nowd = as.data.frame(cal.cbs.nowd[, list(x, t.x, n.cal)])
names(cal.cbs.nowd) = c("x", "t.x", "T.cal")
params.nowd <- pnbd.EstimateParameters(cal.cbs.nowd)
LL.nowd <- pnbd.cbs.LL(params.nowd, cal.cbs.nowd)

# ##########################################################################
# Model prediction and validation plots
# ##########################################################################

# No. of repeat txns by a newly acquired customer in a particular period
pnbd.Expectation(params.nowd, t=52)  # 1.5788

# for a specific customer conditional on their histrory in calibration period
# cust id = 1516
x <- cal.cbs.nowd[rownames(cal.cbs.nowd) == "1516", "x"]
t.x <- cal.cbs.nowd[rownames(cal.cbs.nowd) == "1516", "t.x"]
T.cal <- cal.cbs.nowd[rownames(cal.cbs.nowd) == "1516", "T.cal"]
pnbd.ConditionalExpectedTransactions(params.nowd, T.star = 52,
                                     x, t.x, T.cal)  # 27.30
pnbd.PAlive(params.nowd, x, t.x, T.cal) #  0.9991


# plot intime calibration 
pnbd.PlotFrequencyInCalibration(params.nowd, cal.cbs.nowd, 7)

# predicted frequency vs expected frequency in holdout period

cal.cbs.nowd[["x.star"]] = sapply(rownames(cal.cbs.nowd), function(x)
  nrow(nowd.hold[cust == as.integer(x), ]))

T.star <- 39 # length of the holdout period
censor <- 7 # This censor serves the same purpose described above
x.star <- cal.cbs.nowd[,"x.star"]
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params.nowd, T.star,
                                                    cal.cbs.nowd, x.star, censor)

# > comp
#                           freq.0      freq.1     freq.2     freq.3    freq.4
# transaction.actual      0.2367116   0.6970387   1.392523   1.560000  2.532258
# transaction.expected    0.1740102   0.8870507   1.634184   2.235849  2.976304
# bin.size             1411.0000000 439.0000000 214.000000 100.000000 62.000000
#                           freq.5    freq.6   freq.7+
#   transaction.actual    2.947368  3.862069  6.359375
# transaction.expected  3.538564  4.599028  7.249198
# bin.size             38.000000 29.000000 64.000000


T.cal <- cal.cbs.nowd[,"T.cal"]
T.tot <- 78
n.periods.final <- 78
inc.tracking <- pnbd.PlotTrackingInc(params.nowd, T.cal,
                                     T.tot, w.track.data.nowd,
                                     n.periods.final)

inc.tracking[,20:25]


cum.tracking.data <- cumsum(w.track.data.nowd)
cum.tracking <- pnbd.PlotTrackingCum(params.nowd, T.cal,
                                     T.tot, cum.tracking.data)
cum.tracking[,20:25]

# cum.pred = sapply(1:78, function(i) 2357*pnbd.Expectation(params.nowd, i))
# cum.pred1 = sapply(1:78, function(i) 2357*pnbd.Expectation(params, i))




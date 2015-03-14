##################################################################################
# purpose: building bg-bb model from BTYD package on BP txn daily data
# author: sandeep illuri
# dependencies: BTYD, data.table, ggplot2
# created: 27 Feb, 2015 11:06:15
##################################################################################


library(BTYD)
library(dplyr)
library(data.table)

# Load txn data
bp_txn = read.csv("../data/bp_txn.csv")
bp_txn = data.table(bp_txn)
bp_txn$Txn_date = as.Date(bp_txn$Txn_date, format="%d-%b-%y")

# separate calibration and holdout period
cal.period = as.Date("2014-12-01")
holdout = bp_txn[Txn_date > cal.period]
bp_txn = bp_txn[Txn_date <= cal.period]

# generate random data for testing the code
set.seed(1729)
bp_txn = expand.grid(ID = as.character(110:178),
                     Txn_date = sample(15737:16467, 500))
bp_txn$Txn_date = as.Date(bp_txn$Txn_date, origin="1970-01-01")
bp_txn = data.table(bp_txn)
bp_txn = sample_n(bp_txn, 2000, replace=TRUE)

# sort the data by ID, txn_date
keycols = c("ID", "Txn_date")
setkeyv(bp_txn, keycols)

# keep only one transaction per date
bp_txn = unique(bp_txn)

# taking time unit as month
bp_txn$month = format(bp_txn$Txn_date, "%y%m")

# yearmonth-period lookup table
min_date = min(bp_txn$Txn_date)
max_date = max(bp_txn$Txn_date)

# periodLookup = function(min_date, max_date){
#   a = min_date:max_date
#   period = as.integer(as.factor(format(as.Date(a, origin="1970-01-01"), "%y%m")))
#   month = format(as.Date(a, origin="1970-01-01"), "%y%m")
#   return(unique(data.table(month=month, period=period, key="month")))
# }

periodLookup = function(min_date, max_date){
  dates = seq(min_date, max_date, by="days")
  period = as.integer(cut(dates, breaks = "months"))
  month = format(dates, "%y%m")
  return(unique(data.table(month=month, period=period, key="month")))
}

lookupTable = periodLookup(min_date, max_date)

# merge bp_txn with lookupTable
print(nrow(bp_txn))
bp_txn = merge(bp_txn, lookupTable, by="month", all.x=TRUE)
sum(is.na(bp_txn[["period"]]))  # should be zero
nrow(bp_txn) # should not change

setkeyv(bp_txn, c("ID", "period"))
# get first and last txn period
bp_txn[, ':='(first_period = min(period),
              last_period = max(period)), by=ID]

# keep only one row per period
bplog = unique(bp_txn, by=c("ID", "period"))

# create x, t.x and T.cal variables
periods = max(bplog[,period])
bplog[, ':=' (t.x = last_period - first_period,
              n.cal = periods - first_period,
              cohort = first_period
              )
      ]
bplog[, x := length(month)-1, by=ID]


# make frequency recency matrix for each cohort
bplog[, custs := length(ID), by = list(cohort, x, t.x, n.cal)]

cohorts = unique(bplog[, cohort])
lst.rf.matrix = lapply(setNames(cohorts, cohorts), 
                        function(y) unique(bplog[cohort == y, 
                                                 list(x, t.x, n.cal, custs)]))

# estimate parameters for each cohort
params <- bgbb.EstimateParameters(lst.rf.matrix[["1"]])
LL <- bgbb.rf.matrix.LL(params, lst.rf.matrix[["1"]])

lst.params = lapply(lst.rf.matrix, 
                    function(x) bgbb.EstimateParameters(x))

lst.LL = lapply(as.character(cohorts), 
                function(x) bgbb.rf.matrix.LL(lst.params[[x]], lst.rf.matrix[[x]]))


# goodness-of-fit in calibration period

bgbb.PlotFrequencyInCalibration(params, lst.rf.matrix[["1"]])  # cohort 1

# expected transactions by each cohort

holdout[, month := format(TRAN_DATE, "%y%m")]
holdout[, x.star := .N, by=list(ACCOUNT_NO, month)]

ExpectedTxns = function(row, T.star, params) {
  pnbd.ConditionalExpectedTransactions(params, T.star,
                                       row["x"], row["t.x"], row["T.cal"])
}

# predict no. of transaction in next n txn opp. by cohort

# n = 1
pred.1 = sapply(cohorts, function(y) 
  apply(lst.rf.matrix[[y]], 1, ExpectedTxns(x, 1, params[[y]])))
# n = 2
pred.2 = sapply(cohorts, function(y) 
  apply(lst.rf.matrix[[y]], 1, ExpectedTxns(x, 2, params[[y]])))

cumPred = function(n){
  sapply(1:n, function(z) sum(sapply(cohorts, function(y) 
    apply(lst.rf.matrix[[y]], 1, ExpectedTxns(x, z, params[[y]]))))
}

# cohort 6 data

cohort6 = read.csv("../data/cohort6.csv")
cohort6 = cohort6[,-1]  # removing rownumber column

# sort by t.x and x
cohort61 = cohort6[order(-cohort6$t.x, -cohort6$x),]


params = bgbb.EstimateParameters(cohort61, c(3.2, 2, 1.2,1.5))
# [1]    1.174908    1.459720   19.346423 1000.000000

#param61
# [1]     1.174275     1.458696  1858.860269 96469.253517












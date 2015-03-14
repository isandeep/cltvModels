# ###############################################################################
# Simulation BTYD walkthrough vignettes on CDNOW dataset (1/10 sample)
# ###############################################################################

########################### Pareto NBD ##########################################
library(hypergeo)
library(BTYD)
library(ggplot2)

cdnow = read.table("../data/CDNOW_sample/CDNOW_sample.txt")
names(cdnow) = c("cid", "cidrestricted", "date", "numitems", "value_usd")
cdnow$date = as.Date(as.character(cdnow$date), format = "%Y%m%d")

# keep only one transaction per customer per day
# cdd = with(cdnow, 
#            aggregate(cdnow[,c(4,5)], list(cid=cid, cidrestricted=cidrestricted, 
#                                  date=date), sum))

# keep only one id col, date and value columns
cdd = cdnow[,c(2,3,5)]
names(cdd) = c("cust", "date", "sales")
cdd = dc.MergeTransactionsOnSameDate(cdd)


# divide dataset into calibration period and holdout period
end.cal.period = as.Date("1997-09-30")
cdd.cal = cdd[cdd$date <= end.cal.period,]

# model is concerned with only repeat customers. Remove first transactions in one
# dataset and keep information in another dataset
split.data <- dc.SplitUpElogForRepeatTrans(cdd.cal)
clean.cdd <- split.data$repeat.trans.elog

# create a customer-by-time matirx. each row represents a customer and column 
# values indicate whether he has purchased on given day
freq.cbt <- dc.CreateFreqCBT(clean.cdd);
freq.cbt[1:3,1:5]

# retrive first transactions of repeat customers
tot.cbt <- dc.CreateFreqCBT(cdd)
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)

# create customer-by-sufficient-statistic matrix, timeunit = "week"
birth.periods <- split.data$cust.data$birth.per
last.dates <- split.data$cust.data$last.date
cal.cbs.dates <- data.frame(birth.periods, last.dates,
                            end.cal.period)
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates,
                                      per="week")

# estimation of params for pareto model
params <- pnbd.EstimateParameters(cal.cbs)
LL <- pnbd.cbs.LL(params, cal.cbs)

# checking convergence by providing final params as starting pt.
params <- pnbd.EstimateParameters(cal.cbs, params);
LL <- pnbd.cbs.LL(params, cal.cbs)


############ prediction ########################

# No. of repeat txns by a newly acquired customer in a particular period
pnbd.Expectation(params, t=52)  # 1.4734

# for a specific customer conditional on their histrory in calibration period
# cust id = 1516
x <- cal.cbs["1516", "x"]
t.x <- cal.cbs["1516", "t.x"]
T.cal <- cal.cbs["1516", "T.cal"]
pnbd.ConditionalExpectedTransactions(params, T.star = 52,
                                     x, t.x, T.cal)  # 25.45
pnbd.PAlive(params, x, t.x, T.cal) #  0.9978736

############################### BG/NBD model ###################################

# Same input format as of pareto
# skipping to the parameter estimation part

bgnbd.params = bgnbd.EstimateParameters(cal.cbs)
bgnbd.LL = bgnbd.cbs.LL(params, cal.cbs)

# individual prediction
bgnbd.Expectation(bgnbd.params, t=52)  # 1.444
x <- cal.cbs["1516", "x"]
t.x <- cal.cbs["1516", "t.x"]
T.cal <- cal.cbs["1516", "T.cal"]
bgnbd.ConditionalExpectedTransactions(bgnbd.params, T.star = 52,
                                      x, t.x, T.cal) # 25.756
bgnbd.PAlive(params, x, t.x, T.cal)  # 0.9822



############################# BG/BB model #######################################

# working with donation incidence data
ddon = read.table("../data/1995_cohort_binary/1995_cohort_binary.txt", 
                  fileEncoding="Unicode",header=F)  # wide format
names(ddon) = c("cust", 1995:2006) 

# change to long format id, date, doncation indicator
ldon = reshape(ddon, varying = names(ddon)[-1], v.names="ind", 
               timevar="date", times=paste0(names(ddon)[-1], "-01-01"),
               direction="long")
rownames(ldon) = 1:nrow(ldon)
ldon = ldon[order(ldon$cust),]

# remove the rows where ind == 0
ldon = ldon[ldon$ind != 0,]
ldon$date = as.Date(ldon$date)

# calibration period 2000.
T.cal = as.Date("2001-01-01")

simData <- dc.ElogToCbsCbt(ldon, per="year", T.cal)
cal.cbs <- simData$cal$cbs

freq<- cal.cbs[,"x"]
rec <- cal.cbs[,"t.x"]
trans.opp <- 6 # transaction opportunities
cal.rf.matrix <- dc.MakeRFmatrixCal(freq, rec, trans.opp)
cal.rf.matrix

# parameter estimation
params <- bgbb.EstimateParameters(cal.rf.matrix)
LL <- bgbb.rf.matrix.LL(params, cal.rf.matrix)

# individual levels estimation
bgbb.Expectation(params, n=10)  # 3.189

# customer A
n.cal = 5
n.star = 10
x = 0
t.x = 0
bgbb.ConditionalExpectedTransactions(params, n.cal,
                                     n.star, x, t.x)
# [1] 0.1756
# customer B
x = 3
t.x = 4
bgbb.ConditionalExpectedTransactions(params, n.cal,
                                     n.star, x, t.x)
# [1] 3.33



# measuring goodness of fit in holdout sample
holdout.cbs = simData$holdout$cbs
x.star <- holdout.cbs[,"x.star"]

# making individual pred. for each customer
d = as.data.frame(cal.cbs)
d$t.x = as.integer(d$t.x)
d$T.cal = as.integer(d$T.cal)

d$pred = bgbb.ConditionalExpectedTransactions(params, 5,
                                              6, d$x, d$t.x)
d$actual = x.star
ExpvsAct = with(d, aggregate(d[,c(4,5)], list(x=x, t.x=t.x), sum))
ggplot(ExpvsAct, aes(x=actual, y=pred))+geom_line() + theme_bw()


# time series plot of purchasing behaviour
inc.track.data <- donationsSummary$annual.trans
n.cal <- 6
xtickmarks <- 1996:2006
inc.tracking <- bgbb.PlotTrackingInc(params, cal.rf.matrix,
                                     inc.track.data,
                                     xticklab = xtickmarks)
rownames(inc.tracking) <- c("act", "exp")
inc.tracking


cum.track.data <- cumsum(inc.track.data)
cum.tracking <- bgbb.PlotTrackingCum(params, rf.matrix, cum.track.data,
                                     xticklab = xtickmarks)

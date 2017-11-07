####### Fraud detection case study - unsupervised methods

library(ggplot2)
library(grid)
library(dplyr)
library(ggplot2)
library(forcats)
library(performanceEstimation)
data(sales, package="DMwR2")


############################################################################################
###------------------------------- Data exploration--------------------------------------###
############################################################################################

sales
str(sales)
summary(sales)

## How many salespeople are there? How many different products?
nlevels(sales$ID)
nlevels(sales$Prod)

## How many transactions where both quantity and value are unknown?
filter(sales,is.na(Quant),is.na(Val))

## How many transactions were inspected?
table(sales$Insp)/nrow(sales) * 100

## Look at distribution of transactions per salesman and per product
ggplot(group_by(sales,ID) %>% summarize(nTrans=n()),aes(x=ID,y=nTrans)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
    xlab("Salesmen") + ylab("Nr. of Transactions") +
    ggtitle("Nr. of Transactions per Salesman")
ggplot(group_by(sales,Prod) %>% summarize(nTrans=n()),aes(x=Prod,y=nTrans)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
    xlab("Product") + ylab("Nr. of Transactions") +
    ggtitle("Nr. of Transactions per Product")


## Continue working with unit price
sales <- mutate(sales,Uprice=Val/Quant)
summary(sales$Uprice)

## How's unit price distributed? 
# 1 - which are the most expensive/cheapest products?
prods <- group_by(sales,Prod)
mpProds <- summarize(prods,medianPrice=median(Uprice,na.rm=TRUE))
bind_cols(mpProds %>% arrange(medianPrice) %>% slice(1:5),
          mpProds %>% arrange(desc(medianPrice)) %>% slice(1:5))
# boxplots for log unit price of cheapest and most expensive product
ggplot(filter(sales,Prod %in% c("p3689","p560")),aes(x=fct_drop(Prod),y=Uprice)) +
    geom_boxplot() + scale_y_log10() + 
    xlab("") + ylab("log10(UnitPrice)")


## Which salespeople bring most/least money to the company?
ids <- group_by(sales,ID)
tvIDs <- summarize(ids,totalVal=sum(Val,na.rm=TRUE))
bind_cols(tvIDs %>% arrange(totalVal) %>% slice(1:5),
          tvIDs %>% arrange(desc(totalVal)) %>% slice(1:5))
# how much money is due to top 100 salespeople
arrange(tvIDs,desc(totalVal)) %>% slice(1:100) %>% 
    summarize(t100=sum(totalVal)) / 
    (summarize(tvIDs,sum(totalVal))) * 100
# and how much to bottom 2000
arrange(tvIDs,totalVal) %>% slice(1:2000) %>% 
    summarize(b2000=sum(totalVal)) / 
    (summarize(tvIDs,sum(totalVal))) * 100

## Which products are sold most?
## Enormous variations in quantity!
prods <- group_by(sales,Prod)
qtProds <- summarize(prods,totalQty=sum(Quant,na.rm=TRUE))
bind_cols(qtProds %>% arrange(desc(totalQty)) %>% slice(1:5),
          qtProds %>% arrange(totalQty) %>% slice(1:5))

## quantity due to top 100 products
arrange(qtProds,desc(totalQty)) %>% slice(1:100) %>% 
    summarize(t100=sum(as.numeric(totalQty))) / 
    (summarize(qtProds,sum(as.numeric(totalQty)))) * 100
## quantity due to bottom 4000 products
arrange(qtProds,totalQty) %>% slice(1:4000) %>% 
    summarize(b4000=sum(as.numeric(totalQty))) / 
    (summarize(qtProds,sum(as.numeric(totalQty)))) * 100

## a preliminary look at outliers
# the box plot rule
?boxplot.stats
# $stats =  vector of: extreme of the lower whisker, the lower ‘hinge’, the median, the upper ‘hinge’ and the extreme of the upper whisker.
# $out: outliers (the values of any data points which lie beyond the extremes of the whiskers)
# boxplot.stats example
x <- c(1:100, 1000)
(b1 <- boxplot.stats(x))

nouts <- function(x) length(boxplot.stats(x)$out)
noutsProds <- summarise(prods,nOut=nouts(Uprice))
# outliers per product
arrange(noutsProds,desc(nOut))

# outliers overall
summarize(noutsProds,totalOuts=sum(nOut))
summarize(noutsProds,totalOuts=sum(nOut))/nrow(sales)*100 # percent


############################################################################################
###------------------------------- Data problems-----------------------------------------###
############################################################################################

# How should we handle problems with the data?
# missing values for both quantity and val
# missing values for quantity or val individually
# some products have very few transactions

## How should we deal with transactions that have both quantity and value missing?
# Easiest would be to remove them all
# Would removing all of them remove the majority of transactions for some salesman?
prop.naQandV <- function(q,v) 100*sum(is.na(q) & is.na(v))/length(q)
summarise(ids,nProbs=prop.naQandV(Quant,Val)) %>% arrange(desc(nProbs))
# highest percentage is 13% for somebody, then less...

# Would removing all of them remove the majority of transactions for some product?
summarise(prods,nProbs=prop.naQandV(Quant,Val)) %>% arrange(desc(nProbs))
# 4 products get more than 20-40% of their transactions removed!
# However, filling in the missing information based on just the remaining 60% would be extremely imprudent

# thus, we remove them all
sales <- filter(sales,!(is.na(Quant) & is.na(Val)))

## missing quantities per product, in percent
prop.nas <- function(x) 100*sum(is.na(x))/length(x)
summarise(prods,propNA.Q=prop.nas(Quant)) %>% arrange(desc(propNA.Q))

# 100% NA
filter(sales, Prod %in% c("p2442","p2443")) %>% 
    group_by(Insp) %>% count()
sales <- droplevels(filter(sales,!(Prod %in% c("p2442", "p2443"))))

# are there salespeople who have not filled in quantity on all their reports?
summarise(ids,propNA.Q=prop.nas(Quant)) %>% arrange(desc(propNA.Q))

## same for val: missing val per product, in percent
summarise(prods,propNA.V=prop.nas(Val)) %>% arrange(desc(propNA.V))
# salespeople with val all missing?
summarise(ids,propNA.V=prop.nas(Val)) %>% arrange(desc(propNA.V))

## fill in all remaining missing values now
# rationale: all transactions of the same product should have the same unit price
# "typical unit price" per product
tPrice <- filter(sales, Insp != "fraud") %>% 
          group_by(Prod) %>% 
          summarise(medianPrice = median(Uprice,na.rm=TRUE))
tPrice

noQuantMedPrices <- filter(sales, is.na(Quant)) %>% 
    inner_join(tPrice) %>% 
    select(medianPrice)
noQuantMedPrices
noValMedPrices <- filter(sales, is.na(Val)) %>% 
    inner_join(tPrice) %>% 
    select(medianPrice)

noQuant <- which(is.na(sales$Quant))
noVal <- which(is.na(sales$Val))
# now fill in missing part
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /noQuantMedPrices)
sales[noVal,'Val'] <- sales[noVal,'Quant'] * noValMedPrices

## now fill in missing unit prices
sales$Uprice <- sales$Val/sales$Quant

### some products have very few transactions: can we perhaps analyze some products together?
# we would like to merge products with similar distributions

# basic precondition for dists to be similar: similar median, similar IQR

##
ms <- filter(sales,Insp != "fraud") %>% 
    group_by(Prod) %>% 
    summarize(median=median(Uprice,na.rm=TRUE),
              iqr=IQR(Uprice,na.rm=TRUE),
              nTrans=n(),
              fewTrans=ifelse(nTrans>20,FALSE,TRUE))
ms

##
ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) + 
    geom_point() + 
    xlab("Median") + ylab("IQR")
ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) + 
    geom_point() + 
    scale_y_log10() + scale_x_log10() + 
    xlab("log(Median)") + ylab("log(IQR)")

## create a similarity matric for possible later use
# Kolmogorov-Smirnov Test for similarity of distributions
# test statistic is max distance between the two cumulative distribution functions
# null hypothesis: distributions are equal
ms <- mutate(ms,smedian=scale(median),siqr=scale(iqr))
smalls <- which(ms$fewTrans)
nsmalls <- as.character(ms$Prod[smalls])
similar <- matrix(NA,length(smalls),7,
    dimnames=list(nsmalls,
      c("RowSimProd", "ks.stat", "ks.p", "medP", "iqrP", "medS","iqrS")))
xprods <- tapply(sales$Uprice, sales$Prod, list)
for(i in seq_along(smalls)) {
    d <- scale(ms[,c("smedian","siqr")],
               c(ms$smedian[smalls[i]],ms$siqr[smalls[i]]),
               FALSE)
    d <- sqrt(drop(d^2 %*% rep(1, ncol(d))))
    stat <- ks.test(xprods[[nsmalls[i]]], xprods[[order(d)[2]]])
    similar[i, ] <- c(order(d)[2], stat$statistic, stat$p.value,
                      ms$median[smalls[i]],ms$iqr[smalls[i]],
                      ms$median[order(d)[2]],ms$iqr[order(d)[2]])
}

##
head(similar)

## how we would find most similar product 
bind_rows(filter(ms,Prod==rownames(similar)[1]),
          ms[similar[1,1],])

## how many products have a similar one that is significantly similar with 90% confidence
nrow(similar[similar[, "ks.p"] >= 0.9, ])
sum(similar[, "ks.p"] >= 0.9) # alternative

# Conclusion: we haven't found many similar products for our current purpose (but this information may be useful otherwise)


############################################################################################
###------------------------------- Evaluation Criteria-----------------------------------###
############################################################################################


# Our goal: Identify suspicious transactions
# The context: There are limited resources to verify transactions. We need to employ these resources as well as possible.
# What should be our criterium? Accuracy? Precision? Recall?
# We also need to take into account class imbalance and thus, should investigate different thresholds (using precision-recall curves or ROC curves)
# For the latter purpose, we will make use of: 
  # precision-recall curves
  # lift charts
  # cumulative recall charts

# Inspection effort example
ground_truth <- c("ok", "ok", "fraud", "unk", "fraud", "fraud", "unk")
fraud_probs <- c(.2, .1, .7, .5, .4, .3, .25)
# thus we should inspect reports in this order:
ranked_obs <- c("fraud", "unk", "fraud", "fraud", "unk", "ok", "ok")

# if we could inspect just 2 reports, this would be equivalent to having predicted
preds_effort_budget_2 <- c("fraud", "fraud", "ok", "ok", "ok", "ok", "ok")
ranked_obs_unk_as_ok <-  ranked_obs
ranked_obs_unk_as_ok[ranked_obs=="unk"] <- "ok"
# confusion matrix would then be
table(ranked_obs_unk_as_ok, preds_effort_budget_2)

library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
# precision - recall curve
perf <- performance(pred, "prec", "rec")
plot(perf)

## a smoothed version
PRcurve <- function(preds, trues, ...) {
    require(ROCR, quietly = TRUE)
    pd <- prediction(preds, trues)
    pf <- performance(pd, "prec", "rec")
    pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x))))
    plot(pf, ...)
}
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

## lift chart
# y axis: lift = recall/RPP
# x axis: rate of positive predictions (RPP) = rate of transactions selected for inspection
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift Chart")

## cumulative recall chart
# y axis: recall
# x axis: RPP
CRchart <- function(preds, trues, ...) {
    require(ROCR, quietly = T)
    pd <- prediction(preds, trues)
    pf <- performance(pd, "rec", "rpp")
    plot(pf, ...)
}
CRchart(ROCR.simple$predictions, ROCR.simple$labels, 
        main='Cumulative Recall Chart')


# Another question is: How can we rank outlier detections for different products?
# We need a normalized, thus comparable, version of unit price:
# NDTP = |unit price - typical unit price of product| / IQR of product
avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats)) 
      stop('Provide either the training data or the product stats')
  if (missing(stats)) {
      stats <- as.matrix(filter(train,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
      rownames(stats) <- levels(train$Prod)
      stats[which(stats[,'iqr']==0),'iqr'] <- stats[which(stats[,'iqr']==0),'median']
  }
  
  return(mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
                 stats[toInsp$Prod,'iqr']))
}


############################################################################################
###------------------------------- Experimental Methodology------------------------------###
############################################################################################

# 70-30 train-test division

#### sub-section:  Experimental Methodology

## custom evaluation procedure yields precision, recall, and NDTP
evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds,...) 
{
   ordTS <- testSet[rankOrder,]
   N <- nrow(testSet)
   nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
   cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
   prec <- cm['fraud','fraud']/sum(cm['fraud',])
   rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
   AVGndtp <- avgNDTP(ordTS[1:nF,],stats=statsProds)
   return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}


############################################################################################
###------------------------------- Methods ----------------------------------------------###
############################################################################################


####### Modified boxplot rule ######

# this uses just unit price alone!

BPrule.wf <- function(form,train,test,...) {
    require(dplyr, quietly=TRUE)
    ms <- as.matrix(filter(train,Insp != 'fraud') %>%
                    group_by(Prod) %>%
                    summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                    select(median,iqr))
    rownames(ms) <- levels(train$Prod)
    ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
    ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
               ms[test$Prod,'iqr']
    rankOrder <- order(ORscore,decreasing=TRUE)
    res <- list(testSet=test,rankOrder=rankOrder,
                probs=matrix(c(ORscore,ifelse(test$Insp=='fraud',1,0)),
                             ncol=2))
    res
}

# pre-calculate medians and iqrs
globalStats <- as.matrix(filter(sales,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
rownames(globalStats) <- levels(sales$Prod)
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
    globalStats[which(globalStats[,'iqr']==0),'median']
head(globalStats,3)

bp.res <- performanceEstimation(
    PredTask(Insp ~ ., sales),
    Workflow("BPrule.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                   evaluator="evalOutlierRanking",   # the custom evaluation function from above
                   # precision/recall will be calculated for an inspection limit effort of 10% of the test set
                   evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

summary(bp.res)
# how to read this?
# 52% of known frauds are included in top 10% reports by the boxplot rule: this is not good
# is 10% effort not enough? --> no: precision is very low
# however avg NDTP is quite high

## now check PR curves and cumulative recall curves for efficacy of different inspection efforts
# rate of positive predictions == measure of how many tx to inspect
ps.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,1])
ts.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",avg="vertical")
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',avg='vertical')
# conclusion: for 80% recall we would need to inspect 30% of the reports!

############################################################################################
###-------------------------------Local Outlier Factor-----------------------------------###
############################################################################################


# How to handle categorical data? LOF handles numerical data only
# salesman id will be eliminated
# products will be handled one-by-one
# 
LOF.wf <- function(form, train, test, k, ...) {
    require(DMwR2, quietly=TRUE)
    ntr <- nrow(train)
    all <- as.data.frame(rbind(train,test))
    N <- nrow(all)
    ups <- split(all$Uprice,all$Prod)
    r <- list(length=ups)
    for(u in seq(along=ups)) 
        r[[u]] <- if (NROW(ups[[u]]) > 3) 
                      lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
                  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
                  else NULL
    all$lof <- vector(length=N)
    split(all$lof,all$Prod) <- r
    all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
        SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
    
    res <- list(testSet=test,
                rankOrder=order(all[(ntr+1):N,'lof'],decreasing=TRUE),
                probs=as.matrix(cbind(all[(ntr+1):N,'lof'],
                                      ifelse(test$Insp=='fraud',1,0))))
    res
}

##
lof.res <- performanceEstimation(
    PredTask(Insp ~ . , sales),
    Workflow("LOF.wf", k=7),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
    )

##
summary(lof.res)

##
ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])
ts.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))


############################################################################################
###-------------------------------Hierarchical agglomerative clustering-------------------###
############################################################################################


##
ORh.wf <- function(form, train, test, ...) {
    require(DMwR2, quietly=TRUE)
    ntr <- nrow(train)
    all <- as.data.frame(rbind(train,test))
    N <- nrow(all)
    ups <- split(all$Uprice,all$Prod)
    r <- list(length=ups)
    for(u in seq(along=ups)) 
        r[[u]] <- if (NROW(ups[[u]]) > 3) 
                      outliers.ranking(ups[[u]])$prob.outliers
                  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
                  else NULL
    all$orh <- vector(length=N)
    split(all$orh,all$Prod) <- r
    all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))] <- 
        SoftMax(all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))])
    res <- list(testSet=test,
                rankOrder=order(all[(ntr+1):N,'orh'],decreasing=TRUE),
                probs=as.matrix(cbind(all[(ntr+1):N,'orh'],
                                      ifelse(test$Insp=='fraud',1,0))))
    res
    
}

##
orh.res <- performanceEstimation(
    PredTask(Insp ~ . , sales),
    Workflow("ORh.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
    )

##
summary(orh.res)

##
ps.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,1])
ts.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1,col='grey', avg='vertical')
legend('topright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,col='grey',avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))


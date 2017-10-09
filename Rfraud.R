####### Fraud detection case study

library(ggplot2)
library(grid)
library(dplyr)
library(ggplot2)
library(forcats)
data(sales, package="DMwR2")

####### Data exploration

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


#### How should we handle problems with the data?
# missing values for both quantity and val
# missing values for quantity

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

##
tPrice <- filter(sales, Insp != "fraud") %>% 
          group_by(Prod) %>% 
          summarise(medianPrice = median(Uprice,na.rm=TRUE))

##
noQuantMedPrices <- filter(sales, is.na(Quant)) %>% 
    inner_join(tPrice) %>% 
    select(medianPrice)
noValMedPrices <- filter(sales, is.na(Val)) %>% 
    inner_join(tPrice) %>% 
    select(medianPrice)

noQuant <- which(is.na(sales$Quant))
noVal <- which(is.na(sales$Val))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /noQuantMedPrices)
sales[noVal,'Val'] <- sales[noVal,'Quant'] * noValMedPrices

##
sales$Uprice <- sales$Val/sales$Quant

##
save(sales, file = "salesClean.Rdata")

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

##
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

##
bind_rows(filter(ms,Prod==rownames(similar)[1]),
          ms[similar[1,1],])

##
nrow(similar[similar[, "ks.p"] >= 0.9, ])

##
sum(similar[, "ks.p"] >= 0.9)

##
save(similar, file = "similarProducts.Rdata")

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.7\\textwidth"),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE,out.width="0.9\\textwidth"),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)


####### Section:  Defining the Data Mining Tasks


#### sub-section:  Different Approaches to the Problem


#### sub-section:  Evaluation Criteria

##
library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "prec", "rec")
plot(perf)

##
PRcurve <- function(preds, trues, ...) {
    require(ROCR, quietly = TRUE)
    pd <- prediction(preds, trues)
    pf <- performance(pd, "prec", "rec")
    pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x))))
    plot(pf, ...)
}

##
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

##
library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "prec", "rec")
par( mfrow=c(1,2) )
plot(perf)
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)
par( mfrow=c(1,1) )

##
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift Chart")

##
CRchart <- function(preds, trues, ...) {
    require(ROCR, quietly = T)
    pd <- prediction(preds, trues)
    pf <- performance(pd, "rec", "rpp")
    plot(pf, ...)
}

##
CRchart(ROCR.simple$predictions, ROCR.simple$labels, 
        main='Cumulative Recall Chart')

##
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


#### sub-section:  Experimental Methodology

##
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


####### Section:  Obtaining Outlier Rankings


#### sub-section:  Unsupervised Approaches


#### sub-section:  Supervised Approaches


#### sub-section:  Semi-Supervised Approaches

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(dplyr)
load( "salesClean.Rdata" ) 
source( "evaluationCode.R" ) 

##
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

##
library(dplyr)
globalStats <- as.matrix(filter(sales,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
rownames(globalStats) <- levels(sales$Prod)
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
    globalStats[which(globalStats[,'iqr']==0),'median']
head(globalStats,3)

##
library(performanceEstimation)
bp.res <- performanceEstimation(
    PredTask(Insp ~ ., sales),
    Workflow("BPrule.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

##
summary(bp.res)

##
ps.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,1])
ts.bp <- sapply(getIterationsInfo(bp.res),function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",avg="vertical")
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',avg='vertical')

##
par(mfrow=c(1,2)) 
ps.bp <- sapply(getIterationsInfo(bp.res), function(i) i$probs[,1])
ts.bp <- sapply(getIterationsInfo(bp.res), function(i) i$probs[,2])
PRcurve(ps.bp, ts.bp, main="PR curve", avg="vertical")
CRchart(ps.bp, ts.bp, main='Cumulative Recall curve', avg='vertical')

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(DMwR2)
library(dplyr)
library(performanceEstimation)
load( "salesClean.Rdata" ) 
source( "evaluationCode.R" ) 
load("BPres.Rdata")

##
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

##
par(mfrow=c(1,2)) 
ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])
ts.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,2])
PRcurve(ps.bp, ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof, ts.lof,add=T,lty=2,avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(DMwR2)
library(dplyr)
library(performanceEstimation)
load( "salesClean.Rdata" ) 
source( "evaluationCode.R" ) 
load("BPres.Rdata")
load("LOFres.Rdata")

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

##
par(mfrow=c(1,2)) 
ps.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,1])
ts.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
PRcurve(ps.orh,ts.orh,add=T,lty=1,col='grey', avg='vertical')
legend('topright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
CRchart(ps.orh,ts.orh,add=T,lty=1,col='grey',avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(DMwR2)

##
library(UBL)
data(iris)
data <- iris[, c(1, 2, 5)]
data$Species <- factor(ifelse(data$Species == "setosa", "rare","common"))
table(data$Species)
newData <- SmoteClassif(Species ~ ., data, C.perc = "balance")
table(newData$Species)
newData2 <- SmoteClassif(Species ~ ., data, C.perc = list(common = 1,rare = 6))
table(newData2$Species)

##
library(ggplot2)
ggplot(data,aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
    geom_point() + ggtitle("Original Data")
ggplot(newData2,aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
    geom_point() + ggtitle("SMOTE'd Data")

##
library(grid)
g1 <- ggplot(data, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
    geom_point() + ggtitle("Original Data")
g2 <- ggplot(newData2, aes(x=Sepal.Length,y=Sepal.Width,color=Species)) +
    geom_point() + ggtitle("SMOTE'd Data")
grid.newpage()
pushViewport(viewport(layout= grid.layout(1,2)))
print(g1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(g2,vp=viewport(layout.pos.row=1,layout.pos.col=2))

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(DMwR2)
library(dplyr)
library(UBL)
library(performanceEstimation)
load( "salesClean.Rdata" ) 
source( "evaluationCode.R" ) 
load("BPres.Rdata")
load("LOFres.Rdata")
load("ORHres.Rdata")

##
NB.wf <- function(form,train,test,...) {
    require(e1071,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
    data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    model <- naiveBayes(Insp ~ .,data, ...)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')], type='raw')
    rankOrder <- order(preds[,'fraud'], decreasing=TRUE)
    rankScore <- preds[,'fraud']
    res <- list(testSet=test,
                rankOrder=rankOrder,
                probs=as.matrix(cbind(rankScore,
                                ifelse(test$Insp=='fraud',1,0))))
    res
}

##
nb.res <- performanceEstimation(
    PredTask(Insp ~ . , sales),
    Workflow("NB.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1,
                                       statsProds=globalStats))
    )

##
summary(nb.res)

##
ps.nb <- sapply(getIterationsInfo(nb.res), function(i) i$probs[,1])
ts.nb <- sapply(getIterationsInfo(nb.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))

##
par(mfrow=c(1,2)) 
ps.nb <- sapply(getIterationsInfo(nb.res), function(i) i$probs[,1])
ts.nb <- sapply(getIterationsInfo(nb.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.orh,ts.orh,add=T,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=T,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))

##
NBsm.wf <- function(form,train,test,C.perc="balance",dist="HEOM",...) {
    require(e1071,quietly=TRUE)
    require(UBL,quietly=TRUE)

    sup <- which(train$Insp != 'unkn')
    data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    newData <- SmoteClassif(Insp ~ .,data,C.perc=C.perc,dist=dist,...)
    model <- naiveBayes(Insp ~ .,newData)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
    rankOrder <- order(preds[,'fraud'],decreasing=T)
    rankScore <- preds[,'fraud']
    
    res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
    res
}

##
nbs.res <- performanceEstimation(
    PredTask(Insp ~ ., sales),
    Workflow("NBsm.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1,
                                       statsProds=globalStats))
    )

##
summary(nbs.res) 

##
ps.nbs <- sapply(getIterationsInfo(nbs.res), function(i) i$probs[,1])
ts.nbs <- sapply(getIterationsInfo(nbs.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=2, avg='vertical')
PRcurve(ps.nbs,ts.nbs,add=TRUE,lty=1, col='grey',avg='vertical')
legend('topright',c('NaiveBayes','ORh','smoteNaiveBayes'),lty=c(1,2,1),
       col=c('black','black','grey'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=2,avg='vertical')
CRchart(ps.nbs,ts.nbs,add=TRUE,lty=1,col='grey',avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','smoteNaiveBayes'),lty=c(1,2,1),
       col=c('black','black','grey'))

##
par(mfrow=c(1,2)) 
ps.nbs <- sapply(getIterationsInfo(nbs.res),  function(i) i$probs[,1])
ts.nbs <- sapply(getIterationsInfo(nbs.res),  function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=T,lty=2,avg='vertical')
PRcurve(ps.nbs,ts.nbs,add=T,lty=1,col='grey',avg='vertical')
legend('topright',c('NaiveBayes','ORh','smoteNaiveBayes'),
       lty=c(1,2,1),col=c('black','black','grey'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=T,lty=2,avg='vertical')
CRchart(ps.nbs,ts.nbs,add=T,lty=1,col='grey',avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','smoteNaiveBayes'),
       lty=c(1,2,1),col=c('black','black','grey'))

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(DMwR2)
library(dplyr)
library(performanceEstimation)
load( "salesClean.Rdata" ) 
source( "evaluationCode.R" ) 
load("BPres.Rdata")
load("LOFres.Rdata")
load("ORHres.Rdata")
load("NBres.Rdata")

##
library(RWeka)
WOW(AdaBoostM1)

##
data(iris)
idx <- sample(150,100)
model <- AdaBoostM1(Species ~ .,iris[idx,], control=Weka_control(I=100))
preds <- predict(model,iris[-idx,])
head(preds)
table(preds,iris[-idx,'Species'])

##
ab.wf <- function(form,train,test,ntrees=100,...) {
    require(RWeka,quietly=TRUE)
    sup <- which(train$Insp != 'unkn')
    data <- as.data.frame(train[sup,c('ID','Prod','Uprice','Insp')])
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    model <- AdaBoostM1(Insp ~ .,data,
                       control=Weka_control(I=ntrees))
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                     type='probability')
    rankOrder <- order(preds[,"fraud"],decreasing=TRUE)
    rankScore <- preds[,"fraud"]
    
    res <- list(testSet=test,
                rankOrder=rankOrder,
                probs=as.matrix(cbind(rankScore,
                                      ifelse(test$Insp=='fraud',1,0))))
    res
}

##
ab.res <- performanceEstimation(
    PredTask(Insp ~ .,sales),
    Workflow("ab.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1,
                                       statsProds=globalStats))
    )

##
summary(ab.res)

##
ps.ab <- sapply(getIterationsInfo(ab.res), function(i) i$probs[,1])
ts.ab <- sapply(getIterationsInfo(ab.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1, color='grey', avg='vertical')
PRcurve(ps.ab,ts.ab,add=TRUE,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.ab,ts.ab,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','grey'))

##
par(mfrow=c(1,2)) 
ps.ab <- sapply(getIterationsInfo(ab.res), function(i) i$probs[,1])
ts.ab <- sapply(getIterationsInfo(ab.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.orh,ts.orh,add=T,lty=1,color='grey',avg='vertical')
PRcurve(ps.ab,ts.ab,add=T,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=T,lty=1,color='grey',avg='vertical')
CRchart(ps.ab,ts.ab,add=T,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','grey'))

##
opts_template$set(onlyShow=list(echo=TRUE, eval=FALSE,  tidy=FALSE),
                  onlyRun=list(echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig=list(fig.width=6,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  showFig2=list(fig.width=12,fig.height=6,echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE),
                  runShow=list(echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, tidy=FALSE))
library(ggplot2)
library(DMwR2)
library(dplyr)
library(performanceEstimation)
load( "salesClean.Rdata" ) 
source( "evaluationCode.R" ) 
load("BPres.Rdata")
load("LOFres.Rdata")
load("ORHres.Rdata")
load("NBres.Rdata")
load("ABres.Rdata")

##
library(DMwR2)
library(e1071)
data(iris) 
set.seed(1234)
idx <- sample(150, 100)
tr <- iris[idx, ]
ts <- iris[-idx, ]
nb <- naiveBayes(Species ~ ., tr)
table(predict(nb, ts), ts$Species)
trST <- tr
nas <- sample(100, 90)
trST[nas, "Species"] <- NA
func <- function(m, d) {
    p <- predict(m, d, type = "raw")
    data.frame(cl = colnames(p)[ apply(p, 1, which.max) ], 
               p = apply(p, 1, max))
}
nbSTbase <- naiveBayes(Species ~ ., trST[-nas, ])
table(predict(nbSTbase, ts), ts$Species)
nbST <- SelfTrain(Species ~ ., trST, 
                  learner="naiveBayes", learner.pars=list(),
                  pred="func")
table(predict(nbST, ts), ts$Species)

##
pred.nb <- function(m,d) {
    p <- predict(m,d,type='raw')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],
               p=apply(p,1,max)
               )
}
 
nb.st.wf <- function(form,train,test,...) {
    require(e1071,quietly=TRUE)
    require(DMwR2, quietly=TRUE)
    train <- as.data.frame(train[,c('ID','Prod','Uprice','Insp')])
    train[which(train$Insp == 'unkn'),'Insp'] <- NA
    train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
    model <- SelfTrain(form,train,
                       learner='naiveBayes', learner.pars=list(),
                       pred='pred.nb')
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                     type='raw')

    rankOrder <- order(preds[,'fraud'],decreasing=TRUE)
    rankScore <- preds[,"fraud"]
    
    res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
    res
}

##
nb.st.res <- performanceEstimation(
    PredTask(Insp ~ .,sales),
    Workflow("nb.st.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1,
                                       statsProds=globalStats))
    )

##
summary(nb.st.res)

##
ps.nb.st <- sapply(getIterationsInfo(nb.st.res), function(i) i$probs[,1])
ts.nb.st <- sapply(getIterationsInfo(nb.st.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1, color='grey', avg='vertical')
PRcurve(ps.nb.st,ts.nb.st,add=TRUE,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.nb.st,ts.nb.st,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','grey'))

##
par(mfrow=c(1,2)) 
ps.nb.st <- sapply(getIterationsInfo(nb.st.res), function(i) i$probs[,1])
ts.nb.st <- sapply(getIterationsInfo(nb.st.res), function(i) i$probs[,2])
PRcurve(ps.nb,ts.nb,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.orh,ts.orh,add=T,lty=1, color='grey', avg='vertical')
PRcurve(ps.nb.st,ts.nb.st,add=T,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.nb,ts.nb,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.orh,ts.orh,add=T,lty=1,color='grey',avg='vertical')
CRchart(ps.nb.st,ts.nb.st,add=T,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','grey'))

##
pred.ada <- function(m,d) {
    p <- predict(m,d,type='probability')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],
               p=apply(p,1,max)
               )
}
 
ab.st.wf <- function(form,train,test,ntrees=100,...) {
    require(RWeka,quietly=TRUE)
    require(DMwR2,quietly=TRUE)
    train <- as.data.frame(train[,c('ID','Prod','Uprice','Insp')])
    train[which(train$Insp == 'unkn'),'Insp'] <- NA
    train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
    model <- SelfTrain(form,train,
                       learner='AdaBoostM1',
                       learner.pars=list(control=Weka_control(I=ntrees)),
                       pred='pred.ada')
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                     type='probability')

    rankOrder <- order(preds[,'fraud'],decreasing=T)
    rankScore <- preds[,"fraud"]
    
    res <- list(testSet=test,
              rankOrder=rankOrder,
              probs=as.matrix(cbind(rankScore,
                                    ifelse(test$Insp=='fraud',1,0))))
    res
}

##
ab.st.res <- performanceEstimation(
    PredTask(Insp ~ .,sales),
    Workflow("ab.st.wf"),
    EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                   method=Holdout(nReps=3,hldSz=0.3,strat=TRUE),
                   evaluator="evalOutlierRanking",
                   evaluator.pars=list(Threshold=0.1,
                                       statsProds=globalStats))
    )

##
summary(ab.st.res)

##
ps.ab.st <- sapply(getIterationsInfo(ab.st.res), function(i) i$probs[,1])
ts.ab.st <- sapply(getIterationsInfo(ab.st.res), function(i) i$probs[,2])
PRcurve(ps.orh,ts.orh,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.ab,ts.ab,add=TRUE,lty=1, color='grey', avg='vertical')
PRcurve(ps.ab.st,ts.ab.st,add=TRUE,lty=2,avg='vertical')
legend('topright',c('ORh','AdaBoostM1','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.orh,ts.orh,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.ab,ts.ab,add=TRUE,lty=1,color='grey',avg='vertical')
CRchart(ps.ab.st,ts.ab.st,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('ORh','AdaBoostM1','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','grey'))

##
par(mfrow=c(1,2)) 
ps.ab.st <- sapply(getIterationsInfo(ab.st.res), function(i) i$probs[,1])
ts.ab.st <- sapply(getIterationsInfo(ab.st.res), function(i) i$probs[,2])
PRcurve(ps.orh,ts.orh,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1), avg="vertical")
PRcurve(ps.ab,ts.ab,add=T,lty=1, color='grey', avg='vertical')
PRcurve(ps.ab.st,ts.ab.st,add=T,lty=2,avg='vertical')
legend('topright',c('ORh','AdaBoostM1','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))

CRchart(ps.orh,ts.orh,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.ab,ts.ab,add=T,lty=1,color='grey',avg='vertical')
CRchart(ps.ab.st,ts.ab.st,add=T,lty=2,avg='vertical')
legend('bottomright',c('ORh','AdaBoostM1','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','grey'))

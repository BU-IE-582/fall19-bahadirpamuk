library(data.table)
library(dplyr)
library(tidyr)
DTbetsData <- fread(file="c:/Users/BAHADIR/Desktop/IE 582/HW 1/rCode/bets.csv", header=TRUE, sep="," , stringsAsFactors=TRUE)
DTbookingData <- fread(file="c:/Users/BAHADIR/Desktop/IE 582/HW 1/rCode/booking.csv", header=TRUE, sep=",",stringsAsFactors=TRUE)
DTgoalsData <- fread(file="c:/Users/BAHADIR/Desktop/IE 582/HW 1/rCode/goals.csv", header=TRUE, sep=",",stringsAsFactors=TRUE)
DTmatchesData <- fread(file="c:/Users/BAHADIR/Desktop/IE 582/HW 1/rCode/matches.csv", header=TRUE, sep=",",stringsAsFactors=TRUE)
DTstatsData <- fread(file="c:/Users/BAHADIR/Desktop/IE 582/HW 1/rCode/stats.csv", header=TRUE, sep=",",stringsAsFactors=TRUE)

setnames(DTmatchesData,"match_hometeam_score","goal_1")
setnames(DTmatchesData,"match_awayteam_score","goal_2")

DTgoalsData<-separate(DTgoalsData,score,c("goal_1","goal_2")," - ",convert=TRUE)
DTpremierMatches<- DTmatchesData[match_status=="Finished" & league_id == 148]

premierMatchIds <- DTpremierMatches$match_id
premierScore<-DTpremierMatches
hist(premierScore$goal_1,main = "Home Score(goals)", xlab= "Home Goals", ylab = "Number of Games" , right = FALSE)
q2ax<-0:8
q2ay <-dpois(x=q2ax,lambda=mean(premierScore$goal_1))*NROW(premierScore)
lines(q2ax,q2ay,col="blue")
hist(premierScore$goal_2,main = "Away Score(goals)", xlab= "Away Goals", ylab = "Number of Games", right = FALSE)
q2ay <-dpois(x=q2ax,lambda=mean(premierScore$goal_2))*NROW(premierScore)
lines(q2ax,q2ay,col="blue")
hist(premierScore$goal_1-premierScore$goal_2,main = "Home Score(goals)– Away Score(goals)", xlab= "Home Goals – Away Goals", ylab = "Number of Games", right = FALSE)
q2axx<-seq(min(premierScore$goal_1-premierScore$goal_2),max(premierScore$goal_1-premierScore$goal_2),length.out = 100)
q2ay <-dnorm(x=q2axx,mean(premierScore$goal_1-premierScore$goal_2),sd(premierScore$goal_1-premierScore$goal_2))*NROW(premierScore)
lines(q2axx,q2ay,col="blue")
selectedBookmakers <- c("10Bet","Unibet","bwin","1xBet")
premierBets<-DTbetsData[odd_bookmakers%in%selectedBookmakers&variable%in%c("odd_1","odd_x","odd_2")&match_id%in%premierMatchIds]
setkey(premierBets,odd_epoch,odd_bookmakers)
premierBets <- premierBets %>% spread(variable, value)

premierBets[,c("Podd_1", "Podd_2","Podd_x"):=list((1/odd_1),(1/odd_2),(1/odd_x)),]
premierBets[,c("Psum"):=(Podd_1 + Podd_2 + Podd_x),]
premierBets[,c("Pnorm1", "Pnorm2","PnormX"):=list(Podd_1/Psum,Podd_2/Psum,Podd_x/Psum),]
premierBets[,"Pdiff":=Podd_1-Podd_2,]
bins<-seq(-1,1,by=0.04)
DTpremierMatches[,"result" := ifelse(goal_1>goal_2,1,ifelse(goal_2>goal_1,2,0))]
binCounts <- cut(premierBets$Pdiff, bins, include.lowest = TRUE)
premierBets[,"bins":= binCounts]
setkey(premierBets,match_id)
setkey(DTpremierMatches,match_id)
joinTable <- premierBets[DTpremierMatches, nomatch=0]
joinTable <- joinTable[,.(match_id,odd_bookmakers,result,Pdiff,bins)]
joinTable <-joinTable[,c("total","actual" ,"prob") := list(.N, sum(result==0),(sum(result==0)/.N)),by = bins]
plot(premierBets$Pdiff,premierBets$Podd_x,main = "ALL 4 Bookmakers", xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable$Pdiff,joinTable$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[1]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[1]]$Podd_x,main = selectedBookmakers[1], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[1]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[1]]$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[2]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[2]]$Podd_x,main = selectedBookmakers[2], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[2]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[2]]$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[3]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[3]]$Podd_x,main = selectedBookmakers[3], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[3]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[3]]$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[4]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[4]]$Podd_x,main = selectedBookmakers[4], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[4]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[4]]$prob,col="blue")

DTbookingData[,"redBef15":=ifelse(as.numeric(time)<15&card == "red card" , 1 , 0)]
DTpremierMatches[,"result90" := ifelse(goal_1-match_hometeam_extra_score>goal_2-match_awayteam_extra_score,1,ifelse(goal_2-match_awayteam_extra_score>goal_1-match_hometeam_extra_score,2,0))]
DTpremierMatches[,"extraChange" := (result == 0 & result90 != 0) | (result != 0 & result90 == 0)]

setkey(DTbookingData,match_id)
setkey(DTpremierMatches,match_id)
setkey(joinTable,match_id)
bookSub <- DTbookingData[redBef15 == 1,.(match_id,redBef15)]
joinTable <- bookSub[joinTable, on="match_id"]
matchesSub <- DTpremierMatches[extraChange==1,.(match_id,extraChange)]
joinTable <- matchesSub[joinTable, on="match_id"]
joinTable[extraChange==1,.N]
joinTable[redBef15==1,.N]
joinTable[redBef15==1|extraChange==1,.N]
joinTable<-joinTable[is.na(extraChange) & is.na(redBef15)]

plot(premierBets$Pdiff,premierBets$Podd_x,main = "ALL 4 Bookmakers", xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable$Pdiff,joinTable$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[1]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[1]]$Podd_x,main = selectedBookmakers[1], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[1]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[1]]$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[2]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[2]]$Podd_x,main = selectedBookmakers[2], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[2]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[2]]$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[3]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[3]]$Podd_x,main = selectedBookmakers[3], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[3]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[3]]$prob,col="blue")

plot(premierBets[odd_bookmakers == selectedBookmakers[4]]$Pdiff,premierBets[odd_bookmakers == selectedBookmakers[4]]$Podd_x,main = selectedBookmakers[4], xlab= "P(home Win) - P(away win)", ylab = "P(tie)", ylim = c(0,0.5))
points(joinTable[odd_bookmakers == selectedBookmakers[4]]$Pdiff,joinTable[odd_bookmakers == selectedBookmakers[4]]$prob,col="blue")
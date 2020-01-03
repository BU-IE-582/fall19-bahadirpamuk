library(data.table)
library(dplyr)
library(tidyr)
library(glmnet)
library(rpart)
library("rpart.plot")

#Bahadır Pamuk - Ekin Özgürbüz
#Team BUFAIM

#Reading the data and preprocessing part.
path <- "d:/Dropbox/IE 582/Project/DATA"
DTbetsData <- fread(file=paste(path, "bets.csv", sep = "/", collapse = NULL), header=TRUE, sep="," , stringsAsFactors=TRUE)
DTbookingData <- fread(file=paste(path, "booking.csv", sep = "/", collapse = NULL), header=TRUE, sep="," , stringsAsFactors=TRUE)
DTgoalsData <- fread(file=paste(path, "goals.csv", sep = "/", collapse = NULL), header=TRUE, sep="," , stringsAsFactors=TRUE)
DTmatchesData <- fread(file=paste(path, "matches.csv", sep = "/", collapse = NULL), header=TRUE, sep="," , stringsAsFactors=TRUE)
DTstatsData <- fread(file=paste(path, "stats.csv", sep = "/", collapse = NULL), header=TRUE, sep="," , stringsAsFactors=TRUE)

setnames(DTmatchesData,"match_hometeam_score","goal_1")
setnames(DTmatchesData,"match_awayteam_score","goal_2")
DTgoalsData<-separate(DTgoalsData,score,c("goal_1","goal_2")," - ",convert=TRUE)

#Goals of a team as home or away team in a match, total goals in the match, game result
DTmatchesData <- DTmatchesData[match_status=="Finished"]
DTmatchesData <- cbind(DTmatchesData, DTmatchesData[,goal_1 + goal_2])
setnames(DTmatchesData,18,"TotalGoals")

DTmatchesData <- cbind(DTmatchesData, DTmatchesData[,match_hometeam_halftime_score + match_awayteam_halftime_score])
setnames(DTmatchesData,19,"TotalHalfGoals")

DTmatchesData <- cbind(DTmatchesData, DTmatchesData[,match_hometeam_extra_score + match_awayteam_extra_score])
setnames(DTmatchesData,20,"TotalExtraGoals")

DTmatchesData <- cbind(DTmatchesData, DTmatchesData[,match_hometeam_penalty_score + match_awayteam_penalty_score])
setnames(DTmatchesData,21,"TotalPenaltyGoals")

#In order to create information about lagged data.

TeamMatches <- DTmatchesData[,c(2,3,4,9,11,13,10,12,14,17,18,19,20)]
TeamMatches <- rbind(TeamMatches, DTmatchesData[,c(1,3,4,10,12,14,9,11,13,17,18,19,20)], use.names=FALSE)
TeamMatches <- cbind(TeamMatches,c(rep(1,count(DTmatchesData)),rep(2,count(DTmatchesData))))
setnames(TeamMatches,c(1,4,5,6,7,8,9,14),c("TeamID","Scored","HalfScored","ExtraScored","Conceded","HalfConceded","ExtraConceded","Home/Away"))
TeamMatches<- TeamMatches[order(epoch)]


#TASK 1: Getting the data for a team that played at least 5 matches.
setDT(TeamMatches)[, Index := seq_len(.N), by = TeamID] #Every match has information of order.
unique(TeamMatches[Index > 5]$match_id) #5406 games after 5th games of teams

#TASK 2: After finishing Task 1, create 3,5 and n-lagged data.
CalculateAverages <- function(DT,index,lag){
#DT comes in that format:  [scored , half scored, extra scored , conceded , half conceded , extra conceded , index]
  #print("======================") #These are control print lines, in order to see the code is working.
  
  sum <- 0
  #print(index) #Control
  for(i in (index - lag):(index - 1)){
    #print(DT[Index == i,1:6]) #Control
    sum <- sum + DT[Index == i,1:6]
  }
  #print(sum / lag) #Control
  return(sum / lag)
  
}

#Please save the data and do not run it again.
TeamMatchesGreaterThan6 <- TeamMatches[Index > 5]

for(row in 1:nrow(TeamMatchesGreaterThan6))
{
  currentTeamID <- TeamMatchesGreaterThan6[row,TeamID][1]
  #print(currentTeamID)
  currentIndex <- TeamMatchesGreaterThan6[row,Index]
  #print(currentIndex)
  TeamMatchesGreaterThan6[row,c("Scored5", "HalfScored5" , "ExtraScored5", "Conceded5", "HalfConceded5","ExtraConceded5") := CalculateAverages(TeamMatches[TeamID==currentTeamID,c(4:9,15)],currentIndex,5)]
  TeamMatchesGreaterThan6[row,c("Scored3", "HalfScored3" , "ExtraScored3", "Conceded3", "HalfConceded3","ExtraConceded3") := CalculateAverages(TeamMatches[TeamID==currentTeamID,c(4:9,15)],currentIndex,3)]
  TeamMatchesGreaterThan6[row,c("ScoredN", "HalfScoredN" , "ExtraScoredN", "ConcededN", "HalfConcededN","ExtraConcededN") := CalculateAverages(TeamMatches[TeamID==currentTeamID,c(4:9,15)],currentIndex,currentIndex-1)]
}
#Please do not run this phase, it takes so many time.


DTimportantMatchesData <- DTmatchesData[DTmatchesData$match_id %in% unique(TeamMatches[Index > 5]$match_id)] ## KODA BAK BEE KODA BAK CAY DEMLE
for(row in 1:nrow(DTimportantMatchesData))
{
  currentMatchID <- DTimportantMatchesData[row,match_id][1]
  #print(currentMatchID) #Control
  currentData <- TeamMatchesGreaterThan6[match_id == currentMatchID]
  #print(currentData) #Control
  for(row2 in 1:nrow(currentData))
  {
    #print(currentData[row2,"Home/Away"][1])
    if(currentData[row2,"Home/Away"][1] == 1)
    {
      DTimportantMatchesData[row, c("Scored5_1", "HalfScored5_1" , "ExtraScored5_1", "Conceded5_1", "HalfConceded5_1","ExtraConceded5_1",
                                    "Scored3_1", "HalfScored3_1" , "ExtraScored3_1", "Conceded3_1", "HalfConceded3_1","ExtraConceded3_1",
                                    "ScoredN_1", "HalfScoredN_1" , "ExtraScoredN_1", "ConcededN_1", "HalfConcededN_1","ExtraConcededN_1")
                             := currentData[row2,c("Scored5", "HalfScored5" , "ExtraScored5", "Conceded5", "HalfConceded5","ExtraConceded5",
                                                  "Scored3", "HalfScored3" , "ExtraScored3", "Conceded3", "HalfConceded3","ExtraConceded3",
                                                  "ScoredN", "HalfScoredN" , "ExtraScoredN", "ConcededN", "HalfConcededN","ExtraConcededN")]]
    }
    if(currentData[row2,"Home/Away"][1] == 2)
    {
      DTimportantMatchesData[row, c("Scored5_2", "HalfScored5_2" , "ExtraScored5_2", "Conceded5_2", "HalfConceded5_2","ExtraConceded5_2",
                                    "Scored3_2", "HalfScored3_2" , "ExtraScored3_2", "Conceded3_2", "HalfConceded3_2","ExtraConceded3_2",
                                    "ScoredN_2", "HalfScoredN_2" , "ExtraScoredN_2", "ConcededN_2", "HalfConcededN_2","ExtraConcededN_2")
                             := currentData[row2,c("Scored5", "HalfScored5" , "ExtraScored5", "Conceded5", "HalfConceded5","ExtraConceded5",
                                                   "Scored3", "HalfScored3" , "ExtraScored3", "Conceded3", "HalfConceded3","ExtraConceded3",
                                                   "ScoredN", "HalfScoredN" , "ExtraScoredN", "ConcededN", "HalfConcededN","ExtraConcededN")]]
    }
  }
}
#3,5,N Data are inserted into data.

#TASK 3: odd_1 odd_2 odd_x 2.5o/u and 3.5o/u values are extracted from bets data, for every match.
#setnames(DTimportantMatchesData,"odd_2.5","odd_2.5o")
#setnames(DTimportantMatchesData,"odd_3.5","odd_3.5o")
DTimportantBetsData <- DTbetsData[DTbetsData$variable %in% c("odd_1","odd_x","odd_2","o+2.5","o+3.5","u+2.5","u+3.5"),1:5]
DTimportantBetsData <- DTimportantBetsData[DTimportantBetsData$match_id %in% DTimportantMatchesData$match_id,1:5]
DTimportantMatchesData <- as.data.table(DTimportantMatchesData)
uniqueBetMatches <- unique(DTimportantBetsData$match_id)
for(row in 1:nrow(DTimportantMatchesData))
{
  currentMatchID <- DTimportantMatchesData[row,match_id][1]
  #print(currentMatchID)
  if(currentMatchID %in% uniqueBetMatches)
  {
   #print("==================")
    #print(currentMatchID)
    currentData <- DTimportantBetsData[match_id == currentMatchID,1:5]
    #print(currentData)
    #print(mean(currentData[variable=="o+2.5"][,value]))
    #print(mean(currentData[variable=="o+3.5"][,value]))
    #print(currentData[variable=="odd_1"][,value])
     
    DTimportantMatchesData[row, "odd_2.5o" := mean(currentData[variable=="o+2.5"][,value])]
    DTimportantMatchesData[row, "odd_3.5o" := mean(currentData[variable=="o+3.5"][,value])]
    DTimportantMatchesData[row, "odd_2.5u" := mean(currentData[variable=="u+2.5"][,value])]
    DTimportantMatchesData[row, "odd_3.5u" := mean(currentData[variable=="u+3.5"][,value])]
    DTimportantMatchesData[row, "odd_1" := mean(currentData[variable=="odd_1"][,value])]
    DTimportantMatchesData[row, "odd_x" := mean(currentData[variable=="odd_x"][,value])]
    DTimportantMatchesData[row, "odd_2" := mean(currentData[variable=="odd_2"][,value])]
  }
}
DTimportantMatchesData<-cbind(DTimportantMatchesData,DTimportantMatchesData$TotalGoals>2.5)
setnames(DTimportantMatchesData,65, "IsOver2.5")


DTimportantMatchesData <- cbind(DTimportantMatchesData,ifelse(DTimportantMatchesData$goal_1 - DTimportantMatchesData$goal_2>0,1,ifelse(DTimportantMatchesData$goal_1 - DTimportantMatchesData$goal_2<0,2,0)))
setnames(DTimportantMatchesData,66, "Result")

write.csv(DTimportantMatchesData,paste(path, "ImportantMatches.csv", sep = "\\", collapse = NULL), row.names = FALSE)
#Important matches are written into a .csv file in order to save and be able to use the code without rerunning the long phase.

DTimportantMatchesData <- fread(file=paste(path, "ImportantMatches.csv", sep = "/", collapse = NULL), header=TRUE, sep="," , stringsAsFactors=TRUE)
######## Creating the classes  ############
colnames(DTimportantMatchesData)
MyData <- DTimportantMatchesData[,c(3,9,10,17,18,66,22,40,25,43,28,46,31,49,34,52,37,55,58,60,59,61,62,63,64)]
MyData <- cbind(MyData,ifelse(abs(MyData$goal_1-MyData$goal_2) < 6 , MyData$goal_1-MyData$goal_2, ifelse(MyData$goal_1-MyData$goal_2 > 0, 5 , -5)))
setnames(MyData,26, "Class")
colnames(MyData)
MyData <- drop_na(MyData,Scored5_1,Scored5_2,odd_2.5o)
MyData <- na.omit(MyData)
#NA values in columns are ommitted. However, more clever technique could be used, such as k-NN imputation or M.I.C.E.

#Training ve test data are created here.
set.seed(1907)
trainingData = sample_n(MyData,round(nrow(MyData)*2/3))
testData = MyData[!rownames(MyData) %in% rownames(trainingData),]

#since these variables are dependent and cant use the end result for any of the approaches
trainingDataDropped = trainingData[,-c(2,3,5,6,26)]
testDataDropped = testData[,-c(2,3,5,6,26)]

#Looking at the histogram reveals that most matches end up within the range -5 and +5 goal difference. 
#Therefore, classes are adjusted within this range.
plot(DTimportantMatchesData$goal_1-DTimportantMatchesData$goal_2)
hist(DTimportantMatchesData$goal_1-DTimportantMatchesData$goal_2)
TF4 <- (abs(DTimportantMatchesData$goal_1-DTimportantMatchesData$goal_2) < 4)
summary(TF4)

#Training Data includes 3604 instances
#Test Data includes 1802 instances

#Lasso model is tried through the time horizon of the project, however, excluded from the final model.

#LASSO
#LassoModel = glmnet(as.matrix(trainingDataDropped),(trainingData$goal_1-trainingData$goal_2),family="gaussian",nlambda=100)
#predictionLasso = predict(LassoModel, as.matrix(testDataDropped))

#Accuracy <- rep(0,ncol(predictionLasso))

#for(i in 1:ncol(predictionLasso))
#{
#  Accuracy[i] = (sum(predictionLasso[,i] - (testData$goal_1 - testData$goal_2))/nrow(predictionLasso))
#}

#Accuracy <- Accuracy[which.min(abs(Accuracy))]]
#LASSO



#Minsplit ve CP parameter search with 10 FOLD CROSS VALIDATION
#Cross Validation is done by for loops, as mentioned in detail in project report.
minSplit <- seq(from = 5, to = 100, by = 5)
cP <- seq(from = 0.0005, to = 0.01, by = 0.0005)
RPSVector <- rep(0,400)
for(m in 1:length(minSplit))
  for(c in 1:length(cP))
  {
    SumRPS <- 0
    for(CV in 1:10){
      trainingData = sample_n(MyData,round(nrow(MyData)*2/3))
      testData = MyData[!rownames(MyData) %in% rownames(trainingData),]
      trainingData <- as.data.frame(trainingData)
      
      TreeModel = rpart(Class ~.,data=trainingData[,-c(1,2,3,4,5,6)],method="class",model=TRUE,parms = list(split = "information"), 
                        control = rpart.control(minsplit = minSplit[m], cp = cP[c]))
      #prp(TreeModel,type=1,extra=1)
      TreePrediction=predict(TreeModel,testData,type = "prob")
      Probs <- TreePrediction[,1] + TreePrediction[,2] + TreePrediction[,3] + TreePrediction[,4] + TreePrediction[,5]
      Probs <- as.data.table(Probs)
      Probs <- cbind(Probs,TreePrediction[,6])
      Probs <- cbind(Probs,TreePrediction[,7] + TreePrediction[,8] + TreePrediction[,9] + TreePrediction[,10] + TreePrediction[,11]
      )
      setnames(Probs,c(1,2,3),c("1","0","2"))
      for(i in 1:nrow(testData))
      {
        if(testData[i,Result] == 1)
        {
          SumRPS <- SumRPS + (Probs[i,1] - 1)^2 + (Probs[i,1] +Probs[i,2] - 1)^2
        }
        if(testData[i,Result] == 0)
        {
          SumRPS <- SumRPS + (Probs[i,1])^2 + (Probs[i,1] +Probs[i,2] - 1)^2
        }
        if(testData[i,Result] == 2)
        {
          SumRPS <- SumRPS + (Probs[i,1] )^2 + (Probs[i,1] +Probs[i,2])^2
        }
      }
    }
    print("===========")
    print((m-1)*length(minSplit) +c)
    print( SumRPS /10 / nrow(testData))
    RPSVector[(m-1)*length(minSplit) +c]  <- SumRPS /10 / nrow(testData)
    print(RPSVector[(m-1)*length(minSplit) +c])
  }
RPSVectorYede <- RPSVector
as.vector(RPSVector)/2
which.min(RPSVector)
RPSMatrix <- matrix(RPSVector,byrow = TRUE,nrow=20,ncol=20)
RPSVector[320]
#Minsplit ve CP parameter search with 10 FOLD CROSS VALIDATION
#write.csv(RPSMatrix,paste(path, "RPSMatrix.csv", sep = "\\", collapse = NULL), row.names = FALSE)
#minSplit =35
#cp = 0.1
#RETRAIN THE TREE WITH ALL DATA AND CHECK TEST AS PREMIER LEAGUE PREDICTED MATCHES AND CALCULATE RPS
TreeModel = rpart(Class ~.,data=MyDaya[,-c(1,2,3,4,5,6)],method="class",model=TRUE,parms = list(split = "information"), 
                  control = rpart.control(minsplit = 5, cp = 0.003,maxdepth = 4))
prp(TreeModel,type=1,extra=1)

TreePrediction=predict(TreeModel,MyData[match_id%in%c(273236,273234,273239,273231,273232,273238,273237,273240,273233,273235,273249,273242,273245,273243,273246,273247,273248,273250,273244,273241,273254,273259,273252,273258,273255,273251,273256,273257,273253,273260,273266,273263,273262,273268,273265,273269,273267,273270,273261,273264,327989,273274,273271,273276,273272,273277,273273,273275,273279,273278,273289,273283,273282,273284,273288,273285,273281,273287,273286,273290,273292,273296,273298,273299,273300,273297,273293,273291,273294,273295,273302,273303,273306,273308,273309,273305,273310,273307,273301,273304)],type = "prob")
Probs <- TreePrediction[,1] + TreePrediction[,2] + TreePrediction[,3] + TreePrediction[,4] + TreePrediction[,5]
Probs <- as.data.table(Probs)
Probs <- cbind(Probs,TreePrediction[,6])
Probs <- cbind(Probs,TreePrediction[,7] + TreePrediction[,8] + TreePrediction[,9] + TreePrediction[,10] + TreePrediction[,11])
Probs <- cbind(Probs,MyData[match_id%in%c(273236,273234,273239,273231,273232,273238,273237,273240,273233,273235,273249,273242,273245,273243,273246,273247,273248,273250,273244,273241,273254,273259,273252,273258,273255,273251,273256,273257,273253,273260,273266,273263,273262,273268,273265,273269,273267,273270,273261,273264,327989,273274,273271,273276,273272,273277,273273,273275,273279,273278,273289,273283,273282,273284,273288,273285,273281,273287,273286,273290,273292,273296,273298,273299,273300,273297,273293,273291,273294,273295,273302,273303,273306,273308,273309,273305,273310,273307,273301,273304)][,1])

setnames(Probs,c(1,2,3),c("1","0","2"))
SumRPS <- 0
for(i in 1:nrow(testData))
{
  if(testData[i,Result] == 1)
  {
    SumRPS <- SumRPS + (Probs[i,1] - 1)^2 + (Probs[i,1] +Probs[i,2] - 1)^2
  }
  if(testData[i,Result] == 0)
  {
    SumRPS <- SumRPS + (Probs[i,1])^2 + (Probs[i,1] +Probs[i,2] - 1)^2
  }
  if(testData[i,Result] == 2)
  {
    SumRPS <- SumRPS + (Probs[i,1] )^2 + (Probs[i,1] +Probs[i,2])^2
  }
}

SumRPS / nrow(testData)
#RETRAIN THE TREE WITH ALL DATA AND CHECK TEST AS PREMIER LEAGUE PREDICTED MATCHES

#Class inbalance problem occured here.
MyDataNew<-MyData
MyDataNew <- as.data.table(MyDataNew)
MyDataNew<- cbind(MyDataNew,ifelse(MyDataNew$Class > 2, 2,ifelse(MyDataNew$Class >0 , 1 , ifelse(MyDataNew$Class == 0, 0 , ifelse(MyDataNew$Class > -3 , -1 , -2) ))))
MyDataNew <- MyDataNew[,-26]
setnames(MyDataNew,26, "Class")
#Probabilities are calculated.

#To Do List and Improvements on Code
#RPS should be divided by 2, since the definition is given like that by instructor.
#After determining the parameters, pruning the tree can be done.
#Class balancing problem should be handled. 
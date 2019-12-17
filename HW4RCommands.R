library(data.table)
library(dplyr)
library(tidyr)
path <- "d:/Dropbox/IE 582/HW 4/DATA"
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

TeamMatches <- DTmatchesData[,c(2,3,4,9,11,13,10,12,14,17,18,19,20)]
TeamMatches <- rbind(TeamMatches, DTmatchesData[,c(1,3,4,10,12,14,9,11,13,17,18,19,20)], use.names=FALSE)
TeamMatches <- cbind(TeamMatches,c(rep(1,count(DTmatchesData)),rep(2,count(DTmatchesData))))
setnames(TeamMatches,c(1,4,5,6,7,8,9,14),c("TeamID","Scored","HalfScored","ExtraScored","Conceded","HalfConceded","ExtraConceded","Home/Away"))
TeamMatches<- TeamMatches[order(epoch)]


#TASK 1: SİMDİ BURADA HER MAÇ İÇİN O TAKIMIN DAHA ÖNCE EN AZ 5 MAÇ YAPTIĞI MAÇLARIN SUBSETİNİ ÇEKMEMİZ LAZIM
setDT(TeamMatches)[, Index := seq_len(.N), by = TeamID] #her maçın yanına o takımın kacıncı macı olduğu bilgisini ekledik
unique(TeamMatches[Index > 5]$match_id) #5406 games after 5th games of teams
#TASK 2: TASK 1 BİTTİKTEN SONRA TAKIM TAKIM, HER MAÇ İÇİN LAGLI BİR ŞEKİLDE 3LÜ 5 Lİ N Lİ ATTIĞI YEDİĞİ HOME AWAY LAZIM
CalculateAverages <- function(DT,index,lag){
#DT şu şekilde giriyor  [scored , half scored, extra scored , conceded , half conceded , extra conceded , index]
  #print("======================")
  sum <- 0
  #print(index)
  for(i in (index - lag):(index - 1)){
    #print(DT[Index == i,1:6])
    sum <- sum + DT[Index == i,1:6]
  }
  #print(sum / lag)
  return(sum / lag)
  
}

#BURAYI TEKRAR CALISTIRMA COK UZUN SURUYOR, DATA YI sAVE EDELIM
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
#BURAYI TEKRAR CALISTIRMA COK UZUN SURUYOR, DATA YI sAVE EDELIM

#3,5,N Datasının matches e konulması
DTimportantMatchesData <- DTmatchesData[DTmatchesData$match_id %in% unique(TeamMatches[Index > 5]$match_id)] ## KODA BAK BEE KODA BAK CAY DEMLE
for(row in 1:nrow(DTimportantMatchesData))
{
  currentMatchID <- DTimportantMatchesData[row,match_id][1]
  #print(currentMatchID)
  currentData <- TeamMatchesGreaterThan6[match_id == currentMatchID]
  #print(currentData)
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
#3,5,N Datasının matches e konulması

#TASK 3: 2.5 USTU VE 3.5 USTU BETS DATA DAN ORANLAR CEKİLECEK HER MAÇ İÇİN
#setnames(DTimportantMatchesData,"odd_2.5","odd_2.5o")
#setnames(DTimportantMatchesData,"odd_3.5","odd_3.5o")
DTimportantBetsData <- DTbetsData[DTbetsData$variable %in% c("o+2.5","o+3.5","u+2.5","u+3.5"),1:5]
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
     
    DTimportantMatchesData[row, "odd_2.5o" := mean(currentData[variable=="o+2.5"][,value])]
    DTimportantMatchesData[row, "odd_3.5o" := mean(currentData[variable=="o+3.5"][,value])]
    DTimportantMatchesData[row, "odd_2.5u" := mean(currentData[variable=="u+2.5"][,value])]
    DTimportantMatchesData[row, "odd_3.5u" := mean(currentData[variable=="u+3.5"][,value])]
  }
}
####### 36 + 4 column dolu inş
DTimportantMatchesData<-cbind(DTimportantMatchesData,DTimportantMatchesData$TotalGoals>2.5)
setnames(DTimportantMatchesData,62, "IsOver2.5")



######## Penalized Regression ############

LassoDummyData = DTimportantMatchesData[,c(17:19,22:62)]
for(i in 1:ncol(LassoDummyData)){
  LassoDummyData[is.na(LassoDummyData[,i]), i] <- mean(LassoDummyData[,i], na.rm = TRUE)
}

library(glmnet)
library(rpart)


set.seed(1903)
trainingDataLasso = sample_n(LassoDummyData,round(nrow(LassoDummyData)*2/3))
testDataLasso = LassoDummyData[!rownames(LassoDummyData) %in% rownames(trainingDataLasso),]

#For Classification Data

trainingDataClass = trainingDataLasso[,-c(2)]
testDataClass = testDataLasso[,-c(2)]

#For Regression Data
trainingDataReg = trainingDataLasso[,-c(44)]
testDataReg = testDataLasso[,-c(44)]

#Training Data includes 3604 instances
#Test Data includes 1802 instances

lambdaVector= cbind(0.0005,0.0006,0,0007,0.0008,0.0009,0.001,0.002,0.003,0.004,0.005,0.006,01,0.02,0.03,0.04,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
ncol(lambdaVector)
lassoAccuracy = rep(0,ncol(lambdaVector))
AccuracyInteger = 0


for(i in 1:ncol(lambdaVector)){
  LassoModel = glmnet(as.matrix(trainingDataClass[,-43]),trainingDataClass$IsOver2.5,family = "binomial",alpha=1, nFolds=10,lambda = lambdaVector[,i])
  predictionLasso = predict(LassoModel, as.matrix(testDataClass[,-43]), type ="class")
  AccuracyInteger = sum(predictionLasso == testDataClass$IsOver2.5)/length(predictionLasso)
  lassoAccuracy[i] = AccuracyInteger
  AccuracyInteger=0
}

lassoAccuracy
AccuracyInteger = max(lassoAccuracy)
AccuracyInteger
#Corresponding lambda value is 0.0007. Index 3 has the highest accuracy. Other metrics could also be used. 

#Lets continue with regression case

lassoAccuracy2 = rep(0,ncol(lambdaVector))
MAPEValue = 0

for(i in 1:ncol(lambdaVector)){
  LassoModel2 = glmnet(as.matrix(trainingDataReg[,-2]),trainingDataReg$TotalGoals,family = "gaussian",alpha=1, nFolds=10,lambda = lambdaVector[,i])
  predictionLasso2 = predict(LassoModel2, as.matrix(testDataReg[,-2]))
  MAPEValue = sum(abs(predictionLasso2-testDataReg$TotalGoals))/length(testDataReg$TotalGoals)
  lassoAccuracy2[i] = MAPEValue
  MAPEValue=0
}

lassoAccuracy2
MAPEValue = min(lassoAccuracy2)
MAPEValue

######## Decision Tree ############

mindevController = cbind(0.0005,0.002,0.0011,0.001903,0.1,0.000009)
minsizeController = cbind(1000,500,330,1400,800)
#For classification
treeClassAccuracy =  rep(0,100*ncol(mindevController)*ncol(minsizeController))
AccuracyIntegerTree = 0
#For regression
treeRegMAPE = rep(0,100*ncol(mindevController)*ncol(minsizeController))
MAPEValueTree = 0

library(rattle)

#For Classification Data
testDataReg

trainingDataClassTree = trainingDataClass[,-c(2)]
testDataClassTree = testDataClass[,-c(2)]

#For Regression Data
trainingDataRegTree = trainingDataReg[,-c(3)]
testDataRegTree = testDataReg[,-c(3)]

testDataClassTree$IsOver2.5 = as.factor(testDataClassTree$IsOver2.5)
str(testDataClassTree)


#rpart automatically does to cross validation.
for (i in 1:ncol(mindevController) ){
  for (j in 1:ncol(minsizeController)){
      TreeModel = rpart(IsOver2.5 ~.,data=trainingDataClassTree,method="class",cp=mindevController[,i],minsplit=minsizeController[,j])
      TreePrediction=predict(TreeModel,testDataClassTree,type="class")
      AccuracyIntegerTree = sum(TreePrediction==testDataClass$IsOver2.5)/length(TreePrediction)
            }
  treeClassAccuracy[(100*i)+j]=AccuracyIntegerTree
  #in order to keep information of which parameter is good. #so clever ekin. #thanks
  AccuracyIntegerTree=0
}

controller = max(treeClassAccuracy)

for(i in 1:length(treeClassAccuracy)){
  if(treeClassAccuracy[i] == controller)
  {
    print(i)
    print(controller)
    #lets see which parameters are good
  }
}

TreeModel = rpart(IsOver2.5 ~.,data=trainingDataClassTree,method="class",cp=mindevController[,1],minsplit=minsizeController[,5])
fancyRpartPlot(TreeModel)

TreePrediction=predict(TreeModel,trainingDataClassTree,type="class")
table(TreePrediction,trainingDataClassTree$IsOver2.5)

TreePrediction=predict(TreeModel,testDataClassTree,type="class")
table(TreePrediction,testDataClassTree$IsOver2.5)

####For regression case

controller2=1000
controli = 0
controlj = 0

#rpart automatically does to cross validation.
for (i in 1:ncol(mindevController) ){
  for (j in 1:ncol(minsizeController)){
    TreeModel2 = rpart(TotalGoals ~.,data=trainingDataRegTree,cp=mindevController[,i],minsplit=minsizeController[,j])
    TreePrediction2=predict(TreeModel2,testDataClassTree)
    MAPEValueTree = sum(abs(TreePrediction2-testDataRegTree$TotalGoals))/length(testDataRegTree$TotalGoals)
  }
  
  if(MAPEValueTree < controller2)
  {
    controller2=MAPEValueTree
    controli = i
    controlj = j
    
  }
  #in order to keep information of which parameter is good. #so clever ekin. #thanks
  MAPEValueTree=0
}

controller2 
controli
controlj


TreeModel2 = rpart(TotalGoals ~.,data=trainingDataRegTree,cp=mindevController[,controli],minsplit=minsizeController[,controlj])
fancyRpartPlot(TreeModel2)


#############We are very sorry about not completing the other tasks. We have talked with Mustafa Hoca about our critical situations. :/


#######Comparing the Results

#For Lasso
AccuracyInteger
MAPEValue

#For Tree
controller
controller2

############ REMARKS AND COMMENTS ###############

#In both cases (classification and regression), Lasso Regression seems quite well then decision trees. 
#Intuitively, with respect to our work in the project, random forest will give better results.
#Moreover, we should increase our features.
#Seperating each league in terms of models is very important, since over and under ratios between leagues are differing.
#Calculating and writing corresponding functions to 3-lag,5-lag and n-lag statistics lasted very long.
#Imputing missing data with mean is not correct for skewed data. On the other hand, we assumed data is symmetric.
#k-NN imputation with respect to weighted averages of "distances" would work quite well in such case.
#M.I.C.E. package can be used for imputation also.
#We think odd data for 2.5 u/o and 3.5 u/o are very correlated, which may affect the quality of models.









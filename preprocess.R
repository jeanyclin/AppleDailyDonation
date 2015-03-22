#settings
library(caret)
library(seewave)


Sys.setlocale(,"CHT")
debugMode = T

#file name and file path setting
fileDir = "."
fileName = "df_adjusted.csv"
dataPath = file.path(fileDir, fileName)

#----------------------------Functions----------------------------#

#idnetify the numeric data which is actually categorical data
NumericToFactor = function(DF){
    for(i in 1:ncol(DF)){
        currentCol = DF[,i]
        numObs = length(currentCol[!is.na(currentCol)])
        numOf1 = length(currentCol[currentCol == 1])
        numOf0 = length(currentCol[currentCol == 0])
        if( numObs == numOf1 + numOf0){
            DF[,i] = as.factor(currentCol)
            print(paste(colnames(DF)[i], class(DF[,i]), sep = " "))
        }
    } 
    DF
}

#normalize the data
normalize = function(x){
    (x-min(x))/(max(x)-min(x))
}

#discretize donation
discretizeDonation = function(data, breakpoints =  c(375900, 529100), auto=F, group=5){
    if(auto){
        category = SAX(data, alphabet = group, PAA = 1581)
    }else{
        category = SAX(data, alphabet = group, PAA = 1581, breakpoints = breakpoints) 
    }
    category
}

#the distribution of data type
numDataType = function(DF){
    col.types = as.factor(sapply(DF, class))
    summary(col.types)
}

#select the columns which are the same data type
selectTypeCol = function(DF, type, returnColnames){
    selectCol = data.frame(row.names = row.names(DF))
    numCol = 0;
    
    for(i in 1:ncol(DF)){
        if(class(DF[,i]) == type){
            selectCol = cbind(selectCol, DF[,i])
            numCol = numCol + 1
            colnames(selectCol)[numCol] = colnames(DF)[i]
        }
    }
    
    if(returnColnames){
        selectCol = colnames(selectCol)
    }
    selectCol
}

#find the column that are not discriminative
isNotDiscriminative = function(DF){
    uselessCol = numeric()
    numRows = nrow(DF)
    
    for(i in seq(DF)){
        if(class(DF[,i]) == "factor"){
            if(length(levels(DF[,i])) == numRows | length(levels(DF[,i])) == 1){
                uselessCol = c(uselessCol, i)
            }
        }else if(class(DF[,i]) == "character" | class(DF[,i]) == "Date"){
            uselessCol = c(uselessCol, i)
        }
    }
    uselessCol
}


#-----------------------Data Preprocessing----------------------#
#load the file into eclipse
donationData = read.csv(dataPath, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

#change the category data to factor
donationData = NumericToFactor(donationData)

#identify date variables
donationData$dt.published = as.Date(donationData$dt.published)
donationData$dt.funded = as.Date(donationData$dt.funded)
donationData$dt.published.monthday = as.factor(donationData$dt.published.monthday)
donationData$dt.published.month = factor(donationData$dt.published.month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
donationData$dt.published.weekday = factor(donationData$dt.published.weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri"))
donationData$rank = as.factor(donationData$rank)
donationData$journalist = as.factor(donationData$journalist)
donationData$dep = as.factor(donationData$dep)
donationData = cbind(donationData,dt.deltaPublishedFunded = as.numeric(donationData$dt.funded-donationData$dt.published))

#log(donation)
log.donation = log(donationData$donation, base=exp(1))
donationData = cbind(donationData, log.donation)

#discretize the data
f.donation = discretizeDonation(donationData$donation, auto = T, group = 5)
f.donation = as.factor(f.donation)
donationData = cbind(donationData, f.donation)
groupInfo = tapply(donationData$donation, f.donation, summary)
summary(f.donation)

removeIndex = numeric()
#eliminate the variables which are related to the targets(donation or donor)
for(i in seq(colnames(donationData))){
    splitColName = unlist(strsplit(colnames(donationData)[i], "[.]"))
    if(splitColName[1] == "donation" | splitColName[1] == "donor"){
        removeIndex = c(removeIndex, i)
    }
}
colnames(donationData)[removeIndex]

#eliminate the data that are not discriminative
colnames(donationData)[isNotDiscriminative(donationData)]
removeIndex = c(removeIndex, isNotDiscriminative(donationData))

revisedDonation = donationData[,-removeIndex]
numDataType(revisedDonation)

factorVars = selectTypeCol(revisedDonation, "factor", T)
continuousVars =c(selectTypeCol(revisedDonation, "numeric", T), selectTypeCol(revisedDonation, "integer", T))
categories = levels(revisedDonation$f.donation)
#----------------------------Sampling-----------------------------#
#trainIndex = createDataPartition(f.donation, times = 5, p = 0.7, list = FALSE)
#write.csv(trainIndex, "./trainIndex.csv", row.names=F)
trainIndex = sapply(read.csv("./trainIndex.csv", stringsAsFactors=F), as.integer)
revisedDonation = cbind(revisedDonation, donation = donationData$donation)
trainDonationData = revisedDonation[trainIndex[,2],]
testDonationData = revisedDonation[-trainIndex[,2],]
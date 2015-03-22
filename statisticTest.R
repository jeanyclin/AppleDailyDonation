#statistic test example
p.values = kruskalTest(f.donation, revisedDonation)
names(sort(p.values[p.values < 0.05]))

p.values = anovaTest(trainDonationData[selectedRow,"donation"], trainDonationData[selectedRow,])
names(sort(p.values[p.values < 0.05]))

p.values = chi2Test(f.donation, revisedDonation[,selectVars])
names(sort(p.values[p.values < 0.005]))

p.values = intervalChi2Test(revisedDonation$donation, revisedDonation[,selectVars])
siginificantVars = print(names(sort(p.values[p.values < 0.005])))

p.values = ksTest(trainDonationData[selectedRow,"donation"], trainDonationData[selectedRow,])
names(sort(p.values[p.values < 0.05]))

p.values = friedmanTest(f.donation, revisedDonation)
names(sort(p.values[p.values < 0.05]))

correlation = correlationTest(trainDonationData[,"donation"], trainDonationData[,selectVars])
names(sort(correlation[abs(correlation) > 0.6]))

continuousVars = cbind(selectTypeCol(revisedDonation, "numeric", F), selectTypeCol(revisedDonation, "integer", F))
setRecord = findDisjointSetVar(trainDonationData[,selectVars])
selectVars = tapply(names(setRecord), setRecord, sample, size = 1)

if(debugMode){
    print(summary(donationData))
}


#----------------------------Statistical Test----------------------------#
#independent variable set in mumeric variables
findDisjointSetVar = function(numericVar, continuous = T){
    corResult = data.frame()
    if(continuous){
        corResult = as.data.frame(cor(numericVar))
    }else{
        corResult = as.data.frame(cor(as.numeric(numericVar)))
    }
    varNames = row.names(corResult)
    setRecord = numeric(length(varNames))
    names(setRecord) = varNames
    dependentSets = sapply(corResult, function(x, varNames){varNames[abs(x)==1]}, varNames)
    
    for(i in seq(dependentSets)){
        group = dependentSets[[i]]
        setId = numeric()
        
        # if the group of variables is related to certain sets
        if(sum(setRecord[group]) != 0){
            
            #which sets are corelated to the group
            for(j in 1:length(group)){
                if(setRecord[group[j]] != 0){
                    setId = c(setId, setRecord[group[j]])
                }
            }
            
            #union the group to setId[1]
            setRecord[group] = setId[1]
            
            #union those sets that are related to the group
            if(length(setId) > 1){
                for(id in 2:length(setId)){
                    setRecord[setRecord == setId[id]] = setId[1]
                }
            }
        }else{#if the group of variables is independent
            setRecord[group] = i
        }
    }
    
    setRecord
}

#correlation test
correlationTest = function(dependentVariable, DF){
    colClass = sapply(DF, class)
    numericCol = names(colClass[colClass == "numeric" | colClass == "integer"])
    estimate = numeric()
    p.value = numeric()
    
    for(i in seq(numericCol)){
        result = cor.test(dependentVariable, DF[[numericCol[i]]])
        estimate = c(estimate, result$estimate)
        p.value = c(p.value, result$p.value)
    }
    
    data.frame(row.names = numericCol, estimate = estimate, p.value = p.value)
    estimate
}

#Chi2 test for factor
chi2Test = function(dependentVariable, DF){
    #selectCol = selectTypeCol(DF, "factor", F)
    p.value = numeric()
    
    #start test
    for(i in seq(DF)){
        p.value = c(p.value, chisq.test(table(DF[,i], dependentVariable))[[3]])
    }
    
    names(p.value) = colnames(DF)
    p.value
}

#Chi2 test for interval
intervalChi2Test = function(dependentVariable, DF){
    p.value = numeric()
    
    #start test
    for(i in seq(DF)){
        p.value = c(p.value, interval.chisq.test(dependentVariable, DF[,i]))
    }
    
    names(p.value) = colnames(DF)
    1-p.value
}

interval.chisq.test <- function(x,y){ 
    
    normalizedX = normalize(x)
    
    allMean = mean(normalizedX)
    
    groupChisqValue = tapply(normalizedX, y, chisqValue, allMean = allMean)
    
    p = pchisq(sum(groupChisqValue),length(levels(y))-1)
    
    p_value = 1 - p
    p_value
    
}

chisqValue = function(test, allMean){
    chis_value <- '^'(mean(test)-allMean,2)/allMean
    chis_value
}

#Friedman test
friedmanTest = function(dependentVariable, DF){
    selectCol = selectTypeCol(DF, "numeric", F)
    selectCol = cbind(selectCol, selectTypeCol(DF, "integer", F))
    p.value = numeric()
    print(names(selectCol))
    
    #start test
    for(i in seq(selectCol)){
        p.value = c(p.value, friedman.test(selectCol[,i], dependentVariable)[[3]])
    }
    
    names(p.value) = colnames(selectCol)
    p.value
}

#one way anovas
anovaTest = function(dependentVariable, DF){
    p.value = numeric()
    
    #start test
    for(i in seq(DF)){
        p.value = c(p.value, summary(aov(DF[,i] ~ dependentVariable))[[1]][[5]][[1]])
    }
    
    names(p.value) = colnames(DF)
    p.value
}

#Kruskal test
kruskalTest = function(dependentVariable, DF){
    selectCol = selectTypeCol(DF, "numeric", F)
    selectCol = cbind(selectCol, selectTypeCol(DF, "integer", F))
    p.value = numeric()
    
    #start test
    for(i in seq(selectCol)){
        p.value = c(p.value, kruskal.test(selectCol[,i], dependentVariable)[[3]])
    }
    
    names(p.value) = colnames(selectCol)
    p.value
}

#KS test
ksTest = function(dependentVariable, DF){
    selectCol = selectTypeCol(DF, "numeric", F)
    selectCol = cbind(selectCol, selectTypeCol(DF, "integer", F))
    p.value = numeric()
    
    #start test
    for(i in seq(selectCol)){
        p.value = c(p.value, ks.test(normalize(selectCol[,i]), normalize(dependentVariable))[[2]])
    }
    
    names(p.value) = colnames(selectCol)
    p.value
}

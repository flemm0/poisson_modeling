# Write a script that simulates throwing balls into urns and 
# create a histogram of the number of balls in each urn
# and prints the index of dispersion to the console 


### It is highly unlikely for each urn to hold the same number of balls, the probability of an even distribution increases as the number of balls increases
### On average, the number of balls held by each urn is equal to the number of balls divided by the number of urns
### It will be very unlikely for all of the balls to land in each urn, considering there is an equal chance of them landing in every urn
### If the number of balls is much lower than the number of urns, it is more likely for some urns to not hold any balls, this number decreases as the number of urns decreases or the number of balls increases


nUrns = 39
nBalls = 627
binscore = numeric(nUrns)
##binscore ##makes empty vector to store scores for each urn
comparisonvec = c(0)
for(i in 1:nUrns){
  comparisonvec = c(comparisonvec, i/nUrns)
}
##comparisonvec ##returns vector of cutoff values for determining which urn a ball will go into

results = runif(n = nBalls, min = 0, max = 1)
##results ##random scores generated for each ball throw between 0 and 1

for(i in 1:length(binscore)){
  mymin = comparisonvec[i]
  mymax = comparisonvec[i+1]
  for(x in 1:length(results)){
    if(results[x] > mymin && results[x] < mymax){
      binscore[i] = binscore[i] + 1
    }
  }
}
binscore ##returns number of balls landing in each urn with each index of the vector representing an urn

hist(binscore, breaks = nUrns) ##plots the number of balls in an urn on the x axis versus the frequency of this score on the y axis

total = 0
for(i in 1:length(binscore)){
  total = total + binscore[i]
}
mean = total/nUrns ##calculate mean without built in mean function
indOfDisp = (var(binscore))/mean
print(indOfDisp) ##prints index of dispersion


######### Activities 4-5 ##############

# Now package your balls and urns script from above into a function
# Then write a script to run throwBallsIntoUrns() 1000 times,
# store the indices of dispersion, and graph them in a histogram.


throwBallsIntoUrns <- function(nBalls, nUrns){
  binscore = numeric(nUrns) ##makes empty vector to store scores for each urn
  comparisonvec = c(0)
  for(i in 1:nUrns){
    comparisonvec = c(comparisonvec, i/nUrns)
  } ##returns vector of cutoff values for determining which urn a ball will go into
  
  results = runif(n = nBalls, min = 0, max = 1) ##random scores generated for each ball throw between 0 and 1
  
  for(i in 1:length(binscore)){
    mymin = comparisonvec[i]
    mymax = comparisonvec[i+1]
    for(x in 1:length(results)){
      if(results[x] > mymin && results[x] < mymax){
        binscore[i] = binscore[i] + 1
      }
    }
  }
  total = 0
  for(i in 1:length(binscore)){
    total = total + binscore[i]
  }
  mean = total/nUrns ##calculate mean without built in mean function
  indOfDisp = (var(binscore))/mean
  
return(indOfDisp)
}


indOfDisVec = c()
for(i in 1:1000){
  indOfDisVec = c(indOfDisVec, throwBallsIntoUrns(627, 39))
}
print(indOfDisVec)
hist(indOfDisVec) # The results of IoD of 1 suggests that mean and variance are about the same. 


# Model attraction, repulsion, and different sized urns

#modeling attraction
attractionProbs = function(binscore){
  for(i in 2:length(binscore)){
    binscore[i] = (binscore[i-1] + binscore[i])
  }
  newscore = c(0)
  for(j in binscore){
    newscore = c(newscore, j)
  }
  for(k in 1:length(newscore)){
    newscore[k] = newscore[k]/nBalls
  }
  return(newscore)
}

attractionCompVec = attractionProbs(binscore)
attractionCompVec

attractionscore = numeric(nUrns)
for(i in 1:length(attractionscore)){
  myAMin = attractionCompVec[i]
  myAMax = attractionCompVec[i+1]
  for(x in 1:length(results)){
    if(results[x] > myAMin && results[x] < myAMax){
      attractionscore[i] = attractionscore[i] + 1
    }
  }
}
attractionscore

total1 = 0
for(i in 1:length(attractionscore)){
  total1 = total1 + attractionscore[i]
}
mean1 = total1/nUrns
AIndOfDisp = (var(attractionscore))/(mean1)
print(AIndOfDisp) ###higher index of dispersion than for non-attraction simulation


#modeling repulsion
repulsionProbs = function(binscore){
  for(i in 1:length(binscore)){
    binscore[i] = nBalls - binscore[i]
  }
  for(j in 2:length(binscore)){
    binscore[j] = binscore[j-1] + binscore[j]
  }
  repScore =c(0)
  repMax = binscore[length(binscore)]
  for(k in 1:length(binscore)){
    repScore = c(repScore, binscore[k]/repMax)
  }
  return(repScore)
}

repulsionCompVec = repulsionProbs(binscore)
repulsionCompVec

repulsionscore = numeric(nUrns)
for(i in 1:length(repulsionscore)){
  myRMin = repulsionCompVec[i]
  myRMax = repulsionCompVec[i+1]
  for(x in 1:length(results)){
    if(results[x] > myRMin && results[x] < myRMax){
      repulsionscore[i] = repulsionscore[i] + 1
    }
  }
}
repulsionscore


total2 = 0
for(i in 1:length(repulsionscore)){
  total2 = total2 + repulsionscore[i]
}
mean2 = total2/nUrns
RIndOfDisp = (var(repulsionscore))/mean2
print(RIndOfDisp) ##slightly lower than index of dispersion than for random simulation 


#different urn sizes
newUrnSizeVec = c(0)
for(i in 1:nUrns){
  newUrnSizeVec = c(newUrnSizeVec, sqrt(i/nUrns))
}

newUrnSizeVec
diffSizeScore = numeric(nUrns)

for(i in 1:length(diffSizeScore)){
  mySmin = newUrnSizeVec[i]
  mySmax = newUrnSizeVec[i+1]
  for(x in 1:length(results)){
    if(results[x] > mySmin && results[x] < mySmax){
      diffSizeScore[i] = diffSizeScore[i] + 1
    }
  }
}
diffSizeScore ##returns number of balls landing in each urn with each index of the vector representing an urn

total3 = 0
for(i in 1:length(diffSizeScore)){
  total3 = total3 + diffSizeScore[i]
}
mean = total3/nUrns ##calculate mean without built in mean function
SindOfDisp = (var(diffSizeScore))/mean
print(SindOfDisp) ##increasing the size of some of the urns causes the index of dispersion to increase
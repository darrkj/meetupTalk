library(lattice)
library(Revobase)
library(RevoScaleR)

defaultDataDir <- "C:/rproj/bigr/data"

# This import the data from the text file into the xdf format

#rxImport(inData = "C:/rproj/bigr/airline.txt", outFile = "C:/rproj/bigr/Airline.xdf",
#         firstRowIsColNames = TRUE, delimiter = "|", overwrite = TRUE )

#Define data, since it does not  
airline <- file.path(defaultDataDir,"Airline")

#Basic info on data, similar to str()
rxGetInfoXdf(airline, getVarInfo=TRUE)  

#Prove there is nothing filling up the memory
gc()

# Fit a linear model with the cube option
arrDelayLm1 <- rxLinMod(ArrDelay ~ -1+DayOfWeek, data=airline, cube=TRUE)
summary(arrDelayLm1) # Use the standard R function summary
arrDelayLm1$countDF # Look at the optput from the cube option

xyplot( ArrDelay ~ DayOfWeek, data = arrDelayLm1$countDF, type = "l",
        lwd=3,pch=c(16,17), auto.key=TRUE)

# Predict function computes predicted values and residuals
rxPredict(modelObject=arrDelayLm1,data=airline,computeResiduals=TRUE)
par(mfrow=c(3,3))
start <- runif(16,1,120000000)
for (i in 1:9){
  residualDF <- rxReadXdf(file=airline,varsToKeep="ArrDelay_Resid",
                          startRow= start[i],numRows=1000)
  plot(residualDF$ArrDelay_Resid)}



# Multiple Regression
arrDelayLm2 <- rxLinMod(ArrDelay ~ DayOfWeek:F(CRSDepTime), data=airline,cube=TRUE)
arrDelayDT <- arrDelayLm2$countDF
arrDelayDT[1:5,]
summary(arrDelayLm2)
names(arrDelayDT) <- c("DayOfWeek", "DepartureHour", "ArrDelay", "Counts")
xyplot( ArrDelay ~ DepartureHour|DayOfWeek, data = arrDelayDT,
        type = "l", lwd=3,pch=c(16,17),
        main='Average Arrival Delay by Day of Week by Departure Hour', layout=c(2,4), auto.key=TRUE)



# Create function to transform data
myTransforms <- function(data){
  data$Late <- data$ArrDelay > 15
  data$DepHour <- as.integer(data$CRSDepTime)
  data$Night <- data$DepHour >= 20 | data$DepHour <= 5
  return(data)}
# The rxDataStepXdf function read the existing data set, performs the
# transformations, and creates a new data set.
rxDataStepXdf(outFile="ADS2", inFile=airline, transformFunc=myTransforms,
              varsToKeep=c("ArrDelay","CRSDepTime","DepTime"))
#
rxGetInfoXdf("ADS2", numRows=5)
# Run a logistic regression using the new variables
logitObj <- rxLogit(Late~DepHour+Night, data="ADS2", verbose=TRUE)


rxGetInfoXdf(airline, numRows=5)
# Simple Regression
system.time(delayArr <- rxLinMod(ArrDelay ~ DayOfWeek,data=airline,blocksPerRead=30))
# summary(delayArr)
# Multiple Regression
system.time(delayCarrierLoc <- rxLinMod(ArrDelay ~
                                          UniqueCarrier+Origin+Dest, data=airline,blocksPerRead=30,cube=TRUE))
                    
                    
rxOptions(numCoresToUse=8)


# Compute a 4-way cross tabulation

system.time(delayCarrierLoc <- rxLinMod(ArrDelay ~
                                          UniqueCarrier+Origin+Dest, data=airline,blocksPerRead=30,cube=TRUE))

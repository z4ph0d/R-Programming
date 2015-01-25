rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        states <- data[,7]
        
        statesu <- unique(states)
        print(statesu)
        colNumber <- NULL
        
        
        ## Check that state and outcome are valid

        
        if(is.na(pmatch(outcome,c("heart attack", "heart failure", "pneumonia"))))
                stop("invalid outcome")
        
        if(outcome=="heart attack")
                colNumber <- 11
        if(outcome=="heart failure")
                colNumber <- 17
        if(outcome=="pneumonia")
                colNumber <- 23
        
        ## clean the data from NA hospitals regarding outcome
        cleandata<-data[complete.cases(as.numeric(data[,colNumber])),]
        
        ## order data by state and outcome
        ordereddata <- cleandata[order(cleandata[,7], cleandata[,colNumber]),]
        print(head(ordereddata[,c(7,colNumber)]))
        
        splitod <- split(ordereddata, ordereddata$State)
        print(head(splitod[[2]][,7]))
        
        
        ## For each state, find the hospital of the given rank
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
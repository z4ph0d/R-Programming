rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        states <- data[,7]
        
        statesu <- unique(states)
        #print(statesu)
        colNumber <- NULL
        rank<-NULL
        
        if(num=="best")
                rank<-1
        
        
        
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
        #print(head(ordereddata[,c(7,colNumber)]))
        
        splitod <- split(ordereddata, ordereddata$State)

        #res<-lapply(splitod, function(x) tail(x[,c(2,7)],1))
        res<-(lapply(splitod, getResult, num))
        #return(res)
        #####
        #return(unsplit(res,statesu))
        result<-data.frame(matrix(unlist(res), nrow=length(statesu), byrow=T))
        colnames(result)<-c("hospital", "state")
        return(result)
        #print(res)
        #result<-as.data.frame(res)
        #print(result)
        
        ## For each state, find the hospital of the given rank
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
getResult<-function(x, num) {
        if(num=="best" && length(x[,1])>0){
                print(paste(num,length(x[,1])))
                return(x[1,c(2,7)])    
        }
        else if(num=="worst"&& length(x[,1])>0){
                print(paste(num,length(x[,1])))
                return(x[length(x[,1]),c(2,7)])
        }
        else if(as.numeric(num<=length(x[,1]))){
                print(paste(num,length(x[,1])))
                return(x[as.numeric(num),c(2,7)])
        }
        else{
                print(num)
                return(list("NA", x[1,7]))
        }
}
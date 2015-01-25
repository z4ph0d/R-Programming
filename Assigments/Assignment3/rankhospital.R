rankhospital <- function(state, disease, num = "best") {
        ## Read outcome data
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        states <- data[,7]
        
        colNumber <- NULL

 
        ## Check that state and outcome are valid
        if (is.na(pmatch(state,states)))                
                stop("invalid state")
                
        if(is.na(pmatch(disease,c("heart attack", "heart failure", "pneumonia"))))
                stop("invalid outcome")
        
        if(disease=="heart attack")
                colNumber <- 11
        if(disease=="heart failure")
                colNumber <- 17
        if(disease=="pneumonia")
                colNumber <- 23
        
        ## Return hospital name in that state with the given rank
        
        ## split by state 
        statedata<- as.data.frame(split(data, data$State)[state])
        
        ## remove irrelevant hospitals
        shortstatedata<-statedata[complete.cases(as.numeric(statedata[,colNumber])),]
        
        ## set correct rank
        if(num=="best")
                rank <- 1
        else if (num=="worst")
                rank <- length(shortstatedata[,colNumber])
        else
                rank <- as.integer(num)
        
        if(rank>length(statedata[,1]))
                return(NA)
        
        ##order by rate and name
        shortstatedata <- shortstatedata[ order(as.numeric(shortstatedata[,colNumber]), shortstatedata[,2]),]
        
        ##order -> dd[ order(-dd[,4], dd[,1]), ]
        

        
        
        #print(head(statedata[,c(2,colNumber)]))
        #print(shortstatedata[rank ,c(2,colNumber)])
        return(shortstatedata[rank,2])
        ## 30-day death rate
        
        
}
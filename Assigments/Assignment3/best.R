best <- function(state, disease){
        ## read disease data
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        states <- data[,7]
        
        ## Check state and disease
        ## state = 7
        ## diseases:
        ## heart attack = 11
        ## heart failure = 17
        ## pneumonia = 23
        
        if (!is.na(pmatch(state,states)))
                print("State ok")
        else
                stop("invalid state")
        
        
        if(!is.na(pmatch(disease,c("heart attack", "heart failure", "pneumonia"))))
                print("Disease ok")
        else
                stop("invalid outcome")
        
        ##Return hospital name
        print(summary(as.numeric(data[,11])))
        
        ## heart attack
        minval <- min(as.numeric(data[,11]))
        minHospitals <- data[,11] 
}
        
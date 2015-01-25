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
        
        if (is.na(pmatch(state,states)))
                
                stop("invalid state")
        
        
        if(is.na(pmatch(disease,c("heart attack", "heart failure", "pneumonia"))))
                
                stop("invalid outcome")
        
        ##Return hospital name
      
        
        ##split data by state
        splitdata<- as.data.frame(split(data, data$State)[state])
      
      
        ## heart attack : 11
        if(disease == "heart attack"){
          ## reduce hlist
          hdata<-splitdata[complete.cases(as.numeric(splitdata[,11])),]
          
          ## get min value
          minval <- min(as.numeric(hdata[,11]), na.rm=TRUE)
          #print(paste("HA:", minval, sep =" "))  
          
          ## build and sort hospitallist
          hospitallist <- hdata[as.numeric(hdata[,11])==minval,2]
          sort(hospitallist)
          
          ##return hospitallist
          #print(paste("aa",hospitallist))
          return(hospitallist)
          
        }
        
        ## heart failure : 17
        if(disease == "heart failure"){
          ## reduce hlist
          hdata<-splitdata[complete.cases(as.numeric(splitdata[,17])),]
          
          ## get min value
          minval <- min(as.numeric(hdata[,17]), na.rm=TRUE)
          #print(paste("HA:", minval, sep =" "))  
          
          ## build and sort hospitallist
          hospitallist <- hdata[as.numeric(hdata[,17])==minval,2]
          sort(hospitallist)
          
          ##return hospitallist
          #print(hospitallist)
          return(hospitallist)
        }
        
        ## pneumonia :23
        if(disease == "pneumonia"){
          ## reduce hlist
          hdata<-splitdata[complete.cases(as.numeric(splitdata[,23])),]
          
          ## get min value
          minval <- min(as.numeric(hdata[,23]), na.rm=TRUE)
          #print(paste("HA:", minval, sep =" "))  
          
          ## build and sort hospitallist
          hospitallist <- hdata[as.numeric(hdata[,23])==minval,2]
          sort(hospitallist)
          
          ##return hospitallist
          #print(paste("aa",hospitallist))
          return(hospitallist)
        }
        
      

}
        
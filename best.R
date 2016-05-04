best <- function (State, outcome) {
    outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    list = unique(outcome.data$State)
    
    if (State %in% list){
        
        sub_outcome1 = outcome.data[outcome.data$State==State,]
        sub_outcome = sub_outcome1[,c("Hospital Name" = 2,"State" = 7, "heart attack" = 11, "heart failure"=17, "pneumonia"=23)]
        names(sub_outcome) = c("Hospital Name","State", "heart attack", "heart failure", "pneumonia")             
        name = names(sub_outcome)
        
        if (outcome %in% name){
            
            sub_outcome = sub_outcome[,c("Hospital Name",outcome)]
            sub_outcome [,outcome] = as.numeric(sub_outcome[,outcome])
            sub_outcome = na.omit(sub_outcome)
            min_val = min(sub_outcome[,outcome])
            res = sub_outcome[sub_outcome[,outcome] == min_val,"Hospital Name"]
            min(res)
        }
        else
            print("invalid outcome")
        
    }
    else
        print("invalid state")
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    
    files_list <- list.files(directory,full.name = TRUE)
    
    frames <- list(data.frame())
    for (i in id){
        frames <- c(frames,list(read.csv(files_list[i])))
    }
    ## Results vector
    results <- list(sulfate=numeric(), nitrate=numeric())
    
    ## Initialize start and end indices
    start.sulfate <- start.nitrate <- end.sulfate <- end.nitrate <- 0
    
    for(index in seq_along(frames)) {
        values <- frames[[index]]
        size.sulfate <- length(values$sulfate)
        size.nitrate <- length(values$nitrate)
        
        if(size.sulfate > 0) {
            start.sulfate <- end.sulfate + 1
            end.sulfate <- start.sulfate + size.sulfate - 1
            results$sulfate[start.sulfate:end.sulfate] <- values$sulfate
        }
        if(size.nitrate > 0) {
            start.nitrate <- end.nitrate + 1
            end.nitrate <- start.nitrate + size.nitrate - 1
            results$nitrate[start.nitrate:end.nitrate] <- values$nitrate
        }
    }
    
    if (pollutant == "sulfate"){
        
        mean(results$sulfate,na.rm=TRUE)
    }
    else{
        
        mean(results$nitrate,na.rm=TRUE)
    }
       
}
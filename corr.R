corr <- function(directory,threshold = 0){
    
    files_list <- list.files(directory,full.name = TRUE)
    
    frames <- data.frame()
    res <- data.frame()
    cor_v = numeric()
    
    for (i in 1:332){
        frames <- read.csv(files_list[i])
        
        if (sum(complete.cases(frames)) > threshold){
            
            res = frames[complete.cases(frames),]
            cor_v = c(cor_v,cor(res$sulfate,res$nitrate))
        }

    }
    
    cor_v
}



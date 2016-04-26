complete <- function(directory, id = 1:332) {
    
    
    files_list <- list.files(directory,full.name = TRUE)
    
    res_frame <- data.frame(id = numeric(),nobs = numeric())
    
    for (i in id) {
        res_frame <- rbind(res_frame,data.frame(id = i,nobs = sum(complete.cases(read.csv(files_list[i])))))
    }
    
    res_frame
    
}
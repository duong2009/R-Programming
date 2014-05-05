rankall <- function(outcome, num = "best") {
        # read outcome data
        outcome1 <- read.csv("./Hospital/outcome-of-care-measures.csv", 
                             colClasses = "character")
        outcome1[,11] <- as.numeric(outcome1[,11])
        outcome1[,17] <- as.numeric(outcome1[,17])
        outcome1[,23] <- as.numeric(outcome1[,23])
        
        # loop over states
        vect1 <- c()
        vect2 <- c()
        
        states <- unique(outcome1[,7])
        for (i in states) {
                # use rankhospital function
                name <- rankhospital(i, outcome, num)
                vect1 <- c(vect1, name)
                vect2 <- c(vect2, i)
        }
        outcome <- as.data.frame(cbind(hospital = vect1, state = vect2))
        outcome <- outcome[order(outcome$state),]
                
}
rankhospital <- function(state, outcome, num) {
        # read outcomes
        outcome1 <- read.csv("./Hospital/outcome-of-care-measures.csv", 
                             colClasses = "character")
        outcome1[,11] <- as.numeric(outcome1[,11])
        outcome1[,17] <- as.numeric(outcome1[,17])
        outcome1[,23] <- as.numeric(outcome1[,23])
        
        # check validity of state and outcome
        states <- unique(outcome1[,7])
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(!state %in% states) {
                stop("invalid state")
        }
        else if(!outcome %in% outcomes) {
                stop("invalid outcome")
        }
        else {
                data <- data.frame(
                        hospital_name = outcome1[,2],
                        state = outcome1[,7],
                        heart_attack = outcome1[,11],
                        heart_failure = outcome1[,17],
                        pneumonia = outcome1[,23],
                        stringsAsFactors = FALSE
                )
        }
        
        datasplit <- split(data, data$state)
        datastate <- datasplit[[state]]
        
        # return to the name of the hospital
        
        output <- if(outcome == "heart attack") {
                datastate <- subset(
                        datastate, complete.cases(datastate$heart_attack)
                )
                datastate <- datastate[order(datastate$heart_attack, 
                                             datastate$hospital_name),]
                if(num == "best") {
                        datastate[1,1]
                } 
                else if(num == "worst") {
                        tail(datastate[,1], n=1)
                }
                else if(num > nrow(datastate)) {
                        NA
                }
                else {
                        datastate[num,1]
                }
        }
        else if(outcome == "heart failure") {
                datastate <- subset(
                        datastate, complete.cases(datastate$heart_failure)
                )
                datastate <- datastate[order(datastate$heart_failure,
                                             datastate$hospital_name),]
                if(num == "best") {
                        datastate[1,1]
                } 
                else if(num == "worst") {
                        tail(datastate[,1], n=1)
                }
                else if(num > nrow(datastate)) {
                        NA
                }
                else {
                        datastate[num,1]
                }
        }
        else if(outcome == "pneumonia") {
                datastate <- subset(
                        datastate, complete.cases(datastate$pneumonia)
                )
                datastate <- datastate[order(datastate$pneumonia,
                                             datastate$hospital_name),]
                if(num == "best") {
                        datastate[1,1]
                } 
                else if(num == "worst") {
                        tail(datastate[,1], n=1)
                }
                else if(num > nrow(datastate)) {
                        NA
                }
                else {
                        datastate[num,1]
                }
        }
        return(output)
}
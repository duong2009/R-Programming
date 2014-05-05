best <- function(state, outcome) {
        
        # read outcome
        outcome1 <- read.csv("./Hospital/outcome-of-care-measures.csv",
                            colClasses = "character")
        outcome1[,11] <- as.numeric(outcome1[,11])
        outcome1[,17] <- as.numeric(outcome1[,17])
        outcome1[,23] <- as.numeric(outcome1[,23])
                
        # check state and outcome are valid
        states <- unique(outcome1[,7])
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(!state %in% states) {
                        stop("invalid state")
                }
        else 
                if(!outcome %in% outcomes) {
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
        data_state <- datasplit[[state]]
        # return hospital name in that state with the best rate
        output <- if(outcome == "heart attack") {
                data_state <- subset(
                        data_state, complete.cases(data_state$heart_attack)
                )
                data_state[order(data_state$heart_attack),][1,1]
        }
        else    if(outcome == "heart failure") {
                data_state <- subset(
                        data_state, complete.cases(data_state$heart_failure)
                )
                data_state[order(data_state$heart_failure),][1,1]
        }
        else    if(outcome == "pneumonia") {
                data_state <- subset(
                        data_state, complete.cases(data_state$pneumonia)
                )
                data_state[order(data_state$pneumonia),][1,1]
        }        
        return(output)
}
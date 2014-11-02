best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if (state %in% data$State) {
                        
        } else {
                stop("invalid state")
        }
                
        if (outcome == "heart attack") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        narrow = data[, c("State", "Hospital.Name", column)]
        narrow[,column] = suppressWarnings(as.numeric(narrow[,column]))
        narrow = narrow[narrow$State==state,]
        ordered = narrow[order(narrow[,3], narrow[,2]),]
        ordered[1,]$Hospital.Name
}
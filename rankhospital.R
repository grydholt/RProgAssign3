rankhospital <- function(state, outcome, num = "best") {
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
        if (num == "best") {
                numInteger = 1
        }
        else if (num == "worst") {
                numInteger = nrow(ordered) - sum(is.na(ordered$"Hospital.30"))
        } else {
                numInteger = as.integer(num)
        }
        if (nrow(ordered) < numInteger) {
                NA
        } else {               
                ordered[numInteger,]$Hospital.Name
        }
}
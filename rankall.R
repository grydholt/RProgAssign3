rankall <- function(outcome, num = "best") {
        if (outcome == "heart attack") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == "pneumonia") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        narrow = data[, c("State", "Hospital.Name", column)]
        narrow[,column] = suppressWarnings(as.numeric(narrow[,column]))
        splitter <- narrow$State
        spl <- lapply(split(narrow, splitter), rankhospital(num))
        states = names(spl)
        hospitals = unlist(spl, use.names = FALSE)
        data.frame(state=states, hospital = hospitals, row.names = states)
}

rankhospital <- function(num) {
        function(narrow) {
                ordered = narrow[order(narrow[,3], narrow[,2]),]
                if (num == "best") {
                        numInteger = 1
                }
                else if (num == "worst") {
                        numInteger = nrow(ordered) - sum(is.na(ordered$"Hospital.30"))
                } else {
                        numInteger = as.integer(num)
                }
                stateName <- ordered$State[[1]]
                if (nrow(ordered) < numInteger) {
                        hospitalName = NA
                } else {               
                        hospitalName = ordered[numInteger,]$Hospital.Name
                }
                hospitalName
                #data.frame(state=stateName, hospital=hospitalName)
        }
}
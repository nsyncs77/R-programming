R-programming
=============

### Finding the best hospital in a state

Write a function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. 
The function reads the outcome-of-care-measures.csv and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome in that state. 

best <- function(exactstate, symptom){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    sim <-outcome[,c(2,7,11,17,23)]     ## data.frame scale ##
    ordersim <- sim[order(sim[,2]),]    ## sort by state ##
    splitsim <- split(ordersim,ordersim$State)    ## data.frame split ##    
    
    state <- vector("character", length=nrow(ordersim))     ## ensuere exactstate ## 
    state[1] <- ordersim[1,2]
    nnrow <- nrow(ordersim)-1
    for(i in 1:nnrow){
        if(ordersim[i,2]!=ordersim[i+1,2]){
            state[i+1] <- ordersim[i+1,2]
        }
    }
    c <- state!=""
    existstate <- state[c] 
    for(i in 1:length(existstate)){
        if(exactstate==existstate[i]){
            s <- splitsim[[i]]
        }
    }
    
    names(s) <- c("Hospital","State","heart attack", "heart failure", "pneumonia")  ## ensuere symptom ## 
    sym <- names(s)
    if(symptom=="heart attack"||symptom=="heart failure"||symptom=="pneumonia"){
        for(i in 3:5){
            s[,i] <- as.numeric(s[,i])
            if(symptom==sym[i]){
                final <- s[complete.cases(s[,i]),]      ## NA rows excluded in dataframe ##
                mins <- (min(final[,i]))
                for(j in 1:nrow(final)){
                    if(mins==final[j,i] ){
                        q <- (final[j,1])
                    }
                }
            }    
        }
    }else{
        stop("invalid outcome")
    } 
    print(q)
}

###Ranking hospitals by outcome in a state

Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).

rankhospital <-function(exactstate, symptom, num="best"){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    sim <-outcome[,c(2,7,11,17,23)]     ## data.frame scale ##
    ordersim <- sim[order(sim[,2]),]    ## sort by state ##
    splitsim <- split(ordersim,ordersim$State)    ## data.frame split ##
    

    
    state <- vector("character", length=nrow(ordersim))     ## ensure exactstate ## 
    state[1] <- ordersim[1,2]
    nnrow <- nrow(ordersim)-1
    for(i in 1:nnrow){
        if(ordersim[i,2]!=ordersim[i+1,2]){
            state[i+1] <- ordersim[i+1,2]
        }
    }
    c <- state!=""
    existstate <- state[c] 
    for(i in 1:length(existstate)){
        if(exactstate==existstate[i]){
            s <- splitsim[[i]]
        }
    }

    ord <- function(j){      ## order function construction ##
        j[order(j[,1]),]
    }
        
    names(s) <- c("Hospital","State","heart attack", "heart failure", "pneumonia")  ## ensure symptom ##     
    sym <- names(s)
    if(symptom=="heart attack"||symptom=="heart failure"||symptom=="pneumonia"){    ## ensure symptom spells correct ##
        for(i in 3:5){
            s[,i] <- as.numeric(s[,i])
            if(symptom==sym[i]){
                final <- s[complete.cases(s[,i]),]      ## NA rows excluded in dataframe ##
                orderfinal <- final[order(final[,i]),]       
                splitfinal <- split(orderfinal,orderfinal[[i]])   ## split by symptom rate ##     
                orderrank <- lapply(splitfinal,ord)     ## order hospital name alphabetically ##
                unsplitfinal <- unsplit(orderrank,orderfinal[[i]])
                break
            }
        }    
    }else{
        stop("invalid outcome")
    }    
    addfinal <- cbind(Rank=1:nrow(unsplitfinal),unsplitfinal)    ## add "Rank" column ##           

    if(num<=nrow(addfinal)||num=="best"||num=="worst"){
        for(j in 1:nrow(addfinal)){
            if(num==addfinal[j,1] ){
                q <- (addfinal[j,2])
            }else if(num=="worst"){
                q <- (addfinal[nrow(addfinal),2])
            }else if(num=="best"){
                q <- (addfinal[1,2])
            }
        }
    }else{
        q <- NA
    }    
    print(q)
}

###Ranking hospitals in all states

Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-ing (num). 
The function reads the outcome-of-care-measures.csv and returns a 2-column data frame containing the hospital in each state that has the ranking specified in num.

rankall <- function(symptom, num="best"){
    ord <- function(j){      ## order function construction ##
        j[order(j[,1]),]
    }
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    sim <-outcome[,c(2,7,11,17,23)]     ## data.frame scale ##
    ordersim <- sim[order(sim[,2]),]    ## sort by state ##
    splitsim <- split(ordersim,ordersim$State)    ## data.frame split ##
        
    state <- vector("character", length=nrow(ordersim))     ## ensuere exactstate ## 
    state[1] <- ordersim[1,2]
    nnrow <- nrow(ordersim)-1
    for(i in 1:nnrow){
        if(ordersim[i,2]!=ordersim[i+1,2]){
            state[i+1] <- ordersim[i+1,2]
        }
    }
    
    c <- state!=""
    existstate <- state[c] 
    bind <- data.frame(character(),character(),character(),character())
    
    for(i in 1:length(existstate)){
        s <- splitsim[[i]]
        names(s) <- c("Hospital","State","heart attack", "heart failure", "pneumonia")  ## ensure symptom ##     
        sym <- names(s)        
        if(symptom=="heart attack"||symptom=="heart failure"||symptom=="pneumonia"){    ## ensure symptom spells correct ##
            for(k in 3:5){
                s[,k] <- as.numeric(s[,k])
                if(symptom==sym[k]){
                    final <- s[complete.cases(s[,k]),]      ## NA rows excluded in dataframe ##
                    orderfinal <- final[order(final[,k]),]       
                    splitfinal <- split(orderfinal,orderfinal[[k]])   ## split by symptom rate ##     
                    orderrank <- lapply(splitfinal,ord)     ## order hospital name alphabetically ##
                    unsplitfinal <- unsplit(orderrank,orderfinal[[k]])
                    addfinal <- cbind(Rank=1:nrow(unsplitfinal),unsplitfinal)    ## add "Rank" column ##
                    break
                }
            }    
        }else{
            stop("invalid outcome")
        }
        bind <- rbind(bind,addfinal)
    }
    bindsplit <- split(bind,bind$Rank)      
    l <- max(bind[,1])
    for(i in 1:l){
        if(num==i){
            group <- bindsplit[[i]]
            print(group[,c(2,3)])
            break
        }else if(num=="worst"){
            group <- bindsplit[[l]]
            print(group[,c(2,3)])
            break
        }
    }
} 
.

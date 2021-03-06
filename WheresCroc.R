#Finds the shortest path on the grid with Astar algorithm all costs to 1.
#Returns a string with the shortest path with all nodes, example "23 21 19 18"
findBestPathToGoal=function(startNode,goalNode,edges){
  

  

  #Initialized a data frame with the frontier list
  frontier <- data.frame( 
    name = c(startNode), 
    cost_here = c(0),
    steps_here = "",
    stringsAsFactors=FALSE
  )
  
  #Initialize a data frame with the visited list
  visited <- data.frame(
    name = c("Dummy"), 
    cost_here = c(0),
    steps_here = c(0),
    stringsAsFactors=FALSE
  )
  
  visited <- visited[-1,] #removing dummy value from visited


  while (nrow(frontier)!=0) {
    frontier <- frontier[with(frontier, order(cost_here)),] #sort frontier by cost_here
    frontier <- frontier[!duplicated(frontier[,c('name')]),] #remove duplicates in frontier
    visited <- visited[!duplicated(visited[,c('name')]),] #remove duplicates in visited
    visited[nrow(visited)+1,] <- frontier[1,] #first in frontier to last in visited
  #print(startNode)
  if (frontier$name[[1]]==goalNode) {
    break


    
  }
    
    
  
    options <- getOptions(frontier$name[[1]],edges) #Which neighbors has the first node in the frontier
    options <- options[-length(options)] #Removes itself from this list
 
    sample(options)

    #Iterates through all neighbors and adds them to the frontier
    for (i in 1:length(options)){
      
      newNeighbor <- data.frame(
        name = c(options[i]), 
        cost_here = c(frontier$cost_here[[1]]+1), #Cost for all edges are 1
        steps_here = c(paste(frontier$steps_here[[1]],options[i])),
        stringsAsFactors=FALSE
      )
      frontier <- rbind(frontier,newNeighbor)
    }
    
  frontier <- frontier[-1,] #remove first node in frontier
  }
  pathToGoal <- trimws(frontier$steps_here[[1]]) #remove whitespace
  #print(pathToGoal)
  return(pathToGoal) #return string with path from startNode to goalNode
}

#Gets the proportional probability from a normal distribution
getProbabilityFromND <- function(value, mean, std) {
  cutOff = 0.5
  valueHigh = value+cutOff
  valueLow = value-cutOff
  returnMe <- pnorm(valueHigh, mean=mean, sd=std) - pnorm(valueLow, mean=mean, sd=std)
  return(returnMe)
}


#Gets the probability from a single waterhole this current state
calcProbInWaterhole=function(waterhole, readings, probs, listOfPrevProbs, edges) {

  #Salinity probability from normal distribution
  probSalinity <- getProbabilityFromND(readings[1],probs$salinity[waterhole,1],probs$salinity[waterhole,2])
  
  #Phosphate probability from normal distribution
  probPhosphate <- getProbabilityFromND(readings[2],probs$phosphate[waterhole,1],probs$phosphate[waterhole,2])

  #Nitrogen probability from normal distribution
  probNitrogen <- getProbabilityFromND(readings[3],probs$nitrogen[waterhole,1],probs$nitrogen[waterhole,2])
  
  #3 independent probabilities times eachother in this current state 
  independentObsProb <- probSalinity*probPhosphate*probNitrogen
  
  #The possible different edges from other waterholes
  waterholeOptions <- getOptions(waterhole,edges)
  

  #Iterate through all possible ways to this node, including from itself and add them up
  moveProb = 0
  for (i in 1:length(waterholeOptions)) {
    tempNode <- waterholeOptions[i]
    moveProb = moveProb + (1.0/length(getOptions(tempNode,edges)))*listOfPrevProbs$prob[which(listOfPrevProbs$name==tempNode)]
  }
  return (independentObsProb*moveProb)
}


#Get the probability for each waterhole
#Returns "listOfProbs" a data frame with all holes and probabilties
getProbInAllWaterholes=function(positions,readings, probs, listOfPrevProbs, edges, searchedHole) {
  #Initialize a empty data frame "listOfProbs" that will later 
  #contain all probabilities for all waterholes 
  listOfProbs <- data.frame( 
    name = c(0), 
    prob = c(0.0),
    stringsAsFactors=FALSE
  )

  
  if (sum(length(listOfPrevProbs$prob))==0) { #If this is the first turn the 
                                              #previous probability list is empty, fill it
    listOfPrevProbs <- data.frame()
    possibleWaterholes = 0

    #How many waterholes can the croc be at? If a tourist dies it's 1 less
    
    for (i in 1:40) { #Fills the dataframe listOfPrevProbs with 1 prob on all holes
      tempProb <- data.frame( 
        name = c(i), 
        prob = c(1.0),
        stringsAsFactors=FALSE
      )
      listOfPrevProbs <- rbind(listOfPrevProbs, tempProb)
      
      #If tourist #1 is at a waterhole and croc is there set prob to 1
      if (!is.na(positions[1])) {
        if (positions[1]*(-1)==i) {
          listOfPrevProbs$prob[i] <- 1
        }
      #If tourist #2 is at a waterhole and croc is there set prob to 1
      if(!is.na(positions[2])){
        if (positions[2]*(-1)==i) {
          listOfPrevProbs$prob[i] <- 1
        }
      }
        #if the tourists is at a waterhole, croc is not there set prob to 0
        if (positions[1]==i || (positions[2]==i)) {
          listOfPrevProbs$prob[i] <- 0
        }
        else{
          possibleWaterholes=possibleWaterholes+1
        }
      }
    }
    #Probability is (1 / possible waterholes)
    listOfPrevProbs$prob = sapply(listOfPrevProbs$prob, function(x) x/possibleWaterholes)
  }

  #If it is not the first turn:
  #Initializes data frame 'listOfProbs' and fills it with dummy
  listOfProbs <- data.frame()
  for (i in 1:40) {
    dummy <- data.frame( 
      name = c(i), 
      prob = c(0),
      stringsAsFactors=FALSE)
    listOfProbs <- rbind(listOfProbs, dummy)
  }
  #print(listOfProbs)
  
  #croc eaten tourist #1 set prob to 100% on that node, 0% rest
  if (!is.na(positions[1]) && positions[1]<0) {
      crocNode <- -positions[1]
      listOfProbs$prob <- 0
      listOfProbs$prob[crocNode] <- 1
  }
  
  #croc eaten tourist #2 set prob to 100% on that node, 0% rest
  else if (!is.na(positions[2]) && positions[2]<0) {
      crocNode <- -positions[2]
      listOfProbs$prob <- 0
      listOfProbs$prob[crocNode] <- 1
  }
  
  #Fill upp all 40 waterholes with probabilities in this current state
  else {
    for (i in 1:40) {
      if (i==searchedHole) { #Searched this hole set prob to 0
        listOfProbs$prob[[i]]=0
      }
      else if (!is.na(positions[1]) && i==positions[1]){ #Tourist #1 is here and alive set prob to 0
        listOfProbs$prob[[i]]=0
      }
      else if (!is.na(positions[2]) && i==positions[2]) { #Tourist #2 is here and alive set prob to 0
        listOfProbs$prob[[i]]=0
      }
      else {
        listOfProbs$prob[[i]]=calcProbInWaterhole(i,readings,probs,listOfPrevProbs,edges)
      }
    }
  }

  #Normalize probabilities
  totalProb = sum(listOfProbs$prob)
  listOfProbs$prob = sapply(listOfProbs$prob, function(x) x/totalProb)
  
  

  return(listOfProbs)
}


#Our callable Hidden Markov Model function that wheresCroc calls
ourWC=function(moveInfo,readings,positions,edges,probs) {

  #If this is the first turn, initialize a empty data frame "oneTurnPrevProbs" in mem that 
  #will later contains all probabilities for all waterholes one turn ago
  
  if (length(moveInfo[["mem"]]) == 0) { 
    oneTurnPrevProbs <- data.frame()
    moveInfo[["mem"]][["oneTurnPrevProbs"]] <- oneTurnPrevProbs 
    lastRoundSearch <- 0
    moveInfo$mem <- c(moveInfo$mem, lastRoundSearch)
  }
  
  #If player searched for Croc last round it is saved that hole in 'searchedHole' 
  searchedHole <- moveInfo[["mem"]][[2]]
  
  #Saves the previous probabilites from the last turn here
  listOfPrevProbs <- moveInfo[["mem"]][["oneTurnPrevProbs"]]
  
  #Get the probability for each water hole and put it in the data frame 
  #"listOfProbs" with the probability
  listOfProbs <- getProbInAllWaterholes(positions,readings,probs,listOfPrevProbs,edges,searchedHole)
  
  #print(listOfProbs)
  #Sort data frame listOfProbs by probability
  listOfProbs <- listOfProbs[with(listOfProbs, order(prob,decreasing = TRUE)),]
  
  listOfProbs <- listOfProbs[!duplicated(listOfProbs[,c('name')]),] #remove duplicates in listOfProbs
 
  #print(head(listOfProbs)) #See top holes

  #Set goal node to the most probable node in the list
  goalNode <- listOfProbs[1,]$name
  
  #Find the path to the goalnode
  pathToGoal <- findBestPathToGoal(positions[3],goalNode,edges)

  pathToGoal <- as.integer(unlist(strsplit(pathToGoal," ")))
  
  
  
  #options <- options[-length(options)] #Removes itself from this list
  
  
    
    
    

  move1 = pathToGoal[1]
  move2 = pathToGoal[2]
    
  #When to search, if we are on the goalnode or if we are 1 node away
  if (is.na(pathToGoal[1])) {
    move1 = 0
  }
  if (is.na(pathToGoal[2])) {
    move2 = 0
  }

  #Save the current probabilities-list as the previous turns probabilities-list, for the next round
  moveInfo[["mem"]][["oneTurnPrevProbs"]] <- listOfProbs 

  #Set the 2 next moves into moveInfo
  moveInfo$moves <- c(move1,move2)
  return(moveInfo)
}


#' Run Where's Croc
#' 
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park. 
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record 
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also 
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in 
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game. 
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fiels. The first is a vector of numbers called 'moves', where you will enter 
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the 
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A 
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current 
#' location. (3) A vector giving the positions of the two tourists and yourself. If a tourist
#' has just been eaten by Croc that turn then the position will be NA. (4) a matrix giving the 
#' edges paths between waterholes (edges) present. (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector 
#' and any changes to the 'mem' field you wish to access later on.
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Ignore this.
#' @return A string describing the outcome of the game.
#' @export
#' 
runWheresCroc=function(makeMoves,showCroc=F,pause=1) {
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list())
  while (!is.na(positions[1])) {
    #message("Croc @ node: ", positions[1]) #show croc in console
    #message("Tourist#1 @ node: ", positions[2]) #show tourist1 in console
    #message("Tourist#2 @ node: ", positions[3]) #show tourist2 in console
    #message("Player @ node: ", positions[4]) #show player in console
    move=move+1
    positions[1]=sample(getOptions(positions[1],edges),1)
    if (!is.na(positions[2])&&positions[2]>0) {
      positions[2]=sample(getOptions(positions[2],edges),1)
    } else if (!is.na(positions[2]) && positions[2]<0) {
      positions[2]=NA
    }
    if (!is.na(positions[3])&&positions[3]>0) {
      positions[3]=sample(getOptions(positions[3],edges),1)
    } else if (!is.na(positions[3]) && positions[3]<0) {
      positions[3]=NA
    }
    if (!is.na(positions[2]) && positions[2]==positions[1]) {
      positions[2]=-positions[2]
    }
    if (!is.na(positions[3]) && positions[3]==positions[1]) {
      positions[3]=-positions[3]
    }
    plotGameboard(points,edges,move,positions,showCroc)
    
    Sys.sleep(pause)
    
    readings=getReadings(positions[1],probs)
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m==0) {
        if (positions[1]==positions[4]) {
          #print(paste("Congratualations! You got croc at move ",move,".",sep=""))
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }      
    }
  }
}
#' @export
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @export
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))
  
  return (edges)
}

#' @export
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @export
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}



#' @export
plotGameboard2=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)      
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}
plotGameboard=function(points,edges,move,positions,showCroc) {
}
#' @export
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}
#Gets manhattan distance from 1 Node(x=startX, y=startY) to another Node(x=goalX, y=goalY)
getMDDistance=function(startX,startY,goalX,goalY) {
  return (abs(startX-goalX) + abs(startY-goalY))
}

#Returns the 3rd character in a string
processStepsHereString=function(steps){
  return (substr(steps,3,3))
}

#Creates a Data.frame with a new 'Node' in the graph
addNeighbors=function(v,x,y,frontier,start,goal,minEdgeCost,edgeHereValue) {
    temp <- data.frame(
      id = c(frontier$id[[1]]+v), 
      x = c(x),
      y = c(y),
      h_cost = getMDDistance(start[1],start[2],goal[1],goal[2])*minEdgeCost,
      cost_and_heru = edgeHereValue+frontier$actual_cost_here[[1]]+getMDDistance(x,y,goal[1],goal[2])*minEdgeCost,#TODO roads  
      actual_cost_here = c(frontier$actual_cost_here[[1]]+edgeHereValue),  #TODO roads  
      steps_here = c(paste(frontier$steps_here[[1]],v)),
      stringsAsFactors=FALSE
  )
    return (temp)
}

#Returns the cost of traveling on an edge.
#From: the first node in frontier
#To: The coord x=newX, y=newY
getEdgeFromRoads=function(roads,frontier,newX,newY) {
  edgeCost = 1
  if(frontier$y[[1]]==newY) { #going hori
    if (frontier$x[[1]]<newX) { #right in hori
      edgeCost <- (roads$hroads[frontier$y[[1]],frontier$x[[1]]])
    }
    else if (frontier$x[[1]]>newX){ #left in hori
      edgeCost <- (roads$hroads[frontier$y[[1]],newX])
    }
    else { #should not be possible
      print("Error in getEdgeFromRoads - should not come here 1!")
    }
  }
  else if (frontier$x[[1]]==newX) { #going verti
    if (frontier$y[[1]]<newY) { #up in verti
      edgeCost <- (roads$vroads[frontier$y[[1]],frontier$x[[1]]])
    }
    else if (frontier$y[[1]]>newY) { #down in verti
      edgeCost <- (roads$vroads[newY,frontier$x[[1]]])
    }
    else { #should not be possible
      print("Error in getEdgeFromRoads - should not come here 2!")
    }
  }
  else { #should not be possible
    print("Error in getEdgeFromRoads - should not come here 3!")
  }
  return(edgeCost)
}

#Given a 'packages' matrix, calculates the total manhattan distance from:
#x=1,y=1                  -> package #1 pickup coords +
#package #1 pickup coords -> package #1 delievering coords +
#package #2 pickup coords -> package #2 delievering coords +
#package #3 pickup coords -> package #3 delievering coords +
#package #4 pickup coords -> package #4 delievering coords +
#package #5 pickup coords -> package #5 delievering coords +
totalManhattanDistanceInMatrix=function(packages){
  startToP1start <- abs(1-packages[1,1])+abs(1-packages[1,2])
  p1StartToP1End <- getMDDistance(packages[1,1],packages[1,3],packages[1,2],packages[1,4])
  p1EndToP2Start <- getMDDistance(packages[2,2],packages[1,4],packages[1,3],packages[2,1])
  p2StartToP2End <- getMDDistance(packages[2,1],packages[2,3],packages[2,2],packages[2,4])
  p2EndToP3Start <- getMDDistance(packages[3,2],packages[2,4],packages[2,3],packages[3,1])
  p3StartToP3End <- getMDDistance(packages[3,1],packages[3,3],packages[3,2],packages[3,4])
  p3EndToP4Start <- getMDDistance(packages[4,2],packages[3,4],packages[3,3],packages[4,1])
  p4StartToP4End <- getMDDistance(packages[4,1],packages[4,3],packages[4,2],packages[4,4])

  matrixTotalDistance <- (startToP1start + p1StartToP1End + p1EndToP2Start +
                          p2StartToP2End + p2EndToP3Start + p3StartToP3End + 
                          p3EndToP4Start + p4StartToP4End)
  return(matrixTotalDistance)
}

#Given a 'packages' matrix, find all possible permutations of the rows, calculates the total 
#manhattan distance of all the permutations and returns the best permutation
findBestTotalManhattanDistanceInMatrix=function(packages){
  listOfManhattanNumbers <- list()
  temp = 100
  for (i in 1:5) {
    for (j in 1:5) {
      for (k in 1:5) {
        for (l in 1:5) {
          for (m in 1:5) {
            if (i!=j && i!=k && i!=l && i!=m && j!=k && j!=l && j!=m && k!=l && k!=m && l!=m) {
              newMatrix = matrix(  
                c(packages[i,1],packages[i,2],packages[i,3],packages[i,4],packages[i,5],
                  packages[j,1],packages[j,2],packages[j,3],packages[j,4],packages[j,5],
                  packages[k,1],packages[k,2],packages[k,3],packages[k,4],packages[k,5],
                  packages[l,1],packages[l,2],packages[l,3],packages[l,4],packages[l,5],
                  packages[m,1],packages[m,2],packages[m,3],packages[m,4],packages[m,5]),
                  ncol = 5,
                  nrow = 5,
                  byrow=T) 
              #message(i, + j, + k, + l, + m, " gives total M distance :", totalManhattanDistanceInMatrix(newMatrix))
              listOfManhattanNumbers = c(listOfManhattanNumbers, totalManhattanDistanceInMatrix(newMatrix))
              
              if (temp > totalManhattanDistanceInMatrix(newMatrix)) {
                order = (i*10000+j*1000+k*100+l*10+m)
                vectorWithOrder <- matrix(c(i,j,k,l,m),nrow=5)
                newPackages <- newMatrix
                temp = totalManhattanDistanceInMatrix(newMatrix)
              }
            }
          }
        }
      }
    }
  }
  #message("Reordered packages to: ", vectorWithOrder)
  return(newPackages)
}

#Calculates the optimal path from one Node to another, returns the next move.
Astar=function(start,goal,roads,car,packages) {
  
  frontier <- NULL
  visited <- NULL

  #create frontier list
  frontier <- data.frame( 
    id = c(1), 
    x = c(start[1]),
    y = c(start[2]),
    h_cost = getMDDistance(start[1],start[2],goal[1],goal[2]),
    cost_and_heru = getMDDistance(start[1],start[2],goal[1],goal[2]),
    actual_cost_here = c(0),
    steps_here = c(0),
    stringsAsFactors=FALSE
  )
  
  #create visited list
  visited <- data.frame(
    id = c("Dummy"), 
    x = c(start[1]),
    y = c(start[2]),
    h_cost = c(0),
    cost_and_heru = c(0),
    actual_cost_here = c(0),
    steps_here = c(5),
    stringsAsFactors=FALSE
  )
  visited <- visited[-1,] #removing dummy value from visited

  gridSize = nrow(roads$hroads)
  minEdgeCost <- min(min(roads$vroads),min(roads$hroads))
  
  while (nrow(frontier)!=0) {
    frontier <- frontier[with(frontier, order(cost_and_heru)),] #sort frontier by actual_cost+h
    visited <- visited[!duplicated(visited[,c('x','y')]),] #remove duplicates in visited
    visited[nrow(visited)+1,] <- frontier[1,] #first in frontier to last in visited

    #if we are on the goalnode, break loop
    if (frontier$x[[1]]==goal[1] && frontier$y[[1]]==goal[2]) { 
      break
    }

    #Coordinates for all the neighbors
    btmX = frontier$x[[1]]
    btmY = frontier$y[[1]]-1
    
    leftX = frontier$x[[1]]-1
    leftY = frontier$y[[1]]
    
    rightX = frontier$x[[1]]+1
    rightY = frontier$y[[1]]
    
    topX = frontier$x[[1]]
    topY = frontier$y[[1]]+1

    #Adding bottom neighbor
    if(frontier$y[[1]]>1){ #Do not add nodes outside grid
      btmEdgeCost <- getEdgeFromRoads(roads,frontier,btmX,btmY)
      btmneighbor <- addNeighbors(2,btmX,btmY,frontier,start,goal,minEdgeCost,btmEdgeCost) #create node
      if (!(any(visited$x %in% btmneighbor$x & visited$y %in% btmneighbor$y))) { #add node it's not in visited
        frontier <- rbind(frontier,btmneighbor)
      }
    }
    #Adding left neighbor
    if(frontier$x[[1]]>1){ #Do not add nodes outside grid
      leftEdgeCost <- getEdgeFromRoads(roads,frontier,leftX,leftY)
      leftneighbor <- addNeighbors(4,leftX,leftY,frontier,start,goal,minEdgeCost,leftEdgeCost) #create node
      if (!(any(visited$x %in% leftneighbor$x & visited$y %in% leftneighbor$y))) { #add node it's not in visited
        frontier <- rbind(frontier,leftneighbor)
      }
    }
    #Adding right neighbor
    if(frontier$x[[1]]<gridSize){ #Do not add nodes outside grid
      rightEdgeCost <- getEdgeFromRoads(roads,frontier,rightX,rightY)
      rightneighbor <- addNeighbors(6,rightX,rightY,frontier,start,goal,minEdgeCost,rightEdgeCost) #create node
      if (!(any(visited$x %in% rightneighbor$x & visited$y %in% rightneighbor$y))) { #add node it's not in visited
        frontier <- rbind(frontier,rightneighbor)
      }
    }
    #Adding top neighbor
    if(frontier$y[[1]]<gridSize){ #Do not add nodes outside grid
      topEdgeCost <- getEdgeFromRoads(roads,frontier,topX,topY)
      topneighbor <- addNeighbors(8,topX,topY,frontier,start,goal,minEdgeCost,topEdgeCost) #create node
      if (!(any(visited$x %in% topneighbor$x & visited$y %in% topneighbor$y))) { #add node it's not in visited
        frontier <- rbind(frontier,topneighbor)
      }
    }
    frontier <- frontier[-1,] #remove first node in frontier
    
    #Want to print frontier+visited+path+nextmove?
    if(FALSE){ 
    print("this is the Visited list")
    print(visited)
    print("this is the Frontier list")
    print(frontier)
    print(visited$steps_here[which(visited$x == goal[1] & visited$y == goal[2])]) #printing optimal path
    print(nextMove)
    }
  }
  
  #convert string of optimal path just the first step with 1 char
  nextMove <-processStepsHereString(visited$steps_here[which(visited$x == goal[1] & visited$y == goal[2])]) 
  return(nextMove)
}

#Our implementation of the Astar algorithm in action with the Delievery Man game
aStarDM=function(roads,car,packages) {
  optimizedOrderPackages <- findBestTotalManhattanDistanceInMatrix(packages)
  startNode <- getStartNode(car)
  
  #Want to print stuff?
  if(FALSE){ 
  print("_Org_packages_______")
  print(packages)
  message("Minimal turns: ",totalManhattanDistanceInMatrix(packages))
  print("_Optimized_packages_")
  print(optimizedOrderPackages)
  message("Minimal turns: ",totalManhattanDistanceInMatrix(optimizedOrderPackages))
  print("____________________")
  }
  
  
  
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(optimizedOrderPackages[,5]==0)[1]
  } else {
    toGo=car$load  
    toGo=which(optimizedOrderPackages[,5]==1)
    offset=2
  }
  goalNode <- c(optimizedOrderPackages[toGo,1+offset],optimizedOrderPackages[toGo,2+offset])
  car$nextMove <- Astar(startNode,goalNode,roads,car,optimizedOrderPackages)
  return (car)
}


basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load  
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  
  return (car)
}


manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#' Run Delivery Man
#' 
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are 
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be 
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param doPlot Specifies if you want the game state to be plotted each turn.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,
                            doPlot=T,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    #print(car$wait)

        roads=updateRoads(roads$hroads,roads$vroads)
    if (doPlot) {
      makeDotGrid(dim,i) 
      plotRoads(roads$hroads,roads$vroads) 
      points(car$x,car$y,pch=16,col="blue",cex=3)  
      plotPackages(packages)      
    }
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }      
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    if (pause>0) Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  } 
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")    
  }
  car$nextMove=NA
  return (car)
} 

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0) 
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {
  
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }    
  }
  list (hroads=hroads,vroads=vroads)
}



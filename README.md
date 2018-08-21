# Artificial-Intelligence #
Two assignments in the course Artificial Intelligence - 1DL340

**Language: R**

**Assignment 1:**
DeliveryMan.R - The Delivery Man (A*) 
Detalis: Move around nodes and collect packages and deliver them to a specific destinations. Colors on edges translates into different traveltimes. These traveltimes are randomized each move made. An implementation of the A-star algorithm to find the dynamiclly changing optimal shortest route to collect all packages and retrieve them was made.  
- Blue:You
- Green:Packages
- Red:Destination

run with:

	runDeliveryMan(functionName,gridSize,maxTurns,doPlot,delay,noOfpackages)

example:

	runDeliveryMan(aStarDM,10,2000,T,0.05,5)

![dm](https://github.com/JohanWindahl/Artificial-Intelligence/blob/master/dm.gif)


**Assignment 2:**
WheresCroc.R - Where’s Croc (Hidden Markov models)
Detalis: Move around waterholes (nodes), find the moving invisible crocodile (only shown here for viewing purposes) by standing on the correct waterhole and searching. Three water readings are collected each turn from a standard distribution where the crocodile is at. In the start we get the standard distributions for all water holes. Orange nodes are backpackers, if croc finds them he eats them. Which is the optimal moves to make? Which waterhole has the highest probablity that croc is in it? A hidden markov model was created to find that hole, and make the best decision.
- Green:You
- Red:Crocodile
- Orange:Backpackers

run with:

	runWheresCroc(functionName,doPlot,delay)

example:

	runWheresCroc(ourWC,T,1)

![wc](https://github.com/JohanWindahl/Artificial-Intelligence/blob/master/wc.gif)

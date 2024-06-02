# Add your group details ...

#' myFunction
#' 
#' You can test the outcome of myFunction using the following lines of code. It 
#' will print out the results generated against the test function. Note that the
#' results will vary depending on the solution and approach. Please refer to the
#' Passing Requirements at the Studium assignment page.
#' 
#' > library(WheresCroc)
#' > testWC(myFunction, verbose = 1)
#' 
#' Mean moves: 5.444
#' SD moves: 3.853
#' Time taken: 6.5 seconds.[1] 5.444
#' 
#' -----------------------------------------------------------------------------
#' Available parameters to myFunction. 
#' (More details at runWheresCroc documentation in WheresCroc package)
#' 
#' @param moveInfo = list(moves = c(move1, move2), mem)
#' @param readings = c(salinity, phosphate, nitrogen)
#' @param positions = c(pos1, pos2, pos3)
#' @param edges = matrix(ncol=2)
#' @param probs = list(mat1, mat2, mat3)
#' 
#' @return See runWheresCroc for details
#' 
#' @export
#'
library(WheresCroc)
#get from wheresCroc.r
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

emissionsVector <- function(readings, probs){
  salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  
  prob = replicate(40, 0)
  for (i in 1:40) {
    prob[i] = salinity[i] * phosphate[i] * nitrogen[i]
  }
  sum = sum(prob)
  for (i in 1:40) {
    prob[i] = prob[i] / sum # normalize
  }
  return(prob) # prob of each pos for croc given readings (Vector)
}

#Create a transition matrix. Only done once at the start of a new game
transitionMatrix <- function(edges) {
  matrix = matrix(0, nrow = 40, ncol = 40)
  transitionMatrix = matrix(matrix, nrow = 40)
  for (i in 1:40){
    neighbors = getOptions(i, edges)
    for (n in neighbors){
      transitionMatrix[i,n] = 1/length(neighbors)
    }
  }
  return(transitionMatrix)
}

# Breath-first search to find a path to goal node
bfs <- function(goal, ourPos, edges) {
  if (ourPos == goal){
    return(c(0,0))
  }
  else {
    queue = list(list(pos = ourPos, path = c()))
    visited = c(ourPos)
    
    while (length(queue) != 0){
      node = queue[[1]]
      if (node$pos == goal){
        if (length(node$path) <= 2){return(c(node$path[1],0))}
        else{return(c(node$path[1],node$path[2]))}
      }
      else{
        queue = queue[-1]
        neighbors = getOptions(node$pos, edges)
        for (n in neighbors){
          if (!(n %in% visited)) {
            newNode = list(pos = n, path = c(node$path, n))
            queue = append(queue, list(newNode))
            visited = c(visited, c(n))
          }
        }
      }
    }
  }
}

# Hidden markov to find which waterhole to search in
hiddenMarkov <- function(transitionMatrix, prevProb, readings, positions, edges, probs){
  emissions = emissionsVector(readings, probs)
  newProb = prevProb%*%transitionMatrix
  markovProb = newProb*emissions
  return (markovProb)
}

myFunction <- function(moveInfo, readings, positions, edges, probs){
  # Set up a new game
  if (moveInfo$mem$status == 0 || moveInfo$mem$status == 1) {
    moveInfo$mem$prevProb <- replicate(40,1)
    moveInfo$mem$transitionMatrix <- transitionMatrix(edges)
  }
  
  # Find the probability of each node through HMM
  transitionMatrix <- moveInfo$mem$transitionMatrix
  prevProb <- moveInfo$mem$prevProb
  newProb <- hiddenMarkov(transitionMatrix, prevProb,readings, positions, edges, probs)
  
  
  # Check for hikers
  if (!is.na(positions[1])){
    if (positions[1]<0){
      newProb[-1*positions[1]] = 1
    }
    else{
      newProb[positions[1]] = 0
    }
  }
  if (!is.na(positions[2])){
    if (positions[2]<0){
      newProb[-1*positions[2]] = 1
    }
    else{
      newProb[positions[2]] = 0
    }
  }
  
  # Find highest probable node, and find path to that node
  goal <- which.max(newProb)
  moves <- bfs(goal, positions[3],edges)
  moveInfo$moves <- moves
  
  moveInfo$mem$prevProb <- newProb
  moveInfo$mem$status <- 2
  return(moveInfo)
}

# findMatchingNodeIndex = function(theList, neighbor){
#   if(length(theList)==0){return(0)}
#   index = which(sapply(theList,function(node)(node$currentPos==neighbor$currentPos)))
#   ifelse(length(index)==0, return(0), return(index))
# }
# 
# calculateNeighbors = function(parent, goal, edges, frontier, visited){
#   connecting_col1 = which(edges[,1]==parent$currentPos)
#   conneting_col2 = which(edges[,2]==parent$currentPos)
#   connecting_nodes = sapply(connecting_col1, function(i) edges[i,2])
#   connecting_nodes <- append(connecting_nodes, unlist(sapply(conneting_col2, function(i) edges[i,1])))
#   
#   for(node_i in connecting_nodes){
#     node = list(currentPos = node_i,
#                 path = append(node_i, parent$path),
#                 f = length(parent$path) +1
#     )
#     index = findMatchingNodeIndex(frontier,node)
#     if(index == 0){
#       if(!(node_i %in% visited)){
#         frontier <- append(list(node),frontier)
#       } 
#     }
#   }
#   
#   return(frontier)
# }
# 
# createPathList = function(startPos, goalPos, edges){
#   node = list(currentPos = startPos, 
#               path = list(),
#               f = 0)
#   frontier = list(node)
#   visited <- c()
#   repeat{
#     node_costs = sapply(frontier,function(node) node$f)
#     best_index = which.min(node_costs)
#     node_to_expand = frontier[[best_index]]
#     if(node_to_expand$currentPos == goalPos){break} #Stop if the goal node is about to be expanded
#     frontier <- calculateNeighbors(node_to_expand, goalPos, edges, frontier[-best_index],visited)
#     visited <- unique(c(visited,node_to_expand$currentPos))
#   } 
#   scores=sapply(frontier,function(node)node$f)
#   best_index=which.min(scores)
#   return(frontier[[best_index]]$path) #outputs path backwards
# }

# createTransitionMatrix <- function(edges){
#   transition_matrix <- matrix(0,nrow = 40, ncol = 40)
#   for(i in 1:length(edges[,1])){
#     transition_matrix[edges[i,1],edges[i,2]] <- 1
#     transition_matrix[edges[i,2],edges[i,1]] <- 1
#   }
#   for (i in 1:40) {
#     transition_matrix[i,i] <-1
#   }
#   transition_matrix <- (transition_matrix/rowSums(transition_matrix))
#   return(transition_matrix)
# }
# 
# createObservationMatrix <- function(readings, probs) {
#   probS <- probs$salinity
#   probP <- probs$phosphate
#   probN <- probs$nitrogen
#   crocS <- readings[1]
#   crocP <- readings[2]
#   crocN <- readings[3]
#   
#   densityS <- dnorm(crocS, probS[, 1], probS[, 2])
#   densityP <- dnorm(crocP, probP[, 1], probP[, 2])
#   densityN <- dnorm(crocN, probN[, 1], probN[, 2])
#   
#   observation_matrix <- diag(densityS * densityP * densityN, nrow = 40)
#   
#   return(observation_matrix)
# }
# 
# createStateVector <- function(positions, state_vector, transition_matrix, observation_matrix){
#   s <- state_vector
#   
#   ifelse(is.na(positions[1]), #check if backpacker 1 is eaten and adjust state accordingly
#          s[1,positions[1]] <- 0,
#          if (positions[1] < 0){
#            s[1,abs(positions[1])] <-1 #zero out all but backpacker position
#            s[s != 1] <- 0}
#          else{s[1,positions[1]] <- 0}) 
#   
#   ifelse(is.na(positions[2]), #check if backpacker 2 is eaten and adjust state accordingly
#          s[1,positions[2]] <- 0,
#          if (positions[2] < 0){
#            s[1,abs(positions[2])] <-1 #zero out all but backpacker position
#            s[s != 1] <- 0}
#          else{s[1,positions[2]] <- 0})
#   
#   s <- s%*%transition_matrix%*%observation_matrix
#   s <- (s/rowSums(s))
#   return(s)
# }
# 
# 
# 
# myFunction = function(moveInfo,readings,positions,edges,probs) {
#   
#   if(moveInfo$mem$status == 0){ 
#     state_vector <- matrix(1/39, nrow = 1, ncol = 40)
#     state_vector[positions[3]] <- 0 #croc cannot start where ranger starts
#     
#     transition_matrix <- createTransitionMatrix(edges)
#     
#     #precalculate what different moves to make in different situations
#     nextMove = matrix(list(), nrow = 40, ncol = 40)
#     for (r in 1:40){#row = ranger position, column = croc position
#       for (c in 1:40){
#         nextMove[r,c] <- list(createPathList(r,c,edges))}}
#     
#     moveInfo$mem$state <- state_vector #save
#     moveInfo$mem$status <- 2  
#     moveInfo$mem$transition <- transition_matrix
#     moveInfo$mem$nextMove <- nextMove
#   }
#   else if(moveInfo$mem$status==1){ 
#     moveInfo$mem$status <- 2 
#     state_vector <- matrix(1/39, nrow = 1, ncol = 40)
#     state_vector[positions[3]] <- 0 #croc cannot start where ranger starts
#     transition_matrix <- moveInfo$mem$transition
#   }
#   
#   else{ 
#     state_vector <- moveInfo$mem$state
#     transition_matrix <- moveInfo$mem$transition
#   }
#   
#   observation_matrix <- createObservationMatrix(readings, probs)
#   state_vector <- createStateVector(positions, state_vector, transition_matrix, observation_matrix)
#   crocPos <- which.max(state_vector)
#   path <- rev(unlist(moveInfo$mem$nextMove[positions[3],crocPos]))
#   
#   #choose moves
#   if(length(path) < 1){#no more moves, just search
#     moveInfo$moves <- c(0,0)
#     state_vector[crocPos] <- 0
#   }
#   
#   else if(length(path) < 2) {
#     moveInfo$moves <-c(path[[1]], 0)
#   }
#   
#   else{
#     moveInfo$moves <-c(path[[1]],path[[2]])
#   }
#   
#   moveInfo$mem$state <- state_vector
#   return(moveInfo)
# }

# Of course you can have supporting functions outside to delegate repetitive 
# tasks or to make your code more readable.
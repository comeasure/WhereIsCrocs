# Define the gameboard points and edges (simplified for illustration)
points <- matrix(c(1, 1, 2, 2, 3, 3), ncol = 2)
edges <- matrix(c(1, 2, 2, 3), ncol = 2)

# Function to get options for movement
getOptions <- function(point) {
  options <- edges[which(edges[, 1] == point), 2]
  if (length(options) == 0) {
    options <- point
  }
  return(options)
}

# Function to run the game
runWheresCroc <- function() {
  # Initial position of Croc, BP1, BP2, and Player
  positions <- sample(1:3, 4)
  move <- 0
  
  while (!is.na(positions[1])) {
    move <- move + 1
    options <- getOptions(positions[1])
    
    if (length(options) > 0) {
      # Sample one option randomly
      selected_option <- sample(options, 1)
      
      # Update the position
      positions[1] <- selected_option
    } else {
      cat("Nowhere to move. Croc not found.\n")
      break
    }
    
    # Print the current state
    cat("Move:", move, "Positions:", positions, "\n")
  }
  
  cat("Game Over - Croc Found!\n")
}

# Run the game
runWheresCroc()

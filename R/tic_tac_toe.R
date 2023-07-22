
check_for_three <- function(grid) {
  win <- NULL
  if( all(grid$A == c("X", "X", "X")) |
      all(grid$B == c("X", "X", "X")) |
      all(grid$C == c("X", "X", "X")) |
      all(grid[1, ] == c("X", "X", "X")) |
      all(grid[2, ] == c("X", "X", "X")) |
      all(grid[3, ] == c("X", "X", "X"))) {
    win <- "Opponent"
  }
  if(all(grid$A == c("o", "o", "o")) |
     all(grid$B == c("o", "o", "o")) |
     all(grid$C == c("o", "o", "o")) |
     all(grid[1, ] == c("o", "o", "o")) |
     all(grid[2, ] == c("o", "o", "o")) |
     all(grid[3, ] == c("o", "o", "o")) ) {
    win <- "You"
  }
  if((grid[1, 3] == "X" & grid[2, 2] == "X" & grid[3, 1] == "X") |
     (grid[1, 1] == "X" & grid[2, 2] == "X" & grid[3, 3] == "X")) {
    win <- "Opponent"
  }
  if((grid[1, 3] == "o" & grid[2, 2] == "o" & grid[3, 1] == "o") |
     (grid[1, 1] == "o" & grid[2, 2] == "o" & grid[3, 3] == "o")) {
    win <- "You"
  }
  if(!is.null(win)) win
}

place_X_random <- function(grid) {
  empty <- which(grid == "-", arr.ind = T)[sample(seq(nrow(which(grid == "-", arr.ind = T))), 1), ]
  grid[empty[[1]], empty[[2]]] <- "X"
  grid
}

tic_tac_toe <- function() {
  grid <- data.frame(A = rep("-", 3), B = rep("-", 3), C = rep("-", 3))
  print(grid)
  starter <- sample(1:2, 1)
  starter <- switch(starter, "player", "opponent")
  win <- NULL
  if(starter == "player") {
    print("You start!")
    while (is.null(win)) {
      placed <- FALSE
      win <- check_for_three(grid)
      while(placed == FALSE & is.null(win)) {
        coords <- readline(prompt = "Enter Coordinates for your turn: ")
        coords <- toupper(coords)
        if(grepl("[1-3]{1}[A-C]{1}", coords)){
          coords <- unlist(strsplit(coords, split = ""))
          coords[2] <- switch(coords[2], A = 1, B = 2, C = 3)
          coords <- as.numeric(coords)
          if(grid[coords[1], coords[2]] == "-"){
            grid[coords[1], coords[2]] <- "o"
            placed <- TRUE
          }
          print("Your turn:")
          print(grid)
        }
        else print("The format of the coordinates is NumberLettter, e.g. 2C")
      }
      grid <- place_X_random(grid)
      print("Opponents turn:")
      print(grid)
      win <- check_for_three(grid)
    }
  }
  if(starter == "opponent") {
    print("Opponent starts")
    while (is.null(win)) {
      grid <- place_X_random(grid)
      print("Opponents turn:")
      print(grid)
      placed <- FALSE
      win <- check_for_three(grid)
      while(placed == FALSE & is.null(win)) {
        coords <- readline(prompt = "Enter Coordinates for your turn: ")
        coords <- toupper(coords)
        if(grepl("[1-3]{1}[A-C]{1}", coords)){
          coords <- unlist(strsplit(coords, split = ""))
          coords[2] <- switch(coords[2], A = 1, B = 2, C = 3)
          coords <- as.numeric(coords)
          if(grid[coords[1], coords[2]] == "-"){
            grid[coords[1], coords[2]] <- "o"
            placed <- TRUE
          }
          print("Your turn:")
          print(grid)
        }
        else print("The format of the coordnates is NumberLettter, e.g. 2C")
      }
      win <- check_for_three(grid)
    }
  }
  paste(win, "won!")
}



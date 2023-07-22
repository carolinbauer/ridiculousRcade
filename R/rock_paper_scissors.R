#' Play rock paper scissors
#'
#' @param choice A string. Choices are "rock", "paper", or "scissors"
#' @returns A character string with result.
#' @examples
#' rock_paper_scissors(choice = "paper")

rock_paper_scissors <- function(choice) {
  choice <- tolower(choice)
  match.arg(arg = choice, choices = c("rock", "paper", "scissors"))
  choice_opponent <- switch(sample(1:3, 1), "rock", "paper", "scissors")
  if(choice == "scissors"){
    if(choice_opponent == "scissors") result <- "It's a tie."
    if(choice_opponent == "rock") result <- "You lost."
    if(choice_opponent == "paper") result <- "You won!"
  }
  if(choice == "rock"){
    if(choice_opponent == "rock") result <- "It's a tie."
    if(choice_opponent == "paper") result <- "You lost."
    if(choice_opponent == "scissors") result <- "You won!"
  }
  if(choice == "paper"){
    if(choice_opponent == "paper") result <- "It's a tie."
    if(choice_opponent == "scissors") result <- "You lost."
    if(choice_opponent == "rock") result <- "You won!"
  }
  cat("You chose: ", choice, "\n",
      "Your opponent chose: ", choice_opponent,  "\n",
      result, sep = "")

}



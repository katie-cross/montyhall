#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Select a Door
#' @description
#'  Randomly selects a door from the three available.
#' @details
#'  Contestants pick from one of three random doors, 2 have goats 1 with car
#' @param 
#'  n/a
#' @return 
#'  Numeric value of door chosen
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Goat Door Opens
#' @description
#'  Host shows one door has a goat
#' @details
#'  If they chose a car, then host opens either. IF they chose a goat the host opens other goat door.
#' @param 
#'  game
#'  a.pick
#' @return 
#'  Value of opened door
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Switch or stay
#' @description
#'  Does the contestant keep their initial choice or switch?
#' @details
#'  Switch or stay choice
#' @param 
#'  stay
#'  opened.door
#'  a.pick
#' @return 
#'  final choice numeric number
#' @examples
#'   opened.door <- open_goat_door(create_game(), select_door())
#'   a.pick <- select_door()
#'   change_door(TRUE, opened.door, a.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'  Pick Winner
#' @description
#'  Outcome of the game
#'  
#' @details
#'  The results are shown to the contestant if they win a car
#' @param 
#'  final.pick
#' @return 
#'  WIN or LOSE 
#' @examples
#'   game <- create_game()
#'   final.pick <- change_door(TRUE, open_goat_door(game, select_door()), select_door())
#'   determine_winner(final.pick, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Play game one round
#' @description
#'  Monty Hall simulation in one go
#' @details
#'  Runs the game as one instance
#' @param 
#'  Stay and or swtich result and their win
#' @return 
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Simulate game N times
#' @description
#'  Runs loop of function for n rounds
#' @details
#'  Plays n times and presents results and win/loss rate
#' @param 
#' @return 
#'  Frame of wins and losses
#' @examples
#'   play_n_games(100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
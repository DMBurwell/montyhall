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
#' 	Select First Door
#'
#' @description
#'	This function allows for the selection of one of the three doors.
#'
#' @details
#'	One door will be selected by the participant at random.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'	The contestent selects one of the doors.
#'
#' @examples
#'	select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Host Reveals a Goat
#'
#' @description
#'	The host opens one of the doors the contestent did 
#'	not select and reveals the location of a goat.
#'
#' @details
#'	When the host opens a non-selected door and reveals a goat
#'	the contestent is then given the opportunity to reevaluate
#'	their initial selection.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'	One of the doors hiding a goat is opened.
#'
#' @examples
#'	open_goat_door()
#'
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
#'	Contestent Makes Final Selection
#'
#' @description
#'	The contestent chooses whether they will stay with their
#'	original door selection or make a new selection.
#'
#' @details
#'	This is the step where the contestent makes their final 
#'	decision as to whether they will stay with their original
#'	door selection or choose to swtich to the one remaining
#'	unopened door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'	The contestent's final selection.
#'
#' @examples
#'	change_door()
#'
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
#'	Win or Lose
#'
#' @description
#'	The contestent finds out if they won by selecting the car door
#'	or lost by selecting a goat door.
#'
#' @details
#'	If the contestent's final door selection was the door hiding 
#'	the car, then they will be informed that they won the game. If 
#'	the contestent selected a goat door, they will be informed that
#'	they lost the game.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'	Win or Lose based on selection.
#'
#' @examples
#'	determine_winner()
#'
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
#'	Play the Whole Game
#'
#' @description
#'	This function allows you to play the full game with a single command.
#'
#' @details
#'	This bundles all of the steps of playing the game into a single
#'	function for ease and speed.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'	The results of the full game.
#'
#' @examples
#'	play_game()
#'
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
#'	Repeating the Game
#'
#' @description
#'	This allows the user to replay the game for any specified
#'	number of attempts.
#'
#' @details
#'	This allows the user to replay the game for any specified
#'	number of attempts in order to gather data to compare the 
#'	success rates of contestents who choose to stay with their
#'	original door selection vs. switching to another door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'	Data for wins and losses after all games are played.
#' 
#' @examples
#'	play_n_games
#'
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

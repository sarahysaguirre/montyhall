#' @title
#'   Create a new Monty Hall Problem game.
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
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
#' @param ... no arguments are used by the function.
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#' @examples
#'   create_game()
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Contestant's First Selection
#' @description
#' 'select_door()' generates the contestant's first door selection
#' @details
#' The function records the contestant's first selection, removing it from
#' selections in our future functions. The contestant will either choose
#' doors 1-3 which were generated in our 'create_game()' function.
#' @param
#' ... no arguments are used by the function.
#' @return
#' 'a.pick' will return either 1,2 or 3
#' @examples
#' 'select_door()'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens a Goat Door
#' @description
#' 'open_goat_door()' is when the host will open 1 of the 2 remaining
#' doors to reveal a goat
#' @details
#' The function uses the 'select_door()' function as we cannot choose one of
#' doors the contestant already chose( 1, 2 or 3). The host also cannot choose
#' the door with a car behind it, hence why the function cannot equal the win.
#' @param
#'  ... no arguments are used by the function.
#' @return
#' opened.door
#' @examples
#' open_goat_door(game, a.pick )
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
#' Contestant's Final Decision
#' @description
#' Contestant decides whether they want to stick to their first pick(a.pick)
#' or switch to a different door
#' @details
#' This function states that if the contest chooses to stay with their first
#' pick (a.pick), then that is their final pick. If the contestant chooses
#' to switch doors, then they must choose the remaining door as the other two
#' were previously chosen (a.pick) and already opened by the host (opened.door)
#' @param
#' Stay=T is used in this function as part of an if/else statement. If it is
#' true the contestant stayed with their original decision compared to if they
#' chose to switch doors
#' @return
#' final.pick
#' @examples
#' change_door(stay=T, opened.door, a.pick)
#' change_door(stay=F, opened.door, a.pick)
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
#' Did They Win?
#' @description
#' The function deciphers whether the contestant's final pick was a win or
#' loss.
#' @details
#' Feeding off the 'change_door()' function, the contestant's final selection
#' (final.pick), being either doors 1-3 is determined as a win or loss
#' @param
#' ... no arguments are used by the function.
#' @return
#' If the contestant's final.pick is a car, the game returns "WIN". If they
#' lost, the game returns "LOSE."
#' @examples
#' determine_winner(final.pick,game)
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
#' The Monty Hall Game
#' @description
#' Play a full Monty Hall Game
#' @details
#' The play_game function combines our previous functions into a single
#' function and returns the results of the game each time. The results are
#' printed into a table indicating a loss or win.
#' @param
#' change_door( stay=T, opened.door, first.pick )
#' change_door( stay=F, opened.door, first.pick )
#' @return
#' game.results
#' @examples
#' A table indicating either a win or a loss
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
#' Monty Hall Game Loops
#' @description
#' Play the game n amount of times
#' @details
#' The 'play_n_games' function loops the 'play_game' function n amount of times.
#' In this case, n is set to 100. A data frame was created to list the total
#' count of losses and wins out of the 100 loops.
#' @param
#' We set n to 100 loops
#' @return
#' results.df
#' @examples
#' play_n_games(n = 10000)
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

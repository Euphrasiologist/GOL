#' Polyaurn function
#'
#' Another implementation of the urn game from Games of Life by Karl Sigmund (ISBN: 978-0198547839)
#' This time, an example of runaway drift with an unlikely twist.
#' @param red number of red balls
#' @param black number of black balls
#' @param trials number of trials in game
#' @keywords polyaurn, gamesoflife, genetics
#' @export
#' @examples
#' urn(1,1,1000)

polyaurn <- function(red, black, trials){

  dat <- data.table::data.table(trial = 1:trials,
                    colour = vector(length = trials))

  for(i in 1:trials){
    if(sample(x = c(rep("red", red), rep("black", black)), size = 1, replace = T, prob = rep(1/(red+black), sum(red, black))) == "red"){
      red <- red + 1
      dat$colour[i] <- sample(x = c(rep("red", red), rep("black", black)), size = 1, replace = T, prob = rep(1/(red+black), sum(red, black)))
    } else
      black <- black + 1
    dat$colour[i] <- sample(x = c(rep("red", red), rep("black", black)), size = 1, replace = T, prob = rep(1/(red+black), sum(red, black)))
  }

  dat$outcome <- as.numeric(dat$colour == "red")

  dat <- dat[, cum.sum := cumsum(outcome)/trial]


  return(list(ggplot(dat, aes(x = trial, y = cum.sum))+
                geom_hline(yintercept = 0.5, col = "red", size=0.3)+
                geom_line()+
                xlab(label = "Trial Number")+
                ylab(label = "P(red)")+
                theme_bw(base_line_size = 0),
              dat,
              table(dat$colour)))
}

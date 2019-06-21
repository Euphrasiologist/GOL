#' Urn function
#'
#' Implementation of the urn game from Games of Life by Karl Sigmund (ISBN: 978-0198547839)
#' @param red number of red balls
#' @param black number of black balls
#' @param trials number of trials in game
#' @keywords urn, gamesoflife, genetics
#' @export
#' @examples
#' urn(1,1,1000)



urn <- function(red, black, trials){

  require(data.table); require(ggplot2)
  
  dat <- data.frame(trial = 1:trials,
                    colour = vector(length = trials))
  for(i in 1:trials){
    dat$colour[i] <- sample(x = c(rep("red", red), rep("black", black)), size = 1, replace = T, prob = rep(1/(red+black), sum(red, black)) )
  }

  dat$outcome <- as.numeric(dat$colour == "red")

  setDT(dat)
  dat <- dat[, cum.sum := cumsum(outcome)/trial]


  return(list(ggplot(dat, aes(x = trial, y = cum.sum))+
                geom_hline(yintercept = 0.5, col = "red", size=0.3)+
                geom_line()+
                xlab(label = "Trial Number")+
                ylab(label = "P(red)")+
                theme_bw(base_line_size = 0),
         dat))
}






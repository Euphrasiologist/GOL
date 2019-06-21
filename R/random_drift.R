#' Random drift function
#'
#' Two coloured ball game with entangled probabilities from Games of Life by Karl Sigmund (ISBN: 978-0198547839)
#' @param red number of red balls
#' @param black number of black balls
#' @param trials number of trials in game
#' @keywords urn, gamesoflife, genetics
#' @export
#' @examples
#' random_drift(red = 10000, black = 10000, trials = 100)

random_drift <- function(red, black, trials){
  
  require(data.table); require(ggplot2)
  
  dat <- data.frame(trial = 1:trials,
                    red = vector(length = trials),
                    black = vector(length = trials))
  dat[1,] <- c(1, red, black)
  
  for(i in 2:trials){
    if(is.na(dat[i-1,2])){
      dat[i-1,2] <- 0
    } else dat[i-1,2]
    
    if(is.na(dat[i-1,3])){
      dat[i-1,3] <- 0
    } else dat[i-1,3]
    
    dat[i,] <- c(i, table(sample(x = c(rep("red", dat[i-1,2]*2), rep("black", dat[i-1,3]*2)), 
                                 size = (red + black)))[c(2,1)])
  }
  
  setDT(dat2)
  
  dat2 <- melt(dat, id.vars = 1)
  
  ggplot(dat2, aes(x = trial, y = value))+
    geom_line(aes(colour = variable))+
    scale_colour_manual(values = c("red", "black"))+
    theme_bw()+
    theme(legend.position = "none")+
    xlab(label = "Generation")+
    ylab(label = "Proportion red/black")
}
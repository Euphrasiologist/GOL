#' Drift with many competitors, adjust selection for one lucky colour function
#'
#' Change the selection coefficient, s, to adjust how many copies of that colour are left
#' 
#' @param colours number of different colours of balls
#' @param trials the number of trials the game runs for 
#' @param size size of the initial population of balls
#' @param rel.freq starting frequencies for the colours. If NULL, frequencies are drawn from the uniform distribution.
#' @param s selection coefficient for the first colour
#' @keywords urn, selction, gamesoflife, drift
#' @export
#' @examples
#' selection_drifts(colours = 10, trials = 1000, size = 1000)

selection_drifts <- function(colours = 5, trials, size, rel.freq = NULL, s = 1.04){
  
  require(data.table); require(ggplot2)
  
  
  if(trials < 2){
    stop("Increase number of trials, try 100+")
  }
  
  if(size < colours){
    stop("Population size must be greater than the number of coloured balls!")
  }
  
  # generate some random numbers
  if(is.null(rel.freq)){
    
    gen.freq <- function(n) {
      m <- matrix(runif(n,0,1), ncol=n)
      m <- sweep(m, 1, rowSums(m), FUN="/")
      m <- as.vector(m)
    }
    
    rel.freq <- gen.freq(colours)
    
  }
  
  # if relative frequencies manually defined
  if(colours != length(rel.freq) & sum(rel.freq) != 1){
    stop("Relative frequencies of colours must equal the number of colours and sum to 1")
  }
  
  # starting data frame
  
  dat <- matrix(ncol = colours + 1, nrow = trials)
  colnames(dat) <- c("Trials", paste("colour", 1:colours))        
  dat <- as.data.frame(dat)
  
  # seed the data frame
  
  dat[1,] <- c(1, round(size*rel.freq))
  dat$Trials <- 1:trials
  
  # selection
  
  s <- c(2*s, rep(2, colours -1)) 
  
  # start the loop 
  for(i in 2:trials){
    
    # save each iteration as x
    x <- as.vector(table(sample(x = rep(x = paste("colour", 1:colours), 
                                        times = dat[i-1, c(2:(colours+1))]*s),
                                replace = TRUE,
                                size = size))[c(paste("colour", 1:colours))])
    # remove NA's
    dat[i,] <- c(i, ifelse(test = is.na(x), 
                           yes = 0, 
                           no = x))
  }
  
  dat2 <- data.table::melt(dat, id.vars = 1)
  
  ggplot(dat2, aes(x = Trials, y = value))+
    geom_line(aes(colour = variable))+
    theme_bw()+
    theme(legend.position = "none")+
    xlab(label = "Generation")+
    ylab(label = "Proportion of colours")
}

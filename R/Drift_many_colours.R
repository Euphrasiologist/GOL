#' Drift with many competitors function
#'
#' What happens if we have a population of coloured balls and let them drift?
#' 
#' @param colours number of different colours of balls
#' @param trials the number of trials the game runs for 
#' @param size size of the initial population of balls
#' @param rel.freq proportions 
#' @keywords urn, gamesoflife, genetics
#' @export
#' @examples
#' random_drifts(colours = 10, trials = 1000, size = 1000)

random_drifts <- function(colours = 5, trials, size, rel.freq = NULL){
  
  # size must be greater than colours!
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
    stop("Relative frequencies of colours must equal the number of colours and equal 1")
  }
  
  # starting data frame

  dat <- matrix(ncol = colours + 1, nrow = trials)
  colnames(dat) <- c("Trials", paste("colour", 1:colours))        
  dat <- as.data.frame(dat)
  
  # seed the data frame
  
  dat[1,] <- c(1, round(size*rel.freq))
  dat$Trials <- 1:trials
  
  # start the loop 
  
  for(i in 2:trials){
    
    # save each iteration as x
    x <- as.vector(table(sample(x = rep(x = paste("colour", 1:colours), times = dat[i-1, c(2:(colours+1))]*2),
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



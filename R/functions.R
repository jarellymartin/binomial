# Private Auxiliary Functions

# finds the mean of a binomial distribution
aux_mean <- function(trials, prob){
  return(trials*prob)
}

# finds the variance of a binomial distribution
aux_variance <- function(trials, prob){
  return(trials*prob*(1-prob))
}

# finds the mode of a binomial distribution
aux_mode <- function(trials, prob){
  return(as.integer(trials*prob+prob))
}

# finds skewness of the binomial distribution
aux_skewness <- function(trials, prob){
  return((1-2*prob)/(sqrt(trials*prob*(1-prob))))
}

# finds the kurtosis or the measure of the "tailedness" of the binomial distribution
aux_kurtosis <- function(trials, prob){
  return((1+(-6*prob)*(1-prob))/(trials*prob*(1-prob)))
}

# Private Checker Functions

# private auxiliary function to test if an input prob is a valid probability (i.e. 0<= p<=1)
check_prob <- function(prob){
  if (prob <= 1 & prob >= 0 ){
    return(TRUE)
  }
  else{
    stop('probability has to be a number between 0 and 1')
  }
}

# private auxiliary function to test if an input trials is a valid value for number of trials (non-negative integer)
check_trials <- function(trials){
 if(length(trials) >= 1 & trials >= 0 & (trials %% 1 == 0)){
    return(TRUE)
  }else{
    stop("invalid trials value")
  }
}

# private auxiliary function to test if an input success is a valid value for number of successes (0<=k<=n)
check_success <- function(success, trials){
  if (sum((success <= trials) & length(success) >= 1 & success >= 0 & (success %% 1 == 0)) == length(success)){
    return(TRUE)
  }
  else if(success > trials){
    stop('success cannot be greater than trials')
  }else{
    stop('invalid success value')
  }
}

#' @title Binomial Coefficient
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of n trials
#' @param k  number of k sucesses
#' @return returns total number of combinations in which k success can occur in n trials
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(trials=1,success=1){
  if (success<=trials){
    return(factorial(trials)/(factorial(success)*(factorial(trials-success))))
  #}else if(length(success)>1){
    #return(sapply(k, function(i, j) (factorial(trials)/(factorial(i)*(factorial(trials-i)))), j=trials))
  }else{
    stop('k cannot be greater than n')
  }
}

#' @title Binomial Probability
#' @description calculates the binomial probability
#' @param success number of successes
#' @param trials  number of trials
#' @param prob  probability
#' @return returns the binomial probability
#' @export
#' @examples
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
#' bin_probability(success = 2, trials = 5, prob = 0.5)

bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success)*(prob^success)*(1-prob)^(trials-success))
}

#' @title Binomial Distribution
#' @description creates an object of class \code{c("bindis", "data.frame")}
#' @param trials  number of trials
#' @param prob  probability value
#' @return an object of class \code{c("bindis", "data.frame")}
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)

bin_distribution <- function(trials, prob){
  df <- NULL
  y <- bin_probability(0:trials, trials, prob)
  df <- rbind(df, data.frame("success" = 0:trials,"probability"= y))
  class(df) <- c("bindis","data.frame")
  return(df)
}

#' @export
plot.bindis <- function(x, ...){
  return(barplot(x$probability, xlab='success', ylab='probability', names.arg = x$success))
}


#' @title Binomial Comulative
#' @description creates an object of class \code{c("bincum", "data.frame")}
#' @param trials  number of trials
#' @param prob  probability
#' @return returns an object of class \code{c("bincum", "data.frame")}
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob){
  df <- NULL
  y <- bin_probability(0:trials, trials, prob)
  df <- rbind(df, data.frame("success" = 0:trials,"probability"= y, "cumulative"=cumsum(y)))
  class(df) <- c("bincum","data.frame")
  return(df)
}

#' @export
plot.bincum <- function(x,...){
  return(plot(x$cumulative, xlab='success', ylab='probability', type ='o'))
}

#' @title Binomial Variable
#' @description creates an object of class \code{c("binvar")}
#' @param trials  number of trials
#' @param prob  probability of random variable
#' @return returns an object of class \code{c("binvar")}
#' @export
#' @examples
#' bin_variable(5,0.2)
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  var <- list(trials = trials, prob = prob)
  class(var)<-"binvar"
  return(var)
}

#' @export
print.binvar <- function(x, ...){
  cat('Binomial Variable \n\n')
  cat('Paramaters \n')
  cat('- number of trials:', x$trials, '\n')
  cat('- prob of success:', x$prob, '\n')

}

#' @export
summary.binvar <- function(x, ...) {
  obj <- list(trials = x$trials,
              prob=x$prob,
              mean=aux_mean(x$trials,x$prob),
              variance = aux_variance(x$trials,x$prob),
              mode = aux_mode(x$trials,x$prob),
              skewness = aux_skewness(x$trials,x$prob),
              kurtosis=  aux_kurtosis(x$trials,x$prob)
   )
  class(obj) <-"summary.binvar"
  return(obj)
}
#' @export
print.summary.binvar <-function(x,...){
  cat('"Summary Binomial"\n\n')
  cat('Paramaters \n')
  cat('- number of trials:', x$trials, '\n')
  cat('- prob of success:', x$prob, '\n\n')
  cat('Measures \n')
  cat('- mean:', x$mean, '\n')
  cat('- variance:', x$variance, '\n')
  cat('- mode:', x$mode, '\n')
  cat('- skewness:', x$skewness, '\n')
  cat('- kurtosis:', x$kurtosis, '\n')

}

#' @title Binomial Mean
#' @description gives the mean of a binomial random variable
#' @param trials  number of trials
#' @param prob  probability of random variable
#' @return returns mean given by the number of trials and its probability
#' @export
#' @examples
#' bin_mean(10,0.3)
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}


#' @title Binomial Variance
#' @description gives the variance of a binomial random variable
#' @param trials  number of trials
#' @param prob  probability of random variable
#' @return returns variance given by the number of trials and its probability
#' @export
#' @examples
#' bin_variance(10,0.3)

bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}


#' @title Binomial Mode
#' @description gives the mode of a binomial random variable
#' @param trials  number of trials
#' @param prob  probability of random variable
#' @return returns mode given by the number of trials and its probability
#' @export
#' @examples
#' bin_mode(10,0.3)

bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Binomial Skewness
#' @description gives the skewness of a binomial random variable
#' @param trials  number of trials
#' @param prob  probability of random variable
#' @return returns skewness given by the number of trials and its probability
#' @export
#' @examples
#' bin_skewness(10,0.3)

bin_skewness<- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}


#' @title Binomial Kurtosis
#' @description gives the kurtosis of a binomial random variable
#' @param trials  number of trials
#' @param prob  probability of random variable
#' @return returns kurtosis given by the number of trials and its probability
#' @export
#' @examples
#' bin_kurtosis(10,0.3)

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}











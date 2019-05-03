# Binomial

# Stat 133, Spring 2019

### Workout 3: Binomial Package

#### Decription

The purpose of Workout 3 is to create an R package in order to create functions to calcultate probabilities of a Binomial random variable. The package also includes adjacent functions to compute mean, variance, skewness, mode, and etc. The motivation behind the Binomial package and this workout is to learn how to implement and understand the functionalities behind an R package. 

#### Functions (main)
- 'bin_choose(trials, success)'
- bin_probability(success, trials, prob)
- bin_distribution(trials, prob)
- plot.bindis()
- bin_cumulative(trials, prob)
- plot.bincum()
- bin_variable(trials, prob)
- summary.binvar()
- bin_mean(trials, prob)
- bin_variance(trials, prob)
- bin_mode(trials, prob)
- bin_skewness(trials, prob)
- bin_kurtosis(tirals, prob)

##### File Structure
- R: r scripts that contain main, checker, and auxiliary functions
- man: files for the manual documentation
- tests: r script that contains testthat functions
- vignette: detailed explanations of each function and its usage
- DESCRIPTION: metadata
- NAMESPACE: lists export and import directives
- README
- devtools-flow.R
- binomial.Rproj


#### Author 

- Name: Jarelly Martin
- Github username: jarellymartin
- Email: jarellymartin@berkeley.edu
- Lab section: 105
- GSI: Miyabi Ishihara
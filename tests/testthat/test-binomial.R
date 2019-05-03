#library(testthat)
#source("../R/binMain.R")

context("bin_choose")
test_that("binomial choose is valid", {
  n <- 5
  k <- 0:2
  success1 <- 4
  expect_equal(bin_choose(n, k),c(1,5,10))
  expect_equal(bin_choose(n, success1), 5)
  expect_gte(n,success1)
})

context("bin_probability")
test_that("binomial probability is valid", {
  success1 <- 55
  trials1 <- 100
  prob1 <- 0.45
  prob2 <- 100
  expect_equal(bin_probability(success = success1, trials = trials1, prob = prob1), 0.01075277)
  expect_gte(trials1,success1)
  expect_error(check_prob(prob2))
})

context("bin_distribution")
test_that("binomial distribution is valid", {
  trials2 <- 5
  prob2 <- 0.5
  expect_length(bin_distribution(trials2, prob2)$success, length(0:trials2))
  trials4 <- 4
  prob4 <- 10
  expect_error(bin_distribution(trials4,prob4))
  expect_error(bin_distribution())
})


context("bin_commulative")
test_that("binomial cumulative is valid", {
  trials3 <- 10
  prob3 <- 0.95
  expect_length(bin_cumulative(trials3, prob3)$cumulative, length(0:trials3))
  trials5 <- 5
  prob5 <- 6
  expect_error(bin_cumulative(trials5,prob5))
  expect_error(bin_cumulative())
})







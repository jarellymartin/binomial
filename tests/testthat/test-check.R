#library(testthat)
#source("../R/check.R")

context("check_prob")
test_that("probability is valid", {
  x<-1
  y<-5
  expect_true(check_prob(x))
  expect_length(check_prob(x), 1)
  expect_error(check_prob(y))
})

context("check_trials")
test_that("trials is valid", {
  k <- 1
  y <- -2
  expect_true(check_trials(k))
  expect_length(check_trials(x), 1)
  expect_error(check_trials(y))
})


context("check_success")
test_that("success is valid", {
  success <- 1
  trial <- 2
  #expect_true(check_success(success,trial))
  #expect_length(check_success(success,trial), 1)
  expect_error(check_success(success = trial, trials = success))
})


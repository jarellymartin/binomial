#library(testthat)
#source("../R/auxiliary.R")


context("aux_mean")
test_that("mean is valid", {
  trials<-1
  prob <- 0.3
  expect_equal(aux_mean(trials, prob), 0.3)
  expect_length(aux_mean(trials, prob), 1)
  expect_gte(trials, prob)
})

context("aux_variance")
test_that("variance is valid", {
  trials<-5
  prob <- 0.25
  expect_equal(aux_variance(trials, prob), 0.9375)
  expect_length(aux_variance(trials, prob),1)
  expect_gte(trials, prob)
})

context("aux_mode")
test_that("mode is valid", {
  trials<-4
  prob <- 0.5
  expect_equal(aux_mode(trials, prob), 2)
  expect_length(aux_mode(trials, prob), 1)
  expect_gte(trials,prob)
})

context("aux_skewness")
test_that("skewness is valid", {
  trials<-10
  prob <- 0.4
  expect_equal(aux_skewness(trials, prob), aux_skewness(trials, prob))
  expect_length(aux_skewness(trials,prob), 1)
  expect_gte(trials,prob)
})

context("aux_kurtosis")
test_that("kurtosis is valid", {
  trials <- 100
  prob <- 0.8
  expect_equal(aux_kurtosis(trials, prob),  0.0025)
  expect_length(aux_kurtosis(trials, prob), 1)
  expect_gte(trials,prob)
})


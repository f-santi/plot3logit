
# ordinal::clm
test_that('extract trilogit from "ordinal::clm"', {
  cross_1year %>%
    mutate(finalgrade = factor(
      x = finalgrade,
      levels = c('Low', 'Average', 'High'),
      ordered = TRUE)
    ) %>%
    MASS::polr(finalgrade ~ gender + irregularity, data = .) %>%
    extract3logit -> modref
    
  cross_1year %>%
    mutate(finalgrade = factor(
      x = finalgrade,
      levels = c('Low', 'Average', 'High'),
      ordered = TRUE)
    ) %>%
    ordinal::clm(finalgrade ~ gender + irregularity, data = .) %>%
    extract3logit -> model
  
  expect_is(model, 'model3logit')
  expect_equal(model$B, modref$B, tolerance = 1e-6)
  expect_equal(model$alpha, modref$alpha, tolerance = 1e-6)
})



# ordinal::clm2
test_that('extract trilogit from "ordinal::clm2"', {
  cross_1year %>%
    mutate(finalgrade = factor(
      x = finalgrade,
      levels = c('Low', 'Average', 'High'),
      ordered = TRUE)
    ) %>%
    MASS::polr(finalgrade ~ gender + irregularity, data = .) %>%
    extract3logit -> modref
  
  cross_1year %>%
    mutate(finalgrade = factor(
      x = finalgrade,
      levels = c('Low', 'Average', 'High'),
      ordered = TRUE)
    ) %>%
    ordinal::clm2(finalgrade ~ gender + irregularity, data = .) %>%
    extract3logit -> model
  
  expect_is(model, 'model3logit')
  expect_equal(model$B, modref$B, tolerance = 1e-6)
  expect_equal(model$alpha, modref$alpha, tolerance = 1e-6)
})


# mlogit::mlogit
test_that('extract trilogit from "mlogit::mlogit"', {
  cross_1year %>%
    nnet::multinom(
      formula = employment_sit ~ gender + finalgrade,
      data = ., 
      trace = FALSE
    ) %>%
    extract3logit -> modref
  
  cross_1year %>%
    mlogit::mlogit.data(choice = 'employment_sit', shape = 'wide') %>%
    mlogit::mlogit(employment_sit ~ 0 | gender + finalgrade, data = .) %>%
    extract3logit -> model
  
  expect_is(model, 'model3logit')
  expect_equal(model$B[ , 2:1], modref$B, tolerance = 1e-4)
  #expect_equal(model$vcovB, modref$vcovB)
})



# nnet::multinom
test_that('extract trilogit from "nnet::multinom"', {
  cross_1year %>%
    nnet::multinom(
      formula = employment_sit ~ gender + finalgrade,
      data = ., 
      trace = FALSE
    ) -> model
  
  expect_is(extract3logit(model), 'model3logit')
})



# MASS::polr
test_that('extract trilogit from "MASS::polr"', {
  cross_1year %>%
    mutate(finalgrade = factor(
      x = finalgrade,
      levels = c('Low', 'Average', 'High'),
      ordered = TRUE)
    ) %>%
    MASS::polr(finalgrade ~ gender + irregularity, data = .) %>%
    extract3logit -> model
  
  expect_is(model, 'model3logit')
})



# VGAM::vgam
test_that('extract trilogit from "VGAM::vgam"', {
  cross_1year %>%
    nnet::multinom(
      formula = employment_sit ~ gender + finalgrade,
      data = ., 
      trace = FALSE
    ) %>%
    extract3logit -> modref
  
  cross_1year %>%
    VGAM::vgam(
      formula = employment_sit ~ gender + finalgrade,
      family = VGAM::multinomial(),
      data = .
    ) %>%
    extract3logit -> model
  
  expect_is(model, 'model3logit')
  #expect_equal(model$B, modref$B)
  #expect_equal(model$vcovB, modref$vcovB)
})



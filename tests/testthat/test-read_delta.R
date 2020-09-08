
test_that("function 'get_vdelta' works", {
  depoF <- test_rfield3logit()
  
  # Single change
  k <- sample(seq_along(depoF$vdelta), 1)
  depoC <- get_vdelta(rownames(depoF$B)[k], depoF)
  expect_identical(depoC, versor(k, length(depoF$vdelta)))
  
  # Composite change
  k <- sample(seq_along(depoF$vdelta), 2)
  depoC <- get_vdelta(
    paste(rownames(depoF$B)[k[1]], '-', rownames(depoF$B)[k[2]]),
    depoF
  )
  depoN <- versor(k[1], length(depoF$vdelta)) - versor(k[2], length(depoF$vdelta))
  expect_identical(depoC, depoN)
})



test_that("factor delimiters syntax works", {
  # Fit the model
  modVote <- nnet::multinom(
    formula = vote ~ educ + gender + race + birthyr,
    data = droplevels(USvote2016),
    trace = FALSE
  )
  
  
  # Single factor
  depo <- field3logit(modVote, '<<gender>>')
  expect_is(depo + NULL, 'multifield3logit')
  expect_identical(length(depo + NULL), 1L)
  
  depo <- field3logit(modVote, '<<race>>')
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 5L)
  
  
  # Two factors
  depo <- field3logit(modVote, '<<gender>> + <<race>>')
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 5L)
  
  depo <- field3logit(modVote, '<<race>> + <<race>>')
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 25L)
  
  
  # Nested effects
  list(
    list(delta = 'genderFemale', label = 'Female'),
    list(delta = '<<race>>', label = 'Race')
  ) %>%
    field3logit(modVote, ., nstreams = 1, narrows = 1) -> depo
    
  expect_is(depo, 'multifield3logit')
  expect_identical(length(depo), 6L)
})


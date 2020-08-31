
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


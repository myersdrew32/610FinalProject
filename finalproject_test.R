library(testthat)


# Test whether "x1" predictor is added first
test_that("x1 predictor is added first in forward stepwise selection", {
  
  ls <- forwardstepwise(data = dat, response = "y", predictors = colnames(dat[, -11]))
  
  # Check if "x1" is the first predictor added in the selected list
  expect_equal(ls$RSSselected[1], "x1", 
               info = "The 'x1' predictor should be added first in the forward stepwise selection.")
})


# Test whether "wt" predictor is added first and "cyl" second
test_that("wt and cyl predictors are added in sequence in forward stepwise selection", {
  ls <- forwardstepwise(data = mtcars, response = "mpg", predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
  
  # Check if "wt" is the first predictor added in the selected list
  expect_equal(ls$RSSselected[1], "wt", 
               info = "The 'wt' predictor should be added first in the forward stepwise selection.")
  
  # Check if "cyl" is the second predictor added in the selected list
  expect_equal(ls$RSSselected[2], "cyl", 
               info = "The 'cyl' predictor should be added second in the forward stepwise selection.")
})


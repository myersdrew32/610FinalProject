library(testthat)

#################################
#
#
# Tests for stepwise selection based on reducing RSS
# based off known answers
#
#################################

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


#################################
#
#
# Tests for model selection based on BIC
#
#
#################################


# To choose the model with minimum BIC, the model before and after the one 
# selected should have higher BICs

test_that("BIC is minimized", {
	ls <- forwardstepwise(data = mtcars, 
						  response = "mpg", 
						  predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
	
	# Check if BIC is lower in the selected model than in the one before it  
	expect_equal(TRUE, BIC(ls$all_models[[which.min(ls$BICvector)]]) <= BIC(ls$all_models[[which.min(ls$BICvector) - 1]]),
				 info = "The BIC of the selected model is lower than the model preceding it")
	# Check if BIC is lower in the selcted model than in the one after it. 
	expect_equal(TRUE, BIC(ls$all_models[[which.min(ls$BICvector)]]) <= BIC(ls$all_models[[which.min(ls$BICvector) + 1]]),  
	info = "The BIC of the selected model is lower than the model after it")

})

#################################
#
#
# Tests for model selection based on Adj. R^2
#
#
#################################


# To choose the model with minimum BIC, the model before and after the one 
# selected should have higher BICs

test_that("Adj R squared is maximized", {
	ls <- forwardstepwise(data = mtcars, 
						  response = "mpg", 
						  predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
	
	# Check if BIC is lower in the selected model than in the one before it  
	expect_equal(TRUE, summary(ls$all_models[[which.max(ls$AdjustR2vector)]])$adj.r.squared >= summary(ls$all_models[[which.max(ls$AdjustR2vector) - 1]])$adj.r.squared,
				 info = "The Adj. R squared of the selected model is higher than the model preceding it")
	# Check if BIC is lower in the selcted model than in the one after it. 
	expect_equal(TRUE, summary(ls$all_models[[which.max(ls$AdjustR2vector)]])$adj.r.squared > summary(ls$all_models[[which.max(ls$AdjustR2vector) + 1]])$adj.r.squared,
		info = "The Adj. R squared of the selected model is higher than the model after it")
	
})

###################################################
#
# Stat S610: Statistical Computing
# Final Project Script
#
# Group: Drew, Katya, Soocheol
#
# Project: Forward stepwise selection 
#
###################################################

# Goal: Model selection and post-selection inference: 

#	In regression modeling, we sometimes want to include only 
#	a subset of our variables in the model and would like that 
#	decision to be data dependent. Two solutions to this problem 
#	are forward stepwise selection and backward stepwise
#	selection, in which you fit a sequence of models, either 
#	adding or subtracting one variable at a time until some 
#	stopping criterion is reached. These procedures work for variable
#	selection, but they invalidate the standard methods of 
#	inference in linear models.

# Idea from discussion: 
#		Write a function that takes as arguments: (1) dataset, (2) response 
#		variable, and (3) a list of predictor variables. The function 
#		will then go through and model the response as a function
#		of each predictor variable individually, keeping the one that reduces
#		RSS the most. It will then go through and add variables in a similar
#		fashion until (stopping criterion?)


forwardstepwise <- function(data, response, predictors) {
	# Create vector to store selected variables
	selected <- c()
	
	# Store predictor models in variable in the function's environment that can be
	# updated
	predvars <- predictors
	
	# Set best predictor to empty
	bestpred <- NULL
	
	# Set best model to empty
	best_model <- NULL
	
	# Set best RSS to a very high number, so the first RSS will be lower
	best_rss <- Inf
	
	# Create a vector to store BICs in
	BICvector <- numeric(length(predvars))
	
	# create a vector for Adj. R2s
	Ar2vector <- numeric(length(predvars))
	
	while (length(predvars) > 0) {
		# Create a for loop to cycle through all variables, then keep the one
		# that lowers RSS the most
		for (var in predvars) {
			curr_model <- lm(paste(response, "~", paste(c(selected, var), collapse = "+")),
							 data = data)
			
			# Save RSS from the model
			rss <- sum(resid(curr_model)^2)
			
			# compare the current model to the best model, save var/model/rss if better
			if (rss < best_rss) {
				bestpred <- var
				best_model <- curr_model
				best_rss <- rss
				BICvector[length(selected) + 1] <- BIC(curr_model)
				Ar2vector[length(selected) + 1] <- summary(curr_model)$adj.r.squared
			}
		}
		
		# Save the best predictor
		selected <- c(selected, bestpred)
		
		# Remove the best predictor from the list of predictors
		predvars <- setdiff(predvars, bestpred)
		
		if (length(selected) == length(predictors)) {
			break
		}
	}
	
	return(list(selected = selected, best_model = best_model, BICvector = BICvector, AdjustR2vector = Ar2vector))
}

forwardstepwise(data =mtcars, response = "mpg", predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
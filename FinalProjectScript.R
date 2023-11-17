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
	# Create vector to store selected variables in 
	selected <- c()
	
	# Store predictor models in variable in functions environment that can be
	# updated
	predvars <- predictors
	
	# Set best predictor to empty
	bestpred <- NULL
	
	# Set best model to empty
	bestmodel <- NULL
	
	# Set best RSS to very high number, so first RSS will be lower
	best_rss <- Inf
	
	# Create for loop to cycle through all variables, then keep the one 
	# that lowers RSS the most 
	for(var in predvars) {
		curr_model <- lm(paste(response, "~", paste(c(selectedvars, var), collapse = "+")), 
						 data = data)
		# Save RSS from model
		rss <- sum(resid(curr_model)^2)
		
		# compare the current model to best model, save var/model/rss if better
		if(rss < best_rss) {
			bestvar <- var
			best_model <- curr_model
			best_rss <- rss
		}
	}
	
}
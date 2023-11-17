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
	# 
}
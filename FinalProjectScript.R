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


# Create function that takes dataset, response, and predictors
forwardstepwise <- function(data, response, predictors) {
	# Create vector to store selected variables
	selected <- c()
	
	# Store predictor models in variable in the function's environment 
	# that can be updated
	predvars <- predictors
	
	# Set best predictor to empty
	bestpred <- NULL
	
	# Set models vector to empty
	best_model <- NULL
	
	# All models list 
	all_models <- list()
	
	# Set best RSS to a very high number, so the first RSS will be lower
	best_rss <- Inf
	
	# Create a vector to store RSSs in
	RSSvector <- numeric(length(predvars))
	
	# Create a vector to store BICs in
	BICvector <- numeric(length(predvars))
	
	# Create a vector for Adj. R2s
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
				all_models[[length(selected) + 1]] <- curr_model
				RSSvector[length(selected) + 1] <- rss
				BICvector[length(selected) + 1] <- BIC(curr_model)
				Ar2vector[length(selected) + 1] <- summary(curr_model)$adj.r.squared
			}
		}
		
		# Save the best predictor
		selected <- c(selected, bestpred)
		
		# Remove the best predictor from the list of predictors
		predvars <- setdiff(predvars, bestpred)
		
		# Break if all predictors are selected
		if (length(selected) == length(predictors)) {
			break
		}
	}
	
	# Figure out which model BIC would choose 
	BICspot <- which.min(BICvector)
	
	# Create vector of variables selected by BIC choice
	BICselected <- selected[1:BICspot]
	
	# Store that model to be returned 
	BIC_choice <- all_models[[BICspot]]
	
	# Figure out which model AR2 would choose
	AR2spot <- which.max(Ar2vector)
	
	# Create vector of variables selected by AR2 choice
	AR2selected <- selected[1:AR2spot]
	
	# Store the model that AR2 would choose
	AR2_choice <- all_models[[AR2spot]]
	
	return(list(RSSvector = RSSvector,
				RSSselected = selected, 
				best_model_RSS = summary(best_model), 
				
				BICvector = BICvector, 
				BICselected = BICselected,
				BIC_choice = summary(BIC_choice),
				
				AdjustR2vector = Ar2vector, 
				AR2selected = AR2selected, 
				AR2_choice = summary(AR2_choice)))
}

forwardstepwise(data = mtcars, 
				response = "mpg", 
				predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))


##########################################
#
#
# Using the function with MTCARS dataset 
#
#
##########################################

ls = forwardstepwise(data = mtcars, 
					 response = "mpg", 
					 predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))

par(mfrow=c(1,3))

plot(ls$RSSvector, xlab = "Number of predictors", ylab = "RSS", 
	 col='blue', 
	 type = "l")
points(which(ls$RSSvector %in% min(ls$RSSvector)),min(ls$RSSvector), pch = "X", col = "red", lwd=10)

plot(ls$BICvector, 
	 main = "", 
	 xlab = "Number of predictors", 
	 ylab = "BIC", 
	 col='blue', 
	 type = "l")
points(which(ls$BICvector %in% min(ls$BICvector)),min(ls$BICvector), pch = "X",  col = "red", lwd=10)

plot(ls$AdjustR2vector, 
	 xlab = "Number of predictors", 
	 ylab = "Adjusted R^2", 
	 col='blue', 
	 type = "l")
points(which(ls$AdjustR2vector %in% max(ls$AdjustR2vector)),max(ls$AdjustR2vector), pch = "X",  col = "red", lwd=10)
mtext("Figure 1. Comparison of metrics from MTCARS data", side= 3,  line = - 2, outer = TRUE)


##########################################
#
#
# Simulating Data and using function
#
#
##########################################

# install.packages("faux")
library(faux)

dat <- rnorm_multi(n = 10000, 
				   mu = c(20, 20, 20, 5, 10, 15, 2, 3, 5, 6),
				   sd = c(5, 5, 5, 2, 3, 4, 5, 6, 7, 8),
				   r =0.01, 
				   varnames = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10"),
				   empirical = FALSE)




dat$y <-0.95*dat$x1+0*dat$x2+0*dat$x3+0*dat$x4+0.25*dat$x5+0.25*dat$x6+0.25*dat$x7+0.25*dat$x8+0.25*dat$x9+0*dat$x10+rnorm(10000, 5, 2)
res <- cor(dat)
round(res, 2)

forwardstepwise(data =dat, response = "y", predictors = colnames(dat[, -11]))

ls <- forwardstepwise(data =dat, response = "y", predictors = colnames(dat[, -11]))

par(mfrow=c(1,3))
plot(ls$RSSvector, xlab = "Number of predictors", ylab = "RSS", 
	 col='blue', 
	 type = "l")
points(which(ls$RSSvector %in% min(ls$RSSvector)),min(ls$RSSvector), pch = "X",  col = "red", lwd=10)

plot(ls$BICvector, 
	 main = "", 
	 xlab = "Number of predictors", 
	 ylab = "BIC", 
	 col='blue', 
	 type = "l")
points(which(ls$BICvector %in% min(ls$BICvector)),min(ls$BICvector), pch = "X",  col = "red", lwd=10)

plot(ls$AdjustR2vector, 
	 xlab = "Number of predictors", 
	 ylab = "Adjusted R^2", 
	 col='blue', 
	 type = "l")
points(which(ls$AdjustR2vector %in% max(ls$AdjustR2vector)),max(ls$AdjustR2vector), pch = "X",  col = "red", lwd=10)
mtext("Figure 2. Comparison of metrics from simulated data", side= 3,  line = - 2, outer = TRUE)


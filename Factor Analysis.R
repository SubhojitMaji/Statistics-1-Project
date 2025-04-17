library(psych)
library(GPArotation)
install.packages("corrplot")
library(corrplot)

data <- read.csv("Survey Data.csv", header = TRUE)

# Subset the bullying and victimization variables (db1 to db18, dv1 to dv18, ov1 to ov8)
bully_vars <- data[, grep("^(vb|sb|vv|sv|pv)", names(data))]
cor_matrix <- cor(bully_vars, use = "complete.obs")
print (cor_matrix)
corrplot(cor_matrix, method = "color")

library(mice)
bully_vars <- mice::complete(mice(bully_vars, method = "pmm", m = 1))
KMO(bully_vars)   # KMO test

n<- nrow(bully_vars)
cortest.bartlett(cor(bully_vars, use="pairwise.complete.obs"), n=n)  # Bartlett's test

fa.parallel(bully_vars, fa = "both")

fa_result <- fa(bully_vars, nfactors = 2,fm="pa" , rotate = "varimax", scores= "Bartlett")  # Adjust 'nfactors'
print(fa_result, cut = 0.3)  # Display loadings greater than 0.3
fa.diagram(fa_result)
fa_scores <- fa_result$scores

# Combine the factor scores with the original data
regression_data <- cbind(data, fa_scores)

# Fit the regression model using the extracted factor scores as predictors
model_reg <- lm(Bullying_Aggression ~ Factor1 + Factor2, data = regression_data)

# Display the summary of the regression model
summary(model_reg)

# Load library
library(lavaan)

# Define model: F1 predicts vb, sb, sv, pv, vv
model <- 'F1 =~ vb + sb + sv + pv + vv'

# Simulate data (replace with your actual dataset)
set.seed(123)
data <- simulateData(model, sample.nobs = 200)
fit <- sem(model, data = data)

# Show standardized regression weights
standardizedSolution(fit)[standardizedSolution(fit)$op == "=~", ]

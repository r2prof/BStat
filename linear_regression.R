# Set the working directory
setwd("C:/Users/Raza/Documents/R")

# Load the dataset
possum_data <- read.csv("possum.csv")

# Check the dataset is loaded in the environment
head(possum_data)

# Verify the dimensions of the dataset
dim(possum_data)

# Check the structure of the dataset
str(possum_data)

# Open the dataset in a spreadsheet-like view
View(possum_data)


# Scatter plot of head length vs total length
plot(possum_data$totlngth, possum_data$hdlngth,
     main = "Scatterplot",
     xlab = "Total Length (cm)", 
     ylab = "Head Length (cm)",
     pch = 19, col = "blue")

# Correlation between head length and total length
corr <- cor(possum_data$totlngth, possum_data$hdlngth)
corr

# Fit a linear regression model
model <- lm(hdlngth ~ totlngth, data = possum_data)

# Display the model summary
summary(model)

# Extract coefficients
coef(model)

# Round the extracted coefficients
intercept <- round(coef(model)[1], 2)
slope <- round(coef(model)[2], 3)

# Print these values 
intercept
slope

# Compute R-square
summary(model)$r.squared

# Round the computed R-sq value
r_sq <- round(summary(model)$r.squared, 3)

# Print R-sq rounded value
r_sq

# Create the base plot
plot(possum_data$totlngth, possum_data$hdlngth,
     main = "Simple Linear Regression",
     xlab = "Total Length (cm)",
     ylab = "Head Length (cm)",
     pch = 19, col = "blue")

# Add the regression line
abline(model, col = "red", lwd = 2)

# Add the regression eq and R² as text
eq_text <- paste0("y = ", intercept, 
                  " + ", slope, "x", 
                  "\nR² = ", r_sq)

text(x = min(possum_data$totlngth) + 5, 
     y = max(possum_data$hdlngth) - 2, 
     labels = eq_text, 
     cex = 1.2, 
     col = "black", 
     pos = 4)

# -----------------------------------------
# Assumption Checks for Linear Regression
# Using the 'possum_data' dataset and 'model' from previous videos
# -----------------------------------------

# 1. Residuals vs Fitted Plot
# Purpose: To check linearity and homoscedasticity (constant variance)
plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

#---

# 2. Q-Q Plot of Residuals
# Purpose: To check if residuals are normally distributed
qqnorm(model$residuals,
       main = "Normal Q-Q Plot")
qqline(model$residuals, col = "red", lwd = 2)


# 3. Histogram of Residuals
# Purpose: Another check for normality and symmetry
hist(model$residuals,
     breaks = 10,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "skyblue", border = "white")



# 4. Cook’s Distance Plot
# Purpose: Identify influential observations 
#          that may disproportionately affect 
#          the model
cooks_d <- cooks.distance(model)
plot(cooks_d, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance", 
     col = "purple")

abline(h = 4/length(cooks_d), 
       col = "red", lty = 2)


# Interpretation:
# Points above the horizontal red line may be influential.
# Investigate these observations further.








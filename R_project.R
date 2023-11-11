# Load necessary library 
library(tidyverse)
library(tableone)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)

# Load the data 
data(cancer, package="survival")

## Data exploration 

# View data
view(gbsg)

# Explore dimension and variables

glimpse(gbsg)

# Explore unique value per column
# This support data validation process

unique_values_per_column <- lapply(gbsg, unique)
unique_values_per_column

# Identify missing values
missing_values <- is.na(gbsg)

# Calculate the percentage of missing values for each column
missing_percentage <- colSums(missing_values) / length(gbsg$age) * 100

# Create a data frame to display the results
missing_data_summary <- data.frame(Column = colnames(gbsg), MissingPercentage = missing_percentage)

# Display the columns with missing values and their percentages
print(missing_data_summary)

#There is no missing value in this data set.


# Explore categorical variables
par(mfrow = c(2, 2))  # Set up a 2x2 plotting space

# Categorical columns 
columns_cat <- c("meno", "grade", "hormon", "status")

for (i in 1:length(columns_cat)) {
  col <- columns_cat[i]
  
  # Create a table of frequencies for the column
  freq_table <- table(gbsg[[col]])
  
  # Create a bar plot of the frequency table
  barplot(freq_table,
          main = paste("Bar Plot of", col),
          xlab = "Categories",
          ylab = "Frequency")
}
# Explore numeric variables
par(mfrow = c(3, 3))  # Set up a 3 x 3 plotting space

# Numeric columns
columns_num <- c("age", "size", "nodes", "pgr",'er')
for (i in 1:length(columns_num)) {
  col <- columns_num[i] 
  # Extract data from the specified column
  x <- gbsg[[col]]
  
  # Plot histogram of x
  hist(x,
       main = paste("Histogram of", col),
       xlab = "Values")  # Adjust xlim as needed for binary data
}
# All numeric variables are not normally distributed
## Data Preprocessing

# This analysis aim to determine whether breast cancer patients 
# who received hormonal therapy (HRT) have different 
# recurrence-free survival outcomes compared to those who did not.

# Assign new variable from hormon variable to Expose to HRT and controls to Non-Expose 
gbsg$hrt =ifelse(gbsg$hormon== "0","Non-Expose","Expose to HRT")
gbsg$hrt =factor(gbsg$hrt)
gbsg$hrt <- relevel(gbsg$hrt, ref = "Non-Expose")

# Assign new time variable to years
gbsg$rfsyear <- gbsg$rfstime / 365.25

## Descriptive analysis

# Create table to compare baseline characteristics of expose and non-expose to HRT

column_variable <- c("age","meno","grade", "size", "nodes", "pgr","er")
tab <- CreateTableOne(vars = column_variable,
               strata = c("hrt"), data = gbsg, factorVars = c("grade","meno"))
print(tab, nonnormal = columns_num, formatOptions = list(big.mark = ","))

# Two factors (age, menopause) are significantly different between 
# Expose to HRT and non-expose group 

##Survival analysis

## Kaplan-Meier Curve
# Visualize the survival curves for the "Expose to HRT" and "Non-Expose" groups.

fit <- survfit(Surv(rfsyear, status) ~ hrt, data = gbsg)
ggsurvplot(fit, data = gbsg, title = "Recurrence or Death",pval = TRUE, 
            xlab = "Time in years", break.x.by = 2, 
           legend = c(0.3, 0.4), legend.title = "", legend.lab = c("Expose to HRT", "Non-Expose")
)

## Hazard Ratios: Cox regression
### Unadjusted regression

fit.coxph1 <- coxph(formula = Surv(rfsyear, status) ~ hormon , data = gbsg)
summary(fit.coxph1)

### Adjusted regression
fit.coxph2 <- coxph(formula =  Surv(rfsyear, status) ~ hormon + age + meno , data = gbsg)
summary(fit.coxph2)
#adding estimates in a data.frame

estimates=tibble::rownames_to_column(data.frame(exp(cbind(HR = coef(fit.coxph2), 
                           confint(fit.coxph2))),p=summary(fit.coxph2)$coeff[,5]), "Variables") 
estimates

## Subgroup analysis compare menopause and non-menopause group
menopause <- gbsg[gbsg$meno == 1,]
non_menopause <- gbsg[gbsg$meno == 0,]
menopausecox <- coxph(formula = Surv(rfsyear, status) ~ hormon , data = menopause)
summary(menopausecox) # the result of Log-rank test is displayed in the last row

### Adjusted regression
menopausecox1 <- coxph(formula = Surv(rfsyear, status) ~ hormon+age , data = menopause)
summary(menopausecox1)$conf.int

#adding estimates in a data.frame

estimates_meno=tibble::rownames_to_column(data.frame(exp(cbind(HR = coef(menopausecox1), 
                  confint(menopausecox1))),p=summary(menopausecox1)$coeff[,5]), "Variables") 

estimates_meno
#non_menopause group
non_menopausecox <- coxph(formula = Surv(rfsyear, status) ~ hormon , data = non_menopause)
summary(non_menopausecox) # the result of Log-rank test is displayed in the last row

### Adjusted regression
non_menopausecox1 <- coxph(formula = Surv(rfsyear, status) ~ hormon+age , data = non_menopause)
summary(non_menopausecox1)$conf.int

#adding estimates in a data.frame

estimates_nonmeno=tibble::rownames_to_column(data.frame(exp(cbind(HR = coef(non_menopausecox1), 
                                                               confint(non_menopausecox1))),p=summary(non_menopausecox1)$coeff[,5]), "Variables") 

estimates_nonmeno
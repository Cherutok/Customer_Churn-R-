install.packages("ROCR")
install.packages("corrplot")
install.packages("stringr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("caret")
install.packages("kableExtra")
install.packages("ComplexHeatmap")
install.packages("plotly")
install.packages(c("mice", "VIM"))
install.packages("Metrics")
library(mice)
library(VIM)
library("Metrics")
library("rpart")
library("readr")
library("dplyr")
library("tidyr")
library("purrr")
library("corrplot")
library("stringr")
library("ROCR")
library("ggplot2")
library("gridExtra")
library("caret")
library("kableExtra")
library(plotly)
library(caret)

#Reading the dataset
churn <- read_csv("Churn.csv")
print(churn)

#---------------------- Data cleaning --------------------------
# Replace 'No internet service' with 'No'

recode <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies", "MultipleLines")
churn <- as.data.frame(churn); churn
for (col in recode) {
  churn[,col] <- as.character(churn[,col])
  temp <- if_else(churn[,col] %in% c("No internet service","No phone service"), "No",churn[,col])
  churn[,col] <- as.factor(temp)
}
churn

# Remove (automatic) from PaymentMethod
churn$PaymentMethod <- str_remove(churn$PaymentMethod, "\\(automatic\\)") %>% str_trim(., side = "right") %>% as.factor()
churn$PaymentMethod

# Make tenure as categorical variable for easier ploting 
churn <- churn %>% mutate(tenure_group = case_when(tenure <= 12 ~ "0-12M",
                                                   tenure >12 & tenure <=24 ~ "12-24M",
                                                   tenure > 24 & tenure <= 48 ~ "24-48M",
                                                   tenure > 48 & tenure <= 60 ~ "48-60M",
                                                   tenure >60 ~ ">60M"
))
churn$tenure_group <- as.factor(churn$tenure_group)

# Remove columns not needed
churn <- churn %>% select(-one_of(c("customerID","tenure")))

# Turn character columns to factor
recode <- churn %>% select_if(is.character) %>% colnames()

for(col in recode){
  churn[,col] <-  as.factor(churn[,col])
}

# Change SeniorCitizen from (Yes, No) to (1,2)
churn$SeniorCitizen <- as.factor(recode(churn$SeniorCitizen, "1" = "Yes", "0" = "No"))

## Look at the unique values after cleaning 
#churn[-1] %>% select_if(is.factor) %>% map(function(x) unique(x))

#---------------------- Finding Missing Values -----------------------------
# Call missing_values function 
missing_values <- is.na(churn)
print(missing_values)

missing_counts <- colSums(is.na(churn))
print(missing_counts) # Only TotalCharges have missing values


# Function to find the missing values in the columns of the dataframe
missing_values <- function(df) {
  missing <- df %>%
    tidyr::gather(key = "key", value = "value") %>%
    dplyr::mutate(is.missing = is.na(value)) %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(total = n()) %>%
    dplyr::group_by(key, total, is.missing) %>%
    dplyr::summarise(num.missing = n()) %>%
    dplyr::mutate(perc.missing = num.missing / total * 100) %>%
    dplyr::ungroup()
  
  return(missing)
}

missing <- missing_values(churn)

# remove rows with missing values
churn <- na.omit(churn)

missing_values_plot <- ggplot(missing) + geom_bar(aes( x= reorder(key, desc(perc.missing)), y = perc.missing, fill = is.missing), stat = 'identity', alpha = 0.8) + scale_fill_manual(name = "", values = c('steelblue','tomato3'), label = c("Present", "Missing")) + coord_flip() + labs(title = "Percentage of missing values",x = '',y = '% missing')

# Plot missing values
missing_values_plot


#----------------------- Exploratory Data Analysis ----------------------------
#-------- Univariate ---------

print("Percentage of customers churn")
prop.table(table(churn$Churn))
#Over 26% of Customer Churn
churn
# Correlation matrix of numeric variables
cor_matrix<- churn %>% select_if(is.numeric) %>% cor()

corrplot(cor_matrix,method = "number", type = "upper") 
#currently onlt MonthlyCharges and Total Charges are numerical
#later we will transform the dataset to generate more numerical values

# Create a scatter plot
plot(churn$tenure_group, churn$TotalCharges, main = "Scatter Plot", xlab = "tenure", ylab = "TotalCharges", col = "blue", pch = 16)
#TotalCharges are increasing with Tenure

# pi-char to check the distribution on Payment Method used by the customers
labels <- unique(churn$PaymentMethod)
values <- table(churn$PaymentMethod)

payment_method_distribution <- plot_ly(labels = labels, values = values, type = "pie", hole = 0.3)
payment_method_distribution <- payment_method_distribution %>% 
  layout(title = "<b>Payment Method Distribution</b>");payment_method_distribution
# the Payment Method Distribution states that there is almost equal number of customers who use payments as Bank transfer
# Credit cards, Electronic cards


#Analysing Customer Payment Method distribution with respect to Churn
payment_churn <- plot_ly(churn, x = ~Churn, color = ~PaymentMethod, type = "histogram") %>%
  layout(title = "<b>Customer Payment Method distribution w.r.t. Churn</b>",
         width = 700, height = 500, bargap = 0.1);payment_churn
# Customers who use Electronic check for payments are more likely to Churn

#Analysing Customer Contract with respect to Churn
contract_churn <- plot_ly(churn, x = ~Churn, color = ~Contract, type = "histogram", barmode = "group") %>%
  layout(title = "<b>Customer contract distribution</b>",
         width = 700, height = 500, bargap = 0.1);contract_churn
# Customers who opt for month-to-month payments are more likely to Churn


#------------ Bivariate Data analysis ----------

genderChurnPlot <- churn %>% group_by(gender, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = gender, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.5, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));genderChurnPlot

seniorCitizenChurnPlot <- churn %>% group_by(SeniorCitizen, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = SeniorCitizen, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.0, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));seniorCitizenChurnPlot

partnerChurnPlot <- churn %>% group_by(Partner, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = Partner, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.5, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));partnerChurnPlot

dependentsChurnPlot <- churn %>% group_by(Dependents, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = Dependents, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.5, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));dependentsChurnPlot

phoneServiceChurnPlot <- churn %>% group_by(PhoneService, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = PhoneService, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.0, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));phoneServiceChurnPlot

multipleLinesChurnPlot <- churn %>% group_by(MultipleLines, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = MultipleLines, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.5, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));multipleLinesChurnPlot

internetServiceChurnPlot <- churn %>% group_by(InternetService, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = InternetService, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.5, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));internetServiceChurnPlot

OnlineSecurityChurnPlot <- churn %>% group_by(OnlineSecurity, Churn) %>% summarise(Percent = round(n()/nrow(.),2)) %>% ggplot(aes(x = OnlineSecurity, y = Percent, fill = Churn)) + geom_bar(stat = "identity") + geom_text(aes(label = Percent * 100), vjust = 1.5, hjust = 0.5,color = "white", size = 5.0) + theme_minimal() + scale_fill_brewer(palette="Dark2") + scale_y_continuous(labels = scales::percent_format(accuracy = 1));OnlineSecurityChurnPlot



#-------------------------Transform Dataset-----------------------------
churn_transformed <- data.frame(churn) #Make a copy of churn dataset

#Apply Transformations to the Churn Data set

churn_transformed$Churn <- as.numeric(as.factor(recode(churn_transformed$Churn, "Yes" = "1", "No" = "0")))
churn_transformed$gender <- as.numeric(as.factor(recode(churn_transformed$gender, "Male" = "1", "Female" = "0")))
churn_transformed$SeniorCitizen <- as.numeric(as.factor(recode(churn_transformed$SeniorCitizen, "Yes" = "1", "No" = "0")))
churn_transformed$Partner <- as.numeric(as.factor(recode(churn_transformed$Partner, "Yes" = "1", "No" = "0")))
churn_transformed$Dependents <- as.numeric(as.factor(recode(churn_transformed$Dependents, "Yes" = "1", "No" = "0")))
churn_transformed$PhoneService <- as.numeric(as.factor(recode(churn_transformed$PhoneService, "Yes" = "1", "No" = "0")))
churn_transformed$MultipleLines <- as.numeric(as.factor(recode(churn_transformed$MultipleLines, "Yes" = "1", "No" = "0")))
churn_transformed$InternetService <- as.numeric(as.factor(recode(churn_transformed$InternetService, "DSL" = "2", "Fiber optic" = "1", "No" = "0")))
churn_transformed$OnlineSecurity <- as.numeric(as.factor(recode(churn_transformed$OnlineSecurity, "Yes" = "1", "No" = "0")))
churn_transformed$OnlineBackup <- as.numeric(as.factor(recode(churn_transformed$OnlineBackup, "Yes" = "1", "No" = "0")))
churn_transformed$DeviceProtection <- as.numeric(as.factor(recode(churn_transformed$DeviceProtection, "Yes" = "1", "No" = "0")))
churn_transformed$TechSupport <- as.numeric(as.factor(recode(churn_transformed$TechSupport, "Yes" = "1", "No" = "0")))
churn_transformed$StreamingTV <- as.numeric(as.factor(recode(churn_transformed$StreamingTV, "Yes" = "1", "No" = "0")))
churn_transformed$StreamingMovies <- as.numeric(as.factor(recode(churn_transformed$StreamingMovies, "Yes" = "1", "No" = "0")))
churn_transformed$Contract <- as.numeric(as.factor(recode(churn_transformed$Contract, "Month-to-month" = "2", "One year" = "1", "Two year" = "0")))
churn_transformed$PaperlessBilling <- as.numeric(as.factor(recode(churn_transformed$PaperlessBilling, "Yes" = "1", "No" = "0")))
churn_transformed$PaymentMethod <- as.numeric(as.factor(recode(churn_transformed$PaymentMethod, "Bank transfer" = "3", "Credit card" = "2", "Electronic check" = "1","Mailed check" = "0")))

churn_transformed
summary(churn_transformed)

#-------------------------Corelation-----------------------------

churn_transformed[] <- lapply(churn_transformed, as.numeric)
# Assuming df is your data frame
correlation <- cor(churn_transformed)[, "Churn"]

# Print or plot the correlation values
print(sort(correlation, decreasing = TRUE))
#Highest correlation is with MonthlyCharges, PaperlessBilling and SeniorCitizen
correlation_df <- data.frame(variable = names(correlation), correlation = correlation)

#plotting the positive and negative corelations
ggplot(correlation_df, aes(x = reorder(variable, -correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Correlation with Churn",
       x = "Variable",
       y = "Correlation") +
  coord_flip() +
  theme_minimal()



#-------------------------Logistic regression-----------------------------

set.seed(358) # for reproducible results
train_logit <- sample(1:nrow(churn), (0.8)*nrow(churn))
train.df <- churn[train_logit,]
test.df <- churn[-train_logit,]

model1 <- glm(Churn ~ ., data = train.df, family = "binomial", maxit = 200)
summary(model1)$aliased
summary(model1)

# Predict on the test set
predicted_probs <- predict(model1, newdata = test.df, type = "response")
predicted_probs

# Convert predicted probabilities to class predictions
predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")

# Create a confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(test.df$Churn))
print("Confusion Matrix:")
print(conf_matrix)
#Accuracy is 81.59%

#-------------------------Partition the data-----------------------------

library(caret)

# Set the seed for reproducibility
set.seed(40)

# Create indices for stratified sampling
indices <- createDataPartition(churn_transformed$Churn, p = 0.8, list = FALSE)

# Create training and testing sets
X_train <- churn_transformed[indices, -which(names(churn_transformed) == "Churn"), drop = FALSE]
y_train <- churn_transformed$Churn[indices]
X_test <- churn_transformed[-indices, -which(names(churn_transformed) == "Churn"), drop = FALSE]
y_test <- churn_transformed$Churn[-indices]



#-----------------------------------KNN---------------------------------------

library(class)

# Create a k-nearest neighbors model with k = 11
knn_model <- knn(train = as.matrix(X_train), test = as.matrix(X_test), cl = y_train, k = 11)

# Predict on the test set
predicted_y <- knn_model

# Calculate accuracy
accuracy_knn <- sum(predicted_y == y_test) / length(y_test)
cat("KNN accuracy:", accuracy_knn, "\n")

# Display classification report
table_report <- table(predicted_y, y_test)
cat("Classification Report:\n")
print(table_report)

# consider class "1" as positive
tp <- table_report[1,1];tp
tn <- table_report[2,2];tn
fp <- table_report[1,2];fp
fn<- table_report[2,1];fn


# Accuracy
(tp + tn)/(tp + tn + fp + fn)

# TPR = Recall = Sensitivity
tp/(fn+tp)

# TNR = Specificity
tn/(fp+tn)

# FPR
fp/(fp+tn)

# FNR
fn/(fn+tp)


#---------------------------Decision tree--------------------------

# Create a decision tree model
dt_model <- rpart(y_train ~ ., data = cbind(y_train, X_train), method = "class")

# Predict on the test set
predicted_dt_y <- predict(dt_model, newdata = cbind(y_test, X_test), type = "class")
predicted_dt_y

# Calculate accuracy
accuracy_dt <- sum(predicted_dt_y == y_test) / length(y_test)
cat("Decision Tree accuracy is:", accuracy_dt, "\n")

report <- confusionMatrix(as.factor(predicted_dt_y), as.factor(y_test))
cat("Classification Report:\n")
print(report)

#Accuracy 78.02%

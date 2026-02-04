install.packages("tidyverse")
install.packages("randomForest")
install.packages("pROC")
install.packages("caret")

# For data processing and visualization (includes ggplot2, dplyr, etc.)
library(tidyverse)   

# For Random Forest Models
library(randomForest) 
# For ROC and AUC calculations
library(pROC)       
library(caret)    
# For data manipulation
library(dplyr)


df <- readRDS("retail_supply_chain_sales_cleaned_v2.rds")

# View Basic Data Information
glimpse(df)
summary(df)


############### Data understanding 

## a. Count Is_Profitable

ggplot(df, aes(x = Is_Profitable, fill = Is_Profitable)) +
  geom_bar() +
  labs(title = "Distribution of Profitable vs. Unprofitable Items",
       x = "Is the Item Profitable?",
       y = "Count") +
  theme_minimal()


## b. Relationship Between Discount and Profit
### Discount is typically the most direct factor affecting profit.

# Plot a scatter plot of profit and discount, and add a smoothing curve
ggplot(df, aes(x = discount, y = profit)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") + 
  labs(title = "The Relationship Between Profit and Discount",
       x = "Discount ",
       y = "Profit (USD)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#### As shown in the graph, profits typically decline as discounts increase, 
#### especially in the high-discount range.

## c. Profitability of Different Product Categories
### Plot a bar chart showing the average profit for different product categories
df %>%
  group_by(category) %>%
  summarise(Average_Profit = mean(profit)) %>%
  ggplot(aes(x = reorder(category, -Average_Profit), y = Average_Profit, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Profit by Product Category (USD)",
       x = "Product Category",
       y = "Average Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(
    plot.title = element_text(hjust = 0.5)
  )
#### Profitability varies significantly across different product categories, 
#### with certain categories (such as technology) performing better.

#### Create target variable: Our goal is to predict whether an order is profitable. 
#### Therefore, we need to create a new binary variable Is_Profitable.

# Set it to “Yes” when Profit > 0, and “No” otherwise.
df <- df %>%
  mutate(Is_Profitable = as.factor(ifelse(profit > 0, "Yes", "No")))


df <- df %>%
  mutate(
    category = as.factor(category),
    segment  = as.factor(segment),
    region   = as.factor(region),
    returned = as.factor(returned)
  )

# Select Feature Variables: Based on our business understanding and visual 
# analysis, choose variables that may impact profitability as input features 
# for the model.


# Data splitting: Dividing data into a training set (used to train the model) 
# and a test set (used to evaluate model performance).

# Set the random seed to ensure reproducible results.
set.seed(123)

# Create a partitioned index
trainIndex <- trainControl(method = "cv",
                           number = 10,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = "final")

# logistic

## Train a logistic regression model
## Is_Profitable ~ Discount + Sales + Category + Segment + Region
logit_model <- train(Is_Profitable ~ discount + sales + category + segment + region,
      data = df,
      method = "glm",
      family = "binomial",
      trControl = trainIndex,
      metric = "ROC")

summary(logit_model)



# Training Random Forest Models
rf_model <- train(Is_Profitable ~ discount + sales + category + segment + region,
                     data = df,
                     method = "rf",
                     trControl = trainIndex,
                     metric = "ROC")

# View Model Summary and Feature Importance
print(rf_model)



## Cross-validation results comparison between two models 
# The resamples() function collects CV results from different models, 
# facilitating statistical comparison.
results <- resamples(list(LogisticRegression = logit_model, RandomForest = rf_model))
summary(results)
bwplot(results, metric = "ROC", main = "Box Plot of Model AUC Performance Distribution")
dotplot(results, metric = "ROC", main = "Dot Plot of Model Mean AUC Performance")
# Compare AUC Mean Values by Clicking on the Chart




# --- 3. Refitting on All Data ---

# a. Retrain the final logistic regression model (for strategy explanation) 
# We use all 9,994 data points

final_logit_model <- glm(Is_Profitable ~ discount + sales + category + segment + region,
                         data = df,
                         family = "binomial")

summary(final_logit_model) 


# b. Retrain the final random forest model (for tactical prediction)
# We need to determine the optimal mtry value from the cross-validation results
# rf_model$bestTune$mtry will provide this value (result is 5)
optimal_mtry <- rf_model$bestTune$mtry

final_rf_model <- randomForest(Is_Profitable ~ discount + sales + category + segment + region,
                               data = df,
                               ntree = 500, 
                               mtry = optimal_mtry,
                               importance = TRUE)
print(final_rf_model)


# c.Save the final model for future use.
saveRDS(final_logit_model, "final_logit_model.rds")
saveRDS(final_rf_model, "final_rf_model.rds")

# loaded_rf_model <- readRDS("final_rf_model.rds")

################################################## Deployment

# logistic coefficients visualization
coef_df <- broom::tidy(final_logit_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(direction = ifelse(estimate > 0, "Positive (+)", "Negative (-)"),
         impact = abs(estimate))

ggplot(coef_df, aes(x = reorder(term, impact), y = estimate, fill = direction)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("Positive (+)" = "#2ca02c", "Negative (-)" = "#d62728")) +
  labs(title = "Factors Influencing Profitability (Logistic Regression)",
       x = "Variable", y = "Coefficient (Log Odds)",
       fill = "Effect Direction") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

rf_imp <- importance(final_rf_model, type = 2) %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 10)

ggplot(rf_imp, aes(x = reorder(Feature, MeanDecreaseGini),
                   y = MeanDecreaseGini)) +
  geom_col(fill = "#1f77b4", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Drivers of Profitability (Random Forest)",
       x = NULL, y = "Importance (Mean Decrease Gini)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

library(pdp)
pdp_discount <- partial(final_rf_model, pred.var = "discount", prob = TRUE)

ggplot(pdp_discount, aes(x = discount, y = yhat)) +
  geom_line(color = "#ff7f0e", size = 1.2) +
  labs(title = "Predicted Profitability vs. Discount (Random Forest)",
       x = "Discount",
       y = "Predicted Probability of Profit") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

rf_pred_prob <- predict(final_rf_model, type = "prob")[, "Yes"]

ggplot(data.frame(Prob = rf_pred_prob), aes(x = Prob)) +
  geom_histogram(binwidth = 0.05, fill = "#17becf", color = "white") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 0.62, y = 1000, label = "Decision Threshold = 0.6", color = "red") +
  labs(title = "Predicted Profitability Distribution (Random Forest)",
       x = "Predicted Probability (Profit = Yes)",
       y = "Order Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

thr <- 0.6
df$rf_prob <- rf_pred_prob
df$RiskGroup <- ifelse(df$rf_prob >= thr, "Low Risk (Likely Profitable)", "High Risk (Likely Unprofitable)")

summary_table <- df %>%
  group_by(RiskGroup) %>%
  summarise(Average_Discount = mean(discount),
            Profit_Rate = mean(Is_Profitable == "Yes"),
            Orders = n())
summary_table

metrics <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Sensitivity = c(0.94, 0.88),
  Specificity = c(0.50, 0.78)
)

metrics_long <- metrics %>%
  pivot_longer(cols = c(Sensitivity, Specificity),
               names_to = "Metric", values_to = "Value")

ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Model Tradeoff: Sensitivity vs Specificity",
       y = "Score", x = NULL, fill = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Visualize business impact of model segmentation
ggplot(summary_table, aes(x = RiskGroup, y = Profit_Rate, fill = RiskGroup)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(round(Profit_Rate * 100, 1), "%")),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#d62728", "#2ca02c")) +
  labs(title = "Profit Rate by Risk Group (Model Deployment)",
       x = NULL,
       y = "Profit Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

library(DiagrammeR)

grViz("
digraph deployment {
  graph [layout = dot, rankdir = LR]

  node [shape = rectangle, style = filled, fillcolor = lightblue]
  A [label = 'Data Input']
  B [label = 'Model Scoring\\n(Logistic & Random Forest)']

  node [shape = diamond, style = filled, fillcolor = gray90]
  C [label = 'Risk Segmentation\\nThreshold = 0.6']

  node [shape = rectangle, style = filled, fillcolor = palegreen]
  D [label = 'Low-Risk Orders\\nAuto-Approve']

  node [shape = rectangle, style = filled, fillcolor = lightcoral]
  E [label = 'High-Risk Orders\\nManager Review']

  node [shape = rectangle, style = filled, fillcolor = gold]
  F [label = 'Business Impact\\n(Increase Probability of Profitability)']

  A -> B -> C
  C -> D [label='≥0.6', color='green']
  C -> E [label='<0.6', color='red']
  D -> F
  E -> F
}
")



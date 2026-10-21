# ============================================================
# R PROGRAMMING LAB - CUSTOMER SEGMENTATION & PREDICTIVE ANALYTICS
# Dataset: UCI Online Retail Dataset
# ============================================================
# Problem:
# 1. Preprocess Online Retail transaction data
# 2. Build customer-level RFM and related features
# 3. K-Means clustering + Elbow Method
# 4. Hierarchical clustering + dendrogram
# 5. Silhouette Score
# 6. PCA 2D visualization
# 7. Customer profiling
# 8. High-value customer prediction using Random Forest and SVM
# 9. Accuracy, Precision, Recall, F1, ROC-AUC, Confusion Matrix
# 10. Feature importance
# 11. Interactive 3D Plotly visualization
# 12. Marketing recommendations
# ============================================================

# -----------------------------
# 0. INSTALL / LOAD PACKAGES
# -----------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "lubridate",
  "cluster", "factoextra", "randomForest", "e1071",
  "caret", "pROC", "plotly", "scales", "RColorBrewer"
)

new_packages <- required_packages[
  !(required_packages %in% rownames(installed.packages()))
]

if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

set.seed(42)

# -----------------------------
# 1. LOAD DATA
# -----------------------------
# Option A: If the Excel file is already in the working directory,
# set its exact filename below.
file_path <- "Online Retail.xlsx"

if (!file.exists(file_path)) {
  cat("File not found:", file_path, "\n")
  cat("Place the UCI Online Retail Excel file in the working directory\n")
  cat("or change file_path to the correct location.\n")
  stop("Dataset file not found.")
}

retail <- read_excel(file_path)

cat("\n===== DATASET OVERVIEW =====\n")
print(dim(retail))
print(names(retail))
print(head(retail))

# -----------------------------
# 2. STANDARDIZE COLUMN NAMES
# -----------------------------
# UCI Online Retail normally contains:
# InvoiceNo, StockCode, Description, Quantity, InvoiceDate,
# UnitPrice, CustomerID, Country

names(retail) <- gsub(" ", "", names(retail))
names(retail) <- gsub("\\.", "", names(retail))

# Rename common variants if necessary
rename_if_present <- function(data, old, new) {
  if (old %in% names(data) && !(new %in% names(data))) {
    names(data)[names(data) == old] <- new
  }
  data
}

retail <- rename_if_present(retail, "Invoice", "InvoiceNo")
retail <- rename_if_present(retail, "InvoiceDate", "InvoiceDate")
retail <- rename_if_present(retail, "CustomerID", "CustomerID")
retail <- rename_if_present(retail, "UnitPrice", "UnitPrice")

required_cols <- c(
  "InvoiceNo", "Description", "Quantity",
  "InvoiceDate", "UnitPrice", "CustomerID", "Country"
)

missing_cols <- setdiff(required_cols, names(retail))

if (length(missing_cols) > 0) {
  cat("\nAvailable columns:\n")
  print(names(retail))
  stop(
    paste(
      "The following required columns are missing:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# -----------------------------
# 3. DATA PREPROCESSING
# -----------------------------
cat("\n===== MISSING VALUES BEFORE CLEANING =====\n")
print(colSums(is.na(retail)))

# Convert data types safely
retail <- retail %>%
  mutate(
    InvoiceNo = as.character(InvoiceNo),
    CustomerID = as.character(CustomerID),
    Quantity = as.numeric(Quantity),
    UnitPrice = as.numeric(UnitPrice),
    InvoiceDate = as.POSIXct(InvoiceDate)
  )

# Remove rows without CustomerID because customer-level analysis
# cannot be performed without a customer identifier.
retail <- retail %>%
  filter(!is.na(CustomerID))

# Remove missing descriptions
retail <- retail %>%
  filter(!is.na(Description))

# Cancellations in UCI data are generally represented by invoice
# numbers beginning with "C".
retail <- retail %>%
  filter(!grepl("^C", InvoiceNo, ignore.case = TRUE))

# Remove invalid quantities and prices
retail <- retail %>%
  filter(Quantity > 0, UnitPrice > 0)

# Create transaction value
retail <- retail %>%
  mutate(TotalAmount = Quantity * UnitPrice)

cat("\nRows after basic cleaning:", nrow(retail), "\n")
cat("Unique customers:", n_distinct(retail$CustomerID), "\n")

# -----------------------------
# 4. OUTLIER TREATMENT
# -----------------------------
# Cap extreme transaction values using the IQR rule.
q1 <- quantile(retail$TotalAmount, 0.25, na.rm = TRUE)
q3 <- quantile(retail$TotalAmount, 0.75, na.rm = TRUE)
iqr_value <- IQR(retail$TotalAmount, na.rm = TRUE)

upper_limit <- q3 + 1.5 * iqr_value

retail <- retail %>%
  mutate(
    TotalAmount_Capped = pmin(TotalAmount, upper_limit)
  )

cat("Transaction value upper cap:", round(upper_limit, 2), "\n")

# -----------------------------
# 5. CUSTOMER-LEVEL FEATURE ENGINEERING
# -----------------------------
analysis_date <- max(retail$InvoiceDate, na.rm = TRUE) + days(1)

customer_data <- retail %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(
      difftime(analysis_date, max(InvoiceDate), units = "days")
    ),
    Frequency = n_distinct(InvoiceNo),
    MonetaryValue = sum(TotalAmount_Capped, na.rm = TRUE),
    AverageTransactionValue = mean(
      TotalAmount_Capped, na.rm = TRUE
    ),
    QuantityPurchased = sum(Quantity, na.rm = TRUE),
    PurchaseFrequency = n() / max(
      as.numeric(difftime(
        max(InvoiceDate),
        min(InvoiceDate),
        units = "days"
      )),
      1
    ),
    ProductVariety = n_distinct(StockCode),
    Country = first(Country),
    .groups = "drop"
  )

cat("\n===== CUSTOMER FEATURES =====\n")
print(head(customer_data))
print(summary(customer_data))

# -----------------------------
# 6. HANDLE FEATURE SKEW
# -----------------------------
numeric_features <- c(
  "Recency",
  "Frequency",
  "MonetaryValue",
  "AverageTransactionValue",
  "QuantityPurchased",
  "PurchaseFrequency",
  "ProductVariety"
)

# Log transform positively skewed features
customer_model <- customer_data %>%
  mutate(
    Recency_log = log1p(Recency),
    Frequency_log = log1p(Frequency),
    MonetaryValue_log = log1p(MonetaryValue),
    AverageTransactionValue_log = log1p(AverageTransactionValue),
    QuantityPurchased_log = log1p(QuantityPurchased),
    PurchaseFrequency_log = log1p(PurchaseFrequency),
    ProductVariety_log = log1p(ProductVariety)
  )

model_features <- c(
  "Recency_log",
  "Frequency_log",
  "MonetaryValue_log",
  "AverageTransactionValue_log",
  "QuantityPurchased_log",
  "PurchaseFrequency_log",
  "ProductVariety_log"
)

# -----------------------------
# 7. SCALE FEATURES
# -----------------------------
scaled_data <- scale(customer_model[, model_features])

scaled_data <- as.data.frame(scaled_data)
rownames(scaled_data) <- customer_model$CustomerID

cat("\nScaled feature matrix dimensions:\n")
print(dim(scaled_data))

# -----------------------------
# 8. ELBOW METHOD
# -----------------------------
cat("\n===== ELBOW METHOD =====\n")

wss <- numeric(10)

for (k in 1:10) {
  km <- kmeans(
    scaled_data,
    centers = k,
    nstart = 25,
    iter.max = 100
  )
  wss[k] <- km$tot.withinss
}

elbow_data <- data.frame(
  K = 1:10,
  WSS = wss
)

print(elbow_data)

ggplot(elbow_data, aes(x = K, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Elbow Method for Optimal Number of Clusters",
    x = "Number of Clusters (K)",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

# ------------------------------------------------
# IMPORTANT:
# Choose the elbow after visually inspecting the plot.
# K = 4 is used as a reproducible default below.
# Change optimal_k if your elbow indicates another value.
# ------------------------------------------------
optimal_k <- 4

# -----------------------------
# 9. K-MEANS CLUSTERING
# -----------------------------
kmeans_result <- kmeans(
  scaled_data,
  centers = optimal_k,
  nstart = 50,
  iter.max = 100
)

customer_model$Cluster <- factor(kmeans_result$cluster)

cat("\n===== K-MEANS CLUSTER SIZES =====\n")
print(table(customer_model$Cluster))

# -----------------------------
# 10. SILHOUETTE SCORE
# -----------------------------
silhouette_result <- silhouette(
  kmeans_result$cluster,
  dist(scaled_data)
)

average_silhouette <- mean(silhouette_result[, "sil_width"])

cat("\nAverage Silhouette Score:",
    round(average_silhouette, 4), "\n")

plot(
  silhouette_result,
  main = "Silhouette Plot - K-Means Clustering"
)

# -----------------------------
# 11. HIERARCHICAL CLUSTERING
# -----------------------------
# Hierarchical clustering on all customers can be computationally
# expensive. A sample is therefore used for the dendrogram.

hier_sample_size <- min(1000, nrow(scaled_data))
hier_indices <- sample(seq_len(nrow(scaled_data)), hier_sample_size)

hier_data <- scaled_data[hier_indices, ]

hier_dist <- dist(hier_data)

hierarchical_model <- hclust(
  hier_dist,
  method = "ward.D2"
)

plot(
  hierarchical_model,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Customers",
  ylab = "Height"
)

rect.hclust(
  hierarchical_model,
  k = optimal_k,
  border = 2:6
)

hier_clusters <- cutree(
  hierarchical_model,
  k = optimal_k
)

cat("\nHierarchical clustering sample size:",
    hier_sample_size, "\n")

cat("\nHierarchical cluster sizes:\n")
print(table(hier_clusters))

# -----------------------------
# 12. PCA
# -----------------------------
pca_model <- prcomp(
  scaled_data,
  center = FALSE,
  scale. = FALSE
)

pca_variance <- (pca_model$sdev^2) /
  sum(pca_model$sdev^2)

cat("\n===== PCA VARIANCE =====\n")
print(
  round(
    data.frame(
      Component = paste0("PC", seq_along(pca_variance)),
      VarianceExplained = pca_variance,
      CumulativeVariance = cumsum(pca_variance)
    ),
    4
  )
)

pca_data <- as.data.frame(
  pca_model$x[, 1:3]
)

pca_data$CustomerID <- customer_model$CustomerID
pca_data$Cluster <- customer_model$Cluster

# -----------------------------
# 13. PCA 2D VISUALIZATION
# -----------------------------
ggplot(
  pca_data,
  aes(
    x = PC1,
    y = PC2,
    color = Cluster
  )
) +
  geom_point(alpha = 0.6, size = 1.8) +
  labs(
    title = "Customer Segments - PCA 2D Projection",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal()

# -----------------------------
# 14. CLUSTER PROFILING
# -----------------------------
cluster_profile <- customer_model %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    AvgRecency = mean(Recency),
    AvgFrequency = mean(Frequency),
    AvgMonetaryValue = mean(MonetaryValue),
    AvgTransactionValue = mean(AverageTransactionValue),
    AvgQuantityPurchased = mean(QuantityPurchased),
    AvgPurchaseFrequency = mean(PurchaseFrequency),
    AvgProductVariety = mean(ProductVariety),
    .groups = "drop"
  )

cat("\n===== CLUSTER PROFILE =====\n")
print(cluster_profile)

# -----------------------------
# 15. IDENTIFY HIGH-VALUE CUSTOMERS
# -----------------------------
# Define high-value customers as those in the top 25% of
# MonetaryValue. This is a transparent rule and can be changed
# if your lab/instructor specifies another definition.

high_value_cutoff <- quantile(
  customer_model$MonetaryValue,
  0.75,
  na.rm = TRUE
)

customer_model <- customer_model %>%
  mutate(
    HighValue = factor(
      ifelse(
        MonetaryValue >= high_value_cutoff,
        "Yes",
        "No"
      ),
      levels = c("No", "Yes")
    )
  )

cat("\nHigh-value monetary cutoff:",
    round(high_value_cutoff, 2), "\n")

cat("\nHigh-value distribution:\n")
print(prop.table(table(customer_model$HighValue)))

# -----------------------------
# 16. PREPARE SUPERVISED ML DATA
# -----------------------------
ml_features <- customer_model %>%
  select(
    Recency_log,
    Frequency_log,
    MonetaryValue_log,
    AverageTransactionValue_log,
    QuantityPurchased_log,
    PurchaseFrequency_log,
    ProductVariety_log
  )

ml_target <- customer_model$HighValue

ml_data <- cbind(
  ml_features,
  HighValue = ml_target
)

# Stratified train/test split
train_index <- createDataPartition(
  ml_data$HighValue,
  p = 0.80,
  list = FALSE
)

train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]

# -----------------------------
# 17. RANDOM FOREST
# -----------------------------
cat("\n===== RANDOM FOREST =====\n")

rf_model <- randomForest(
  HighValue ~ .,
  data = train_data,
  ntree = 300,
  importance = TRUE
)

rf_predictions <- predict(
  rf_model,
  newdata = test_data,
  type = "response"
)

rf_probabilities <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)[, "Yes"]

print(rf_model)
print(confusionMatrix(
  rf_predictions,
  test_data$HighValue,
  positive = "Yes"
))

# -----------------------------
# 18. SVM
# -----------------------------
cat("\n===== SUPPORT VECTOR MACHINE =====\n")

svm_model <- svm(
  HighValue ~ .,
  data = train_data,
  kernel = "radial",
  probability = TRUE
)

svm_predictions <- predict(
  svm_model,
  newdata = test_data,
  probability = TRUE
)

svm_probabilities <- attr(
  svm_predictions,
  "probabilities"
)[, "Yes"]

print(confusionMatrix(
  svm_predictions,
  test_data$HighValue,
  positive = "Yes"
))

# -----------------------------
# 19. EVALUATION METRICS
# -----------------------------
get_metrics <- function(actual, predicted, probabilities) {

  cm <- confusionMatrix(
    predicted,
    actual,
    positive = "Yes"
  )

  roc_obj <- roc(
    response = actual,
    predictor = probabilities,
    levels = c("No", "Yes"),
    direction = "<",
    quiet = TRUE
  )

  data.frame(
    Accuracy = as.numeric(cm$overall["Accuracy"]),
    Precision = as.numeric(cm$byClass["Precision"]),
    Recall = as.numeric(cm$byClass["Recall"]),
    F1_Score = as.numeric(cm$byClass["F1"]),
    ROC_AUC = as.numeric(auc(roc_obj))
  )
}

rf_metrics <- get_metrics(
  test_data$HighValue,
  rf_predictions,
  rf_probabilities
)

svm_metrics <- get_metrics(
  test_data$HighValue,
  svm_predictions,
  svm_probabilities
)

model_comparison <- rbind(
  Random_Forest = rf_metrics,
  SVM = svm_metrics
)

cat("\n===== MODEL PERFORMANCE COMPARISON =====\n")
print(round(model_comparison, 4))

# -----------------------------
# 20. CONFUSION MATRICES
# -----------------------------
cat("\n===== RANDOM FOREST CONFUSION MATRIX =====\n")
print(
  confusionMatrix(
    rf_predictions,
    test_data$HighValue,
    positive = "Yes"
  )
)

cat("\n===== SVM CONFUSION MATRIX =====\n")
print(
  confusionMatrix(
    svm_predictions,
    test_data$HighValue,
    positive = "Yes"
  )
)

# -----------------------------
# 21. ROC CURVES
# -----------------------------
rf_roc <- roc(
  test_data$HighValue,
  rf_probabilities,
  levels = c("No", "Yes"),
  direction = "<",
  quiet = TRUE
)

svm_roc <- roc(
  test_data$HighValue,
  svm_probabilities,
  levels = c("No", "Yes"),
  direction = "<",
  quiet = TRUE
)

plot(
  rf_roc,
  main = "ROC Curves - Random Forest vs SVM"
)

lines(
  svm_roc,
  lty = 2
)

legend(
  "bottomright",
  legend = c(
    paste0("Random Forest AUC = ",
           round(auc(rf_roc), 3)),
    paste0("SVM AUC = ",
           round(auc(svm_roc), 3))
  ),
  lty = c(1, 2)
)

# -----------------------------
# 22. RANDOM FOREST FEATURE IMPORTANCE
# -----------------------------
importance_values <- importance(rf_model)

importance_df <- data.frame(
  Feature = rownames(importance_values),
  MeanDecreaseGini = importance_values[, "MeanDecreaseGini"]
)

importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

cat("\n===== RANDOM FOREST FEATURE IMPORTANCE =====\n")
print(importance_df)

ggplot(
  importance_df,
  aes(
    x = reorder(Feature, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = "Mean Decrease in Gini"
  ) +
  theme_minimal()

# -----------------------------
# 23. INTERACTIVE 3D PCA PLOT
# -----------------------------
pca_3d <- plot_ly(
  pca_data,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  color = ~Cluster,
  colors = "Set1",
  type = "scatter3d",
  mode = "markers",
  text = ~paste(
    "Customer:", CustomerID,
    "<br>Cluster:", Cluster
  ),
  hoverinfo = "text"
)

pca_3d <- pca_3d %>%
  layout(
    title = "Interactive 3D PCA Customer Clusters",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

pca_3d

# -----------------------------
# 24. CLUSTER-WISE MARKETING RECOMMENDATIONS
# -----------------------------
# The following labels are based on relative cluster statistics.
# They are descriptive starting points; inspect cluster_profile
# before using them in the final report.

overall_median_frequency <- median(customer_model$Frequency)
overall_median_monetary <- median(customer_model$MonetaryValue)
overall_median_recency <- median(customer_model$Recency)

cluster_profile <- cluster_profile %>%
  mutate(
    SegmentDescription = case_when(
      AvgMonetaryValue >= overall_median_monetary &
        AvgFrequency >= overall_median_frequency &
        AvgRecency <= overall_median_recency ~
        "High-value / loyal customers",

      AvgMonetaryValue >= overall_median_monetary &
        AvgFrequency < overall_median_frequency ~
        "High-spend but less frequent customers",

      AvgMonetaryValue < overall_median_monetary &
        AvgRecency <= overall_median_recency ~
        "Recent / developing customers",

      TRUE ~
        "Low-engagement customers"
    )
  )

cat("\n===== CUSTOMER SEGMENT INTERPRETATION =====\n")
print(cluster_profile)

cat("\n===== MARKETING RECOMMENDATIONS =====\n")
cat("\n1. High-value / loyal customers:\n")
cat("   - Loyalty rewards\n")
cat("   - Exclusive offers\n")
cat("   - Early access to products\n")

cat("\n2. High-spend but less frequent customers:\n")
cat("   - Personalized reminders\n")
cat("   - Cross-selling and repeat-purchase offers\n")

cat("\n3. Recent / developing customers:\n")
cat("   - Welcome campaigns\n")
cat("   - Product recommendations\n")
cat("   - Limited-time incentives\n")

cat("\n4. Low-engagement customers:\n")
cat("   - Re-engagement campaigns\n")
cat("   - Targeted discounts\n")
cat("   - Win-back emails\n")

# -----------------------------
# 25. SAVE IMPORTANT OUTPUTS
# -----------------------------
write.csv(
  customer_model,
  "customer_features_and_clusters.csv",
  row.names = FALSE
)

write.csv(
  cluster_profile,
  "cluster_profile.csv",
  row.names = FALSE
)

write.csv(
  model_comparison,
  "model_comparison.csv",
  row.names = TRUE
)

write.csv(
  importance_df,
  "random_forest_feature_importance.csv",
  row.names = FALSE
)

cat("\n============================================================\n")
cat("EXPERIMENT COMPLETED\n")
cat("Files saved:\n")
cat(" - customer_features_and_clusters.csv\n")
cat(" - cluster_profile.csv\n")
cat(" - model_comparison.csv\n")
cat(" - random_forest_feature_importance.csv\n")
cat("============================================================\n")

# END OF PROGRAM

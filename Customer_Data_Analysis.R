# Step 1: Data Acquisition
# Import the customer transaction data into R
install.packages("table")
library('ggplot2')
setwd("/Users/jomilojuuuu/Downloads")
customer_data <- read.csv("Customers.csv", header = TRUE)

# Step 2: Data Cleaning and Preprocessing
# Handle missing values
customer_data <- na.omit(customer_data)

# Remove duplicates
customer_data <- unique(customer_data)

# Step 3: Exploratory Data Analysis (EDA)
# Perform descriptive statistics
summary(customer_data)

# Visualize data distributions
hist(customer_data$Annual.Income....)
hist(customer_data$Age)
hist(customer_data$Spending.Score..1.100.)
# Plot a bar chart
barplot(table(Gender), main = "Frequency of Genders", xlab = "Genders", ylab = "Frequency")
data <- read.csv("Customer_data.csv")
gender_counts <- table(customer_data$Gender)
barplot(gender_counts, main = "Gender Distribution", xlab = "Gender", ylab = "Count")

profession_counts <- table(customer_data$Profession)
barplot(profession_counts, main = "Profession Distribution", xlab = "Profession", ylab = "Count")

# Identify patterns or trends
scatter.smooth(customer_data$Work.Experience, customer_data$Family.Size)

plot(customer_data$Annual.Income...., customer_data$Spending.Score..1.100. )

library(ggplot2)
Genderplt =ggplot(customer_data, aes(x = Gender)) + geom_bar(fill = "orange") + labs(title = "Customer Gender Distribution")
Genderplt

# Assuming you have loaded the Customer_data.csv into the customer_data object

# Create the histogram plot
ageplt=(ggplot(customer_data, aes(x = Age)) +
        geom_histogram(fill = "maroon",bins = 10) +
        labs(title = "Customer Age Distribution"))
ageplt



Ageplt = ggplot(customer_data, aes(x = Age)) + geom_histogram(fill = "pink", bins = 10) + labs(title = "Customer Age Distribution")
Ageplt


# Create the scatter plot
Sctplt = ggplot(customer_data, aes(x = Annual.Income...., y = Spending.Score..1.100.)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Income vs Spending Score")

# Display the scatter plot
print(Sctplt)

# Step 4: Feature Engineering
# Extract relevant features
customer_features <- customer_data[, c("CustomerID", "Annual.Income....")]

# Step 5: Data Preparation
# Normalize the features
normalized_features <- scale(customer_features[, -1])

# Step 6: Clustering Analysis (K-means)
# Determine optimal number of clusters using the elbow method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(normalized_features, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

# Select the optimal number of clusters based on the plot

# Perform K-means clustering
k <- 4  # Update with the selected number of clusters
kmeans_model <- kmeans(normalized_features, centers = k, nstart = 10)

# Step 7: Interpretation and Visualization
# Analyze clustering results
cluster_labels <- kmeans_model$cluster
cluster_centers <- kmeans_model$centers

# Step 8: Insights and Recommendations
# Analyze characteristics of each cluster
for (i in 1:k) {
  cluster <- customer_data[cluster_labels == i, ]}

install.packages("dplyr")
install.packages("farver")
install.packages("ggplot2")
install.packages("colorspace")
install.packages("scatterplot3d")
install.packages("gridExtra")
install.packages("superml")
install.packages('party')
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")

library(rpart)
library(rpart.plot)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(scatterplot3d)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library('superml')
library(caret)
library(party)
library(fpc)
library(FSelector)
library(cluster)
library(factoextra)
library('DPBBM')

dataset <- read_csv("Dataset/students_adaptability_level_online_education.csv")

# Sample of the raw dataset
View(dataset)

shape<-dim(dataset)
row<-shape[1]
col<-shape[2]
print(paste("Number of rows are:", row))
print(paste("Number of columns are:", col))

summary(dataset)
names(dataset)
str(dataset)

# Missing values
sum(is.na(dataset))


# Bar plot of the gender attribute
ggplot(dataset, mapping = aes(x = Gender)) +
  geom_bar(aes(fill = Gender), position = "dodge")

# Bar plot of the age attribute
ggplot(dataset, mapping = aes(x = Age)) +
  geom_bar(aes(fill = Age), position = "dodge")

# Bar plot of the education level attribute
ggplot(dataset, mapping = aes(x = `Education Level`)) +
  geom_bar(aes(fill = `Education Level`), position = "dodge")


# Mosaic plot of the educational level attribute with the financial condition
counts <- table(dataset$'Education Level', dataset$'Financial Condition')
mosaicplot(counts, xlab='Education Level', ylab='Financial Condition',
           main='Education Level by Financial Condition', col='forestgreen')


# Pie chart for the self-LMS attribute
tab <- dataset$'Self Lms'%>% table()
precentages <- tab %>% prop.table() %>% round(2) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') 
pie(tab, labels=txt, main = "Distribution of self-LMS")

# Pie chart for the IT student attribute
tab <- dataset$'IT Student'%>% table()
precentages <- tab %>% prop.table() %>% round(2) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') 
pie(tab, labels=txt, main = "Distribution of IT student")

# Pie chart for the adaptivity level attribute
tab <- dataset$'Adaptivity Level' %>% table()
percentages <- tab %>% prop.table() %>% round(3) * 100
txt <- paste0(names(tab), '\n', percentages, '%')
pie(tab, labels = txt, main = "Distribution of Adaptivity Levels")


# Encoding categorical attributes
categorical_cols <- dataset %>%
  select_if(is.character) %>%
  names()
categorical_cols
# Convert categorical columns to factors
dataset[categorical_cols] <- lapply(dataset[categorical_cols], as.factor)


##############################################


### 90/10 Distribution ###
set.seed(1234)
split90 <- createDataPartition(dataset$`Adaptivity Level`, p = 0.9, list = FALSE, times = 1)
trainData <- dataset[split90,]
testData <- dataset[split90,]

# Checking that both the training and testing sets have the same label proportions.
train90Prop <- trainData %>% 
  select(`Adaptivity Level`) %>% 
  group_by(`Adaptivity Level`) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(prop.table(n), 2))

test90Prop <- testData %>% 
  select(`Adaptivity Level`) %>% 
  group_by(`Adaptivity Level`) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(prop.table(n), 2))

train90Prop
test90Prop

infoGainTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "information"))
gainRatioTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "anova"))
GiniIndexTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "gini"))

# Information Gain
IG1.ctree <- ctree(infoGainTree, data = trainData)
table(predict(IG1.ctree), trainData$`Adaptivity Level`)

print(IG1.ctree)
plot(IG1.ctree, type="simple")

testPred <- predict(IG1.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

results <- confusionMatrix(testPred, testData$`Adaptivity Level`)
print(results)

# Gain Ratio
GR1.ctree <- ctree(gainRatioTree, data = trainData)
table(predict(GR1.ctree), trainData$`Adaptivity Level`)

print(GR1.ctree)
plot(GR1.ctree, type="simple")

testPred <- predict(GR1.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

# Create confusion matrix
confusion_matrix <- confusionMatrix(data = testPred, reference = testData$`Adaptivity Level`)
# Calculate evaluation metrics
accuracy <- confusion_matrix$overall["Accuracy"]
accuracy
# Calculate precision
precision <- confusion_matrix$table[2,2] / sum(confusion_matrix$table[,2])
precision
as.matrix(results, what = "overall")



# Gini Index
GI1.ctree <- ctree(GiniIndexTree, data = trainData)
table(predict(GI1.ctree), trainData$`Adaptivity Level`)

print(GI1.ctree)
plot(GI1.ctree, type="simple")

testPred <- predict(GI1.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

results <- confusionMatrix(testPred, testData$`Adaptivity Level`)
print(results)


### 80/20 Distribution ###
set.seed(1234)
split80 <- createDataPartition(dataset$`Adaptivity Level`, p = 0.8, list = FALSE, times = 1)
trainData <- dataset[split80,]
testData <- dataset[split80,]

# Checking that both the training and testing sets have the same label proportions.
train80Prop <- trainData %>% 
  select(`Adaptivity Level`) %>% 
  group_by(`Adaptivity Level`) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(prop.table(n), 2))

test80Prop <- testData %>% 
  select(`Adaptivity Level`) %>% 
  group_by(`Adaptivity Level`) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(prop.table(n), 2))

train80Prop
test80Prop

infoGainTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "information"))
gainRatioTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "anova"))
GiniIndexTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "gini"))

# Information Gain
IG2.ctree <- ctree(infoGainTree, data = trainData)
table(predict(IG2.ctree), trainData$`Adaptivity Level`)

print(IG2.ctree)
plot(IG2.ctree, type="simple")

testPred <- predict(IG2.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

results <- confusionMatrix(testPred, testData$`Adaptivity Level`)
print(results)

# Gain Ratio
GR2.ctree <- ctree(gainRatioTree, data = trainData)
table(predict(GR2.ctree), trainData$`Adaptivity Level`)

print(GR2.ctree)
plot(GR2.ctree, type="simple")

testPred <- predict(GR2.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

# Create confusion matrix
confusion_matrix <- confusionMatrix(data = testPred, reference = testData$`Adaptivity Level`)
# Calculate evaluation metrics
accuracy <- confusion_matrix$overall["Accuracy"]
accuracy
# Calculate precision
precision <- confusion_matrix$table[2,2] / sum(confusion_matrix$table[,2])
precision
as.matrix(results, what = "overall")



# Gini Index
GI2.ctree <- ctree(GiniIndexTree, data = trainData)
table(predict(GI2.ctree), trainData$`Adaptivity Level`)

print(GI2.ctree)
plot(GI2.ctree, type="simple")

testPred <- predict(GI2.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

results <- confusionMatrix(testPred, testData$`Adaptivity Level`)
print(results)


### 70/30 Distribution ###
set.seed(1234)
split70 <- createDataPartition(dataset$`Adaptivity Level`, p = 0.7, list = FALSE, times = 1)
trainData <- dataset[split70,]
testData <- dataset[split70,]

# Checking that both the training and testing sets have the same label proportions.
train70Prop <- trainData %>% 
  select(`Adaptivity Level`) %>% 
  group_by(`Adaptivity Level`) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(prop.table(n), 2))

test70Prop <- testData %>% 
  select(`Adaptivity Level`) %>% 
  group_by(`Adaptivity Level`) %>% 
  summarize(n = n()) %>% 
  mutate(pct = round(prop.table(n), 2))

train70Prop
test70Prop

infoGainTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "information"))
gainRatioTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "anova"))
GiniIndexTree <- rpart(formula = `Adaptivity Level` ~ ., data = trainData, method = "class", parms = list(split = "gini"))

# Information Gain
IG3.ctree <- ctree(infoGainTree, data = trainData)
table(predict(IG3.ctree), trainData$`Adaptivity Level`)

print(IG3.ctree)
plot(IG3.ctree, type="simple")

testPred <- predict(IG3.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

results <- confusionMatrix(testPred, testData$`Adaptivity Level`)
print(results)

# Gain Ratio
GR3.ctree <- ctree(gainRatioTree, data = trainData)
table(predict(GR3.ctree), trainData$`Adaptivity Level`)

print(GR3.ctree)
plot(GR3.ctree, type="simple")

testPred <- predict(GR3.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

# Create confusion matrix
confusion_matrix <- confusionMatrix(data = testPred, reference = testData$`Adaptivity Level`)
# Calculate evaluation metrics
accuracy <- confusion_matrix$overall["Accuracy"]
accuracy
# Calculate precision
precision <- confusion_matrix$table[2,2] / sum(confusion_matrix$table[,2])
precision
as.matrix(results, what = "overall")



# Gini Index
GI3.ctree <- ctree(GiniIndexTree, data = trainData)
table(predict(GI3.ctree), trainData$`Adaptivity Level`)

print(GI3.ctree)
plot(GI3.ctree, type="simple")

testPred <- predict(GI3.ctree, newdata = testData)
result<-table(testPred, testData$`Adaptivity Level`)

results <- confusionMatrix(testPred, testData$`Adaptivity Level`)
print(results)



#######CLUSTERING#######

# load the dataset 
df<-dataset


#drop the class label (Adaptivity Level))
df<-df[1:13]
str(df)

distinct_data_points <- nrow(unique(df))
print(distinct_data_points)


#make this example reproducible
set.seed(1234)

df_numeric <- as.data.frame(lapply(df, as.numeric))

# Determine the maximum value of k you want to try
max_k <- 5

# Create an empty list to store the results
kmeans_results <- list()

# Loop through different k values
for (k in 1:max_k) {
  kmeans_results[[k]] <- kmeans(df_numeric, centers = k, nstart = 25)
}

# Access the results for a specific k value (e.g., k=5)
k5_results <- kmeans_results[[5]]
k5_results
k4_results <- kmeans_results[[4]]
k4_results
k2_results <- kmeans_results[[2]]
k2_results


# Visualize the clustering
fviz_cluster(kmeans_results[[5]], data = df_numeric, geom = "point")

# Evaluate and compare the clustering results
silhouette_score <- silhouette(kmeans_results[[5]]$cluster, dist(df_numeric))
silhouette_score


# Calculate the total within-cluster sum of squares
within_cluster_sum_squares <- kmeans_results[[5]]$tot.withins
within_cluster_sum_squares



Bcubed_score <- BCubed_metric(dataset$`Adaptivity Level`, kmeans_results[[5]]$cluster,0.5)
Bcubed_score

df1<-data.frame(Actual=dataset$`Adaptivity Level`,cluster=kmeans_results[[5]]$cluster)
head(df1,15)
cont_table <- table(df1$Actual, df1$cluster)
cont_table
differences <- sum(apply(cont_table, 1, max)) - sum(diag(cont_table))
differences
heatmap(cont_table, 
        col = heat.colors(max(cont_table)),
        main = "Cluster Assignments vs. Actual Class Labels",
        xlab = "Cluster Labels",
        ylab = "Actual Class Labels")



# Visualize the clustering
fviz_cluster(kmeans_results[[2]], data = df_numeric, geom = "point")

# Evaluate and compare the clustering results
silhouette_score <- silhouette(kmeans_results[[2]]$cluster, dist(df_numeric))
silhouette_score


# Calculate the total within-cluster sum of squares
within_cluster_sum_squares <- kmeans_results[[2]]$tot.withins
within_cluster_sum_squares



Bcubed_score <- BCubed_metric(dataset$`Adaptivity Level`, kmeans_results[[2]]$cluster,0.5)
Bcubed_score

df1<-data.frame(Actual=dataset$`Adaptivity Level`,cluster=kmeans_results[[2]]$cluster)
head(df1,15)
cont_table <- table(df1$Actual, df1$cluster)
cont_table
differences <- sum(apply(cont_table, 1, max)) - sum(diag(cont_table))
differences
heatmap(cont_table, 
        col = heat.colors(max(cont_table)),
        main = "Cluster Assignments vs. Actual Class Labels",
        xlab = "Cluster Labels",
        ylab = "Actual Class Labels")


#perform k-means clustering with k = 4 |clusters
# Visualize the clustering
fviz_cluster(kmeans_results[[4]], data = df_numeric, geom = "point")

# Evaluate and compare the clustering results
silhouette_score <- silhouette(kmeans_results[[4]]$cluster, dist(df_numeric))
silhouette_score


# Calculate the total within-cluster sum of squares
within_cluster_sum_squares <- kmeans_results[[4]]$tot.withins
within_cluster_sum_squares



Bcubed_score <- BCubed_metric(dataset$`Adaptivity Level`, kmeans_results[[4]]$cluster,0.5)
Bcubed_score

df1<-data.frame(Actual=dataset$`Adaptivity Level`,cluster=kmeans_results[[4]]$cluster)
head(df1,15)
cont_table <- table(df1$Actual, df1$cluster)
cont_table
differences <- sum(apply(cont_table, 1, max)) - sum(diag(cont_table))
differences
heatmap(cont_table, 
        col = heat.colors(max(cont_table)),
        main = "Cluster Assignments vs. Actual Class Labels",
        xlab = "Cluster Labels",
        ylab = "Actual Class Labels")

fviz_nbclust(df, kmeans, method = "wss")

# we can observer from the plot is 3 is the optimal value 

#perform k-means clustering with k = 3 |clusters
k3_results <- kmeans_results[[3]]
k3_results

#plot results of final k-means model
fviz_cluster(kmeans_results[[3]], data = df_numeric, geom = "point")

# Evaluate and compare the clustering results
silhouette_score <- silhouette(kmeans_results[[3]]$cluster, dist(df_numeric))
silhouette_score


# Calculate the total within-cluster sum of squares
within_cluster_sum_squares <- kmeans_results[[3]]$tot.withins
within_cluster_sum_squares



Bcubed_score <- BCubed_metric(dataset$`Adaptivity Level`, kmeans_results[[3]]$cluster,0.5)
Bcubed_score

df1<-data.frame(Actual=dataset$`Adaptivity Level`,cluster=kmeans_results[[3]]$cluster)
head(df1,15)
cont_table <- table(df1$Actual, df1$cluster)
cont_table
differences <- sum(apply(cont_table, 1, max)) - sum(diag(cont_table))
differences
heatmap(cont_table, 
        col = heat.colors(max(cont_table)),
        main = "Cluster Assignments vs. Actual Class Labels",
        xlab = "Cluster Labels",
        ylab = "Actual Class Labels")



Actual<-dataset$`Adaptivity Level`
cluster<-km$cluster
# Calculate BCubed precision and recall
stats <- cluster.stats(Actual, cluster)
stats


clusplot(df, km$cluster, color = TRUE, shade = TRUE, labels = 2)


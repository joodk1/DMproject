install.packages("dplyr")
install.packages("farver")
install.packages("ggplot2")
install.packages("colorspace")
install.packages("scatterplot3d") 

library(scatterplot3d)
library(ggplot2)
library(dplyr)
library(readr)
dataset <- read_csv("Dataset/students_adaptability_level_online_education.csv")

# Sample of the raw dataset
View(dataset)
summary(dataset)

# Missing values
sum(is.na(dataset))

# Data summary
summary(dataset$Age)
summary(dataset$Gender)
summary(dataset$Device)
summary(dataset$Location)
summary(dataset$'Self Lms')
summary(dataset$'IT Student')
summary(dataset$'Network Type')
summary(dataset$'Internet Type')
summary(dataset$'Load-shedding')
summary(dataset$'Class Duration')
summary(dataset$'Education Level')
summary(dataset$'Adaptivity Level')
summary(dataset$'Institution Type')
summary(dataset$'Financial Condition')


# Converting data types from char to factors so they can be plotted
dataset$'Financial Condition' <- as.factor(dataset$'Financial Condition')
dataset$'Institution Type' <- as.factor(dataset$'Institution Type')
dataset$'Education Level' <- as.factor(dataset$'Education Level')
dataset$'Load-shedding' <- as.factor(dataset$'Load-shedding')
dataset$'Internet Type'<- as.factor(dataset$'Internet Type')
dataset$'Network Type'<- as.factor(dataset$'Network Type')
dataset$Device <- as.factor(dataset$Device)
dataset$Gender<- as.factor(dataset$Gender)
dataset$Age <- as.factor(dataset$Age)


# Bar plot of the age attribute
ggplot(dataset, aes(x = Age)) + geom_bar()


# Mosaic plot of the educational level attribute with the financial condition
counts <- table(dataset$'Education Level', dataset$'Financial Condition')
mosaicplot(counts, xlab='Education Level', ylab='Financial Condition',
           main='Education Level by Financial Condition', col='forestgreen')


# Scatter plot of the gender and device and class duration attributes
scatterplot3d(dataset$Gender,dataset$Device, dataset$`Class Duration`)
# Scatter plot of the internet type and load-shedding and institution type attributes
scatterplot3d(dataset$'Internet Type',dataset$'Load-shedding', dataset$'Institution Type')


# Pie chart for the adaptivity level attribute
tab <- dataset$'Adaptivity Level'%>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') 
pie(tab, labels=txt) 
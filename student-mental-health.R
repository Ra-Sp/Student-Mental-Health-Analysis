library(tidyverse)
library(plotly)
library(readr)
library(ggplot2)

health <- read.csv("C:\\Users\\Radhika\\Desktop\\Semester 4\\Stat\\Project\\Student Mental health.csv")

# Data Cleaning
names(health) <- c('Timestamp', 'Gender', 'Age', 'Course', 'Year', 'CGPA', 'Married', 'Depression', 'Anxiety', 'Panic_Attack', 'Treatment')
health$Age[is.na(health$Age)] <- median(health$Age, na.rm = TRUE)


#------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data Visualization

# Age Distribution
health %>%
  group_by(Age) %>%
  summarize(count = n()) %>%
  plot_ly(x = ~Age, y = ~count, type = 'bar',
          text = ~count,
          textposition = 'outside',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'black',
                                    width = 1.0))) %>%
  layout(title = 'Distribution of Age')

# Gender Distribution
Health_SummaryStat <- health %>%
  group_by(Gender) %>%
  summarise(count = n(),
            percentage = round((n()/ nrow(health)), digits = 4))
colors <- c('rgb(211,94,96)','rgb(114,147,203)')
Gender_PieChart <- plot_ly(data = Health_SummaryStat, labels = ~Gender, values = ~percentage,
                           type = 'pie', sort = FALSE,
                           textposition = 'inside',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = 'White'),
                           hoverinfo = 'text',
                           text = ~count,
                           marker = list(colors = colors,
                                         line = list(color = 'Black', width = 1)),
                           showlegend = TRUE) 
Gender_PieChart <- Gender_PieChart %>% layout(title = 'Pie Chart of Gender')
Gender_PieChart

# Depression 
Health_SummaryStat2 <- health %>%
  group_by(Depression) %>%
  summarise(count = n(),
            percentage = round((n()/ nrow(health)), digits = 4))
Depression_PieChart <- plot_ly(data = Health_SummaryStat2, labels = ~Depression, values = ~percentage,
                               type = 'pie', sort = FALSE,
                               textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = 'White'),
                               hoverinfo = 'text',
                               text = ~count,
                               marker = list(colors = colors,
                                             line = list(color = 'Black', width = 1)),
                               showlegend = TRUE) 
Depression_PieChart %>% layout(title = 'Pie Chart of Depression')


# Depression vs Gender
health %>% 
  count(Gender, Depression, sort = FALSE) %>%
  group_by(Gender) %>%
  mutate(prop = round((n / sum(n)),digits = 4)) %>%
  plot_ly(x = ~Gender, y = ~prop, color = ~Depression, type = "bar",
          text = ~paste(Gender, prop*100 ,'%'),
          textposition = 'outside') %>%
  layout(barmode = 'stack',
         title = 'Barplot of Depression amongst Genders')

# CGPA
health$CGPA <- as.factor(health$CGPA)
health %>%
  group_by(CGPA)%>%
  summarize(count = n()) %>%
  plot_ly(x =~CGPA, y=~count, type = 'bar',
          text = ~count,
          textposition = 'outside',
          marker = list(color = 'rgb(158,202,225)',
                        line = list(color = 'black',
                                    width = 1.0))) %>%
  layout(title = 'Distibution of CGPA')

health %>%
  count(CGPA, Depression, sort = F) %>%
  mutate(proportion = round((n/sum(n)),digits=4)) %>%
  plot_ly(x =~CGPA, y=~proportion, color = ~Depression, type = 'bar') %>%
  layout(barmode = 'Group',
         title = 'Barplot of Depression vs CGPA')

# Courses
health %>% 
  group_by(Course) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count >2)
health %>%
  filter(grepl('BIT|KOE|BCS|Engineering|Biomedical science', Course)) %>%
  count(Course, Depression, sort = T) %>%
  group_by(Course) %>%
  mutate(prop = round((n / sum(n)),digits = 4)) %>%
  plot_ly(x = ~Course, y=~n, color = ~Depression, type = "bar",
          text = ~paste(Course, n),
          textposition = 'outside') %>%
  layout(barmode = 'Stacked',
         title = 'Barplot of Depression amongst the top 5 Courses')

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Logistic Regression

CGPA = health$CGPA
Depressed = health$Depression
data <- data.frame(CGPA, Depressed)
data$Depressed = factor(data$Depressed, labels = c(0, 1))

# fitting the logistic regression model
model <- glm(Depressed ~ CGPA, data = data, family = binomial())

summary(model)

# predict the probability of depression based on CGPA
newdata <- data.frame(CGPA = "3.00 - 3.49")
predict(model, newdata, type = "response")

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Plotting the regression model

Age = health$Age
Depressed = ifelse(health$Depression == "Yes", 1, 0)
data <- data.frame(Age, Depressed)

age_seq <- seq(min(Age), max(Age), length.out = 7)

# predict the probability of depression vs age
probs <- predict(model, newdata = data.frame(Age = age_seq), type = "response")

# plot the logistic regression curve
ggplot(health, aes(x = Age, y = Depressed)) +
  geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE, method.args = list(family=binomial)) +
  xlab("Age") +
  ylab("Probability of Depression") +
  ggtitle("Logistic Regression Model")

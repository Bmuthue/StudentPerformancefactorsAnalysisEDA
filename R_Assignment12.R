raw_data<-StudentPerformanceFactors

#number of records and variables 
dim(raw_data)

#names of the variables 
names(raw_data)

#types of the data within 
str(raw_data)

#Data Preparation
#Handling missing values 
print(colSums(is.na(raw_data)))

#there are some null values so removing those
clean_data<-na.omit(raw_data)
print(colSums(is.na(clean_data)))

#checking for the number of records after cleaning 
dim(clean_data)

#removing duplicates 
data<-unique(clean_data)
print(data)

#checking for the number of records after removing duplicates 
dim(data)

#UNIVARIATE ANALYSIS
#analyzing & visualizing numerical variables 
num_vars<-c(
  "Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", 
  "Exam_Score", "Tutoring_Sessions", "Physical_Activity")

library(e1071)
for (var in num_vars) {
  cat("\nVariable:", var, "\n")
  cat("Max: ", max(data[[var]]),"\n")
  cat("Min: ", min(data[[var]]),"\n")
  cat("Mean: ", mean(data[[var]]),"\n")
  cat("Median: ", median(data[[var]]),"\n")
  mode_val <- as.numeric(names(sort(table(data[[var]]), decreasing = TRUE)[1]))
  cat("Mode: ", mode_val, "\n")
  cat("Percentiles (10th, 25th, 50th, 75th, 90th):\n ")
  print(quantile(data[[var]], probs = c(0.10, 0.25, 0.50, 0.75, 0.90)))
  cat("Standard Deviation: ", sd(data[[var]]), "\n")
  cat("Variance: ", var(data[[var]]), "\n")
  cat("\n", var, "\n")
  cat("Skewness:", skewness(data[[var]]), "\n")
  cat("Kurtosis:", kurtosis(data[[var]]), "\n")
  
  
  hist(data[[var]],
       main = paste("Histogram of",var),
       xlab = var,
       col = "lightblue",
       border = "black")
}

# sleep hours, tutoring sessions, physical activity; why gaps?? 
# check for it !!!!

#visualizing categorical variables 
cat_vars <- c(
  "Parental_Involvement", "Access_to_Resources", "Extracurricular_Activities",
  "Motivation_Level", "Internet_Access", "Family_Income", "Teacher_Quality",
  "School_Type", "Peer_Influence", "Learning_Disabilities",
  "Parental_Education_Level", "Distance_from_Home", "Gender")

for (var in cat_vars) {
  cat("\nVariable:", var, "\n")
  freq_tbl <- table(data[[var]])
  print(freq_tbl)
  pie(freq_tbl,
      main = paste("Pie Chart of", var),
      col = rainbow(length(freq_tbl)))
}

#BIVARIATE ANALYSIS
#finding relationship between exam_score and other variables 
num_vars01 <- c("Hours_Studied", "Attendance", "Sleep_Hours", 
              "Previous_Scores", "Tutoring_Sessions", "Physical_Activity")

for (var in num_vars01) {
  cat("Relationship between", var, "and Exam Score\n")
  plot(data[[var]], data$Exam_Score,
       main = paste("Scatter Plot:", var, "vs Exam Score"),
       xlab = var,
       ylab = "Exam Score",
       col = "darkgreen", pch = 19)
  
  corr_val <- cor(data[[var]], data$Exam_Score, use = "complete.obs")
  cat("Correlation: ", round(corr_val, 3), "\n\n")
}

#following relationships are shown as Boxplot 
for (var in cat_vars) {
  cat("Relationship between", var, "and Exam Score\n")
  boxplot(data$Exam_Score ~ data[[var]],
          main = paste("Exam Score by", var),
          xlab = var,
          ylab = "Exam Score",
          col = c("lightblue", "lightpink", "lightgreen"))
  cat("Summary statistics of Exam Score by", var, ":\n")
  print(tapply(data$Exam_Score, data[[var]], summary))
}

#relationship between School type & Teacher Quality(c&c)
freq_table <- table(data$School_Type, data$Teacher_Quality)
print(freq_table)
barplot(freq_table,
        main = "School Type by Teacher Quality",
        xlab = "Teacher Quality",
        ylab = "Count",
        col = c("pink","blue"),
        beside = FALSE)                 

#relationship between sleep hours & physical activity(n&n)
# Calculate correlation
corr <- cor(data$Sleep_Hours, data$Physical_Activity)

# Display correlation coefficient
cat("Correlation coefficient between Sleep Hours and Physical Activity:", round(corr, 2), "\n")

# Scatter plot with correlation
plot(data$Sleep_Hours, data$Physical_Activity,
     main = "Sleep Hours vs Physical Activity",
     xlab = "Sleep Hours",
     ylab = "Physical Activity",
     col = "blue", pch = 19)


#relationship between sleep hours & motivation level 
#check it outttt!!!!!!
boxplot(data$Sleep_Hours ~ data$Motivation_Level, 
        main = "Sleep Hours by Motivation Level",
        xlab = "Motivation Level",
        ylab = "Sleep Hours",
        col = "pink")


# Summary statistics
tapply(data$Sleep_Hours, data$Motivation_Level, summary)

# Boxplot
boxplot(data$Sleep_Hours ~ data$Motivation_Level, 
        main = "Sleep Hours by Motivation Level",
        xlab = "Motivation Level",
        ylab = "Sleep Hours",
        col = "pink")


#MULTIVARIATE ANALYSIS
#plot for sleep hours + motivation level + exam score
boxplot(Exam_Score ~ Motivation_Level+Sleep_Hours,
        data = data,
        main = "Exam Score by Sleep hours and Motivational level",
        xlab = "Motivational level and Sleep hours",
        ylab = "Exam Score",
        col = c("green","blue","pink"))
#plot for Hours_Studied + Physical_Activity + Sleep_Hours+ Exam_Score
cor_data <-cor(data[,c("Hours_Studied","Physical_Activity","Sleep_Hours","Exam_Score")])
library(corrplot)
corrplot::corrplot(cor_data, 
                   method = "color", addCoef.col="red")
cor_data2 <- cor(data[,c("Sleep_Hours", "Physical_Activity","Exam_Score")])
corrplot::corrplot(cor_data2,
                   method = "color",addCoef.col = "red")

#Multivariate Analysis
#multivariate analysis for exam_score_access resources_family income


interaction_factor <- interaction(StudentPerformanceFactors$Access_to_Resources,
                                  StudentPerformanceFactors$Family_Income)

# Boxplot
boxplot(Exam_Score ~ interaction_factor, data = StudentPerformanceFactors,
        las = 2, col = rainbow(length(unique(interaction_factor))),
        main = "Exam Score by Access to Resources and Income Level",
        xlab = "Access × Income Level", ylab = "Exam Score")
#Previous_Score + Physical_Activity + Exam_Score

colors <- heat.colors(100)[cut(StudentPerformanceFactors$Exam_Score, breaks = 100)]

# Scatter plot: Previous_Score vs Physical_Activity, colored by Exam_Score
plot(StudentPerformanceFactors$Previous_Score, StudentPerformanceFactors$Physical_Activity,
     col = colors, pch = 19,
     xlab = "Previous Score", ylab = "Physical Activity",
     main = "Previous Score vs Physical Activity (Color = Exam Score)")

# Optional: add color legend manually if needed
legend("topright", legend = "Color = Exam Score", fill = "orange", border = "black")
cor_data1 <-cor(data[,c("Hours_Studied", "Attendance", "Exam_Score")])
library(corrplot)
corrplot::corrplot(cor_data1, method= "color",addCoef.col ="Black")

cor_data2 <-cor(data[,c("Sleep_Hours", "Physical_Activity","Exam_Score")])
corrplot::corrplot(cor_data2, method = "color", addCoef.col = "Black" )

#MULTIVARIATE ANALYSIS

# Interaction plot for Exam Score by Peer Influence and Extracurricular Activities
interaction.plot(data$Peer_Influence, data$Extracurricular_Activities, data$Exam_Score,
                 main = "Interaction: Peer Influence & Extracurricular Activities on Exam Score",
                 xlab = "Peer Influence",
                 ylab = "Mean Exam Score",
                 col = c("blue", "green", "red"),
                 legend = TRUE)

# Mean Exam Score table
with(data, tapply(Exam_Score, list(Peer_Influence, Extracurricular_Activities), mean))


# Interaction plot
interaction.plot(data$Parental_Education_Level, data$Distance_from_Home, data$Exam_Score,
                 main = "Interaction: Parental Education & Distance from Home on Exam Score",
                 xlab = "Parental Education Level",
                 ylab = "Mean Exam Score",
                 col = c("orange", "purple", "darkgreen"),
                 legend = TRUE)

# Mean Exam Score table
with(data, tapply(Exam_Score, list(Parental_Education_Level, Distance_from_Home), mean))


# Q-Q plot for Hours_Studied
qqnorm(data$Hours_Studied)
qqline(data$Hours_Studied)

qqnorm(data$Attendance)
qqline(data$Attendance)

qqnorm(data$Sleep_Hours)
qqline(data$Sleep_Hours)

qqnorm(data$Previous_Scores)
qqline(data$Previous_Scores)

qqnorm(data$Tutoring_Sessions)
qqline(data$Tutoring_Sessions)

qqnorm(data$Physical_Activity)
qqline(data$Physical_Activity)

qqnorm(data$Exam_Score)
qqline(data$Exam_Score)

gender_school_table <- table(data$Gender, data$School_Type)
chi_test1 <- chisq.test(gender_school_table)
print(chi_test1)

table2 <- table(data$Parental_Education_Level, data$Family_Income)
chi_test2<-chisq.test(table2)
print(chi_test2)

table3 <- table(data$School_Type, data$Internet_Access)
chi_test3<-chisq.test(table3)
print(chi_test3)

t_test1 <- t.test(Exam_Score ~ Gender, data = data)
print(t_test1)

t_test2<-t.test(Exam_Score ~ School_Type, data = data)
print(t_test2)

t_test3<-t.test(Exam_Score ~ Internet_Access, data = data)
print(t_test3)




cor_test <- cor.test(data$Hours_Studied, data$Exam_Score, method = "pearson")
print(cor_test)

cor.test(data$Hours_Studied, data$Exam_Score, method = "spearman")

#find outliers from boxplots
num_vars <- c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", "Exam_Score", "Tutoring_Sessions", "Physical_Activity")
for(var in num_vars){
  boxplot(data[[var]],
          main = paste("Outliers of ",var),
          ylab = "Values",
          col = "lightblue")
}


library(readr)
data <- read_csv("E:/KDU Semester 02/P & S/R Assignment/StudentPerformanceFactors.csv")
View(StudentPerformanceFactors)
head(data)
names(data)
summary(data)
str(data)
dim(data)
#Use na.rm = TRUE to return Null values as NA#
#Hours Studied: Numerical, Continuous#
paste("Max Hours Studied: ", max(data$Hours_Studied, na.rm = TRUE))
paste("Min Hours Studied: ", min(data$Hours_Studied, na.rm = TRUE))
paste("Mean Hours Studied: ", mean(data$Hours_Studied, na.rm = TRUE))
paste("Median Hours Studied: ", median(data$Hours_Studied, na.rm = TRUE))
mode_hours <- as.numeric(names(sort(table(data$Hours_Studied), decreasing = TRUE)[1]))
paste("Mode Hours Studied: ", mode_hours)
paste("Mode Hours Studied: ", mode_hours)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Hours Studied: ", quantile(data$Hours_Studied, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
hist(data$Hours_Studied,
     main = "Histogram of Hours Studied",
     xlab = "Hours Studied",
     col = "lightblue",
     border = "black")
#Exam Score : Numerical, Continuous#
paste("Max Exam Score: ", max(data$Exam_Score, na.rm = TRUE))
paste("Min Exam Score: ", min(data$Exam_Score, na.rm = TRUE))
paste("Mean Exam Score: ", mean(data$Exam_Score, na.rm = TRUE))
paste("Median Exam Score: ", median(data$Exam_Score, na.rm = TRUE))
mode_exam <- as.numeric(names(sort(table(data$Exam_Score), decreasing = TRUE)[1]))
paste("Mode Exam Score: ", mode_exam)
paste("Mode Exam Score: ", mode_exam)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Exam Score: ", quantile(data$Exam_Score, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
hist(data$Exam_Score,
     main = "Histogram of Exam Score",
     xlab = "Exam Score",
     col = "lightblue",
     border = "black")
#Attendance : Numerical, Continuous(Percentage)#
paste("Max Attendance: ", max(data$Attendance, na.rm = TRUE))
paste("Min Attendance: ", min(data$Attendance, na.rm = TRUE))
paste("Mean Attendance: ", mean(data$Attendance, na.rm = TRUE))
paste("Median Attendance: ", median(data$Attendance, na.rm = TRUE))
mode_attendance <- as.numeric(names(sort(table(data$Attendance), decreasing = TRUE)[1]))
paste("Mode Attendance: ", mode_attendance)
paste("Mode Attendance: ", mode_attendance)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Attendance: ", quantile(data$Attendance, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
boxplot(data$Attendance,
        main = "Boxplot of Attendance",
        ylab = "Attendance",
        col = "lightgreen")
#Sleep Hours: Numerical, Continuous#
paste("Max Sleep Hours: ", max(data$Sleep_Hours, na.rm = TRUE))
paste("Min Sleep Hours: ", min(data$Sleep_Hours, na.rm = TRUE))
paste("Mean Sleep Hours: ", mean(data$Sleep_Hours, na.rm = TRUE))
paste("Median Sleep Hours: ", median(data$Sleep_Hours, na.rm = TRUE))
mode_sleep <- as.numeric(names(sort(table(data$Sleep_Hours), decreasing = TRUE)[1]))
paste("Mode Sleep Hours: ", mode_sleep)
paste("Mode Sleep Hours: ", mode_sleep)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Sleep Hours: ", quantile(data$Sleep_Hours, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
boxplot(data$Sleep_Hours,
        main = "Boxplot of Sleep Hours",
        ylab = "Sleep Hours",
        col = "lightgreen")
#Previous Scores : Numerical, Continuous#
paste("Max Previous Scores: ", max(data$Previous_Scores, na.rm = TRUE))
paste("Min Previous Scores: ", min(data$Previous_Scores, na.rm = TRUE))
paste("Mean Previous Scores: ", mean(data$Previous_Scores, na.rm = TRUE))
paste("Median Previous Scores: ", median(data$Previous_Scores, na.rm = TRUE))
mode_prev_scores <- as.numeric(names(sort(table(data$Previous_Scores), decreasing = TRUE)[1]))
paste("Mode Previous Scores: ", mode_prev_scores)
paste("Mode Previous Scores: ", mode_prev_scores)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Previous Scores: ", quantile(data$Previous_Scores, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
hist(data$Previous_Scores,
     main = "Histogram of Previous Scores",
     xlab = "Previous Scores",
     col = "lightblue",
     border = "black")
#Tutoring Sessions : Numerical, Discrete#
paste("Max Tutoring Sessions: ", max(data$Tutoring_Sessions, na.rm = TRUE))
paste("Min Tutoring Sessions: ", min(data$Tutoring_Sessions, na.rm = TRUE))
paste("Mean Tutoring Sessions: ", mean(data$Tutoring_Sessions, na.rm = TRUE))
paste("Median Tutoring Sessions: ", median(data$Tutoring_Sessions, na.rm = TRUE))
mode_tutoring <- as.numeric(names(sort(table(data$Tutoring_Sessions), decreasing = TRUE)[1]))
paste("Mode Tutoring Sessions: ", mode_tutoring)
paste("Mode Tutoring Sessions: ", mode_tutoring)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Tutoring Sessions: ", quantile(data$Tutoring_Sessions, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
boxplot(data$Tutoring_Sessions,
        main = "Boxplot of Tutoring Sessions",
        ylab = "Tutoring Sessions",
        col = "lightgreen")
#Physical activity : Numerical,Discrete#
paste("Max Physical Activity: ", max(data$Physical_Activity, na.rm = TRUE))
paste("Min Physical Activity: ", min(data$Physical_Activity, na.rm = TRUE))
paste("Mean Physical Activity: ", mean(data$Physical_Activity, na.rm = TRUE))
paste("Median Physical Activity: ", median(data$Physical_Activity, na.rm = TRUE))
mode_physical_activity <- as.numeric(names(sort(table(data$Physical_Activity), decreasing = TRUE)[1]))
paste("Mode Physical Activity: ", mode_physical_activity)
paste("Mode Physical Activity: ", mode_physical_activity)
paste("Percentiles (10th, 25th, 50th, 75th, 90th) of Physical Activity: ", quantile(data$Physical_Activity, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
hist(data$Physical_Activity,
     main = "Histogram of Physical Activity",
     xlab = "Physical Activity (Hours)",
     col = "lightblue",
     border = "black")
#Parental_Involvement : Categorical#
freq_parental <- table(data$Parental_Involvement)
print(freq_parental)
barplot(freq_parental,
        main = "Barplot of Parental Involvement",
        xlab = "Parental Involvement",
        ylab = "Frequency",
        col = "orange")
#Access_to_Resources : Categorical#
freq_resources <- table(data$Access_to_Resources)
print(freq_resources)
pie(freq_resources,
    main = "Pie Chart of Access to Resources",
    col = rainbow(length(freq_resources)))
#Extracurricular_Activities : Categorical#
freq_activities <- table(data$Extracurricular_Activities)
print(freq_activities)
barplot(freq_activities,
        main = "Barplot of Extracurricular Activities",
        xlab = "Extracurricular Activities",
        ylab = "Frequency",
        col = "orange")
#Internet_Access: Categorical#
freq_internet <- table(data$Internet_Access)
print(freq_internet)
pie(freq_internet,
    main = "Pie Chart of Internet Access",
    col = rainbow(length(freq_internet)))
#Family_Income: Categorical#
freq_income <- table(data$Family_Income)
print(freq_income)
barplot(freq_income,
        main = "Barplot of Family Income",
        xlab = "Family Income",
        ylab = "Frequency",
        col = "orange")
#School_Type: Categorical#
freq_school <- table(data$School_Type)
print(freq_school)
barplot(freq_school,
        main = "Barplot of School Type",
        xlab = "School Type",
        ylab = "Frequency",
        col = "orange")

#Motivation_Level: Categorical#
freq_Motivation_Level <- table(data$Motivation_Level)
print(freq_Motivation_Level)
pie(freq_Motivation_Level,
    main = "Pie Chart of Motivation Level",
    col = rainbow(length(freq_Motivation_Level)))
#Teacher_Quality: Categorical#
freq_Teacher_Quality <- table(data$Teacher_Quality)
print(freq_Teacher_Quality)
barplot(freq_Teacher_Quality,
        main = "Barplot of Teacher Quality",
        xlab = "Teacher_Quality",
        ylab = "Frequency",
        col = "blue")
#Peer_Influence: Categorical#
freq_Peer_Influence <- table(data$Peer_Influence)
print(freq_Peer_Influence)
pie(freq_Peer_Influence,
    main = "Pie Chart of Peer Influence",
    col = rainbow(length(freq_Peer_Influence)))
#Learning_Disabilities: Categorical#
freq_Learning_Disabilities <- table(data$Learning_Disabilities)
print(freq_Learning_Disabilities)
pie(freq_Learning_Disabilities,
    main = "Pie Chart of Learning Disabilities",
    col = rainbow(length(freq_Learning_Disabilities)))
#Parental_Education_Level: Categorical#
freq_Parental_Education_Level <- table(data$Parental_Education_Level)
print(freq_Parental_Education_Level)
barplot(freq_Parental_Education_Level,
        main = "Barplot of Parental Education Level",
        xlab = "Parental Education_Level",
        ylab = "Frequency",
        col = "green")
#Distance_from_Home: Categorical#
freq_Distance_from_Home <- table(data$Distance_from_Home)
print(freq_Distance_from_Home)
pie(freq_Distance_from_Home,
    main = "Pie Chart of Distance from Home",
    col = rainbow(length(freq_Distance_from_Home)))

#Gender: Categorical#
freq_Gender <- table(data$Gender)
print(freq_Gender)
pie(freq_Gender,
    main = "Pie Chart of Gender",
    col = rainbow(length(freq_Gender)))

plot(data$Hours_Studied, data$Exam_Score, 
     main = "Scatter Plot: Hours Studied vs Exam Score",
     xlab = "Hours Studied",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)
cor(data$Hours_Studied, data$Exam_Score, use = "complete.obs")

plot(data$Attendance, data$Exam_Score, 
     main = "Scatter Plot: Attendance vs Exam Score",
     xlab = "Attendance",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)
cor(data$Attendance, data$Exam_Score, use = "complete.obs")

plot(data$Sleep_Hours, data$Exam_Score,
     main = "Scatter Plot: Sleep Hours vs Exam Score",
     xlab = "Sleep Hours",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)
cor(data$Sleep_Hours, data$Exam_Score, use = "complete.obs")

plot(data$Previous_Scores, data$Exam_Score,
     main = "Scatter Plot: Previous Scores vs Exam Score",
     xlab = "Previous Scores",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)
cor(data$Previous_Scores, data$Exam_Score, use = "complete.obs")

plot(data$Physical_Activity,data$Exam_Score,
     main = "Scatter Plot: Physical Activity vs Exam Score",
     xlab = "Physical Activity",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)
cor(data$Physical_Activity, data$Exam_Score, use = "complete.obs")
plot(data$Tutoring_Sessions, data$Exam_Score,
     main = "Scatter Plot: Tutoring Sessions vs Exam Score",
     xlab = "Tutoring Sessions",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)
cor(data$Tutoring_Sessions, data$Exam_Score, use = "complete.obs")

plot(data$Parental_Involvement, data$Exam_Score,
     main = "Scatter Plot: Parental Involvement vs Exam Score",
     xlab = "Parental Involvement",
     ylab = "Exam Score",
     col = "darkgreen", pch = 19)

boxplot(Exam_Score ~ Parental_Involvement, data = data,
        main = "Exam Score by Parental Involvement",
        xlab = "Parental Involvement", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Access_to_Resources, data = data,
        main = "Exam score by Access to Resources",
        xlab = "Access to Resources", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Motivation_Level, data = data,
        main = "Exam score by Motivation Level",
        xlab = "Motivation Level", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Extracurricular_Activities, data = data,
        main = "Exam score by Extracurricular_Activities",
        xlab = "Extracurricular_Activities", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Internet_Access, data = data,
        main = "Exam score by Internet_Access",
        xlab = "Internet_Access", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Family_Income, data = data,
        main = "Exam score by Family_Income",
        xlab = "Family_Income", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Teacher_Quality, data = data,
        main = "Exam score by Teacher_Quality",
        xlab = "Teacher_Quality", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ School_Type, data = data,
        main = "Exam score by School Type",
        xlab = "School Type", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Peer_Influence, data = data,
        main = "Exam score by Peer Influence",
        xlab = "Family_Income", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Learning_Disabilities, data = data,
        main = "Exam score by Learning Disabilities",
        xlab = "Learning Disabilities", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Parental_Education_Level, data = data,
        main = "Exam score by Parental Education Level",
        xlab = "Parental Education Level", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Distance_from_Home, data = data,
        main = "Exam score by Distance from Home",
        xlab = "Distance from Home", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

boxplot(Exam_Score ~ Gender, data = data,
        main = "Exam score by Gender",
        xlab = "Gender", ylab = "Exam Score",
        col = c("lightblue", "lightpink"))

table_dataMG <- table(data$Motivation_Level, data$Gender)
print(table_dataMG)data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
chisq_resultMG <- chisq.test(table_dataMG)
print(chisq_resultMG)
if (a%%)
tablegender <- table(data$Gender)
prop.table(tablegender)

model <- lm(Exam_Score ~ Hours_Studied + Sleep_Hours + Attendance, data = data)
summary(model)







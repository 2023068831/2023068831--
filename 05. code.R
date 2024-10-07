# step1
survey <- readxl::read_xlsx('05. Data_Graph(3)/05. 2024STB_survey_80.xlsx')
# Redefine column names
colnames(survey) <- c(
  "Gender", 
  "Age", 
  "Grade", 
  "Nationality", 
  "Residential_Area", 
  "Internet_Usage_Last_Year", 
  "Health_Satisfaction", 
  "Income_Satisfaction", 
  "Housing_Satisfaction", 
  "Family_Relationship_Satisfaction", 
  "Social_Relationship_Satisfaction", 
  "Leisure_Satisfaction", 
  "Overall_Life_Satisfaction", 
  "Regular_Donation_Volunteer", 
  "Donation_Amount_2023", 
  "Volunteer_Activities_2022",
  "Parent_Separation_Status", 
  "Parent_Visit_Frequency_2023", 
  "Parent_Call_Frequency_2023",
  "Lifetime_Smoking_Amount", 
  "First_Smoking_Age", 
  "Total_Smoking_Years", 
  "Current_Smoking_Status", 
  "Daily_Smoking_Amount", 
  "Quit_Smoking_Attempts_1Year", 
  "Future_Quit_Smoking_Plans", 
  "Secondhand_Smoke_Exposure_Hours", 
  "Secondhand_Smoke_Exposure_Hours_Avg", 
  "Alcohol_Frequency", 
  "Drinks_Per_Session", 
  "Heavy_Drinking_Frequency", 
  "Inability_to_Stop_Drinking", 
  "Uncompleted_Tasks_Due_to_Alcohol", 
  "Morning_After_Drinking_Frequency", 
  "Alcohol_Regret_Frequency", 
  "Blackout_Frequency", 
  "Injury_Due_to_Alcohol", 
  "Concerns_from_Others_about_Alcohol"
)

# Convert 'Gender' column: "남자 男人" -> 1 (Male), "여자 女子" -> 2 (Female)
survey$Gender <- ifelse(survey$Gender == "남자 男人", 1, 
                        ifelse(survey$Gender == "여자 女子", 2, NA))

# step 2

# Frequency distribution for Gender
gender_freq <- table(survey$Gender)
gender_freq

# step 3
# Relative frequency distribution for Gender
gender_relative_freq <- prop.table(table(survey$Gender))
gender_relative_freq

# step 4 
# Crosstab for Gender and Grade

survey$Grade <- ifelse(survey$Grade == "2학년 / 2年级", 'Grade 2',
                               ifelse(survey$Grade == "3학년 / 3年级", 'Grade 3',
                                      ifelse(survey$Grade == "4학년 / 4年级", 'Grade 4',
                                             ifelse(survey$Grade == "5학년 / 5年级", 'Grade 5', NA))))

gender_grade_crosstab <- table(survey$Gender, survey$Grade)
gender_grade_crosstab

# step 5
# Bar plot for Nationality
survey$Nationality <- ifelse(survey$Nationality == "한국 / 韓國", "Korea",
                                     ifelse(survey$Nationality == "중국 / 中國", "China",
                                            ifelse(survey$Nationality == "몽골", "Mongolia", NA)))

barplot(table(survey$Nationality), 
        main = "Bar Plot for Nationality", 
        xlab = "Nationality", 
        ylab = "Frequency",
        col = "lightblue")

# step 6
# Horizontal bar plot for Residential Area
survey$Residential_Area <- ifelse(survey$Residential_Area == "경기 競技", "Gyeonggi",
                                          ifelse(survey$Residential_Area == "인천 仁川", "Incheon",
                                                 ifelse(survey$Residential_Area == "전남 全南", "Jeonnam",
                                                        ifelse(survey$Residential_Area == "서울 首尔", "Seoul",
                                                               ifelse(survey$Residential_Area == "충남 忠南", "Chungnam",
                                                                      ifelse(survey$Residential_Area == "경남 庆南", "Gyeongnam", NA))))))
par(cex.axis = 0.7)
barplot(table(survey$Residential_Area), 
        main = "Horizontal Bar Plot for Residential Area", 
        xlab = "Frequency", 
        ylab = "Residential Area",
        col = "green", 
        horiz = T)
# step 7
barplot(table(survey$Gender, survey$Grade), 
        beside = TRUE,
        legend = rownames(table(survey$Gender, survey$Grade)),
        main = "Bar Plot for Gender and Grade",
        xlab = "Grade", 
        ylab = "Frequency",
        col = c("red", "blue"))
# step 8
grade_freq <- table(survey$Grade)
pie(grade_freq, 
    main = "Pie Chart for Grade", 
    col = rainbow(length(grade_freq)),
    labels = names(grade_freq))
# step 9
hist(as.numeric(survey$Age), 
     main = "Histogram of Age", 
     xlab = "Age", 
     ylab = "Frequency", 
     col = "lightblue")
# step 10
# Boxplot for Age by Grade
par(cex.axis = 0.8)
boxplot(as.numeric(survey$Age) ~ survey$Grade, 
        main = "Boxplot of Age by Grade", 
        xlab = "Grade", 
        ylab = "Age",
        col = 1:4)

# step 11

survey$Grade_numeric <- ifelse(survey$Grade == "Grade 2", 2,
                               ifelse(survey$Grade == "Grade 3", 3,
                                      ifelse(survey$Grade == "Grade 4", 4,
                                             ifelse(survey$Grade == "Grade 5", 5, NA))))

# Scatter plot 
plot(as.numeric(survey$Age), survey$Grade_numeric, 
     main = "Scatter Plot of Age vs Grade", 
     xlab = "Age", 
     ylab = "Grade (numeric)",
     pch = 20)


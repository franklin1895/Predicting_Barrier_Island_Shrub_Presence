# R script for neccesary functions and analysises needed to recreate Franklin et al. (in review)
# Written by Benton Franklin
# Updated 09/13/2023
# 


######################
# Load in data. Dataframe includes 8 morphometric variables used in figures and analysis
paper_data = read.csv('Morphometric_Data.csv', header = T, row.names = 1)
# Sample year and number are given in each sample rowname

###############
# Separate transects into shrub and non-shrub transects
Shrub_Transects = paper_data[paper_data$Shrub_Presence == 'Shrub',]
Non_Shrub_Transects = paper_data[paper_data$Shrub_Presence == 'Non-shrub',]

################
# Test for statstically significant differences between shrub and non-shrub transects with the Mann-Whitney test

# V1: Dune Crest Elevation
wilcox.test(Non_Shrub_Transects$Dune_Crest_Elevation,Shrub_Transects$Dune_Crest_Elevation)

# V2: Lowest Dune Crest Elevation Between Surveys
wilcox.test(Non_Shrub_Transects$Lowest_Dune_Crest_Elevation_Between_Surveys,Shrub_Transects$Lowest_Dune_Crest_Elevation_Between_Surveys)

# V3: Island Interior Width
wilcox.test(Non_Shrub_Transects$Island_Interior_Width,Shrub_Transects$Island_Interior_Width)

# V4: Minimum Island Interior Width Between Surveys
wilcox.test(Non_Shrub_Transects$Minimum_Island_Interior_Width_Between_Surveys,Shrub_Transects$Minimum_Island_Interior_Width_Between_Surveys)

# V5: Beach Width
wilcox.test(Non_Shrub_Transects$Beach_Width,Shrub_Transects$Beach_Width)

# V6: Change in Dune Crest Elevation
wilcox.test(Non_Shrub_Transects$Change_in_Dune_Crest_Elevation,Shrub_Transects$Change_in_Dune_Crest_Elevation)

# V7: Change in Island Interior Width
wilcox.test(Non_Shrub_Transects$Change_in_Island_Interior_Width,Shrub_Transects$Change_in_Island_Interior_Width)

# V8: Change in Beach Width
wilcox.test(Non_Shrub_Transects$Change_in_Beach_Width,Shrub_Transects$Change_in_Beach_Width)

########################
# Check correlation between IIWNR and DCE for all data and for shrub and non-shrub transects

# Test linear regression for all data
Linear_DCE_IIWNR = lm(formula = Dune_Crest_Elevation ~ Minimum_Island_Interior_Width_Between_Surveys,data =paper_data)
summary(Linear_DCE_IIWNR)

# Test polynomial regression for all data
Poly_DCE_IIWNR = lm(formula = Dune_Crest_Elevation ~ poly(Minimum_Island_Interior_Width_Between_Surveys,2, raw = TRUE),data =paper_data)
summary(Poly_DCE_IIWNR)

# Test linear regression for shrub data 
Linear_Shrub = lm(formula = Dune_Crest_Elevation ~ Minimum_Island_Interior_Width_Between_Surveys,data = Shrub_Transects)
summary(Linear_Shrub)

# Test polynomial regression for shrub data 
Poly_Shrub = lm(formula = Dune_Crest_Elevation ~ poly(Minimum_Island_Interior_Width_Between_Surveys,2, raw = TRUE),data =Shrub_Transects)
summary(Poly_Shrub)

# Test linear regression for shrubless data 
Linear_Shrubless = lm(formula = Dune_Crest_Elevation ~ Minimum_Island_Interior_Width_Between_Surveys,data = Non_Shrub_Transects)
summary(Linear_Shrubless)

# Test polynomial regression for shrubless data 
Poly_Shrubless = lm(formula = Dune_Crest_Elevation ~ poly(Minimum_Island_Interior_Width_Between_Surveys,2, raw = TRUE),data =Non_Shrub_Transects)
summary(Poly_Shrubless)

##################
# Separate out all the data into 4 categories based on DT thresholds

Total_index = c(1:1551)

# Category 1 (IIWNR < 159 m)
Category_1_Index = paper_data$Minimum_Island_Interior_Width_Between_Surveys < 159
Category_1_Data = paper_data[Category_1_Index,]

# Category 2 (IIWNR > 159 m & DCE < 1.9 m)
Category_2_Width_Index = Total_index[paper_data$Minimum_Island_Interior_Width_Between_Surveys > 159 & paper_data$Minimum_Island_Interior_Width_Between_Surveys < 495]
Category_2_Dune_Elev_Index = Total_index[paper_data$Dune_Crest_Elevation < 1.9]
Category_2_Index = intersect(Category_2_Width_Index,Category_2_Dune_Elev_Index)
Category_2_Data = paper_data[Category_2_Index,]

# Category 3 (IIWNR > 498 m & DCE < 1.9 m))
Category_3_Width_Index = Total_index[paper_data$Minimum_Island_Interior_Width_Between_Surveys > 495]
Category_3_Dune_Elev_Index = Total_index[paper_data$Dune_Crest_Elevation < 1.9]
Category_3_Index = intersect(Category_3_Width_Index,Category_3_Dune_Elev_Index)
Category_3_Data = paper_data[Category_3_Index,]

# Category 4 (IIWNR > 159 m & DCE > 1.9 m))
Category_4_Width_Index = Total_index[paper_data$Minimum_Island_Interior_Width_Between_Surveys > 159]
Category_4_Dune_Elev_Index = Total_index[paper_data$Dune_Crest_Elevation > 1.9]
Category_4_Index = intersect(Category_4_Width_Index,Category_4_Dune_Elev_Index)
Category_4_Data = paper_data[Category_4_Index,]

####################
# Prepare data for machine learning
set.seed(257) # Set a seed for random numbers

# Load needed librarys
library(randomForest)
#library(tree)
library(rpart)
library(rpart.plot)
library(caret)

# Make sure .csv data is a factor
paper_data$Shrub_Presence = as.factor(paper_data$Shrub_Presence) 

# Split 75% of data for training and remaining 25% for validation
Validation_index =createDataPartition(paper_data$Shrub_Presence, p=.75, list = FALSE)

Dataset_Training_data = paper_data[Validation_index,1:9] 
Validation_data = paper_data[-Validation_index,1:9]

##################
# Run random forest model

RF_Training_Values = randomForest(Shrub_Presence ~., data = Dataset_Training_data, metric = metric, trControl = control, importance = TRUE)
Validation_Predicted_Values = predict(RF_Training_Values, Validation_data)

##################
# Run decision tree model

Decision_Tree = rpart(Shrub_Presence ~., data = Dataset_Training_data)

DT_Training_Prediction = predict(Decision_Tree, Dataset_Training_data, type = 'class')
DT_Validation_Prediction = predict(Decision_Tree, Validation_data, type = 'class')


##################
# Table 3 Confusion Matrix Values
Table_3_RF_Training_Validation_Matrix = RF_Training_Values$confusion
Table_3_RF_Validation_Matrix = confusionMatrix(Validation_Predicted_Values, Validation_data$Shrub_Presence)
Table_3_DT_Training_Validation_Matrix = confusionMatrix(DT_Training_Prediction, Dataset_Training_data$Shrub_Presence)
Table_3_DT_Validation_Matrix = confusionMatrix(DT_Validation_Prediction, Validation_data$Shrub_Presence)

###############
# Figure 4
# Make series of boxplots showing difference between shrub and non-shrub transects

# V1: Dune Crest Elevation
boxplot(Non_Shrub_Transects$Dune_Crest_Elevation, Shrub_Transects$Dune_Crest_Elevation, ylab = 'Dune Crest Elevation (m)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5, ylim = c(0,4.25))

# V2: Lowest Dune Crest Elevation Between Surveys
boxplot(Non_Shrub_Transects$Lowest_Dune_Crest_Elevation_Between_Surveys,Shrub_Transects$Lowest_Dune_Crest_Elevation_Between_Surveys,ylab = 'Lowest Dune Crest Elevation Between Surveys (m)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5,ylim = c(0,4.25))

# V3: Island Interior Width
boxplot(Non_Shrub_Transects$Island_Interior_Width,Shrub_Transects$Island_Interior_Width,ylab = 'Island Interior Width (m)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5, ylim =c(-225,1500))


# V4: Minimum Island Interior Width Between Surveys
boxplot(Non_Shrub_Transects$Minimum_Island_Interior_Width_Between_Surveys,Shrub_Transects$Minimum_Island_Interior_Width_Between_Surveys,ylab = 'Minimum Island Interior Width Between Surveys (m)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5, ylim =c(-225,1500))

# V5: Beach Width
boxplot(Non_Shrub_Transects$Beach_Width,Shrub_Transects$Beach_Width,ylab = 'Beach Width (m)', names = c('Non-shrub','Shrub'), ylim = c(-50,120),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5)


# V6: Change in Dune Crest Elevation
boxplot(Non_Shrub_Transects$Change_in_Dune_Crest_Elevation,Shrub_Transects$Change_in_Dune_Crest_Elevation,ylab = 'Change in Dune Crest Elevation (%)', names = c('Non-shrub','Shrub'), ylim = c(-2,2),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5)


# V7: Change in Island Interior Width
boxplot(Non_Shrub_Transects$Change_in_Island_Interior_Width,Shrub_Transects$Change_in_Island_Interior_Width,ylab = 'Change in Island Interior Width (%)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5, ylim = c(-100,100))


# V8: Change in Beach Width
boxplot(Non_Shrub_Transects$Change_in_Beach_Width,Shrub_Transects$Change_in_Beach_Width,ylab = 'Change in Beach Width (%)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5, ylim = c(0,200))


#################
# Figure 5
# Make decision tree
rpart.plot(Decision_Tree)

############
# Figure 6
# Scatter plot of DCE vs IIW NR

plot(paper_data$Minimum_Interior_Width,paper_data$Dune_Elevation, type = 'n', ylim = c(0.5,4.25),xlim = c(0,555), ylab = 'Dune Crest Elevation (m)',
     xlab = 'Minimum Island Interior Width Between Surveys (m)', main = 'Dune Crest Elevation vs Minimum Island Interior Width Between Surveys (m)')
lines( x = c(159,2000), y =c(1.9,1.9))
lines( x = c(159,159), y =c(-1.9,19))
lines( x = c(459,459), y =c(-1.9,1.9))

points(Non_Shrub_Transects$Minimum_Island_Interior_Width_Between_Surveys,Non_Shrub_Transects$Dune_Crest_Elevation, col ='dodgerblue', pch =2)
points(Shrub_Transects$Minimum_Island_Interior_Width_Between_Surveys,Shrub_Transects$Dune_Crest_Elevation, col ='darkgreen', pch =1) 

#################
# Analyze differences in shrub colonization and removal

# Smith
Smith_Removal_2011 = read.csv('Shrub_Colonization_Removal_Data/Smith_Removal_2011.csv')
Smith_Removal_2016 = read.csv('Shrub_Colonization_Removal_Data/Smith_Removal_2016.csv')

# Cedar
Cedar_Removal_2011 = read.csv('Shrub_Colonization_Removal_Data/Cedar_Removal_2011.csv')
Cedar_Colonization_2016 = read.csv('Shrub_Colonization_Removal_Data/Cedar_Colonization_2016.csv')

# Cobb
Cobb_Removal_2011 = read.csv('Shrub_Colonization_Removal_Data/Cobb_Removal_2011.csv')
Cobb_Colonization_2011 = read.csv('Shrub_Colonization_Removal_Data/Cobb_Colonization_2011.csv')
Cobb_Removal_2016 = read.csv('Shrub_Colonization_Removal_Data/Cobb_Removal_2016.csv')

# Hog
Hog_Colonization_2011 = read.csv('Shrub_Colonization_Removal_Data/Hog_Colonization_2011.csv')
Hog_Colonization_2016 = read.csv('Shrub_Colonization_Removal_Data/Hog_Colonization_2016.csv')

# Parramore
Parramore_Removal_2011 = read.csv('Shrub_Colonization_Removal_Data/Parramore_Removal_2011.csv')
Parramore_Removal_2016 = read.csv('Shrub_Colonization_Removal_Data/Parramore_Removal_2016.csv')

# Wreck
Wreck_Colonization_2011 = read.csv('Shrub_Colonization_Removal_Data/Wreck_Colonization_2011.csv')
Wreck_Colonization_2016 = read.csv('Shrub_Colonization_Removal_Data/Wreck_Colonization_2016.csv')

# Transect Numbers

# Smith
Smith_Removal_2011 = Smith_Removal_2011$Automorph_Transect_ID
Smith_Removal_2016 = Smith_Removal_2016$Automorph_Transect_ID

# Cedar
Cedar_Removal_2011 = Cedar_Removal_2011$Automorph_Transect_ID
Cedar_Colonization_2016 = Cedar_Colonization_2016$Automorph_Transect_ID

# Cobb
Cobb_Removal_2011 = Cobb_Removal_2011$Automorph_Transect_ID
Cobb_Colonization_2011 = Cobb_Colonization_2011$Automorph_Transect_ID
Cobb_Removal_2016 = Cobb_Removal_2016$Automorph_Transect_ID

# Hog
Hog_Colonization_2011 = Hog_Colonization_2011$Automorph_Transect_ID
Hog_Colonization_2016 = Hog_Colonization_2016$Automorph_Transect_ID

# Parramore
Parramore_Removal_2011 = Parramore_Removal_2011$Automorph_Transect_ID
Parramore_Removal_2016 = Parramore_Removal_2016$Automorph_Transect_ID

# Wreck
Wreck_Colonization_2011 = Wreck_Colonization_2011$Automorph_Transect_ID
Wreck_Colonization_2016 = Wreck_Colonization_2016$Automorph_Transect_ID


# Load Morphometric data

Morphometrics_2010 = paper_data[paper_data$Year == 2010,]
Morphometrics_2016 = paper_data[paper_data$Year == 2016,]
Morphometrics_2017 = paper_data[paper_data$Year == 2017,]

# Smith
Smith_2010 = Morphometrics_2010[Morphometrics_2010$Island_Name =='Smith',]
Smith_2016 = Morphometrics_2016[Morphometrics_2016$Island_Name =='Smith',]
Smith_2017 = Morphometrics_2017[Morphometrics_2017$Island_Name =='Smith',]

# Cedar
Cedar_2010 = Morphometrics_2010[Morphometrics_2010$Island_Name =='Cedar',]
Cedar_2016 = Morphometrics_2016[Morphometrics_2016$Island_Name =='Cedar',]
Cedar_2017 = Morphometrics_2017[Morphometrics_2017$Island_Name =='Cedar',]

# Cobb
Cobb_2010 = Morphometrics_2010[Morphometrics_2010$Island_Name =='Cobb',]
Cobb_2016 = Morphometrics_2016[Morphometrics_2016$Island_Name =='Cobb',]
Cobb_2017 = Morphometrics_2017[Morphometrics_2017$Island_Name =='Cobb',]

# Hog
Hog_2010 = Morphometrics_2010[Morphometrics_2010$Island_Name =='Hog',]
Hog_2016 = Morphometrics_2016[Morphometrics_2016$Island_Name =='Hog',]
Hog_2017 = Morphometrics_2017[Morphometrics_2017$Island_Name =='Hog',]

# Parramore
Parramore_2010 = Morphometrics_2010[Morphometrics_2010$Island_Name =='Parramore',]
Parramore_2016 = Morphometrics_2016[Morphometrics_2016$Island_Name =='Parramore',]
Parramore_2017 = Morphometrics_2017[Morphometrics_2017$Island_Name =='Parramore',]

# Wreck
Wreck_2010 = Morphometrics_2010[Morphometrics_2010$Island_Name =='Wreck',]
Wreck_2016 = Morphometrics_2016[Morphometrics_2016$Island_Name =='Wreck',]
Wreck_2017 = Morphometrics_2017[Morphometrics_2017$Island_Name =='Wreck',]

# Smith 
Smith_Removal_Morpho_2011 = c() 
for (i in seq(1:length(Smith_Removal_2011))){
  x = Smith_2010[Smith_2010$Transect_Number == Smith_Removal_2011[i],]
  Smith_Removal_Morpho_2011 = rbind(Smith_Removal_Morpho_2011,x)
}

Smith_Removal_Morpho_2016 = c() 
for (i in seq(1:length(Smith_Removal_2016))){
  x = Smith_2016[Smith_2016$Transect_Number == Smith_Removal_2016[i],]
  Smith_Removal_Morpho_2016 = rbind(Smith_Removal_Morpho_2016,x)
}
for (i in seq(1:length(Smith_Removal_2016))){
  x = Smith_2017[Smith_2017$Transect_Number == Smith_Removal_2016[i],]
  Smith_Removal_Morpho_2016 = rbind(Smith_Removal_Morpho_2016,x)
}

# Cedar

Cedar_Removal_Morpho_2011 = c() 
for (i in seq(1:length(Cedar_Removal_2011))){
  x = Cedar_2010[Cedar_2010$Transect_Number == Cedar_Removal_2011[i],]
  Cedar_Removal_Morpho_2011 = rbind(Cedar_Removal_Morpho_2011,x)
}

Cedar_Colonization_Morpho_2016 = c() 
for (i in seq(1:length(Cedar_Colonization_2016))){
  x = Cedar_2016[Cedar_2016$Transect_Number == Cedar_Colonization_2016[i],]
  Cedar_Colonization_Morpho_2016 = rbind(Cedar_Colonization_Morpho_2016,x)
}
for (i in seq(1:length(Cedar_Colonization_2016))){
  x = Cedar_2017[Cedar_2017$Transect_Number == Cedar_Colonization_2016[i],]
  Cedar_Colonization_Morpho_2016 = rbind(Cedar_Colonization_Morpho_2016,x)
}


# Cobb
Cobb_Removal_Morpho_2011 = c() 
for (i in seq(1:length(Cobb_Removal_2011))){
  x = Cobb_2010[Cobb_2010$Transect_Number == Cobb_Removal_2011[i],]
  Cobb_Removal_Morpho_2011 = rbind(Cobb_Removal_Morpho_2011,x)
}

Cobb_Colonization_Morpho_2011 = c()
for (i in seq(1:length(Cobb_Colonization_2011))){
  x = Cobb_2010[Cobb_2010$Transect_Number == Cobb_Colonization_2011[i],]
  Cobb_Colonization_Morpho_2011 = rbind(Cobb_Colonization_Morpho_2011,x)
}

Cobb_Removal_Morpho_2016 = c() 
for (i in seq(1:length(Cobb_Removal_2016))){
  x = Cobb_2016[Cobb_2016$Transect_Number == Cobb_Removal_2016[i],]
  Cobb_Removal_Morpho_2016 = rbind(Cobb_Removal_Morpho_2016,x)
}
for (i in seq(1:length(Cobb_Removal_2016))){
  x = Cobb_2017[Cobb_2017$Transect_Number == Cobb_Removal_2016[i],]
  Cobb_Removal_Morpho_2016 = rbind(Cobb_Removal_Morpho_2016,x)
}

# Hog
Hog_Colonization_Morpho_2011 = c() 
for (i in seq(1:length(Hog_Colonization_2011))){
  x = Hog_2010[Hog_2010$Transect_Number == Hog_Colonization_2011[i],]
  Hog_Colonization_Morpho_2011 = rbind(Hog_Colonization_Morpho_2011,x)
}

Hog_Colonization_Morpho_2016 = c() 
for (i in seq(1:length(Hog_Colonization_2016))){
  x = Hog_2016[Hog_2016$Transect_Number == Hog_Colonization_2016[i],]
  Hog_Colonization_Morpho_2016 = rbind(Hog_Colonization_Morpho_2016,x)
}
for (i in seq(1:length(Hog_Colonization_2016))){
  x = Hog_2017[Hog_2017$Transect_Number == Hog_Colonization_2016[i],]
  Hog_Colonization_Morpho_2016 = rbind(Hog_Colonization_Morpho_2016,x)
}

# Parramore
Parramore_Removal_Morpho_2011 = c() 
for (i in seq(1:length(Parramore_Removal_2011))){
  x = Parramore_2010[Parramore_2010$Transect_Number == Parramore_Removal_2011[i],]
  Parramore_Removal_Morpho_2011 = rbind(Parramore_Removal_Morpho_2011,x)
}

Parramore_Removal_Morpho_2016 = c() 
for (i in seq(1:length(Parramore_Removal_2016))){
  x = Parramore_2016[Parramore_2016$Transect_Number == Parramore_Removal_2016[i],]
  Parramore_Removal_Morpho_2016 = rbind(Parramore_Removal_Morpho_2016,x)
}
for (i in seq(1:length(Parramore_Removal_2016))){
  x = Parramore_2017[Parramore_2017$Transect_Number == Parramore_Removal_2016[i],]
  Parramore_Removal_Morpho_2016 = rbind(Parramore_Removal_Morpho_2016,x)
}


# Wreck
Wreck_Colonization_Morpho_2011 = c() 
for (i in seq(1:length(Wreck_Colonization_2011))){
  x = Wreck_2010[Wreck_2010$Transect_Number == Wreck_Colonization_2011[i],]
  Wreck_Colonization_Morpho_2011 = rbind(Wreck_Colonization_Morpho_2011,x)
}

Wreck_Colonization_Morpho_2016 = c() 
for (i in seq(1:length(Wreck_Colonization_2016))){
  x = Wreck_2016[Wreck_2016$Transect_Number == Wreck_Colonization_2016[i],]
  Wreck_Colonization_Morpho_2016 = rbind(Wreck_Colonization_Morpho_2016,x)
}
for (i in seq(1:length(Wreck_Colonization_2016))){
  x = Wreck_2017[Wreck_2017$Transect_Number == Wreck_Colonization_2016[i],]
  Wreck_Colonization_Morpho_2016 = rbind(Wreck_Colonization_Morpho_2016,x)
}

# Combine All Colonization
Shrub_Colonization = rbind(Cedar_Colonization_Morpho_2016,Cobb_Colonization_Morpho_2011,
                           Hog_Colonization_Morpho_2011,Hog_Colonization_Morpho_2016,Wreck_Colonization_Morpho_2011,Wreck_Colonization_Morpho_2016)

# Shrub Removal
Shrub_Removal = rbind(Smith_Removal_Morpho_2011,Smith_Removal_Morpho_2016,Cedar_Removal_Morpho_2011, 
                      Cobb_Removal_Morpho_2011,Cobb_Removal_Morpho_2016,Parramore_Removal_Morpho_2011,Parramore_Removal_Morpho_2016)


# Shrub Colonization Transects Compared to Minimum-Island-Interior Width Between Surveys
Threshold_Width_Colonization = Shrub_Colonization[Shrub_Colonization$Minimum_Island_Interior_Width_Between_Surveys > 158.6,]
Threshold_Width_Colonization_100 = Shrub_Colonization[Shrub_Colonization$Minimum_Island_Interior_Width_Between_Surveys > 100,]

lacks_Threshold_Width_Colonization = Shrub_Colonization[Shrub_Colonization$Minimum_Island_Interior_Width_Between_Surveys < 158.6,]
Percent_Threshold = length(Threshold_Width_Colonization$Dune_Crest_Elevation)/length(Shrub_Colonization$Dune_Crest_Elevation)
Percent_Threshold_100 = length(Threshold_Width_Colonization_100$Dune_Crest_Elevation)/length(Shrub_Colonization$Dune_Crest_Elevation)



Threshold_Width_Colonization_Current = Shrub_Colonization[Shrub_Colonization$Island_Interior_Width > 158.6,]
Threshold_Width_Colonization_Current_100 = Shrub_Colonization[Shrub_Colonization$Island_Interior_Width > 100,]

Percent_Threshold_Current = length(Threshold_Width_Colonization_Current$Dune_Crest_Elevation)/length(Shrub_Colonization$Dune_Crest_Elevation)
Percent_Threshold_100_Current = length(Threshold_Width_Colonization_Current_100$Dune_Crest_Elevation)/length(Shrub_Colonization$Dune_Crest_Elevation)


lacks_Threshold_Width_Colonization_Current = Shrub_Colonization[Shrub_Colonization$Island_Interior_Width < 158.6,]


# Shrub Removal
lacks_Threshold_Width_Removal = Shrub_Removal[Shrub_Removal$Minimum_Island_Interior_Width_Between_Surveys < 158.6,]
Percent_Threshold_Removal = length(lacks_Threshold_Width_Removal$Dune_Crest_Elevation)/length(Shrub_Removal$Dune_Crest_Elevation)


# Dune Crest Elevation
Threshold_Elevation_Colonization = Shrub_Colonization[Shrub_Colonization$Dune_Crest_Elevation > 1.89,]
Percent_Threshold_Elevation = length(Threshold_Elevation_Colonization$Dune_Crest_Elevation)/length(Shrub_Colonization$Dune_Crest_Elevation)

Threshold_Elevation_Removal = Shrub_Removal[Shrub_Removal$Dune_Crest_Elevation > 1.89,]
Percent_Threshold_Elevation = length(Threshold_Elevation_Removal$Dune_Crest_Elevation)/length(Shrub_Removal$Dune_Crest_Elevation)

# Both
# 160 m and 1.9 m
# Removal
Sufficent_Width_Elevation_Shrub_Removal = Shrub_Removal[Shrub_Removal$Dune_Crest_Elevation > 1.89 & Shrub_Removal$Minimum_Island_Interior_Width_Between_Surveys > 159,]
Sufficent_Width_Elevation_Shrub_Removal_Percent = length(Sufficent_Width_Elevation_Shrub_Removal$X)/length(Shrub_Removal$X)

# Colonization
Sufficent_Width_Elevation_Shrub_Colonization = Shrub_Colonization[Shrub_Colonization$Dune_Crest_Elevation > 1.89 & Shrub_Colonization$Minimum_Island_Interior_Width_Between_Surveys > 159,]
Sufficent_Width_Elevation_Shrub_Colonization_Percent = length(Sufficent_Width_Elevation_Shrub_Colonization$X)/length(Shrub_Colonization$X)

#################
# Figure 8

Fig_8 = varImpPlot(RF_Training_Values, type = 1, scale = FALSE, cex =1, main = 'Variable Importance')

#############
# Salinity Measures
Salinity_Dataset = read.csv('Sabo_Thesis_Data_Combined.csv',skip = 21, header = T, comment.char = "#")

Salinity_Values = na.omit(Salinity_Dataset$soil_chlorides)
Mean_Salinity = mean(Salinity_Values)
Standard_Deviation_Salinity = sd(Salinity_Values)

# Shrub transects
Sabo_Shrub_Transects = Salinity_Dataset[Salinity_Dataset$Species == 'Shrub',]
Sabo_Salinity = na.omit(Sabo_Shrub_Transects$soil_chlorides)
Mean_Sabo_Salinity = mean(Sabo_Salinity)
SD_Sabo_Salinity = sd(Sabo_Salinity)

# See what percent are above thresholds are above salinity thresholds

Highest_Threshold = 500
Majority_Threshold = 50

Above_Highest_Threshold = Salinity_Values[Salinity_Values < 500]
Above_Majority_Threshold = Salinity_Values[Salinity_Values < 50]

Percent_Above_Highest = length(Above_Highest_Threshold)/length(Salinity_Values)
Percent_Above_Majority = length(Above_Majority_Threshold)/length(Salinity_Values)
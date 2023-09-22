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

# V2: Lowest Recent Dune Crest Elevation
wilcox.test(Non_Shrub_Transects$Lowest_Recent_Dune_Crest_Elevation,Shrub_Transects$Lowest_Recent_Dune_Crest_Elevation)

# V3: Island Interior Width
wilcox.test(Non_Shrub_Transects$Island_Interior_Width,Shrub_Transects$Island_Interior_Width)

# V4: Narrowest Recent Island Interior Width
wilcox.test(Non_Shrub_Transects$Narrowest_Recent_Island_Interior_Width,Shrub_Transects$Narrowest_Recent_Island_Interior_Width)

# V5: Beach Width
wilcox.test(Non_Shrub_Transects$Beach_Width,Shrub_Transects$Beach_Width)

# V6: Change in Dune Crest Elevation
wilcox.test(Non_Shrub_Transects$Change_in_Dune_Crest_Elevation,Shrub_Transects$Change_in_Dune_Crest_Elevation)

# V7: Change in Island Interior Width
wilcox.test(Non_Shrub_Transects$Change_in_Island_Interior_Width,Shrub_Transects$Change_in_Island_Interior_Width)

# V8: Change in Beach Width
wilcox.test(Non_Shrub_Transects$Change_in_Beach_Width,Shrub_Transects$Change_in_Beach_Width)

##################
# Separate out all the data into 4 categories based on DT thresholds

Total_index = c(1:1551)

# Category 1 (IIWNR < 159 m)
Category_1_Index = paper_data$Narrowest_Recent_Island_Interior_Width < 159
Category_1_Data = paper_data[Category_1_Index,]

# Category 2 (IIWNR > 159 m & DCE < 1.9 m)
Category_2_Width_Index = Total_index[paper_data$Narrowest_Recent_Island_Interior_Width > 159 & paper_data$Narrowest_Recent_Island_Interior_Width < 495]
Category_2_Dune_Elev_Index = Total_index[paper_data$Dune_Crest_Elevation < 1.9]
Category_2_Index = intersect(Category_2_Width_Index,Category_2_Dune_Elev_Index)
Category_2_Data = paper_data[Category_2_Index,]

# Category 3 (IIWNR > 498 m & DCE < 1.9 m))
Category_3_Width_Index = Total_index[paper_data$Narrowest_Recent_Island_Interior_Width > 495]
Category_3_Dune_Elev_Index = Total_index[paper_data$Dune_Crest_Elevation < 1.9]
Category_3_Index = intersect(Category_3_Width_Index,Category_3_Dune_Elev_Index)
Category_3_Data = paper_data[Category_3_Index,]

# Category 4 (IIWNR > 159 m & DCE > 1.9 m))
Category_4_Width_Index = Total_index[paper_data$Narrowest_Recent_Island_Interior_Width > 159]
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

# V2: Lowest Recent Dune Crest Elevation
boxplot(Non_Shrub_Transects$Lowest_Recent_Dune_Crest_Elevation,Shrub_Transects$Lowest_Recent_Dune_Crest_Elevation,ylab = 'Lowest Recent Dune Crest Elevation (m)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5,ylim = c(0,4.25))

# V3: Island Interior Width
boxplot(Non_Shrub_Transects$Island_Interior_Width,Shrub_Transects$Island_Interior_Width,ylab = 'Island Interior Width (m)', names = c('Non-shrub','Shrub'),
        col = c('dodgerblue','darkgreen'),cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5, cex.sub = 1.5, ylim =c(-225,1500))


# V4: Narrowest Recent Island Interior Width
boxplot(Non_Shrub_Transects$Narrowest_Recent_Island_Interior_Width,Shrub_Transects$Narrowest_Recent_Island_Interior_Width,ylab = 'Narrowest Recent Island Interior Width(m)', names = c('Non-shrub','Shrub'),
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
     xlab = 'Minimum Interior Width (m)', main = 'Dune Crest Elevation vs Lowest Recent Island Interior Width (m)')
lines( x = c(159,2000), y =c(1.9,1.9))
lines( x = c(159,159), y =c(-1.9,19))
lines( x = c(459,459), y =c(-1.9,1.9))

points(Non_Shrub_Transects$Narrowest_Recent_Island_Interior_Width,Non_Shrub_Transects$Dune_Crest_Elevation, col ='dodgerblue', pch =2)
points(Shrub_Transects$Narrowest_Recent_Island_Interior_Width,Shrub_Transects$Dune_Crest_Elevation, col ='darkgreen', pch =1) 

#################
# Figure 8

Fig_8 = varImpPlot(RF_Training_Values, type = 1, scale = FALSE, cex =1, main = 'Variable Importance')

#############
# Figure 11

# prep data for plotting in Figure 11

# Category 1+2 seperated by island
Cat_1_2_Smith = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Smith',],Category_2_Data[Category_2_Data$Island_Name == 'Smith',])
Cat_1_2_Myrtle = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Myrtle',],Category_2_Data[Category_2_Data$Island_Name == 'Myrtle',])
Cat_1_2_Ship = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Ship',],Category_2_Data[Category_2_Data$Island_Name == 'Ship',])
Cat_1_2_Wreck = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Wreck',],Category_2_Data[Category_2_Data$Island_Name == 'Wreck',])
Cat_1_2_Cobb = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Cobb',],Category_2_Data[Category_2_Data$Island_Name == 'Cobb',])
Cat_1_2_Hog = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Hog',],Category_2_Data[Category_2_Data$Island_Name == 'Hog',])
Cat_1_2_Parramore = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Parramore',],Category_2_Data[Category_2_Data$Island_Name == 'Parramore',])
Cat_1_2_Cedar = rbind(Category_1_Data[Category_1_Data$Island_Name == 'Cedar',],Category_2_Data[Category_2_Data$Island_Name == 'Cedar',])

# Category 3+4 seperated by island
Cat_3_4_Smith = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Smith',],Category_4_Data[Category_4_Data$Island_Name == 'Smith',])
Cat_3_4_Myrtle = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Myrtle',],Category_4_Data[Category_4_Data$Island_Name == 'Myrtle',])
Cat_3_4_Ship = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Ship',],Category_4_Data[Category_4_Data$Island_Name == 'Ship',])
Cat_3_4_Wreck = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Wreck',],Category_4_Data[Category_4_Data$Island_Name == 'Wreck',])
Cat_3_4_Cobb = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Cobb',],Category_4_Data[Category_4_Data$Island_Name == 'Cobb',])
Cat_3_4_Hog = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Hog',],Category_4_Data[Category_4_Data$Island_Name == 'Hog',])
Cat_3_4_Parramore = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Parramore',],Category_4_Data[Category_4_Data$Island_Name == 'Parramore',])
Cat_3_4_Cedar = rbind(Category_3_Data[Category_3_Data$Island_Name == 'Cedar',],Category_4_Data[Category_4_Data$Island_Name == 'Cedar',])

# Percentage of total island transects represented by Cat 1 + 2
Percent_Cat_1_2_Smith = (length(Cat_1_2_Smith$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Smith']))*100
Percent_Cat_1_2_Myrtle = (length(Cat_1_2_Myrtle$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Myrtle']))*100
Percent_Cat_1_2_Ship = (length(Cat_1_2_Ship$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Ship']))*100
Percent_Cat_1_2_Wreck = (length(Cat_1_2_Wreck$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Wreck']))*100
Percent_Cat_1_2_Cobb = (length(Cat_1_2_Cobb$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Cobb']))*100
Percent_Cat_1_2_Hog = (length(Cat_1_2_Hog$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Hog']))*100
Percent_Cat_1_2_Parramore = (length(Cat_1_2_Parramore$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Parramore']))*100
Percent_Cat_1_2_Cedar = (length(Cat_1_2_Cedar$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Cedar']))*100

# Percentage of total island transects represented by Cat 3 + 4
Percent_Cat_3_4_Smith = (length(Cat_3_4_Smith$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Smith']))*100
Percent_Cat_3_4_Myrtle = (length(Cat_3_4_Myrtle$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Myrtle']))*100
Percent_Cat_3_4_Ship = (length(Cat_3_4_Ship$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Ship']))*100
Percent_Cat_3_4_Wreck = (length(Cat_3_4_Wreck$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Wreck']))*100
Percent_Cat_3_4_Cobb = (length(Cat_3_4_Cobb$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Cobb']))*100
Percent_Cat_3_4_Hog = (length(Cat_3_4_Hog$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Hog']))*100
Percent_Cat_3_4_Parramore = (length(Cat_3_4_Parramore$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Parramore']))*100
Percent_Cat_3_4_Cedar = (length(Cat_3_4_Cedar$Shrub_Presence) / length(Total_index[paper_data$Island_Name == 'Cedar']))*100

# Calculate the actual observed shrub presence on each island
Actual_Shrub_Amount_Smith = paper_data[paper_data$Island_Name == 'Smith' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Myrtle = paper_data[paper_data$Island_Name == 'Myrtle' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Ship = paper_data[paper_data$Island_Name == 'Ship' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Wreck = paper_data[paper_data$Island_Name == 'Wreck' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Cobb = paper_data[paper_data$Island_Name == 'Cobb' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Hog = paper_data[paper_data$Island_Name == 'Hog' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Parramore = paper_data[paper_data$Island_Name == 'Parramore' & paper_data$Shrub_Presence == 'Shrub',]
Actual_Shrub_Amount_Cedar = paper_data[paper_data$Island_Name == 'Cedar' & paper_data$Shrub_Presence == 'Shrub',]

# Calculate the % of transects on each island with observed shrub presence
Percent_Actual_Shrub_Amount_Smith = (length(Actual_Shrub_Amount_Smith$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Smith']))*100
Percent_Actual_Shrub_Amount_Myrtle = (length(Actual_Shrub_Amount_Myrtle$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Myrtle']))*100
Percent_Actual_Shrub_Amount_Ship = (length(Actual_Shrub_Amount_Ship$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Ship']))*100
Percent_Actual_Shrub_Amount_Wreck = (length(Actual_Shrub_Amount_Wreck$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Wreck']))*100
Percent_Actual_Shrub_Amount_Cobb = (length(Actual_Shrub_Amount_Cobb$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Cobb']))*100
Percent_Actual_Shrub_Amount_Hog = (length(Actual_Shrub_Amount_Hog$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Hog']))*100
Percent_Actual_Shrub_Amount_Parramore = (length(Actual_Shrub_Amount_Parramore$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Parramore']))*100
Percent_Actual_Shrub_Amount_Cedar = (length(Actual_Shrub_Amount_Cedar$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Cedar']))*100

# Calculate the actual observed shrub presence on each island
Actual_Non_shrub_Amount_Smith = paper_data[paper_data$Island_Name == 'Smith' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Myrtle = paper_data[paper_data$Island_Name == 'Myrtle' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Ship = paper_data[paper_data$Island_Name == 'Ship' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Wreck = paper_data[paper_data$Island_Name == 'Wreck' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Cobb = paper_data[paper_data$Island_Name == 'Cobb' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Hog = paper_data[paper_data$Island_Name == 'Hog' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Parramore = paper_data[paper_data$Island_Name == 'Parramore' & paper_data$Shrub_Presence == 'Non-shrub',]
Actual_Non_shrub_Amount_Cedar = paper_data[paper_data$Island_Name == 'Cedar' & paper_data$Shrub_Presence == 'Non-shrub',]

# Calculate the % of transects on each island with observed shrub presence
Percent_Actual_Non_shrub_Amount_Smith = (length(Actual_Non_shrub_Amount_Smith$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Smith']))*100
Percent_Actual_Non_shrub_Amount_Myrtle = (length(Actual_Non_shrub_Amount_Myrtle$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Myrtle']))*100
Percent_Actual_Non_shrub_Amount_Ship = (length(Actual_Non_shrub_Amount_Ship$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Ship']))*100
Percent_Actual_Non_shrub_Amount_Wreck = (length(Actual_Non_shrub_Amount_Wreck$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Wreck']))*100
Percent_Actual_Non_shrub_Amount_Cobb = (length(Actual_Non_shrub_Amount_Cobb$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Cobb']))*100
Percent_Actual_Non_shrub_Amount_Hog = (length(Actual_Non_shrub_Amount_Hog$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Hog']))*100
Percent_Actual_Non_shrub_Amount_Parramore = (length(Actual_Non_shrub_Amount_Parramore$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Parramore']))*100
Percent_Actual_Non_shrub_Amount_Cedar = (length(Actual_Non_shrub_Amount_Cedar$Shrub_Presence)/length(Total_index[paper_data$Island_Name == 'Cedar']))*100

# Combine all % values into 1 variable for plotting
Percent_Actual_Shrub_Amount_Total = c(Percent_Actual_Shrub_Amount_Smith,Percent_Actual_Shrub_Amount_Myrtle,Percent_Actual_Shrub_Amount_Ship,Percent_Actual_Shrub_Amount_Wreck,
                                      Percent_Actual_Shrub_Amount_Cobb,Percent_Actual_Shrub_Amount_Hog,Percent_Actual_Shrub_Amount_Parramore,Percent_Actual_Shrub_Amount_Cedar)
Percent_Actual_Non_shrub_Amount_Total = c(Percent_Actual_Non_shrub_Amount_Smith,Percent_Actual_Non_shrub_Amount_Myrtle,Percent_Actual_Non_shrub_Amount_Ship,Percent_Actual_Non_shrub_Amount_Wreck,
                                          Percent_Actual_Non_shrub_Amount_Cobb,Percent_Actual_Non_shrub_Amount_Hog,Percent_Actual_Non_shrub_Amount_Parramore,Percent_Actual_Non_shrub_Amount_Cedar)

Percent_Cat_1_2 = c(Percent_Cat_1_2_Smith,Percent_Cat_1_2_Myrtle,Percent_Cat_1_2_Ship,Percent_Cat_1_2_Wreck,Percent_Cat_1_2_Cobb,Percent_Cat_1_2_Hog,Percent_Cat_1_2_Parramore,Percent_Cat_1_2_Cedar)
Percent_Cat_3_4 = c(Percent_Cat_3_4_Smith,Percent_Cat_3_4_Myrtle,Percent_Cat_3_4_Ship,Percent_Cat_3_4_Wreck,Percent_Cat_3_4_Cobb,Percent_Cat_3_4_Hog,Percent_Cat_3_4_Parramore,Percent_Cat_3_4_Cedar)

# Plot Figure 11 
Groups_Total = rbind(Percent_Cat_1_2,Percent_Actual_Non_shrub_Amount_Total,Percent_Cat_3_4,Percent_Actual_Shrub_Amount_Total)
barplot(height = Groups_Total, beside = TRUE,names.arg = c('Smith','Myrtle','Ship','Wreck','Cobb','Hog','Parramore','Cedar'), col = c('dodgerblue','lightblue1','darkgreen','green'),
        main = 'Total % of Transects in Low and High Shrub Probability Groups')
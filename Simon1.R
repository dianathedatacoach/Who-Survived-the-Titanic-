#1.  Load Packages#####################################
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('data.table') # classification algorithm

#2 Set Working Directory################################
setwd('C:/Users/diana/OneDrive/KAGGLE')

#3 Upload Data##########################################
train<- read.csv("train.csv", stringsAsFactors = F)
test<- read.csv("test.csv", stringsAsFactors = F)
full <-bind_rows(train, test) #bind training & test data
#By using similar data for training and testing, 
#you can minimize the effects of data discrepancies and 
#better understand the characteristics of the model. 
#After a model has been processed by using the training set, 
#you test the model by making predictions against the test set
#dataframe are two dimensional array like table
#by default R put character strings to factors
#stringasfactors is a logical argument whether the strings in
#a dataframe should be treated as factor variables
#or as plain string



#4.View Data Characters and Variables#################
str(full)
#Class types, 1309 observations of 12 variables

#4A. View the first six rows using head function#####
head(train)

#4B. View the first six rows using head function#######
head(test)
#Test does not have survived column because that is 
#what we are trying to predict

#5.What is included in the file?#########################
#Variable Name / Description
#Passenger ID Id Passenger identification 
#Survived Survived (1) or died (0)
#Pclass - Passenger's class
#Name - Passenger's Name
#Sex - Passenger' sex
#Age - Passenger's age
#Sibsp - Number of siblings or spouses abroad
#Parch - Number of parents/children abroad
#Ticket - Ticket number 
#Fare - Fare
#Cabin - Cabin
#Embarked Port of embarkation

#6.Grab Title From Passenger Names##########################
#use surname as predictions
full$title <- gsub('(.*, )|(\\..*)', '', full$Name)

#7.View All Unique Titles#################################
table(full$title)

#8.Show Title Counts By Sex##############################
table(full$title, full$Sex)

#9. View Titles of those Survived##################################
table(full$Survived, full$title)
#Females with title Miss used 260 times Mrs used 197 times
#Mr shows Master used 61 times and Mr 757 times
#Low cell counts used Capt (1), Col (4), Dr (7), Jonkeer (1)
#Major(1), Mme (1), Ms(2), Rev(8), Sir(1), and the Countess(1)
#Mlle - french title used equivalent to misses
#Mme-  means madame or married female
# Also reassign mlle, ms, and mme accordingly


#10.Titles with less cell counts to be combined to RARE_TITLE
rare_title <- c('Capt', 'Col', 'Don', 'Dona', 'Dr', 'Jonkheer', 'Lady',
                'Major', 'Rev', 'Sir', 'the Countess')

#11. Move Ms and Mlle to Miss and Mme to Mrs##################
library(stringr)
library(tidyverse)

full <- full %>% 
  mutate(across('title', str_replace, 'Mlle', 'Miss'))
#replace Mlle with Miss in title column
full <- full %>% 
  mutate(across('title', str_replace, 'Mme', 'Mrs'))
#replace Mme with Mrs in title column
full <- full %>% 
  mutate(across('title', str_replace, 'Ms', 'Miss'))
#replace Ms with Miss in title column

#11A View Sex and Title Columns#########################
colnames(full)
(table(full$Sex, full$title))
#Miss increased from 260 to 264 as we moved Mlle and Ms to Miss
#Mrs increased from 197 to 198 as we moved Mme to Mrs

#12. Grab surnmae from passer name, Split name column to grab LName and FName to view unqiue names########################
full <- full %>% separate(Name, c('LName', 'FName'))

#13.Any unique names by last name ######################
unique(full$`LName`)
#We have 853 unique names. 


#2.0 Did the families sink or swim?##############
#1.Read in Full data###################################################
colnames(full)
#Split passenger name into new variables to look further steps and make new family variables
#Create family size variable based on SibSP (Siblings/Spouses)- maybe 
#someone has more than one spouse

#1A. Create a family size variable including the passenger themselves#############
#created a column based if siblings/spouses has parents or children
full$Fsize <- full$SibSp + full$Parch + 1
#In Family Size column shows the number of siblings and/or parents and children plus themselves

full %>%
  filter(full$`LName`== "Palsson")
#Searching by Last Name - Palsson, we see the family size is shows 5. This person had four other siblings/parents
#or children


#2. Create a family column to view how it may relate with survival#################
full$Family <- paste(full$`LName`, full$Fsize, sep='_')
#combined last_name and family size


#3.Use ggplot2 to visualize the relationship between family size & survival#######
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', fill= 'pink', colour = 'yellow', position='dodge') +
  labs(title = 'Titanic Family Size & Survival Count', subtitle = 'Bar Plot In R')+
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


#we can see that if the family size is One or after a family size of 4
#that you did not survive. To get a better look lets collapse 
#into three levels
#Not Survived = 0, Survived = 1

#4. We can collapse this variable into three levels which will be helpful######### 
#since there are comparatively 
#fewer large families. Let’s create a discretized family size variable.
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'Single'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'Small_Family'
full$FsizeD[full$Fsize > 4] <- 'Large_Family'
#Three levels created Single, Small_Family and Large_Family

#5.View in mosaic plot to show the relationships and to provide a visual#########
#comparison groups
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
#Small Family in Red shows less than 4 Did Not Survive
#Family Size (Single or Large Family)is 2 to 4 either did not survive or survived
#If from 2 to 0 or 0to 2 you may have survived or did not survive
#Between 2 to 4 shows in Family Size Large Family and Singles did not survive mostly
#Blue shows those families less than 4 either survived or did not survive

#2.3 Lets look further in the Family staying in which cabin decides if they surivived or not survived#########################
#Format a few more variables - Passenger Cabin

#1. View Cabin Values#####################################################
#viewed a small amount of values
full$Cabin[1:28]

#2.View First Character we see C85 POSITOIN 2################################
strsplit(full$Cabin[2], NULL)[[1]]

#3. CREATE A DECK VARIABLE AND RETRIEVE PASSENGER DECK A -F#########################
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
#created column 18. Extracted Deck from Cabin column

#3A.VIEW DECK INFORMAITON########################################################
table(full$Deck)
#Decks show from A to T

#We want to see where did they embark from
#4. Get rid of our missing passenger IDs############################################
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

#5.Use ggplot2 to visualize embankment, passenger class, & median fare####################
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  labs(title = 'Titanic Passenger Embarked & Fare Received', subtitle = 'Box Plot In R')+
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
##1 equals = First  Class, 2 = Second Class Passenger, and 3 = Third Class Passenger
#Median fare shows first class passenger departing fro Charbourg
#coincides nicely with the $80 paid by our embankment deficient passengers
#We can safely replace the NA values with C
#6B. Since their fare was $80 for 1st class, they most likely embarked from 'C',, we place C in the column#####################
full$Embarked[c(62, 830)] <- 'C'

#We’re close to fixing the handful of NA 
#values here and there. Passenger on row 1044 has an NA Fare value.

#6 Show row 1044###########################################################
full[1044, ]
#Is a 3 Third Class Passenger Embarked from Southampton(S). 

#7.#Visualizing fares who share the same class(n=494)#######################
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  labs(title = 'Titanic Same Passenger Class (3) & Embarked', subtitle = 'Geom_VLine In R')
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
#From this visualization, it seems quite reasonable to replace the NA Fare value with median for their ###################################
#class and embankment which is $8.05.

#8.Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
#Just like row number 1044 add median when Passenger Class is 3 and Embarcked from S- Southampton

#3.2 Predictive Imputation###################################################
#1.Missing Age values so we will create a model predicting ages based on other variables#####
#1A. Show number of missing Age values######################################
sum(is.na(full$Age))
#263 are missing ages in the columns

#2.Transform variables in factors or numeric##################################
library(dplyr)
full <- full %>%
  mutate(
    PassengerId = as.factor(PassengerId),
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Sex = as.factor(Sex),
    title = as.factor(title),
    Family = as.factor(Family),
    FsizeD = as.factor(FsizeD))

#3.View data structure#######################################################
str(full)
#We have now transformed some of the columns to factors

#4.Imputation part##########################################################
#4. Lets proceed with Imputation 

library(mice) #load package
init = mice(full, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#5.Performed mice imputation, excluding certain less than useful variables#####
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','LName',
                                            'Ticket','Cabin','Family',
                                            'Survived')], 
                 method='rf') 
#We were able to predict age depending on the column names above.

#6. Save Complete Output#######################################################
mice_output <- complete(mice_mod)
#shows age fulfilled in Age column using other variable columns

#7. Compare results from Original Distribution of Passenger Ages to Ensure that nothing#############################
#has gone completely weird
# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', #title of plot
     col='darkgreen', ylim=c(0,0.04))   #make graph bars dark green

hist(mice_output$Age, freq=F, main='Age: MICE Output', #title of plot
     col='lightgreen', ylim=c(0,0.04)) #make second graph light green


#8.Now replace Age variable from the Mice Model################################
full$Age <- mice_output$Age

#9.Now lets view any number of Missing Age Values$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####
sum(is.na(full$Age))
#shows as 0 no missing values we achieved it


#3.3 Feature Engineering: Round 2 - Using Age now that we know everyone s#########################################
#age  lets create a Age-Dependent Variables Child and Mother. 
#What is a child? A Child is someone under 18 years of age
#What is a mother? Mother is 1. Female and 2. Over 18, 3. has more than 0 children
#4. Does not have a title of Miss

#1. First we'll look at the relationship between age & survival####################################
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
#2. I include Sex since we know (a priori) it's a significant predictor#####################################################
  facet_grid(.~Sex) + 
  theme_few()
#Female and Male Charts showing who Survived and Not survived. Noticed
#Ages between 20 to 40 as a female did not survive but below 20 and over 40 survived
# Ages in Males from all ages mostly did not survive. Ages over 60 to 80 did not survive

#3. Create the column child, and indicate whether child or adult####################################################################
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

#4. Show counts########################################################################################################################
table(full$Child, full$Survived)
#482 of Adults and 67 Children did not survive; 
#271 Adults survived and 71 children survived
#Noticed whether a child you either survived or did not survive

#5.Moving on to Mother variables - Adding Mother Variable#######################################################################################
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & 
              full$Age > 18 & full$title != 'Miss'] <- 'Mother'
#looking at Females Mothers  that hare over 18, and does not contain the word Miss in the title column


#6.Show counts############################################################################################################################################
table(full$Mother, full$Survived)
#16 mothers and 533 Not Mothers did not survive 
#303 Not Mothers and 39 Mothers survived

#7.# Finish by factorizing our two new factor variables###############################################################################################################
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

#8.There should be no missing values lets check ########################################################################################################
md.pattern(full)
#we have treated all those relevant missing values Age is now 0

#4. PREDICTOIN##############################################################################################################################
#now we can predict who survives among passengers of the Titanic based on 
#variables we treated curated and treated missing values. Lets rely on Random
#Forest classification algorithm.

#4.1 Split the data back into a train set and a test set################################################################################
train <- full[1:891,]
test <- full[892:1309,]


#4.2 Building the model############################################################################################################################
#1.Set a random model 
set.seed(754)

#2. Build the model (note:not all possible variabls are used$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + title + 
                           FsizeD + Child + Mother,
                         data = train)

#3.Show model error#################################################################################################################
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
#Black line shows Overall Error Rate below 20%
#Red and Green Error Rate Died & Survived 
#We can successfully make a prediction of death than survival


#4.3 Variable Importance#####################################################################################################
#1.Looking at the relative variable importance  by plotting the mean decrease
#in Gini calculated across all the trees
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

#2.Create a rank variable based on importance#########################################################
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#3.Use ggplot2 to visualize the relative importance of variables#####################################
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'yellow') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

#The highest relative importance is Title of all predictors variables


#4.4 Making the Prediction
# We’re ready for the final step — making our prediction! When we finish here, we could iterate through the preceding steps making tweaks as we go or fit the data using different models or use different combinations of variables to achieve better predictions. But this is a good starting (and stopping) point for me now.

#1.Predict using the test set################################################################
prediction <- predict(rf_model, test)

#2..Save the solution to a data frame with two columns: Passenger Id and Survived (prediction)###########################################
prediction <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

#3. Write the solution to file##########################################################################################################
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


#Diana has completed this project! Awesome! 







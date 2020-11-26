#LDA
# apply this to if you have a bunch of data and they know thye can be divided into groups()
# 3 subspecies of butterlies- can we split up into the subspecies?
# can you accurately decide if a certain mushrooms are edible/non edible


#PCA- 
  # 2 diff organisms living in diff environments. separating into clusters based off the environment they like
  # temp may have a larger impact on where they live than pH
  # or eg which genes have more of an impact on a trait
  # which factors have more influence than others on something

library(tidyverse)
library(caret)
library(GGally)

# importing the data

seeds <- read.table("seeds_dataset.txt")

# renaming the column headers
seeds2 <- seeds %>% 
  rename(
    Area = V1,
    Perimeter = V2,
    Compactness = V3,
    "Length of kernel" = V4,
    "Width of kernel" = V5,
    "Asymmetry coefficient" = V6,
    "Length of kernel groove" = V7,
    Variety = V8
  )

# changing the varieties to their actual names
seeds3 <- within(seeds2, Variety <- factor(Variety, labels = c("Kama", "Rosa", "Canadian")))


# train and test on same set of data
# training model- telling can you clister tthigns based on factors in this dataset. can be too specific


ids_seeds <- createDataPartition(y = seeds3$Variety,
                           p = 0.75,
                           list = FALSE)


# Create the training set:
train_seeds <- seeds3 %>% slice(ids_seeds)
# Create the testing set:
test_seeds <- seeds3 %>% slice(-ids_seeds)

lda_seeds <- train_seeds %>%
  select(Area, 
         Perimeter, 
         Compactness,
         "Length of kernel",
         "Width of kernel",
         "Asymmetry coefficient",
         "Length of kernel groove") %>%
  MASS::lda(grouping = train_seeds$Variety)

# And predict classes of the test data based on lda model:
plda_seeds <- test_seeds %>%
  select(Area, 
         Perimeter, 
         Compactness,
         "Length of kernel",
         "Width of kernel",
         "Asymmetry coefficient",
         "Length of kernel groove") %>%
  predict(object = lda_seeds)

# Examining the confusion matrix:
confusionMatrix(plda_seeds$class,factor(test_seeds$Variety))


# Reference- observed class along the top and predicted along the side. 
  # in every single case these were correct

# accuraacey = 51/51 = 1

# No intro rate - number of the most common over the total
# is accuracy significantly bettwe than no information rate. 1 (in this case) is much better than you would get by chance

# is our model performing equally well for all the classes
  # sensitivity - number of true positives
  # specificity - number of true negatives
  # prevalence - how common it is in data set








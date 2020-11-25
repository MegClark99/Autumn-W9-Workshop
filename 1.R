# LDA on penguins without training and testing

#loading package
library(palmerpenguins)

# Clean the variable names for ease of use:
penguin <- penguins_raw %>%
  janitor::clean_names()


# We repeat the simple filtering and tidying we did previously.
# Filter out the rows with missing values:
penguin <- penguin %>% 
  filter(!is.na(body_mass_g))
# Split species into common_name and scientific_name:
penguin2 <- penguin %>% 
  extract(species, 
          c("common_name", "scientific_name"),
          "([a-zA-Z]+\\s[a-zA-Z]+)\\s\\(([a-zA-Z]+\\s[a-zA-Z]+)\\)")


# Now to run the LDA.
# Select the four variables and pipe into the MASS::lda() function which does the PCA:
lda <- penguin2 %>% 
  select(body_mass_g,
         ends_with("_mm")) %>%
  MASS::lda(grouping = penguin2$common_name)


# This is a good example of when we definitely want to use package::function().
# MASS has a function called select() like dplyr, 
  # but it works differently (the function is defined differently).
# You can spend hours of your life wondering what is wrong with your code when it looks fine and worked before if you load both packages. 
  #I have done this with these particular functions too many times!


# Just as we could see the importance of each variable in each Principal Component using pca$loadings, 
  # we can see the importance of each variable in each discriminant using lda$scaling.

# View the importance of each variable in each discriminant:
lda$scaling
##                        LD1       LD2
## body_mass_g        0.00130  0.001711
## culmen_length_mm   0.08833 -0.417871
## culmen_depth_mm   -1.03730 -0.021005
## flipper_length_mm  0.08616  0.013475



# The generic function predict() is used to give the species predicted by the model for each observation.

#Select the variables from Penguin that are in the model and predict the species from the lda model object:
plda <- penguin2 %>% 
  select(body_mass_g,
         ends_with("_mm")) %>%
  predict(object = lda)


# caret provides us with a useful function to examine the confusion matrix.
# A confusion matrix is a table that tells us about the performance of a classification model. The table gives the number of:
    # correct predictions: the species predicted matches the observed species
    # incorrect predictions: the species predicted does not match the observed species. for each species.


# The confusionMatrix() function also outputs:
    # Accuracy - No. correct predictions / No. of observations
    # 95% CI - 95 percent confidence interval on the accuracy (using binom.test())
    # No Information Rate - No. observations in the largest class / Number of observations
    # P-Value [Acc > NIR] - Is the model significantly better than than you could do by always predicting the most common class (again using binom.test()).


# Examining the confusion matrix:
confusionMatrix(plda$class, factor(penguin2$common_name))
## Confusion Matrix and Statistics
## 
##                    Reference
## Prediction          Adelie Penguin Chinstrap penguin Gentoo penguin
##   Adelie Penguin               150                 3              0
##   Chinstrap penguin              1                65              0
##   Gentoo penguin                 0                 0            123
## 
## Overall Statistics
##                                        
##                Accuracy : 0.988        
##                  95% CI : (0.97, 0.997)
##     No Information Rate : 0.442        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.982        
##                                        
##  Mcnemar's Test P-Value : NA           
## 
## Statistics by Class:
## 
##                      Class: Adelie Penguin Class: Chinstrap penguin
## Sensitivity                          0.993                    0.956
## Specificity                          0.984                    0.996
## Pos Pred Value                       0.980                    0.985
## Neg Pred Value                       0.995                    0.989
## Prevalence                           0.442                    0.199
## Detection Rate                       0.439                    0.190
## Detection Prevalence                 0.447                    0.193
## Balanced Accuracy                    0.989                    0.976
##                      Class: Gentoo penguin
## Sensitivity                           1.00
## Specificity                           1.00
## Pos Pred Value                        1.00
## Neg Pred Value                        1.00
## Prevalence                            0.36
## Detection Rate                        0.36
## Detection Prevalence                  0.36
## Balanced Accuracy                     1.00


# The model had an accuracy of 98.8304%. There were 151 Adelie Penguins of which 1 were predicted incorrectly; 
  # 68 Chinstrap Penguins of which 3 were predicted incorrectly; and 123 Gentoo Penguins of which 0 were predicted incorrectly.


# To plot, we might want to use the scores on each of the new axes and colour them by species. The scores are in a variable called $x in plda
  # Extract the scores into a dataframe with the species names:
lda_labelled <- data.frame(plda$x,
                           common_name = penguin2$common_name)
# create a scatter plot
lda_labelled %>% 
  ggplot(aes(x = LD1, y = LD2, color = common_name)) +
  geom_point()
# The separation between species is stronger in the LDA than in the PCA.








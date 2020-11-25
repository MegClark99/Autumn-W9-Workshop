# LDA on single-cell RNASeq data with training and testing

# The data in scrna_data.csv are the single-cell RNASeq data we considered last week. 
# Each row is a cell (an observation) and each column is a gene (a variable / feature).
# The values are gene expression values.

# Import the data and the cell labels:
file <- "scrna_data.csv"
rna <- read_csv(file)
file2 <- "scrna_meta.csv"
meta <- read_csv(file2) %>% select(louvain)

# Add the cell labels to the data:
rna$cell <- meta$louvain

# Split the dataset in to training and testing sets using createDataPartition()
ids3 <- createDataPartition(y = rna$cell,
                           p = 0.75,
                           list = FALSE)


# Now we use those row numbers to select the rows from rna to create the training and test datasets.
# Create the training set:
train3 <- rna %>% slice(ids3)
# Create the testing set:
test3 <- rna %>% slice(-ids3)

# Perform the LDA on the training data:
lda3 <- train3 %>% 
  select(-cell) %>%
  MASS::lda(grouping = train3$cell)

# How well does our model perform on the training set data.
# Predict on the training data:
plda_train3 <- train3 %>% 
  select(-cell) %>%
  predict(object = lda3)
  

# Examining the confusion matrix:
confusionMatrix(plda_train3$class,factor(train3$cell))
## Confusion Matrix and Statistics
## 
##                    Reference
## Prediction          B CELLS CD14+ Monocytes CD4 T CD8 T Dendritic
##   B CELLS               256               0     0     0         0
##   CD14+ Monocytes         0             360     0     0         0
##   CD4 T                   0               0   865     0         0
##   CD8 T                   0               0     0   228         0
##   Dendritic               0               0     0     0        27
##   FCGR3A+ Monocytes       0               0     0     0         0
##   Megakaryocytes          0               0     0     0         0
##   NK CELLS                0               0     0     0         0
##                    Reference
## Prediction          FCGR3A+ Monocytes Megakaryocytes NK CELLS
##   B CELLS                           0              0        0
##   CD14+ Monocytes                   0              0        0
##   CD4 T                             0              0        0
##   CD8 T                             0              0        0
##   Dendritic                         0              0        0
##   FCGR3A+ Monocytes               115              0        0
##   Megakaryocytes                    0             12        0
##   NK CELLS                          0              0      118
## 
## Overall Statistics
##                                     
##                Accuracy : 1         
##                  95% CI : (0.998, 1)
##     No Information Rate : 0.437     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 1         
##                                     
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: B CELLS Class: CD14+ Monocytes Class: CD4 T
## Sensitivity                   1.000                  1.000        1.000
## Specificity                   1.000                  1.000        1.000
## Pos Pred Value                1.000                  1.000        1.000
## Neg Pred Value                1.000                  1.000        1.000
## Prevalence                    0.129                  0.182        0.437
## Detection Rate                0.129                  0.182        0.437
## Detection Prevalence          0.129                  0.182        0.437
## Balanced Accuracy             1.000                  1.000        1.000
##                      Class: CD8 T Class: Dendritic Class: FCGR3A+ Monocytes
## Sensitivity                 1.000           1.0000                   1.0000
## Specificity                 1.000           1.0000                   1.0000
## Pos Pred Value              1.000           1.0000                   1.0000
## Neg Pred Value              1.000           1.0000                   1.0000
## Prevalence                  0.115           0.0136                   0.0581
## Detection Rate              0.115           0.0136                   0.0581
## Detection Prevalence        0.115           0.0136                   0.0581
## Balanced Accuracy           1.000           1.0000                   1.0000
##                      Class: Megakaryocytes Class: NK CELLS
## Sensitivity                        1.00000          1.0000
## Specificity                        1.00000          1.0000
## Pos Pred Value                     1.00000          1.0000
## Neg Pred Value                     1.00000          1.0000
## Prevalence                         0.00606          0.0596
## Detection Rate                     0.00606          0.0596
## Detection Prevalence               0.00606          0.0596
## Balanced Accuracy                  1.00000          1.0000


# The model had an accuracy of 100%. Wow, that's good. Or is it??



# But what about performance on the test set? Is our modelling overftting? 
  # Would it be equally good on the scRNASeq data for a new dataset?

# Predict classes of the test data based on LDA model:
plda_test3 <- test3 %>% 
  select(-cell) %>%
  predict(object = lda3)

# Examining the confusion matrix:
confusionMatrix(plda_test3$class, factor(test3$cell))
## Confusion Matrix and Statistics
## 
##                    Reference
## Prediction          B CELLS CD14+ Monocytes CD4 T CD8 T Dendritic
##   B CELLS                75               0    15     3         0
##   CD14+ Monocytes         1              97     7     0         2
##   CD4 T                   4               8   203    27         0
##   CD8 T                   4               1    59    40         0
##   Dendritic               1               3     2     0         7
##   FCGR3A+ Monocytes       0              10     1     0         0
##   Megakaryocytes          0               0     0     0         0
##   NK CELLS                0               1     1     5         0
##                    Reference
## Prediction          FCGR3A+ Monocytes Megakaryocytes NK CELLS
##   B CELLS                           0              0        1
##   CD14+ Monocytes                  10              0        0
##   CD4 T                             4              1        1
##   CD8 T                             0              1        1
##   Dendritic                         1              0        0
##   FCGR3A+ Monocytes                23              0        0
##   Megakaryocytes                    0              1        0
##   NK CELLS                          0              0       36
## 
## Overall Statistics
##                                         
##                Accuracy : 0.734         
##                  95% CI : (0.698, 0.767)
##     No Information Rate : 0.438         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.649         
##                                         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: B CELLS Class: CD14+ Monocytes Class: CD4 T
## Sensitivity                   0.882                  0.808        0.705
## Specificity                   0.967                  0.963        0.878
## Pos Pred Value                0.798                  0.829        0.819
## Neg Pred Value                0.982                  0.957        0.792
## Prevalence                    0.129                  0.183        0.438
## Detection Rate                0.114                  0.148        0.309
## Detection Prevalence          0.143                  0.178        0.377
## Balanced Accuracy             0.925                  0.886        0.791
##                      Class: CD8 T Class: Dendritic Class: FCGR3A+ Monocytes
## Sensitivity                0.5333           0.7778                   0.6053
## Specificity                0.8866           0.9892                   0.9822
## Pos Pred Value             0.3774           0.5000                   0.6765
## Neg Pred Value             0.9365           0.9969                   0.9759
## Prevalence                 0.1142           0.0137                   0.0578
## Detection Rate             0.0609           0.0107                   0.0350
## Detection Prevalence       0.1613           0.0213                   0.0518
## Balanced Accuracy          0.7100           0.8835                   0.7937
##                      Class: Megakaryocytes Class: NK CELLS
## Sensitivity                        0.33333          0.9231
## Specificity                        1.00000          0.9887
## Pos Pred Value                     1.00000          0.8372
## Neg Pred Value                     0.99695          0.9951
## Prevalence                         0.00457          0.0594
## Detection Rate                     0.00152          0.0548
## Detection Prevalence               0.00152          0.0654
## Balanced Accuracy                  0.66667          0.9559


# The model had an accuracy of 73.3638%. That is much more honest and robust test.




# We will plot the training data and then the test data.

# Extract the scores from the training set with the cell names:
lda_labelled_train3 <- data.frame(plda_train3$x,
                                   cell = train3$cell)
# Extract the scores from the training set with the cell names:
  
lda_labelled_test3 <- data.frame(plda_test3$x,
                                  cell = test3$cell)

# Create a scatter plot for the training data:
lda_labelled_train3 %>% 
  ggplot(aes(x = LD1, y = LD2, color = cell)) +
  geom_point()
# Based on this plot, you might be surprised by the accuracy of the model predictions on the training set - there seems to be a lot of overlap.
# However, you are only looking at LD1 and LD2. There are many dimensions in this dataset and the separation of groups might not be obvious from the first to LD.
# GGally (Schloerke Cook, et al., 2020) can let us examine several pairwise LD comparisons.

# Select the first 5 LDs and pipe in to ggpairs():
lda_labelled_train3 %>% 
  select(LD1:LD5, cell) %>% 
  ggpairs(aes(color = cell))
# You can see how LD1 really separates Megakaryocytes from the other cell types but that other LD are needed to distinguish all the cell types.

# Now consider the test set.
# Create a scatter plot for the test data:
lda_labelled_test3 %>% 
  ggplot(aes(x = LD1, y = LD2, color = cell)) +
  geom_point()
# There's a lot of overlap here. Perhaps we will better see the difference by examining additional LDs. However, 
  # remember that the predictions were less good on the test set so we would expect it to be difficult to distinguish all cells.

# Select the first 5 LDs and pipe in to ggpairs():
lda_labelled_test3 %>% 
  select(LD1:LD5, cell) %>% 
  ggpairs(aes(color = cell))


# SUMMARY
    # Linear discriminant analysis is a supervised ML method
    # It is applied when you have many continuous variables and allows you to visualised the data in fewer dimensions, and thus see group/patterns more easily.
    # LDA is a fast, linear, parametric method
    # the maximum number of discrimants is one fewer than the number of dimensions.
    # overfitting occurs when a model too closely fits a limited set of data points and does not generalise
    # partitioning data into training and testing sets is the primary way to avoid overfitting
    # the performance of a classification model can be evaluated using a confusion matrix



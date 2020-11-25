# installing packages for the prework
library(caret)
library(lattice)
library(ggplot2)

# Linear Discriminant Analysis (LDA) aims 
# to find linear combination of variables the maximise differences between groups.

# It is supervised because we label observations by their class and determine the allocation rules based on these.
# A ‘discriminant’ is a linear combination of variables that best separates the groups. If there n
# classes we have, at most, n-1 discriminants

# In these slides show you will apply:
  # LDA to the Penguin data without training and testing
  # LDA to the Penguin data with training and testing
  # LDA to the scRNASeq data with training and testing but we will consider how good the model is at predicting classes from the training set compared to the test set.


# Load the tidyverse, caret and GGally and for each RStudio Project.
library(tidyverse)
library(caret)
library(GGally)
# Do not load MASS, we will access the lda() function with MASS::lda() instead.






library(caret)
library(randomForest)
library(e1071)
library(yardstick)

# Read dataset as in original paper, see http://bit.ly/1oZnEKG
datf <- read.csv('sigite2014-difficulty-data.csv', header=T, sep=";")
# Read imputed dataset. Obtained using imputation Python scripts
datfimp <- read.csv('imputed_data.csv', header=T, sep=',')

# Set path where to store the models and other data
emodelpath = 'models_e/'

# Assignments: 1-71, 72.1-72.3, 73-106 & 108, 107 = feedback (omitted)
exercise_count <- 109
exercise_max_index <- 108
exercise_feedback_index <- 107

# Models F1 score: 1st column using original data, 2nd column using imputed data
mdlsf1 <- array(0, dim=c(exercise_count, 2))

# Models Matthews correlation coefficient (MCC) score: 1st column using original data, 2nd column using imputed data
mdlsmcc <- array(0, dim=c(exercise_count, 2))

# Selected training dataset entries for given exercise, negation is testing dataset
trainingSetIndices <- array(0, dim=c(exercise_count, 417))

# Extract single exercise information. Remove unnecessary features and add computed ones.
get_subset <- function(ldatf, subsetStr) {
  # Select single assignment columns
  ldfss <- ldatf[, grepl(subsetStr, names(ldatf))]

  # Remove 'WORKED_ON_*', 'SUBMITTED_*' and 'EDUCATIONA_VALUE_*' columns
  ldfss <- ldfss[, !grepl('WORKED|SUBMITTED|EDUCATIONAL', names(ldfss))]
  
  # Use only complete cases, i.e. no missing values
  ldfss_filtered <- ldfss[complete.cases(ldfss),]
  
  # Rename columns
  colnames(ldfss_filtered) <- c('SECONDS_SPENT','COMPILES1','DIFFICULTY','STROKES','SECONDS_IN_COMPILING','SECONDS_IN_NON_COMPILING_STATE', 'LOC', 'FOC')
  
  # Add calculated entries
  ldfss_filtered$COMPILES2 <- ldfss_filtered$SECONDS_IN_COMPILING/(ldfss_filtered$SECONDS_IN_COMPILING+ldfss_filtered$SECONDS_IN_NON_COMPILING)
  ldfss_filtered$TIME <- ldfss_filtered$SECONDS_IN_COMPILING+ldfss_filtered$SECONDS_IN_NON_COMPILING

  # Remove no longer needed columns
  ldfss_filtered <- ldfss_filtered[, !grepl('SECONDS', names(ldfss_filtered))]
  
  return(ldfss_filtered)
}

# Perform 10-fold cross-validation on model and get its predictions for original and imputed data
# If usedIndices is given utilize stored models instead
get_stats_imputed <- function(ledatf, ledatfi, exIndex, usedIndices = NULL) {
  # Ensure that difficulty is factor due to being used in classification
  ledatf$DIFFICULTY <- as.factor(ledatf$DIFFICULTY)
  ledatfi$DIFFICULTY <- as.factor(ledatfi$DIFFICULTY)
  
  # Check if indices already exists
  if(is.null(usedIndices)) {
    # Split data into train (80%) and validation (20%) sets
    ftrain <- createDataPartition(y=ledatf$DIFFICULTY, p=0.8, list=FALSE, groups = min(1, length(ledatf$DIFFICULTY)))
  } else {
    # Use previously selected indices instead
    ftrain <- usedIndices[exIndex, ]
    ftrain <- ftrain[ftrain > 0]
  }
  trainSet <- ledatf[ftrain,]
  validationSet <- ledatf[-ftrain,]
  
  # Store selected training set indices
  trainingSetIndices[exIndex, 1:length(ftrain)] <<- ftrain
  
  # Create training dataset from imputed data by removing validation dataset items based on their row names
  validationRowNames <- as.numeric(rownames(validationSet))
  itrainSet <- ledatfi[!rownames(ledatfi) %in% as.numeric(validationRowNames),]
  
  if(is.null(usedIndices)) {  
    # Following utilizes 10-fold cross-validation on RF
    rf_ctrl <- trainControl(method = 'cv', number = 10)
    mfit <- train(DIFFICULTY ~ ., data = trainSet, method = 'rf', trControl = rf_ctrl, metric='Kappa')
    imfit <- train(DIFFICULTY ~ ., data = itrainSet, method = 'rf', trControl = rf_ctrl, metric='Kappa')
    
    # Store models
    saveRDS(mfit, sprintf(paste0(emodelpath, 'e_mdl_%d.rds'), exIndex))
    saveRDS(imfit, sprintf(paste0(emodelpath, 'e_mdli_%d.rds'), exIndex))
  } else {
    # Load earlier models
    mfit <- readRDS(sprintf(paste0(emodelpath, 'e_mdl_%d.rds'), exIndex))
    imfit <- readRDS(sprintf(paste0(emodelpath, 'e_mdli_%d.rds'), exIndex))
  }
  # Make predictions
  preds1 <- predict(mfit, validationSet, type='raw')
  preds2 <- predict(imfit, validationSet, type='raw')
  
  # Get confusion matrices
  cm1 <- confusionMatrix(preds1, validationSet$DIFFICULTY)
  cm2 <- confusionMatrix(preds2, validationSet$DIFFICULTY)
  
  # Calculate F1 scores
  mdlsf1[exIndex, 1] <<- f1_score(preds1, validationSet$DIFFICULTY)
  mdlsf1[exIndex, 2] <<- f1_score(preds2, validationSet$DIFFICULTY)
  
  if(is.null(usedIndices)) {  
    # Store confusion matrices
    saveRDS(cm1, sprintf(paste0(emodelpath, 'e_mdl_%d_cm.rds'), exIndex))
    saveRDS(cm2, sprintf(paste0(emodelpath, 'e_mdli_%d_cm.rds'), exIndex))
  } else {
    mdlsmcc[exIndex, 1] <<- mcc(cm1$table)[[3]]
    mdlsmcc[exIndex, 2] <<- mcc(cm2$table)[[3]]
  }
}

# Calculate macro-averaged F1 for multi-class case or in binary case
# https://stackoverflow.com/a/36843900
f1_score <- function(predicted, expected, positive.class="1") {
  cm = as.matrix(table(expected, predicted))
  
  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  #Binary F1 or Multi-class macro-averaged F1
  ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
}

# Check that number of values in each difficulty category is at least 3
checkCats <- function(ldfss) {
  incl = TRUE
  # Loop through categories
  for(i in 1:5) {
    # Check minimum number of entries in given category
    if(nrow(ldfss[ldfss$DIFFICULTY == i,]) < 3) {
      incl = FALSE
      break
    }
  }
  return(incl)
}

# If models have been created earlier and they need to be utilized in e.g. calculating metrics or the like
# uncomment following line and perform few more operations from the process loop as indicated
#emind <- readRDS(paste0(emodelpath, 'e_training_indices.rds'))

#
# Run the process
#
cmsIndex = 1
for(ai in 1:exercise_max_index) {
  # Skip course feedback
  if (ai == exercise_feedback_index) next
  
  # Assignment #72 is exception and ID is in different format as in 072.1
  if(ai == 72) {
    # There are three such sub IDs, so handle them within assignment #72 processing
    # There is no 072 at all
    for(ais in 1:3) {
      # Get assignment ID in format XXX.X with zero padding in front, e.g. 072.2
      subsetID <- formatC(ai, width=3, format='d', flag='0')
      subsetID <- sprintf('%s.%d', subsetID, ais)

      # Get assignment related data
      dfss <- get_subset(datf, subsetID)
      dfssi <- get_subset(datfimp, subsetID)
      
      if(checkCats(dfss)) {
        get_stats_imputed(dfss, dfssi, cmsIndex)
        # Comment line above and uncomment this one if using previously created indices
        #get_stats_imputed(dfss, dfssi, cmsIndex, emind)
      }
      cmsIndex = cmsIndex + 1
    }
  }
  else {
    # Get assignment ID in format XXX with zero padding in front, e.g. 057
    subsetID <- formatC(ai, width=3, format='d', flag='0')
    
    # Get assignment related data
    dfss <- get_subset(datf, subsetID)
    dfssi <- get_subset(datfimp, subsetID)
    if(checkCats(dfss)) {
      get_stats_imputed(dfss, dfssi, cmsIndex)
      # Comment line above and uncomment this one if using previously created indices
      #get_stats_imputed(dfss, dfssi, cmsIndex, emind)
    }
    cmsIndex = cmsIndex + 1
  }
  # Idea here is that as a model training takes some time then storing these bits of data
  # here allows us to restart this loop from any assignment and continue until end in case
  # process needs to be or gets interrupted for some reason. Just change for loop starting
  # index from 1 to wanted. Do note that numbering changes after 71 since there is no exercise
  # 72 but instead 72.1, 72.2 & 72.3, thus 72 = 72.1, 73 = 72.2, 74 = 72.3, 75 = 73, 76 = 74 and so on.

  # Store training set indices and models F1 & MCC
  saveRDS(trainingSetIndices, paste0(emodelpath, 'e_training_indices.rds'))
  saveRDS(mdlsf1, paste0(emodelpath, 'e_mdls_f1.rds'))
  saveRDS(mdlsmcc, paste0(emodelpath, 'e_mdls_mcc.rds'))
}

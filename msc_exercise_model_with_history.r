library(caret)
library(randomForest)
library(e1071)
library(stringr)
library(dplyr)
library(yardstick)

# Read dataset as in original paper, see http://bit.ly/1oZnEKG
datf <- read.csv('sigite2014-difficulty-data.csv', header=T, sep=";")
# Read imputed dataset. Obtained using imputation Python scripts
datfimp <- read.csv('imputed_data.csv', header=T, sep=',')

# Set path where to store the models and other data
ewhmodelpath = 'models_ewh/'

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

# Generate column names for computed fields
gen_name <- function(orgColNames, prefixStr, subsetStr) {
  strWeekLoc <- str_locate(orgColNames[1], '_viikko')
  suffixStr <- substr(orgColNames[1], str_locate(orgColNames[1], '\\.'), str_length(orgColNames[1]))
  newCol <- paste(prefixStr, substr(orgColNames[1], strWeekLoc[1], strWeekLoc[2] + 2), '_', subsetStr, suffixStr, sep='')
  return(newCol)
}

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
  ldfss_filtered$COMPILES2 <- ldfss_filtered$SECONDS_IN_COMPILING/(ldfss_filtered$SECONDS_IN_COMPILING + ldfss_filtered$SECONDS_IN_NON_COMPILING)
  ldfss_filtered$TIME <- ldfss_filtered$SECONDS_IN_COMPILING + ldfss_filtered$SECONDS_IN_NON_COMPILING
  
  # Remove no longer needed columns
  ldfss_filtered <- ldfss_filtered[, !grepl('SECONDS', names(ldfss_filtered))]
  
  return(ldfss_filtered)  
}

# Create dataframe containing assignments up to and including current one
create_history <- function(ldatf, ldfss, subsetNr, subsetExt, excludeDifficulty = TRUE) {
  # ldatf - dataframe from CSV file
  # ldfss - selected subset based on get_subset function
  # subsetStr - ID of exercise being handled
  
  # For the second and beyond include previous assignments into dataframe
  if(subsetNr > 1) {
    rows_to_include <- as.numeric(rownames(ldfss))

    # Collect exercises before this one
    if(subsetNr == 72 && subsetExt > 1) {
      subsetUpTo <- subsetNr
    }
    else {
      subsetUpTo <- subsetNr - 1
    }
    
    # Skip feedback (107), as last exercise is 108 there is no need values beyond 106
    if(subsetUpTo > 106) {
      subsetUpTo = 106
    }

    # Collect assignments prior to current one    
    subRange = 0
    for(si in 1:subsetUpTo) {
      # For 72.2 & 72.3
      if(si == 72 && subsetExt > 1) {
        subRange = subsetExt - 1
        # For > 72
      } else if(si > 71 && subsetExt == 0) {
        subRange = 3
      }
      # Handle 72.1 - 72.3
      if(subRange > 0 && si == 72) {
        for(sis in 1:subRange) {
          subID <- formatC(si, width=3, format='d', flag='0')
          subID <- sprintf('%s.%d', subID, sis)
          ldfs <- get_subset_hist(ldatf, subID)
          ldfs <- ldfs[rownames(ldfs) %in% rows_to_include, ]
          # Omit past difficulties
          if(excludeDifficulty == TRUE) {
            ldf_hist <- ldf_hist[, !grepl('DIFFICULTY', names(ldf_hist))]
          }
          ldf_hist <- cbind(ldf_hist, ldfs)
        }
      }
      else {
        subID <- formatC(si, width=3, format='d', flag='0')
        ldfs <- get_subset_hist(ldatf, subID)
        ldfs <- ldfs[rownames(ldfs) %in% rows_to_include, ]
        if(exists('ldf_hist') == FALSE) {
          ldf_hist <- ldfs
        } else {
          # Omit past difficulties
          if(excludeDifficulty == TRUE) {
            ldf_hist <- ldf_hist[, !grepl('DIFFICULTY', names(ldf_hist))]
          }
          ldf_hist <- cbind(ldf_hist, ldfs)
        }
      }
    }
    # Omit past difficulties
    if(excludeDifficulty == TRUE) {
      ldf_hist <- ldf_hist[, !grepl('DIFFICULTY', names(ldf_hist))]
    }
    # Append currently handled assignemnt data at the end of history
    ldf_hist <- cbind(ldf_hist, ldfss)
  }
  # For the first set of assignments there is no history to include
  else {
    ldf_hist = ldfss
  }

  return(ldf_hist)
}

# Extract single exercise information but restore original column names before returning.
# Remove unnecessary features and add computed ones.
get_subset_hist <- function(ldatf, subsetStr) {
  # Select single assignment columns
  ldfss <- ldatf[, grepl(subsetStr, names(ldatf))]
  
  # Remove 'WORKED_ON_*', 'SUBMITTED_*' and 'EDUCATIONA_VALUE_*' columns
  ldfss <- ldfss[, !grepl('WORKED|SUBMITTED|EDUCATIONAL', names(ldfss))]
  
  # Store original column names
  org_col_names <- colnames(ldfss)
  
  # Rename columns
  colnames(ldfss) <- c('SECONDS_SPENT','COMPILES1','DIFFICULTY','STROKES','SECONDS_IN_COMPILING','SECONDS_IN_NON_COMPILING_STATE', 'LOC', 'FOC')
  
  # Add calculated entries
  ldfss$COMPILES2 <- ldfss$SECONDS_IN_COMPILING/(ldfss$SECONDS_IN_COMPILING + ldfss$SECONDS_IN_NON_COMPILING)
  ldfss$TIME <- ldfss$SECONDS_IN_COMPILING + ldfss$SECONDS_IN_NON_COMPILING
  
  # Replace with original column names with newly added
  compiles2_col <- gen_name(org_col_names, 'COMPILES2', subsetStr)
  time_col <- gen_name(org_col_names, 'TIME', subsetStr)
  colnames(ldfss) <- c(org_col_names, compiles2_col, time_col)
  
  # Remove no longer needed columns
  ldfss <- ldfss[, !grepl('SECONDS', names(ldfss))]
  
  return(ldfss)
}

# Perform 10-fold cross-validation on model and get its predictions for original and imputed data
# If usedIndices is given utilize stored models instead
get_stats_imputed <- function(ldfss, ldfssi, exIndex, usedIndices = NULL) {
  # Ensure that difficulty is factor due to being used in classification
  ldfss$DIFFICULTY <- as.factor(ldfss$DIFFICULTY)
  ldfssi$DIFFICULTY <- as.factor(ldfssi$DIFFICULTY)
  
  # Check if indices already exists
  if(is.null(usedIndices)) {
    # Split data into train (80%) and validation (20%) sets
    ftrain <- createDataPartition(y=ldfss$DIFFICULTY, p=0.8, list=FALSE, groups = min(1, length(ldfss$DIFFICULTY)))
  } else {
    # Use previously selected indices instead
    ftrain <- usedIndices[exIndex, ]
    ftrain <- ftrain[ftrain > 0]
  }
  trainSet <- ldfss[ftrain,]
  testSet <- na.omit(ldfss[-ftrain,])
  
  # Store selected training set indices
  trainingSetIndices[exIndex, 1:length(ftrain)] <<- ftrain
  
  # Create training dataset from imputed data by removing validation dataset items based on their row names
  validationRowNames <- as.numeric(rownames(testSet))
  itrainSet <- ldfssi[!rownames(ldfssi) %in% as.numeric(validationRowNames),]
  
  if(is.null(usedIndices)) {  
    # Following utilizes 10-fold cross-validation on RF
    rf_ctrl <- trainControl(method='cv', number = 10)
    mfit <- train(DIFFICULTY ~ ., data = trainSet, method = 'rf', trControl = rf_ctrl, metric='Kappa', na.action = na.omit)
    imfit <- train(DIFFICULTY ~ ., data = itrainSet, method = 'rf', trControl = rf_ctrl, metric='Kappa', na.action = na.omit)
    
    # Store models
    saveRDS(mfit, sprintf(paste0(ewhmodelpath, 'ewh_mdl_%d.rds'), exIndex))
    saveRDS(imfit, sprintf(paste0(ewhmodelpath, 'ewh_mdli_%d.rds'), exIndex))
  } else {
    # Load earlier models
    mfit <- readRDS(sprintf(paste0(ewhmodelpath, 'ewh_mdl_%d.rds'), exIndex))
    imfit <- readRDS(sprintf(paste0(ewhmodelpath, 'ewh_mdli_%d.rds'), exIndex))
  }
  # Make predictions
  preds1 <- predict(mfit, na.omit(testSet), type='raw')
  preds2 <- predict(imfit, na.omit(testSet), type='raw')
  
  # Get confusion matrices
  cm1 <- confusionMatrix(preds1, testSet$DIFFICULTY)
  cm2 <- confusionMatrix(preds2, testSet$DIFFICULTY)
  
  # Calculate F1 scores
  mdlsf1[exIndex, 1] <<- f1_score(preds1, testSet$DIFFICULTY)
  mdlsf1[exIndex, 2] <<- f1_score(preds2, testSet$DIFFICULTY)
  
  if(is.null(usedIndices)) {  
    # Store confusion matrices
    saveRDS(cm1, sprintf(paste0(ewhmodelpath, 'ewh_mdl_%d_cm.rds'), exIndex))
    saveRDS(cm2, sprintf(paste0(ewhmodelpath, 'ewh_mdli_%d_cm.rds'), exIndex))
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
#ewhmind <- readRDS(paste0(ewhmodelpath, 'ewh_training_indices.rds'))

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
      
      # Add history to assignment
      dfssh <- create_history(datf, dfss, ai, ais)
      dfssih <- create_history(datfimp, dfssi, ai, ais)

      if(checkCats(dfssh)) {
        get_stats_imputed(dfssh, dfssih, cmsIndex)
        # Comment line above and uncomment this one if using previously created indices
        #get_stats_imputed(dfssh, dfssih, cmsIndex, ewhmind)
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

    # Add history to assignment
    dfssh <- create_history(datf, dfss, ai, 0)
    dfssih <- create_history(datfimp, dfssi, ai, 0)

    if(checkCats(dfssh)) {
      get_stats_imputed(dfssh, dfssih, cmsIndex)
      # Comment line above and uncomment this one if using previously created indices
      #get_stats_imputed(dfssh, dfssih, cmsIndex, ewhmind)
    }
    cmsIndex = cmsIndex + 1
  }
  # Idea here is that as a model training takes some time then storing these bits of data
  # here allows us to restart this loop from any assignment and continue until end in case
  # process needs to be or gets interrupted for some reason. Just change for loop starting
  # index from 1 to wanted. Do note that numbering changes after 71 since there is no exercise
  # 72 but instead 72.1, 72.2 & 72.3, thus 72 = 72.1, 73 = 72.2, 74 = 72.3, 75 = 73, 76 = 74 and so on.
  
  # Store training set indices and models F1 & MCC
  saveRDS(trainingSetIndices, paste0(ewhmodelpath, 'ewh_training_indices.rds'))
  saveRDS(mdlsf1, paste0(ewhmodelpath, 'ewh_mdls_f1.rds'))
  saveRDS(mdlsmcc, paste0(ewhmodelpath, 'ewh_mdls_mcc.rds'))
}

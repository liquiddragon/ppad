# Graph in figure 4.2
#
# Before using this script to produce graphs you must have created imputed
# data using appropriate Python script.
##############################################################################

# 1-71, 72.1-72.3, 73-106 & 108, 107 = feedback (omitted)
exercise_count <- 109
exercise_max_index <- 108
exercise_feedback_index <- 107

# Read dataset (this is the original data file name)
datf <- read.csv('sigite2014-difficulty-data.csv', header=T, sep=";")
# Read imputed dataset (this is one produced by Python script)
datfimp <- read.csv('imputed_data.csv', header=T, sep=',')

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

# Obtain number of entries in each exercise for original and imputed data
entriesPerExercise <- function(ldatf, ldatfi) {
  lexentries <- array(dim=(exercise_count))
  liexentries <- array(dim=(exercise_count))
  exIndex = 1
  for(ai in 1:exercise_max_index) {
    if (ai == exercise_feedback_index) next
    if(ai == 72) {
      for(ais in 1:3) {
        subsetID <- formatC(ai, width=3, format='d', flag='0')
        subsetID <- sprintf('%s.%d', subsetID, ais)
        dfss <- get_subset(ldatf, subsetID)
        dfssi <- get_subset(ldatfi, subsetID)
        lexentries[exIndex] <- nrow(dfss)
        liexentries[exIndex] <- nrow(dfssi)
      }
    }
    else {
      subsetID <- formatC(ai, width=3, format='d', flag='0')
      dfss <- get_subset(ldatf, subsetID)
      dfssi <- get_subset(ldatfi, subsetID)
      lexentries[exIndex] <- nrow(dfss)
      liexentries[exIndex] <- nrow(dfssi)
    }
    exIndex <- exIndex + 1
  }
  
  retVals <- list('ex' = lexentries, 'exi' = liexentries)
  return(retVals)
}

# Plot assignment counts graph
# Input:
#  exo: entries per exercise using original data
#  exi: entries per exercise using imputed data
#  Following affects only for saved graph and can be set to NULL if save-parameter is false
#  fname: file name
#  imgwidth: created graph width
#  imgheight: create graph height
#  save: true = save the graph, false = display it (this relies on the RStudio or the like)
plot_entry_counts <- function(exo, exi, fname, imgwidth, imgheight, save=TRUE) {
  if(save) png(fname, width = imgwidth, height = imgheight)
  
  lmts <- range(exo, exi, na.rm = T)
  plot(exi, main = "Number of submissions per assignment", xlab = "Assignment #", ylab = "Available submission counts", col='darkorange', pch=20, ylim = c(40, 400))
  points(exo, col='blue', pch=5)
  legend('topright', legend = c('Imputed data', 'Original data'), col = c('darkorange', 'blue'), pch = c(20, 5), bg='ivory', cex=0.75)
  
  if(save) dev.off()
}

# Get number of entries and create plot
ecnt <- entriesPerExercise(datf, datfimp)
plot_entry_counts(ecnt[['ex']], ecnt[['exi']], 'figs/assignment_counts.png', 450, 450)

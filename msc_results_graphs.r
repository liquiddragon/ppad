# Graphs for results section except for Ctree
#
# Before using this script to produce graphs you must have executed scripts
# that will create every models confusion matrices and MCC scores.
##############################################################################

# 1-71, 72.1-72.3, 73-106 & 108, 107 = feedback (omitted)
exercise_count <- 109

# Read confusion matrices into a list of lists
# Input:
#  mdlpath: path to directory containing the models
#  mdlpattern: a model file pattern in which number (%d) is replaced with number
#    e.g. x_mdl_%d_cm.rds
read_cms <- function(mdlpath, mdlpattern) {
  mdlcms <- list()
  for(i in 1:exercise_count) {
    mdlfile <- paste0(mdlpath, sprintf(mdlpattern, i))
    
    # If model file exists add it to the list, otheriwse indicate it is missing
    if(file.exists(mdlfile)) {
      cm <- readRDS(mdlfile)
    }
    else {
      cm <- NULL
    }
    mdlcms[[i]] <- cm
  }
  return(mdlcms)
}

# For more information of this see following blog post:
# https://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
# Input: actual & predicted vectors or actual vs predicted confusion matrix
# Some of these can be found from Caret confusionMatrix results also
cm_metrics <- function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    naVals = union(which(is.na(actual)), which(is.na(predicted)))
    if(length(naVals) > 0) {
      actual = actual[-naVals]
      predicted = predicted[-naVals]
    }
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  
  #accuracy
  accuracy = sum(diag) / n
  
  #per class prf
  recall = diag / rowsums
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  
  #macro prf
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  
  #1-vs-all matrix
  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})
  
  s = matrix(0, nrow=2, ncol=2)
  for(i in 1:nc){s=s+oneVsAll[[i]]}
  
  #avg accuracy
  avgAccuracy = sum(diag(s))/sum(s)
  
  #micro prf
  microPrf = (diag(s) / apply(s,1, sum))[1];
  
  #majority class
  mcIndex = which(rowsums==max(rowsums))[1] # majority-class index
  mcAccuracy = as.numeric(p[mcIndex]) 
  mcRecall = 0*p;  mcRecall[mcIndex] = 1
  mcPrecision = 0*p; mcPrecision[mcIndex] = p[mcIndex]
  mcF1 = 0*p; mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] + 1)
  
  #random/expected accuracy
  expAccuracy = sum(p*q)
  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  
  #random guess
  rgAccuracy = 1 / nc
  rgPrecision = p
  rgRecall = 0*p + 1 / nc
  rgF1 = 2 * p / (nc * p + 1)
  
  #random weighted guess
  rwgAccurcy = sum(p^2)
  rwgPrecision = p
  rwgRecall = p
  rwgF1 = p
  
  classNames = names(diag)
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  
  metrics = rbind(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    MacroAvgPrecision = macroPrecision,
    MacroAvgRecall = macroRecall,
    MacroAvgF1 = macroF1,
    AvgAccuracy = avgAccuracy,
    MicroAvgPrecision = microPrf,
    MicroAvgRecall = microPrf,
    MicroAvgF1 = microPrf,
    MajorityClassAccuracy = mcAccuracy,
    MajorityClassPrecision = mcPrecision,
    MajorityClassRecall = mcRecall,
    MajorityClassF1 = mcF1,
    Kappa = kappa,
    RandomGuessAccuracy = rgAccuracy,
    RandomGuessPrecision = rgPrecision,
    RandomGuessRecall = rgRecall,
    RandomGuessF1 = rgF1,
    RandomWeightedGuessAccuracy = rwgAccurcy,
    RandomWeightedGuessPrecision = rwgPrecision,
    RandomWeightedGuessRecall = rwgRecall,
    RandomWeightedGuessF1 = rwgF1)
  
  colnames(metrics) = classNames
  
  # Return results
  return(metrics)
}

# Create a list containing all single model metrics
# Input:
#  mdlcm: List as outputted by read_cms function
set_metrics <- function(mdlcm) {
  mtrs <- list()
  for(i in 1:length(mdlcm)) {
    # Include only models that data exists
    if(!is.null(mdlcm[[i]])) {
      mtrs[[i]] <- cm_metrics(cm = t(mdlcm[[i]]$table))
    }
  }
  return(mtrs)
}

# Plot models accuracy graphs
# Input:
#  mdlmtrs: model metrics as output by set_metrics function using original data
#  mdlimtrs: model metrics as output by set_metrics function using imputed data
#  ptitle: graph title
#  Following affects only for saved graph and can be set to NULL if save-parameter is false
#  fname: file name prefix
#  imgwidth: created graph width
#  imgheight: create graph height
#  save: true = save the graph, false = display it (this relies on the RStudio or the like)
plot_accuracy_from_metrics <- function(mdlmtrs, mdlimtrs, ptitle, fname, imgwidth, imgheight, save=TRUE) {
  acco <- array(0, dim=c(exercise_count, 4))
  acci <- array(0, dim=c(exercise_count, 4))
  # Store results in consecutive numbers (used assignments in the paper)
  fidx <- 1
  # Store selected metrics into two-dimensional array
  for(i in 1:length(mdlmtrs)) {
    if(!is.null(mdlmtrs[[i]])) {
      acco[fidx, 1] <- mdlmtrs[[i]]['Accuracy',1]
      acco[fidx, 2] <- mdlmtrs[[i]]['AvgAccuracy',1]
      acco[fidx, 4] <- mdlmtrs[[i]]['RandomGuessAccuracy',1]
      acco[fidx, 3] <- mdlmtrs[[i]]['MajorityClassAccuracy',1]
    }
    if(!is.null(mdlimtrs[[i]])) {
      acci[fidx, 1] <- mdlimtrs[[i]]['Accuracy',1]
      acci[fidx, 2] <- mdlimtrs[[i]]['AvgAccuracy',1]
      acci[fidx, 4] <- mdlimtrs[[i]]['RandomGuessAccuracy',1]
      acci[fidx, 3] <- mdlimtrs[[i]]['MajorityClassAccuracy',1]
      fidx <- fidx + 1
    }
  }
  acco[acco == 0] <- NA
  acci[acci == 0] <- NA
  
  # Set parameters for saved graph
  # NOTE: This graph is not used in the paper, thus code is commented out
  #if(save) png(paste0(fname, '_1.png'), width = imgwidth, height = imgheight)
  
  # Plot accuracies
  #plot(acco[,1], type='l', main = ptitle, xlab = "Used assignments", ylab = "Accuray", ylim = c(0, 1), xlim = c(1, fidx - 1), col = 'lightblue', pch=15)
  #points(acci[,1], col = 'orange', pch = 4)
  # Average accuracy
  #lines(acco[,2], col = 'blue', pch = 15)
  #points(acci[,2], col = 'darkorange', pch = 1, lwd = 2 )
  # Random guess accuracy
  #lines(acco[,4], col = 'black', pch = 3)
  #legend('bottomright', legend = c('Imputed', 'Original', 'Imputed average', 'Original average', 'Random guess'), col = c('orange', 'lightblue', 'darkorange', 'blue', 'black'), pch = c(4, 15, 1, 15, 45), bg='ivory', ncol=3)
  
  #if(save) dev.off()
  
  # Set parameters for saved graph
  # In the paper figures 5.4, 5.6 and 5.8 are results of this
  if(save) png(paste0(fname, '_2.png'), width = imgwidth, height = imgheight)
  
  # Plot accuracies
  plot(acco[,3], type='l', main = ptitle, xlab = "Used assignments", ylab = "Accuray", ylim = c(0, 1), xlim = c(1, fidx - 1), col = 'lightblue', pch=15)
  points(acci[,3], col = 'orange', pch = 4)
  
  # Average accuracy
  lines(acco[,2], col = 'blue', pch = 15)
  points(acci[,2], col = 'darkorange', pch = 1, lwd = 2 )
  
  # Random guess accuracy
  lines(acco[,4], col = 'black', pch = 3)
  
  legend('bottomright', legend = c('Imputed majority class', 'Original majority class', 'Imputed average', 'Original average', 'Random guess'), col = c('orange', 'lightblue', 'darkorange', 'blue', 'black'), pch = c(4, 15, 1, 15, 45), bg='ivory', ncol=3)
  
  if(save) dev.off()
}

# Plot models combined summary boxplot from metrics
# Input:
#  mdlmtrs: model metrics as output by set_metrics function using original data
#  mdlimtrs: model metrics as output by set_metrics function using imputed data
#  lmcc: model MCCs as output by corresponding model script
#  ptitle: graph title
#  Following affects only for saved graph and can be set to NULL if save-parameter is false
#  fname: file name
#  imgwidth: created graph width
#  imgheight: create graph height
#  save: true = save the graph, false = display it (this relies on the RStudio or the like)
plot_combo_boxplot_from_metrics <- function(mdlmtrs, mdlimtrs, lmcc, ptitle, fname, imgwidth, imgheight, save=TRUE) {
  cmbo <- array(NA, dim=c(exercise_count, 7))
  cmbi <- array(NA, dim=c(exercise_count, 7))
  # Store results in consecutive numbers (used assignments in the paper)
  fidx <- 1
  # Store selected metrics into two-dimensional array
  for(i in 1:length(mdlmtrs)) {
    if(!is.null(mdlmtrs[[i]])) {
      cmbo[fidx, 1] <- mdlmtrs[[i]]['AvgAccuracy',1]
      cmbo[fidx, 2] <- mdlmtrs[[i]]['MajorityClassAccuracy',1]
      cmbo[fidx, 3] <- mdlmtrs[[i]]['MicroAvgF1',1]
      cmbo[fidx, 4] <- mdlmtrs[[i]]['MajorityClassF1',1]
      cmbo[fidx, 5] <- mdlmtrs[[i]]['RandomGuessF1',1]
    }
    if(!is.null(mdlimtrs[[i]])) {
      cmbi[fidx, 1] <- mdlimtrs[[i]]['AvgAccuracy',1]
      cmbi[fidx, 2] <- mdlimtrs[[i]]['MajorityClassAccuracy',1]
      cmbi[fidx, 3] <- mdlimtrs[[i]]['MicroAvgF1',1]
      cmbi[fidx, 4] <- mdlimtrs[[i]]['MajorityClassF1',1]
      cmbi[fidx, 5] <- mdlimtrs[[i]]['RandomGuessF1',1]
      fidx <- fidx + 1
    }
  }
  fidx <- 1
  # Add MCC values to the array
  for(i in 1:length(mdlmtrs)) {
    if(!is.null(mdlmtrs[[i]])) {
      cmbo[fidx, 6] <- lmcc[i,1]
    }
    if(!is.null(mdlimtrs[[i]])) {
      cmbi[fidx, 6] <- lmcc[i,2]
      fidx <- fidx + 1
    }
  }
  
  # Set ranges for all the boxplots
  ylimits = c(-0.4, 1.0)
  
  # Set parameters for saved graph
  if(save) png(fname, width = imgwidth, height = imgheight)

  # Created combined boxplot as in figures 5.5, 5.7 and 5.9 in the paper  
  boxplot(cmbo[,1], cmbi[,1], cmbo[,2], cmbi[,2], cmbo[,3], cmbi[,3], cmbo[,4], cmbi[,4], cmbo[,5], cmbi[,5], cmbo[,6], cmbi[,6],
          ylim=ylimits, col=c('blue', 'darkorange'), main=ptitle,
          boxwex=0.2, cex.axis = 0.7, xlab='Metric pairs',
          names=c('AAO', 'AAI', 'MCAO', 'MCAI', 'MAF1O', 'MAF1I', 'MCF1O', 'MCF1I', 'RGF1O', 'RGF1I', 'MCCO', 'MCCI'))
  legend('bottom', col=c('blue', 'darkorange'), legend=c('Original', 'Imputed'), bg='ivory', pch=c(19,19), ncol=2)#, cex = 0.6)

  # Get Tukey's five number summaries for the boxplots
  valabs_avgo <- c(round(fivenum(cmbo[,1])[2], digits = 2), round(fivenum(cmbo[,1])[3], digits = 2), round(fivenum(cmbo[,1])[4], digits = 2))
  valabs_avgi <- c(round(fivenum(cmbi[,1])[2], digits = 2), round(fivenum(cmbi[,1])[3], digits = 2), round(fivenum(cmbi[,1])[4], digits = 2))
  valabs_amo <- c(round(fivenum(cmbo[,2])[2], digits = 2), round(fivenum(cmbo[,2])[3], digits = 2), round(fivenum(cmbo[,2])[4], digits = 2))
  valabs_ami <- c(round(fivenum(cmbi[,2])[2], digits = 2), round(fivenum(cmbi[,2])[3], digits = 2), round(fivenum(cmbi[,2])[4], digits = 2))
  valabs_mf1o <- c(round(fivenum(cmbo[,3])[2], digits = 2), round(fivenum(cmbo[,3])[3], digits = 2), round(fivenum(cmbo[,3])[4], digits = 2))
  valabs_mf1i <- c(round(fivenum(cmbi[,3])[2], digits = 2), round(fivenum(cmbi[,3])[3], digits = 2), round(fivenum(cmbi[,3])[4], digits = 2))
  valabs_mcf1o <- c(round(fivenum(cmbo[,4])[2], digits = 2), round(fivenum(cmbo[,4])[3], digits = 2), round(fivenum(cmbo[,4])[4], digits = 2))
  valabs_mcf1i <- c(round(fivenum(cmbi[,4])[2], digits = 2), round(fivenum(cmbi[,4])[3], digits = 2), round(fivenum(cmbi[,4])[4], digits = 2))
  valabs_rf1o <- c(round(fivenum(cmbo[,5])[2], digits = 2), round(fivenum(cmbo[,5])[3], digits = 2), round(fivenum(cmbo[,5])[4], digits = 2))
  valabs_rf1i <- c(round(fivenum(cmbi[,5])[2], digits = 2), round(fivenum(cmbi[,5])[3], digits = 2), round(fivenum(cmbi[,5])[4], digits = 2))
  valabs_mccf1o <- c(round(fivenum(cmbo[,6])[2], digits = 2), round(fivenum(cmbo[,6])[3], digits = 2), round(fivenum(cmbo[,6])[4], digits = 2))
  valabs_mccf1i <- c(round(fivenum(cmbi[,6])[2], digits = 2), round(fivenum(cmbi[,6])[3], digits = 2), round(fivenum(cmbi[,6])[4], digits = 2))
  # Add lower-hinge, median and upper-hinge values to the graph
  text(x=c(0.7,1.3,0.7), y=valabs_avgo, labels=valabs_avgo, cex = 0.6)
  text(x=c(2.3,1.7,2.3), y=valabs_avgi, labels=valabs_avgi, cex = 0.6)
  text(x=c(2.7,3.3,2.7), y=valabs_amo, labels=valabs_amo, cex = 0.6)
  text(x=c(4.3,3.7,4.3), y=valabs_ami, labels=valabs_ami, cex = 0.6)
  text(x=c(4.7,5.3,4.7), y=valabs_mf1o, labels=valabs_mf1o, cex = 0.6)
  text(x=c(6.3,5.7,6.3), y=valabs_mf1i, labels=valabs_mf1i, cex = 0.6)
  text(x=c(6.7,7.3,6.7), y=valabs_mcf1o, labels=valabs_mcf1o, cex = 0.6)
  text(x=c(8.3,7.7,8.3), y=valabs_mcf1i, labels=valabs_mcf1i, cex = 0.6)
  text(x=c(8.7,9.3,8.7), y=valabs_rf1o, labels=valabs_rf1o, cex = 0.6)
  text(x=c(10.3,9.7,10.3), y=valabs_rf1i, labels=valabs_rf1i, cex = 0.6)
  text(x=c(10.7,11.3,10.7), y=valabs_mccf1o, labels=valabs_mccf1o, cex = 0.6)
  text(x=c(12.3,11.7,12.3), y=valabs_mccf1i, labels=valabs_mccf1i, cex = 0.6)

  if(save) dev.off()
}

# Plot combined models accuracies
# Input:
#  mdlcmtrs: course model metrics as output by set_metrics function
#  mdlemtrs: exercise model metrics as output by set_metrics function
#  mdlewhmtrs: exercise model with history metrics as output by set_metrics function
#  lcmcc: course model MCCs as output by corresponding model script
#  lemcc: exercise model MCCs as output by corresponding model script
#  lewhmcc: exercise model with history MCCs as output by corresponding model script
#  ptitle: graph title
#  Following affects only for saved graph and can be set to NULL if save-parameter is false
#  fname: file name prefix (suffix .png is added to it)
#  imgwidth: created graph width
#  imgheight: create graph height
#  save: true = save the graph, false = display it (this relies on the RStudio or the like)
plot_combo_acc_mcc_from_metrics <- function(mdlcmtrs, mdlemtrs, mdlewhmtrs, lcmcc, lemcc, lewhmcc, mccind, ptitle, fname, imgwidth, imgheight, save=TRUE) {
  cmb <- array(0, dim=c(exercise_count, 6))
  # Store results in consecutive numbers (used assignments in the paper)
  fidx <- 1
  # Store selected metrics into two-dimensional array
  for(i in 1:length(mdlcmtrs)) {
    if(!is.null(mdlcmtrs[[i]])) {
      cmb[fidx, 1] <- mdlcmtrs[[i]]['AvgAccuracy',1]
      cmb[fidx, 2] <- mdlemtrs[[i]]['AvgAccuracy',1]
      cmb[fidx, 3] <- mdlewhmtrs[[i]]['AvgAccuracy',1]
      fidx <- fidx + 1
    }
  }
  cmb[cmb == 0] <- NA
  fidx <- 1
  # Add MCC values to the array
  for(i in 1:length(mdlcmtrs)) {
    if(!is.null(mdlcmtrs[[i]])) {
      cmb[fidx, 4] <- lcmcc[i, mccind]
      cmb[fidx, 5] <- lemcc[i, mccind]
      cmb[fidx, 6] <- lewhmcc[i, mccind]
      fidx <- fidx + 1
    }
  }
  
  # Set parameters for saved graph
  if(save) png(paste0(fname, '.png'), width = imgwidth, height = imgheight)
  
  # Plot accuracies show in figures 5.10 and 5.11 in the paper
  # Course
  plot(cmb[,1], type='l', main = ptitle, xlab = "Used assignments", ylab = "Average accuray",
       ylim = c(-0.6, 1.0), xlim = c(1, fidx - 1), col = 'mediumblue', pch=15, lty=3, lwd=2)
  # Exercise
  lines(cmb[,2], col = 'chocolate2', pch = 16, lty=2, lwd=2)
  # Exercise with history
  lines(cmb[,3], col = 'darkgreen', pch = 18, lty=1)
  
  # Add MCCs to graph in same order than above
  lines(cmb[,4], col='dodgerblue2', lty=3, pch = 4, lwd=2, type = 'b')
  lines(cmb[,5], col='chocolate1', lty=2, pch = 0, lwd=2, type = 'b')
  lines(cmb[,6], col='forestgreen', lty=1, pch = 5, type = 'b')
  
  legend('bottomleft', legend = c('Course AA', 'Exercise AA', 'Exercise with history AA',
                                  'Course MCC', 'Exercise MCC', 'Exercise with history MCC'),
         col = c('mediumblue', 'chocolate2', 'darkgreen'), bg='ivory', ncol=2,
         lty = c(3,2,1,3,2,1), lwd=c(2,2,1,2,2,1), pch=c(46, 45, 46, 4, 0, 5)) #, cex=0.8)
  
  if(save) dev.off()
}

# Exercise model confusion matrices directory
emodelpath = 'models_e/'
# Original data
emodelcmo = 'e_mdl_%d_cm.rds'
emdlcmso <- read_cms(emodelpath, emodelcmo)
# Imputed data
emodelcmi = 'e_mdli_%d_cm.rds'
emdlcmsi <- read_cms(emodelpath, emodelcmi)

# Exercise model metrics for original and imputed data
emtrso <- set_metrics(emdlcmso)
emtrsi <- set_metrics(emdlcmsi)

# Exercise model MCCs from the corresponding model script
emcc <- readRDS(paste0(emodelpath,'e_mdls_mcc.rds'))

# Example call for printing the graph in e.g. RStudio environment without saving it
# plot_accuracy_from_metrics(mtrso, mtrsi, 'Exercise model accuracies', NULL, 0, 0, F)
# Produce exercise model graphs
plot_accuracy_from_metrics(emtrso, emtrsi, 'Exercise model accuracies', 'figs/ex_mdl_accuracies', 700, 350)
plot_combo_boxplot_from_metrics(emtrso, emtrsi, emcc, 'Exercise model metrics', 'figs/ex_mdl_summary.png', 700, 400)

# Exercise model with history confusion matrices directory
ewhmodelpath = 'models_ewh/'
# Original data
ewhmodelcmo = 'ewh_mdl_%d_cm.rds'
ewhmdlcmso <- read_cms(ewhmodelpath, ewhmodelcmo)
# Imputed data
ewhmodelcmi = 'ewh_mdli_%d_cm.rds'
ewhmdlcmsi <- read_cms(ewhmodelpath, ewhmodelcmi)

# Exercise model with history metrics for original and imputed data
ewhmtrso <- set_metrics(ewhmdlcmso)
ewhmtrsi <- set_metrics(ewhmdlcmsi)

# Exercise model with history MCCs from the corresponding model script
ewhmcc <- readRDS(paste0(ewhmodelpath,'ewh_mdls_mcc.rds'))

# Produce exercise model with history graphs
plot_accuracy_from_metrics(ewhmtrso, ewhmtrsi, 'Exercise model with history accuracies', 'figs/exwh_mdl_accuracies', 700, 350)
plot_combo_boxplot_from_metrics(ewhmtrso, ewhmtrsi, ewhmcc, 'Exercise model with history metrics', 'figs/exwh_mdl_summary.png', 700, 400)

# Course model confusion matrices directory
cmodelpath = 'models_c/'
# Original data
cmodelcmo = 'c_mdl_%d_cm.rds'
cmdlcmso <- read_cms(cmodelpath, cmodelcmo)
# Imputed data
cmodelcmi = 'c_mdli_%d_cm.rds'
cmdlcmsi <- read_cms(cmodelpath, cmodelcmi)

# Course model metrics for original and imputed data
cmtrso <- set_metrics(cmdlcmso)
cmtrsi <- set_metrics(cmdlcmsi)

# Course model MCCs from the corresponding model script
cmcc <- readRDS(paste0(cmodelpath,'c_mdls_mcc.rds'))

# Produce course model graphs
plot_accuracy_from_metrics(cmtrso, cmtrsi, 'Course model accuracies', 'figs/c_mdl_accuracies', 700, 350)
plot_combo_boxplot_from_metrics(cmtrso, cmtrsi, cmcc, 'Course model metrics', 'figs/c_mdl_summary.png', 700, 400)

# Produce combine graphs
plot_combo_acc_mcc_from_metrics(cmtrso, emtrso, ewhmtrso, cmcc, emcc, ewhmcc, 1, 'Model average accuracy and MCC on original data', 'figs/all_mdls_aa_mcc_org', 700, 400)
plot_combo_acc_mcc_from_metrics(cmtrsi, emtrsi, ewhmtrsi, cmcc, emcc, ewhmcc, 2, 'Model average accuracy and MCC on imputed data', 'figs/all_mdls_aa_mcc_imp', 700, 400)

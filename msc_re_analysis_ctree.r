# Utilize same ctree than in the original paper. Partykit contains a reimplementation of this.
library(party)

# Read dataset
datf <- read.csv('sigite2014-difficulty-data.csv', header=T, sep=";")

# Extract single exercise information. Remove unnecessary features and add computed ones.
get_subset <- function(datf, subsetStr) {
  # Select single assignment columns
  dfss <- datf[, grepl(subsetStr, names(datf))]

  # Remove 'WORKED_ON_*', 'SUBMITTED_*' and 'EDUCATIONA_VALUE_*' columns
  dfss <- dfss[, !grepl('WORKED|SUBMITTED|EDUCATIONAL', names(dfss))]
  
  # Use only complete cases, i.e. no missing values
  dfss_filtered <- dfss[complete.cases(dfss),]
  
  # Rename columns
  colnames(dfss_filtered) <- c('SECONDS_SPENT','COMPILES1','DIFFICULTY','STROKES','SECONDS_IN_COMPILING','SECONDS_IN_NON_COMPILING_STATE', 'LOC', 'FOC')
  
  # Add calculated entries
  dfss_filtered$COMPILES2 <- dfss_filtered$SECONDS_IN_COMPILING/(dfss_filtered$SECONDS_IN_COMPILING+dfss_filtered$SECONDS_IN_NON_COMPILING)
  dfss_filtered$TIME <- dfss_filtered$SECONDS_IN_COMPILING+dfss_filtered$SECONDS_IN_NON_COMPILING

  # Remove no longer needed columns
  dfss_filtered <- dfss_filtered[, !grepl('SECONDS', names(dfss_filtered))]

  return(dfss_filtered)  
}

# Collect exercise information in dataframe similarly to original and omit the first ten like done in original.
for(ai in 11:108) { # To include all change range from 1 to 108

  # Assignment #72 is an exception as its ID is in different format, 072.1
  if(ai == 72) {
    # There are three such sub IDs, so handle them within assignment #72 processing
    for(ais in 1:3) {
      subsetID <- formatC(ai, width=3, format='d', flag='0')
      subsetID <- sprintf('%s.%d', subsetID, ais)
      
      # Get assignment related data and combine them into one
      dfss_sub <- get_subset(datf, subsetID)
      
      if(ais == 1) {
        dfss <- dfss_sub
      } else {
        dfss <- rbind(dfss, dfss_sub)
      }
    }
  } else {
    # Get assignment ID in format XXX with zero padding in front, e.g. 057
    subsetID <- formatC(ai, width=3, format='d', flag='0')
    
    # Get assignment related data
    dfss <- get_subset(datf, subsetID)
  }
  
  # Combine assignment data to new dataframe
  if(exists('df_combined') == FALSE) {
    df_combined <- dfss
  } else {
    df_combined <- rbind(df_combined, dfss)
  }
}

# Remove unnecessary variables
rm(dfss, dfss_sub, ai, ais, subsetID)

# Run CTree on difficulty
dfct <- ctree(DIFFICULTY ~ ., data=df_combined)

# Create picture
png('figs/dctree_11-108.png', height=600, width=1400, res=80)
# Plot with customizations on panels; remove p-value and ID on nodes and set boxplot parameters (mainly width and remove ID) to mimick original graph
plot(dfct, inner_panel=node_inner(dfct, pval = FALSE, id = FALSE), terminal_panel = node_boxplot(dfct, col='black', fill='lightgray', width=0.2, id=FALSE))
dev.off()

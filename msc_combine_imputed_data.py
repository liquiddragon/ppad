# Combine imputed data CSV files into single CSV file for the R use.
import pandas as pd
import numpy as np

# Combine invidually imputed exercise files into single file
all_exercise_indices = list(range(1,72)) + [72.1, 72.2, 72.3] + list(range(73, 107)) + [108]
combined_df = None
for exercise_index in all_exercise_indices:
    exdf = pd.read_csv('imputed_{0}.csv'.format(exercise_index), sep=',', index_col=0)

    if combined_df is None:
        combined_df = exdf
    else:
        combined_df = combined_df.join(exdf)

combined_df.to_csv('imputed_data.csv', index=False)

# Predicting programming assignment difficulty related code

The paper associated with this data will be linked here once it is available.

## Software

Following software was used originally.

- Imputation: Python 3.5.6, Pandas 0.23.4, Numpy 1.15.4, SciPy 1.1.0, Scikit-learn 0.20.0 and Torch 0.4.1
- Models and figures: R version 3.4.4 and RStudio version 1.1.463
- OS: Ubuntu 14.04LTS with customizations by the university

## Usage

How to use and re-create obtained results as closely as possible. Why not exact? E.g. pseudo-random generator seed has not been fixed in order to prevent issues relating to it like by obtaining good split due to searched suitable value. This means that imputation will unlikely produce exact same results and similarly the models will differ somewhat from those that are used in the paper.

*Notes*:
- Programs expect to find input data files in the same directory than where they are executed.
- All output will be put in same directory than the program is except for
  - programs that create models store them in sub-directories called *models_c* for the course model, *models_e* for the exercise model and *models_ewh* for the exercise model with history, and
  - all figures are created in sub-directory called *figs*.
- Programs creating models and graphs expect to have imputed data file available.
- Re-analysis uses only original data CSV file.

I) Obtain original data from http://bit.ly/1oZnEKG
II) Extract sigite2014-difficulty-data.csv

Imputation
III) Execute msc_impute_difficulty.py
IV) Combine output files using msc_combine_imputed_data.py

Course model
V) Execute msc_course_model.r

Exercise model
VI) Execute msc_exercise_model.r

Exercise model with history
VII) Execute msc_exercise_model_with_history.r

Obtaining figures in results chapter
VIII) Execute msc_results_graphs.r

Others
For re-analysis part execute msc_re_analysis_ctree.r
For information on imputation and original data counts execute msc_number_of_submissions.r


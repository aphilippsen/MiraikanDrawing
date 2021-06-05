# MiraikanDrawing

Code for analysing children's drawings, replicating results for:

Philippsen, Tsuji, and Nagai:
"Quantifying developmental and individual differences of spontaneous drawing completion in children" (under review)

An OSF preregistration of this study has been performed for part 1 of the analysis, covering the definition of the hypotheses and the statistical tests regarding the change of drawing behavior with age and AQ score, see:
https://osf.io/tzwkv/?view_only=a37b7fbfcd8444bb9e991eccba874adb

## Data locations

All drawings (as listed in scripts/python/all-drawings.txt) have to be located in a folder data/all_drawings.
Download here:
https://drive.google.com/file/d/1iWXP4yQmBSyD9gMc2pwxcgEsp3PJUfzf/view?usp=sharing

(Note: This folder contains all children's drawings, except for drawings of children whose parents did not agree in the informed consent form to publish data anonymously on online platforms. Although raw data is not provided in these cases, the CNN features are included in the repository such that it is possible to replicate the paper results exactly.)

### Info about the participants (age, gender, AQ score, number of questions answered in the AQ score questionnaire, sub-scales of AQ score):

* only children:
data/participants-age-aq.txt

* children and adults:
data/participants-age-aq-incl-adults.txt
 
Child participants are labeled as P8...P121.
Adult participants are labeled as P01...P05.
(The age of adult participants is only a rough estimate, as the exact age in month is not used for the analysis.)

### Adult rating results:

Rating batches from Amazon Mechanical Turk can be found in: data/mturk/*.csv

The following batch results are used for the analysis:

#### Online ratings:
* data/mturk/Batch_3978316_batch_results.csv
* data/mturk/Batch_3980777_batch_results.csv
* data/mturk/Batch_3981674_batch_results.csv

#### Offline ratings:
* BGI_YYO_Batch_271874_batch_results.csv
* MEP_Batch_272379_batch_results.csv
* MGL_Batch_272377_batch_results.csv

Additionally, predefined values of 0 for all ratings are predefined for empty pictures, see:

* empty-pictures.csv

## Setup

* Results were generated using R 3.4.4 and Python 3.6.9 (Tensorflow 2.3)

* Depending on the environment, the installation works as follows:

* Optionally create/load virtual environment:
> `python3 -m venv venv`
>
> `source venv/bin/activate`

* Install requirements
  `pip install --upgrade pip`
  `pip install -r requirements.txt`

## Execute analyses

### Analysis part 1: Rating study analysis

`Rscript scripts/R/ReadDataMTurk.R`

* Creates variables such as `all_compl`, `all_scribble` etc. which contain all ratings for all images.

* Creates plots of completion/scribbling across age, AQ score etc. as `results/R_plot/compl-outer-age.pdf` etc. (If incorrect pdf files are generated, run the script content instead directly from the R command line.)

* Writes list of all drawings ranked higher than a certain rating score to `results/csv/highest_ranked_tracing-80.csv` etc.

> `source("scripts/R/MTurkStatistics.R')`

* Performs statistical tests for preregistered analyses.

### Analysis part 2: CNN analyses

#### Load (or compute) the activations of different network layers for the drawings:

Based on the full drawings
> `%run -i scripts/python/prepare_network_activations.py -- Full`

> `%run -i scripts/python/prepare_network_activations.py -- Delete`

Loads or stores activation data from/to:
`results/python_data/vgg19_features-False.npy` (including black/given part)
`results/python_data/vgg19_features-True.npy` (without black/given part)

####  How activations in different network layers change with the child's age

1. Run `scripts/python/prepare_network_activations.py` (with `Full` to keep the full drawings, including the presented part).
> `%run -i scripts/python/prepare_network_activations.py -- Full`

2. Run scripts/python/evaluate_activations.py.
> `%run -i scripts/python/evaluate_activations.py`

This script calculates the distances of all children's drawings to adult drawings (assuming that more adult-like drawings will have a smaller distance to adult drawings).
As a result, it generates a csv file for analysis in R: `results/csv/all-dist-to-adults.csv`

3. Run in R:
> `source('scripts/R/AnalyzeDevelopmentDistanceToAdults.R')`

Results will be plotted to: `results/R_plot/all-dists-inner-all-layers.pdf` etc.

4. Statistics test are also included in `AnalyzeDevelopmentDistanceToAdults.R`


#### Drawing styles: Quantify the distances of drawings of different styles to adult drawings in different layers of the network

1. CSV files which list the highest ranked pictures for each drawing style (according to MTurk ratings) are located in: `results/csv/highest_ranked_*`. The number 70, 80 or 90 is the percentage of the rating number that is used as a cut-off.

2. Run `scripts/python/prepare_network_activations.py` (with `Full` to keep the full drawings, including the presented part).

> `%run -i scripts/python/prepare_network_activations.py -- Full`

3. The python script `scripts/python/evaluate_drawing_styles.py` reads the CSV files and evaluates all drawings by comparing the activations to the activations of adult drawings of the same class/type.

> `%run -i scripts/python/evaluate_drawing_styles.py`

Results are written to `results/csv/drawing-style-data-*`. Run this script for multiple values of m to get different percentages of highly rated images.

4. Afterwards, statistics analysis can be performed using `scripts/R/AnalyzeDrawingStyle.R`. For the paper, significance scores were computed for cut-offs 60, 70 and 80 individually and only findings p<.01 are reported which are found in all three conditions.


#### Create the representational dissimilarity matrices (RDMs) for different age groups of children
1. Run `scripts/python/prepare_network_activations.py` (with argument Delete to delete the existing part of the drawing).

> `%run -i scripts/python/prepare_network_activations.py -- Delete`

2. Run `scripts/python/create_RDM.py` script to create the RDMs for different groups of children, equally distributed depending on age.

> `%run -i scripts/python/create_RDM.py`

Results are plotted to `results/python_plot/rdms-with-age-layer-6.pdf` etc.

3. `scripts/R/AnalyzeRDMvalues.R` performs the statistical test for the values of different age groups and adults.

#### Quantifying individual differences

The idea is to use the RDMs generated for individual children and analyse them according to the degree of diversity.

* Evaluating how children changed their style of drawing when the original/presented stimuli was modified.
1. Run `scripts/python/prepare_network_activations.py` (with argument Delete to delete the existing part of the drawing).

> `%run -i scripts/python/prepare_network_activations.py -- Delete`

2. Run script `scripts/python/create_RDM_per_child.py`:

> `%run -i scripts/python/create_RDM_per_child.py`

The resulting scores are written to: `results/csv/score-evaluation.csv`
And the RDMs are stored to the folder: `results/python_plot/RDMs_per_child`

3. To combine the calculated scores with the rating data, call `scripts/R/CombineIndividualScoreWithRatings.R`. This script reads the `score-evaluation.csv` and creates csv files `results/csv/score-vs-ratings-*.csv` for all layers.

#### Visualizing individual differences

By creating an interactive plot, different measures from the rating or the individual differences analysis per child can be compared to each other.

Run `scripts/python/visualize_individual_RDM_score.py`, and change the code accordingly for displaying other x/y axis information.
An interactive figure opens. When selecting points in the figure via mouse click, the child data corresponding to these values is copied in the main folder in the subfolder `pictureExplorer_current'.

(Note: No plots might be displayed in some cases if the child's parents did not give consent to publish the drawing data online.)


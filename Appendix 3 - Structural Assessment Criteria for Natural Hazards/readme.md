# Extreme Value Analysis Projection

GW's code, refactored by AW

* Input: this program takes in input data relating exceedances (hazard values) and their MRI (mean recurrence interval)
* Output: this tool -
  * Fits that data to an extreme value model
  * Simulates this model and calculates error bars (90% confidence interval)
  * Finds the projected exceedance values for desired MRI's.

The field of statistics that deals with the risk of extreme, rare events is EVA (extreme value analysis).

This is important for natural hazards like earthquakes, floods, and rain. Natural hazards like floods can be described as a "100-year flood", which has a 1% probability of occuring at least once each year but has a 26% probability of occuring at least once in the next fifty years. The MRI (mean return/recurrence interval) is the average or estimated time between events - in the above example, the MRI is 100 years.

Standards like ASCE7 and this program utilize GED models (generalized extreme value distribution). With these models, the severity and occurence data of natural hazards can be statistically described and predicted. It is possible to describe a relationship between the severity/exceedance of natural hazards and their MRI.

This program takes existing exceedance-MRI data and reverse-engineers the GED model to make projections/guesses for the exceedance-MRI relationship. The analysis includes visualization of a 90% confidence interval error bar and the reverse-engineered parameters that go into the GED model.

This program is easy and straightforward to use. All the hard work is done behind the scenes.

## dependencies

R package dependencies:
* here
* extRemes
* fExtremes
* dplyr
* HDInterval
* utils
* svDialogs
* readxl

## getting started

1. Download this program from github. Extract the project into your desired folder.
2. Place your input data into the "inputs/" folder (see example inputs in the "inputs/" folder to see how your file should be formatted.)
3. Setup your analysis parameters, run the program, save the output plot, and save the output chart.

There are two different ways to perform step 3.

* `PEMEPT-auto-script.R`. The automatic version uses a graphical user interface and operates similar to a computer program installation wizard. Simply run this file by sourcing it (click source in the top right or press ctrl+shift+s), then follow the instructions in the pop-up windows. Save the plot image and copy-paste the output chart if desired.
* `PEMEPT-manual-script.R`. The manual version uses a traditional script style. Change the `file.name`, `MRI.col`, and other analysis parameters in the script file. Then run the file and save your outputs like in the automatic version. There are comments in the script file to guide the setup of the analysis parameters.

## input data

The input data can be in the form of an .xlsx, .xls, or .csv file. Note that the excel formats will not work on a computer without Microsoft Excel. Look at the examples in the inputs/ folder.

The tabular data must have at least one column with MRI values and one column with exceedance or "y" values. If they are not named MRI and y, indicate which columns correspond to MRI and y (how this is done depends on which script you chose).

At least 3 data points is recommended - with only 2 data points, the program will assume a linear fit.

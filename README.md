# EMOTE Directory Summary

The following workbooks are available in the Emote directory:

Old Files Directory:

Old iterations of the models before we implemented our own versions of `RHRV`. This directory doesn't contain very useful workbooks but is there for consistency.

DataOutputs Directory:

This directory contains the datafiles that we output at various stages of the pipeline. Note that there is **no raw data** in this folder; all of the files are comma-separated files of outputs from the pipeline. The version with the most recent iterations of the pipeline is `data_out_20181113.csv` and should be used from here moving forward for all IBI analysis.

Workbooks Directory:

This directory documents the notebooks of our work in R. The most important files are:

* PipelineDemo.Rmd
* AnalysisOfPipeline.Rmd

In `PipelineDemo`, this file walks you through how we read in data, how we modify `RHRV` to be appropriate for our uses, and how we create the final loop that outputs the comma-separated files in `DataOutputs`. This is important for knowing how the input to machine learning and statistical models are produced. `AnalysisOfPipeline` is used to modify the loop with different parameters. Currently, it only has one parameter set, but can be modified as necessary. Some of the latest versions of notebooks are listed below:

* Time+Freq_20181203.Rmd shows the development of the machine learning model for both time and frequency domain
* FrequencyDomainML26112018.Rmd shows the most recent development of machine learning for the frequency domain
* Slimmed_models.Rmd shows the most recent development of machine learning for the time domain

EDA_Analysis Directory:

This directory contains documents for the current EDA efforts:

* `Data` Directory: a sample of raw data directly from the emote web download for testing
* `eda-explorer` Directory: a fork of the eda-explorer python module that has been adjusted to our needs
* `Notebooks` Directory: compilation of analyses of EDA
* `outputs` Directory: (currently) data outputs from eda analysis; at present, there is a subdirectory for each participant's eda analysis outputs (eda peak data sampled at 4hz for the 30 minute episodes and controls, as well as a csv for accelerometer features; these are joined together and saved as R objects in the .rds file)
* `scripts` Directory: a scratch directory
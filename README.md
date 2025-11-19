# APSIM Random-Forest Emulator & Figure Examples

This repository provides example R scripts for building a random-forest emulator of APSIM crop yield outputs and for generating diagnostic and impact figures. The script `rf_emulator_apsim.R` illustrates how to load APSIM simulation outputs, tune random-forest hyperparameters (e.g. `mtry`, `num.trees`) using out-of-bag RMSE, fit a final emulator, and apply it to new predictor inputs such as multi-GCM climate scenarios. The script `figure_plot.R` shows how to visualise emulator performance (tuning curves for R² and RMSE, APSIM vs emulator scatter plots, and variable-importance bar plots) and how to map spatial patterns and distributions of extreme climate indices and crop yield changes. These scripts are intended as examples; users should adapt file paths, variable names, and plotting options to their own projects.

APSIM output data are available at: https://zenodo.org/records/7444483  
Model input data are available from the ISIMIP portal: https://www.isimip.org/ or from the corresponding author.  
The GGCMI crop calendar is accessible at: https://doi.org/10.5281/zenodo.5062513  
An updated version of the crop-calendar model (R package) is available at: https://github.com/AgMIP-GGCMI/cropCalendars

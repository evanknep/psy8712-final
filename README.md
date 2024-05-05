This repository contains output that is reliant on running code in the correct order.

The R folder contains initial processing code that is necessary for later analyses. The initial_processing.R file will run analyses on data found in the data folder, which contains a single file for each individual with trial by trial task data. Running this file will output summary files to the out folder and the shiny folder, for the write-up and shiny implementation. It will also save the ML classification results to the out folder.

The out folder contains an .Rmd file that was used to create the final.docx output, and when knit will run on its own to recreate the output.

The interactive shiny web app can be found here: https://knepx001.shinyapps.io/psy8712-final/

An online web-binder of this project can be found here: 

## NWTKlearn README
##### Last updated AXL 20/09/2023

This is a README file documenting the NWTKlearn directory structure.

- `data` contains the raw form of the data, with a separate .csv file for each subject
- `data_tidy` contains the tidied form of the data, from various stages of the preprocessing and analysis scripts. 
- `experiment` contains the files needed to run both studies 1 and 2. To run both experiments, open the `study_1.html`/`study_2.html` files with a web browser (right click > open with > e.g., Google Chrome). The other four folders (`cues`, `jspsych-6.0.5`, `NWTKimg`, and `sounds`) must be placed in the same directory as the html files.
- `plots` contains the figures outputted by the analysis scripts. 
- `scripts` contains the R scripts needed to preprocess the data and run all analyses reported both in the main text of our manuscript, and the Supplementary Information.
  - Each study has a `process` script for preprocessing, and `analyse` script for analyses.
  - The `process` script for each study combines and cleans the raw data, outputting `s1_data_cleaned.csv` and `s2_data_cleaned.csv`. These files are then read in by the `analyse` scripts for analysis.
  - To run the scripts, first open the RStudio project `nwtklearn.Rproj`, then open the scripts inside the project session.


### Study 1 

An explanation of the `choice` variable in the data (after cleaning during preprocessing). Note that `choice == 1` means the target option was chosen.

| Choice Value      | Info Cond |  Option (stimulus)  | Outcome     |
| ----------------- | --------- | --------------------| ----------- |
| 0                 | noninfo   | square or triangle | optimal~120 |
| 1                 | noninfo   | square or triangle            | sub~80      |
| 0                 | info      | KIS                | optimal~120 |
| 1                 | info      | FON                | sub~80      |

### Study 2 

An explanation of the `choice` variable in the data (after cleaning during preprocessing). Note that `choice == 1` means the target option was chosen.

| Choice Value      | Info Cond |  Optim Cond  | Option (stimulus)  | Outcome     |
| ----------------- | --------- | ------------ |--------------------| ----------- |
| 0                 | noninfo   | sub          | square or triangle | optimal~120 |
| 1                 | noninfo   | sub          | square or triangle            | sub~80      |
| 0                 | info      | sub          | KIS                | optimal~120 |
| 1                 | info      | sub          | FON                | sub~80      |
| 0                 | noninfo   | eq           | square or triangle             | ~100        |
| 1                 | noninfo   | eq           | square or triangle             | ~100        |
| 0                 | info      | eq           | KIS                | ~100        |
| 1                 | info      | eq           | FON                | ~100        |
| 0                 | noninfo   | optim        | square or triangle             | sub~80      |
| 1                 | noninfo   | optim        | square or triangle             | optimal~120 |
| 0                 | info      | optim        | KIS                | sub~80      |
| 1                 | info      | optim        | FON                | optimal~120 |
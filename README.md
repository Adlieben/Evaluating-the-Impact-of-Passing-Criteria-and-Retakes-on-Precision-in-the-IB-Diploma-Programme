# Project

This is the code used to complete the project 'Evaluating the Impact of Passing Conditions and Retakes on Precision in the IB’s Diploma Programme', originally as part of a Master's thesis in joint collaboration between Utrecht University (UU) and the International Baccalaureate Organization (IBO).

The study aimed to evaluate the impact of several sets of passing criteria on the precision of the International Baccalaureate Diploma Programme (IBDP). To this end, true and observed scores within a Classical Test Theory (CTT) framework were simulated across 1\,000 iterations. The simulation was done in Fortran, whilst the storage and result manipulation was performed in R. At each iteration, precision, specificity, sensitivity, positive predictive value (PPV) and negative predictive value (NPV) were calculated. These were compared to each other to provide evidence for or against amending the current IB passing criteria.

Further information about the project is given in the full paper included in this folder ([Final_report](Final_report.pdf)). For questions regarding the code, please contact Adam Maghout (a.maghout@students.uu.nl) or raise an issue on this repository. This repository is made available via Github to any researcher and will not be taken down.

# Data

As part of the data privacy policy of the IBO, the data used for the project is not made available here, although it remains stored in the International Baccalaureate records. Instead, only the code in R and Fortran is provided for use in potential future studies. To request access to the data for reproducibility purposes, please contact Dr. Anton Béguin (anton.beguin@ibo.org), Director of Educational Innovation at the IBO. Requests will be reviewed on a case-by-case basis.

Requests should be made for the following datasets:

- **M23 component data**: The component scores for the May 2023 session. This should include 2\,743\,498 observations across 33 variables.
- **M23 component grade boundaries**: The component grade boundaries for the May 2023 session. This should include 14\,446 observations across 8 variables.
- **M23 item data**: The item scores for the May 2023 session. This should include 39\,521\,984 observations across 4 variables. Certain item scores are stored separately by the IB and are given in [A. Data/B. MCQ](A.%20Data/B.%20MCQ) folder.
- **M23 subject boundaries**: The subject grade boundaries for the May 2023 session. This should include 2\,887 observations across 5 variables.
- **Component key**: The key to the randomisation of component names conducted by Maghout to avoid mentioning component names within this open repository.
- **nl1**: The component scores for components nl373, nl908, nl860 and nl221, whose names are given in the component key. This should include 4 sheets, one per component, with 5\,027 observations across 80 variables,  6\,188 observations across 60 variables, 11\,622 observations across 80 variables and 8\,633 observations across 60 variables respectively.

Variable names and their descriptions are provided in the [A. Data](A.%20Data) folder. Once access to the datasets is granted, they should be placed in the same folder as their corresponding description files. To ensure compatibility with the analysis scripts, dataset filenames must match the names of their respective description files.

# Structure

The code is organised in several folders:
- **[A. Data](A.%20Data)**: A folder containing the data used for the study. Currently empty.
- **[B. Preparation](B.%20Preparation)**: A folder containing scripts in R used to obtain intermediate results, such as reliabilities and the increment distributions.
- **[C. Main Analysis](C.%20Main%20Analysis)**: A folder containg scripts in R and Fortran used to obtain the main results concerning precision and passing rates.
- **[D. Results](D.%20Results)**: A folder containing the results of the analysis. Results are given for each iteration so other plots and summary statistics can be computed.
- **[Final_report](Final_report.pdf)**: The final report that was submitted as a result of this study.

The original structure of the project is maintained with empty folders to facilitate its replication. Once the data files are obtained, they can thus simply be added to the data folder. 

# Step-by-step guide

# License

# Ethical approval

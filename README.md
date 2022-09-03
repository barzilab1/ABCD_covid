# ABCD_covid


#### This project uses the following ABCD instruments [version 4.0]:

1. pdem02
2. abcd_lpds01
3. pabcdcovid19questionnaire01
4. yabcdcovid19questionnaire01
5. acspsw03
6. abcd_ksad01
7. abcd_lt01



#### How to run the code:

1. update the [config.R](config.R) to reflect the location of the instruments above.
2. In the data-scripts folder, run scripts in any order. These scripts go over the abcd instruments and create new variables and datasets that are placed in the “outputs” folder.
3. Run the [merging_fin_disc.R](/scripts/merging_fin_disc.R) script to create the dataset.
4. Run the [descriptive_table1.Rmd](/scripts/descriptive_table1.Rmd) to generate table 1.
5. Run the [mixmodels.R](/script/mixmodels.R) for mixed models.
6. Run the [mediation_analysis.Rmd](/script/mediation_analysis.Rmd) for mediation analyses.
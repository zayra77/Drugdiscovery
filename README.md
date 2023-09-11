# Drugdiscovery
This ShinyApp could help researchers to obtain gene related drugs in CTRP database, which includes drug-response data along with response profiles for 481 compounds in 860 cell lines, as well as RNA-seq data. There are three types of analyses in this ShinyApp. To start an analysis, users have to submit an interested gene to ‘Gene Related Drugs’ module. This allows users to visualize the association between the expression of interested gene and drug response by volcano plot. 
“Expression” could then display the expression level for the submitted gene across all tumor cell lines in CTRP database. Distributions of the gene expression levels are displayed by box plot. All the gene expression values are log2 transformed. 
In “Fraction” page, users could visualize the proportion of responsive drugs by calculating the ratio of drugs with absolute correlation coefficient greater than 0.3 and p value less than 0.05 in different cell lines, and defined as resistant group (correlation coefficients >0.3) and sensitivity group (correlation coefficients< −0.3), respectively. Pyramid plot was then utilized to visualize the ratio distribution of related drugs between these two groups. 

Taken together, this server could aid researchers to find candidate drugs to regulate interested gene expression so that they could design an experiment to study its biological function in vitro. 


app.R is the main script, which you could simply import into R studio, and type "Run APP" to run it. 

volcanoplot.R is a function that was called by app.R

all the files in Data directory are used to generate valcanoplot.

Users could also go to the website https:/zzdlab.com:3838/Drugdiscovery to directly utilize this ShinyApp.

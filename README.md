# Drugdiscovery
This ShinyApp serves as a valuable tool for researchers seeking to access gene-related drug information within the Cancer Therapeutics Response Portal (CTRP) database. CTRP encompasses extensive drug-response data, including response profiles for 481 compounds across 860 cell lines, as well as RNA-seq data. The ShinyApp offers three distinct types of analyses, each providing valuable insights.

To initiate an analysis, users are required to submit a gene of interest to the 'Gene Related Drugs' module. This step allows users to visualize the relationship between the expression of the selected gene and drug response using a volcano plot.

The 'Expression' module provides users with the expression levels of the submitted gene across all tumor cell lines in the CTRP database. These gene expression distributions are effectively presented using box plots, with all gene expression values logarithmically transformed to the base 2 (log2).

Within the 'Fraction' page, users can gain insights into the proportion of responsive drugs by calculating the ratio of drugs with an absolute correlation coefficient exceeding 0.3 and a p-value less than 0.05 in various cell lines. These drugs are categorized into a 'resistant group' (correlation coefficients >0.3) and a 'sensitivity group' (correlation coefficients < −0.3). The module employs a Pyramid plot to visualize the ratio distribution of related drugs between these two groups.

In summary, this server serves as a valuable resource for researchers aiming to identify candidate drugs that can modulate the expression of their target genes. This, in turn, facilitates the design of in vitro experiments to study the biological functions of these genes.

To utilize this ShinyApp, simply import the 'app.R' main script into R Studio and click 'Run App.' Additionally, the 'volcanoplot.R' function, called by 'app.R,' and the files within the 'Data' directory, which are used to generate the volcano plot, are essential components of this resource. Alternatively, users can directly access the ShinyApp at https://zzdlab.com:3838/Drugdiscovery."

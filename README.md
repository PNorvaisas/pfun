# PFun
Collection of useful functions that I employ in data analysis
- adjust.contrast - Adjust contrasts when there is an interaction between group and H0: group_difference!=0
- adjustments - Calculates FDR for each contrast and adds p-Stars (will become a part of *getresults*)
- enrichment.melt - Creates table for "enrichment" without loosing grouping (will be deprecated)
- enrichment - Callculates enrichment using hypergeometric test
- getellipse - Generates SD ellipses
- getresults - Generates LM and Tukey-HSD results using *hypothesise2* output
- group.filter - Filters experimental groups in EdgeR
- hypthesise - Applies Tukey's HSD based on provided contrasts for experimental groups in LM fit object (uses *un-tidy* data, will be deprecated)
- hypthesise2 - Applies Tukey's HSD based on provided contrasts for experimental groups in LM fit object (implements tidyverse)
- makereplicates - Generate replicate indeces in the table (will be deprecated)
- MinMeanSDMax - Generates Min, Mean, SD, Max values of data vector for ggplot boxplots
- pStars - Generates clean p-Stars
- read.and.clean - Contrast table reader (will be deprecated)
- read.contrasts - Contrast table reader (will be deprecated)
- read.contrasts2 - Contrast table reader (implements tidyverse)
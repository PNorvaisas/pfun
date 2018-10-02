# PFun
Collection of useful functions employed in data analysis

- adjustments - Calculates FDR for each contrast and adds p-Stars
- contrast.adjust - Adjust contrasts when there is an interaction between group and H0: group_difference!=0
- clustorder  - Orders factor levels using hierarchical clustering
- covariation - Calculates covariation between contrasts and estimates this correlation linear regression parameters
- enrichment - Calculates enrichment using hypergeometric test
- getellipse - Generates SD ellipses
- getresults - Generates LM and Tukey-HSD results using *hypothesise* output
- group.filter - Filters experimental groups in EdgeR
- HMap - Draw heatmap using ggplot2
- lmtest - Quick way to perform 1-way/2-way ANOVA analysis
- hypothesise - Applies Tukey's HSD based on provided contrasts for experimental groups in LM fit object
- MinMeanSDMax - Generates Min, Mean, SD, Max values of data vector for ggplot boxplots
- multiplex - Generates multidimensional pair-wise comparison table
- pStars - Generates clean p-Stars
- PlotEnrichment - Plots enrichment heatmap
- PCAprep - PCA analysis results for standardised plotting
- pStars - Generates p-Stars
- read.contrasts - Contrast table reader
- theme_Heatmap.R - ggplot2 heatmap theme
- theme_PN - Custom ggplot2 theme
- theme_Publication - Custom publication friendly ggplot2 theme
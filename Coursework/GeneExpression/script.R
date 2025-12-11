########################################
#      Author: Jiaan Randhawa-Heer     #
#     Title: Gene Expression Report    #
#           Dataset number: 6          #
########################################

#install.packages ("readr") #installs package readr if required
#install.packages("dplyr") #installs package dplyr if required
#install.packages("tidyverse") #installs package tidyverse if required
#install.packages("ggplot2") #installs ggplot 2 if required

library(readr) #loads package
library(dplyr) #loads package
library(tidyverse) #loads package
library(ggplot2) #loads package

file_1 <- "A_vs_C.deseq2.results.tsv" #sets the path to the first tsv and gives it a value

file_2 <- "A_vs_E.deseq2.results.tsv" #sets the path to the second tsv and gives it a value

if (!file.exists(file_1)) {
  stop("file_1 not found, check the file path or file name") #checks file_1 exists and stops if it does not, producing an output message
}

if (!file.exists(file_2)) {
  stop("file_2 not found, check the file path or file name") #checks file_2 exists and stops if it does not, producing an output message
}

results_1 <- read.delim(file_1) #loads data into a table of results

results_2 <- read.delim(file_2) #loads data into a table of results

head(results_1) #checks the top of the dataframe to ensure it has loaded correctly

head(results_2) #checks the top of the dataframe to ensure it has loaded correctly

summary(results_1) #summary identifies if there are any NAs in padj to remove

summary(results_2) #summary identifies if there are any NAs in padj to remove

clean_results_1 <- results_1 |> dplyr::filter(!is.na(padj)) #Tidyverse approach to remove padj NA values and create cleaned data

clean_results_2 <- results_2 |> dplyr::filter(!is.na(padj)) #Tidyverse approach to remove padj NA values and create cleaned data

summary(clean_results_1) #summarises clean results to check removal success
summary(clean_results_2)

significant_genes_1 <- clean_results_1 |> 
  dplyr::filter(padj < 0.05) #tidyverse approach applies standard threshold to pajd

up_genes_1 <- significant_genes_1 |> 
  dplyr::filter(log2FoldChange > 0) |> nrow() #counts upregulated genes 

down_genes_1 <- significant_genes_1 |>
  dplyr::filter(log2FoldChange < 0) |> nrow() #counts downregulated genes

up_genes_1 #ouputs number of up regulated genes
down_genes_1 #outputs the number of down regulated genes

significant_genes_2 <- clean_results_2 |> 
  dplyr::filter(padj < 0.05) #applies standard pajd threshold

up_genes_2 <- significant_genes_2 |>
  dplyr::filter(log2FoldChange > 0) |> nrow() #counts upregulated genes

down_genes_2 <- significant_genes_2 |>
  dplyr::filter(log2FoldChange < 0) |> nrow() #counts downregulated genes

up_genes_2 #ouputs number of up regulated genes
down_genes_2 #outputs the number of down regulated genes

summary(clean_results_1$log2FoldChange) #summarises an output for log2FoldChange
summary(clean_results_1$pvalue) #summarises an ouutput for pvalues
summary(clean_results_1$padj) #summarises and output for padj

summary(clean_results_2$log2FoldChange) #summarises an output for log2FoldChange
summary(clean_results_2$pvalue) #summarises an ouutput for pvalues
summary(clean_results_2$padj) #summarises and output for padj


volcanoplot_results_1 <- clean_results_1 |> #creates new dataframe to be used in volcano plot
  mutate(neg_log10_padj = -log10(padj), #calculates -log10 for the y axis, which is adjusted p values
         gene_regulation = ifelse(         
           padj < 0.05 & log2FoldChange > 0, "UPREGULATED", #thresholds to determine upregulated genes
           ifelse(
             padj < 0.05 & log2FoldChange < 0, "DOWNREGULATED", #threshoholds to determine downregulated genes
             "NOT SIGNIFICANT")) #plots as not signifcant for genes outside thresholds
  )

ggplot(volcanoplot_results_1, aes(x = log2FoldChange, y = neg_log10_padj, #sets data for axis
                                  colour = gene_regulation)) + #determines colour by regulation classification
  geom_point(alpha = 0.5, size = 2) + #sets mid transparency and size for plots
  scale_color_manual(values = c("UPREGULATED" = "red", "DOWNREGULATED" = "blue", "NOT SIGNIFICANT" = "green")) + #colours classified gene regulations
  labs(title = "Volcano plot: A vs C (DESeq2 data)", x = "Log2 Fold Change", y = "-Log10 (P-value)") #titles the plot and axis

volcanoplot_results_2 <- clean_results_2 |> #creates new dataframe to be used in volcano plot
  mutate(neg_log10_padj = -log10(padj), #calculates -log10 for the y axis, which is adjusted p values
         gene_regulation = ifelse(         
           padj < 0.05 & log2FoldChange > 0, "UPREGULATED", #thresholds to determine upregulated genes
           ifelse(
             padj < 0.05 & log2FoldChange < 0, "DOWNREGULATED", #threshoholds to determine downregulated genes
             "NOT SIGNIFICANT")) #plots as not signifcant for genes outside thresholds
  )

ggplot(volcanoplot_results_2, aes(x = log2FoldChange, y = neg_log10_padj, #sets data for axis
                                  colour = gene_regulation)) + #determines colour by regulation classification
  geom_point(alpha = 0.5, size = 2) + #sets mid transparency and size for plots
  scale_color_manual(values = c("UPREGULATED" = "red", "DOWNREGULATED" = "blue", "NOT SIGNIFICANT" = "green")) + #colours classified gene regulations
  labs(title = "Volcano plot: A vs E (DESeq2 data)", x = "Log2 Fold Change", y = "-Log10 (P-value)") #titles the plot and axis

MAplot_results_1 <- clean_results_1 |> #creates a new dataframe to be used in the  MA plot
  mutate (log10_baseMean = log10(baseMean), #applies mean to log10 values
          significance_class = ifelse(padj < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT") #classifies genes by the threshold
  )

ggplot(MAplot_results_1, aes(x = log10_baseMean, y = log2FoldChange, #sets data for the axis
                             colour = significance_class)) + #determines colour by classification of genes
  geom_point(alpha = 0.5, size = 1.5) + #applies some transparency and sets size for plots
  scale_color_manual(values = c("SIGNIFICANT" = "red", "NOT SIGNIFICANT" = "green"))+ #sets colour differences for classified genes
  geom_hline(yintercept = 0, colour = "black")+ #function that produces a horiztonal line to show no fold change
  labs(title = "MA plot: A vs C (DESeq2 data)", x = "Log10 (mean expression)", y = "Log2 Fold Change") #titles the MA plot and labels the axis
MAplot_results_2 <- clean_results_2 |> #creates a new dataframe to be used in the  MA plot
  mutate (log10_baseMean = log10(baseMean), #applies mean to log10 values
          significance_class = ifelse(padj < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT") #classifies genes by the threshold
  )

ggplot(MAplot_results_2, aes(x = log10_baseMean, y = log2FoldChange, #sets data for the axis
                             colour = significance_class)) + #determines colour by classification of genes
  geom_point(alpha = 0.5, size = 1.5) + #applies some transparency and sets size for plots
  scale_color_manual(values = c("SIGNIFICANT" = "red", "NOT SIGNIFICANT" = "green"))+ #sets colour differences for classified genes
  geom_hline(yintercept = 0, colour = "black")+ #function that produces a horiztonal line to show no fold change
  labs(title = "MA plot: A vs E (DESeq2 data)", x = "Log10 (mean expression)", y = "Log2 Fold Change") #titles the MA plot and labels the axis
ggplot(clean_results_1, aes(x = pvalue))+ #selects data to be used, and axis input

geom_histogram(binwidth = 0.05, colour = "black", fill = "green")+ #selects size, colours lines black and makes bars green
labs(title = "Histogram: P-values of A vs C", x = "p-value", y = "Frequency") #titles the graph and labels the axis

ggplot(clean_results_2, aes(x = pvalue))+ #selects data to be used, and axis input
  geom_histogram(binwidth = 0.05, colour = "black", fill = "green")+ #selects size, colours lines black and makes bars green
  labs(title = "Histogram: P-values of A vs E", x = "p-value", y = "Frequency") #titles the graph and labels the axis

merged_clean_results <- clean_results_1 |> #merges clean data
  dplyr::select(gene_id, log2FoldChange, padj) |>  #selects relevent columns for the heatmap
  dplyr::rename(log2FoldChange_A_vs_C = log2FoldChange, padj_A_vs_C = padj) |> #renames columns so they are clearly linked with the data they are from
  dplyr::inner_join(clean_results_2 |>  #joins to A vs E data
                      dplyr::select(gene_id, log2FoldChange, padj) |>#selects relevent columns for the heatmap
                      dplyr::rename(log2FoldChange_A_vs_E = log2FoldChange, padj_A_vs_E = padj), #renames columns so they are clearly linked with the data they are from
                    by = "gene_id" ) #links the data by their shared gene IDs

merged_clean_results$min_padj <- pmin(merged_clean_results$padj_A_vs_C, merged_clean_results$padj_A_vs_E) #pmin function calculates smallest padj value for genes shared in both datasets being compared
top_genes <- merged_clean_results|>
  arrange(min_padj) |> #arrange used to order from most to least in significance
  slice(1:20) #keeps the top 20 genes for plotting in the heatmap

heatmap_merged_data <- top_genes |>
  dplyr::select(gene_id, log2FoldChange_A_vs_C, log2FoldChange_A_vs_E) #select is used to keep data for the columns

heatmap_long_data <- heatmap_merged_data |> #turns into long data for heatmap use
  pivot_longer(cols = c(log2FoldChange_A_vs_C, log2FoldChange_A_vs_E), #takes log2FoldChange columns from both data for long data
               names_to = "data", #makes a column for data set allocation
               values_to = "log2FoldChange" #makes a column for log2FoldChange values
  )

ggplot(heatmap_long_data, aes(x = data, y = gene_id, fill = log2FoldChange))+ #sets data to be used in the heatmap, axis data and data for the fill
  geom_tile(colour = "black")+ #colours tiles black
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", #sets a measumrent for colour where lowest to middle to highest goes from yellow, orange and red
                       midpoint = 0, #value for the middle of the colour gradient
                       name = "log2FoldChange")+ #provides a legend title for the colour gradient legend
  labs(title = "Heatmap: The Top Differentially Expressed Genes in A vs C and A vs E", #titles tbe heatmap
       x = "Data", y = "Gene ID") #titles the axis
up_regulated_AvC <- clean_results_1 |> #creates data for upregulated genes from clean A vs C data
  dplyr::filter(padj < 0.05 & log2FoldChange > 0) |> #applies threshold to keep statistically significant genes (for upregulation)
  dplyr::select(gene_id, log2FoldChange, pvalue, padj) |> #selects necessary data
  arrange(desc(log2FoldChange)) #orders by the most significant log2FoldChange

down_regulated_AvC <- clean_results_1 |> #creates data for downregulated genes from clean A vs C data
  dplyr::filter(padj < 0.05 & log2FoldChange < 0) |> #applies threshold to keep statistically signifant genes (for downregulation)
  dplyr::select(gene_id, log2FoldChange, pvalue, padj) |> #selects necessary data
  arrange(log2FoldChange) #orders so from most negative log2FoldChange to least negative

up_regulated_AvE <- clean_results_2 |> #creates data for upregulated genes from clean A vs C data
  dplyr::filter(padj < 0.05 & log2FoldChange > 0) |> #applies threshold to keep statistically significant genes (for upregulation)
  dplyr::select(gene_id, log2FoldChange, pvalue, padj) |> #selects necessary data
  arrange(desc(log2FoldChange)) #orders by the most significant log2FoldChange

down_regulated_AvE <- clean_results_2 |>  #creates data for downregulated genes from clean A vs C data
  dplyr::filter(padj < 0.05 & log2FoldChange < 0) |> #applies threshold to keep statistically signifant genes (for downregulation)
  dplyr::select(gene_id, log2FoldChange, pvalue, padj) |> #selects necessary data
  arrange(log2FoldChange) #orders so from most negative log2FoldChange to least negative

head(up_regulated_AvC) #outputs upregulated genes in A vs C
head(down_regulated_AvC) #outputs downregulated genes in A vs C
head(up_regulated_AvE) #outputs upregulated genes in A vs E
head(down_regulated_AvE) #outputs downregulated genes in A vs E
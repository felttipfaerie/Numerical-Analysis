library(psych)
data(swiss)

# Source: Bader and Leuzinger (2024, Chapter 8)

pairs.panels(swiss,
    method = "pearson", # correlation method
    hist.col = "grey", # colour of histograms
    density = TRUE,  # show density plots
    ellipses = F, scale = T, cex.cor = 2.5 # additional settings
             )


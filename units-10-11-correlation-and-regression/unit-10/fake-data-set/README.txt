SYNTHETIC PRACTICE DATA FOR R-TICULATE CHAPTER 9

These are generated substitutes, not recovered Wiley data or measured observations. Each CSV contains 100 rows. Keep this note with the CSVs.

leaf_respiration_1.csv: leafresp and temp. Preserves the 10 supplied respiration values and the first five supplied temperature values (using 18.18 and 19.51 from the later chapter excerpt). Calibrated to intercept -5.84233, slope 0.68390 and residual standard error 0.7729. Other summaries, individual predictions, residuals and point labels differ.

leaf_respiration_2.csv: leafresp2 and temp. Generated with constant noise in cube-root space to demonstrate increasing variance on the original scale and improvement after transformation. The Box-Cox optimum and printed head values will differ.

leaf_respiration_3.csv: leafresp, temp, sla and nitro. Nitrogen is almost a linear combination of the other predictors, demonstrating severe collinearity. Removing nitro gives stable coefficients. The main full and reduced model coefficients approximate the printed example. Interaction tests, selection results, VIF values, predictions and diagnostic plots will differ.

Unzip all files into one folder. In RStudio set that folder as the working directory, then follow the book or source("chapter9_practice.R"). Use ordinary quotes and ASCII ~ in copied R code; omit console > prompts.

The attached chapter excerpt supplied schemas and calibration targets. Random seed: 9717997. These data support practicing the methods; they cannot reproduce all book results or establish biological conclusions.

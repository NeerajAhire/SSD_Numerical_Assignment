
install.packages('readxl')
library(datasets)
library(readxl)
set.seed(123)

df_mtcars <- mtcars

data_overview <- function(df, dataset) {
 cat("Following is the result of the dataframe",dataset,":", "\n" )
 str(df)
 cat("Number of observations:", nrow(df), "\n")
 cat("Number of variables:", ncol(df), "\n")
}

data_overview(df_mtcars, "mtcars")

summary_stats <- function(df, variable_name) {
  
  # Calculating summary statistics
  mean <- mean(df)
  median <- median(df)
  sd <- sd(df)
  min <- min(df)
  max <- max(df)
  
  # Printing the results
  cat("Following are the summmary statistics of the variable",variable_name,":", "\n")
  cat("Mean:", mean, "\n")
  cat("Median:", median, "\n")
  cat("Standard Deviation:", sd, "\n")
  cat("Minimum:", min, "\n")
  cat("Maximum:", max, "\n")
}

summary_stats(df_mtcars$mpg, "mpg")



distribution_visualization <- function(df, variable_name, lab){
  hist(df, 
     main = paste("Histogram of", paste("'",variable_name, "'", sep = "")),
     xlab = lab, 
     col = "lightblue", 
     border = "black")

  boxplot(df, 
        main = paste("Boxplot of", paste("'",variable_name, "'", sep = "")), 
        ylab = lab,
        col = "lightgreen")
}

distribution_visualization(df_mtcars$mpg, "mpg", "Miles Per Gallon")




categorical_var_analysis <- function(df,variable_name){  
  chosen_categorical <- as.factor(df)
  
  # Creating a bar plot to visualize the distribution
  barplot(table(chosen_categorical), 
          main = paste("Bar Plot of Number of",paste("'",variable_name, "'", sep = "")), 
          xlab = variable_name, 
          ylab = "Frequency", 
          col = "orange", 
          border = "black")
}

    
  



correlation <- function(df_x, df_y, variable_x, variable_y){
  # Calculating the Pearson correlation coefficient
  correlation <- cor(df_x, df_y, method = "pearson")
  
  # Printing the correlation coefficient
  cat("Pearson Correlation Coefficient between", variable_x, "and", variable_y, ":", correlation, "\n")
}

correlation(df_mtcars$mpg, df_mtcars$hp, "mpg", "hp")



scatter_plot <- function(df_x, df_y, variable_x, variable_y){
  # Scatter plot to visualize the relationship
  plot(df_x, df_y, 
       main = paste("Scatter Plot of", variable_y, "vs", variable_x),
       xlab = variable_x, 
       ylab = variable_y, 
       col = "blue", 
       pch = 16)
  
  # Adding a trend line (linear fit)
  abline(lm(df_y ~ df_x), col = "red", lwd = 2)

}

scatter_plot(df_mtcars$hp,df_mtcars$mpg, "hp", "mpg" )



multiple_reg <- function(df_y, df_x1, df_x2) { 
  model <- lm(df_y ~ df_x1 + df_x2)
  return(model)
}

mlr_mtcars <- multiple_reg(df_mtcars$mpg, df_mtcars$hp, df_mtcars$wt)
summary(mlr_mtcars)



mlr_diagnostics <- function(model) {
  par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
  plot(model)
  
  # Check the plots:
  # 1. Residuals vs Fitted: Look for no clear pattern (homoscedasticity).
  # 2. Normal Q-Q: Points should lie on the line (normality of residuals).
  # 3. Scale-Location: Points should be spread equally along the line (variance consistency).
  # 4. Residuals vs Leverage: Look for influential outliers (Cook's distance).
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}

mlr_diagnostics(mlr_mtcars)




pca <- function(df) {
  # Standardizing the numerical variables in the mtcars dataset
  df_scaled <- scale(df)

  # Performing PCA
  pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
  
  return(pca_result)
}

pca_analysis <- function(pca_result, dataset){
  # Summary of PCA to display explained variance
  

  # Extracting the proportion of variance explained
  explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  # Creating a scree plot to visualize explained variance
  plot(explained_variance, type = "b", 
       xlab = "Principal Components", 
       ylab = "Proportion of Variance Explained", 
       main = paste("Scree Plot of PCA of", paste("'",dataset,"'", sep = "")),
       col = "blue", pch = 16)
  
}

pca_mtcars <- pca(df_mtcars[,c("mpg", "disp", "hp", "drat", "wt", "qsec")])

summary(pca_mtcars)

pca_analysis(pca_mtcars, "mtcars")




pca_biplot <- function(pca_result, dataset, cex){
  par(cex = cex)
  # Biplot to visualize the PCA results
  
  biplot(pca_result, scale = 0, 
         main = paste("Biplot of PCA of",paste("'", dataset, "'",sep = "")),
         col = c("blue", "red"))
  
  # Extracting loadings (correlations between variables and components)
  loadings <- pca_result$rotation
  print(loadings[, 1:2])  # Loadings of the first two principal components
}

pca_biplot(pca_mtcars, "mtcars", 0.7)
par(cex = 1)




df_iris <- iris



data_overview(df_iris, "iris")



summary_stats(df_iris$Sepal.Length, "sepal length (cm)")




distribution_visualization(df_iris$Sepal.Length, "sepal length", "cm")




categorical_var_analysis(df_iris$Species, "Species")
  



correlation(df_iris$Sepal.Length, df_iris$Sepal.Width, "sepal length", "sepal width")




scatter_plot(df_iris$Sepal.Width,df_iris$Sepal.Length, "sepal width", "sepal length" )



mlr_iris <- multiple_reg(df_iris$Sepal.Length, df_iris$Sepal.Width, df_iris$Petal.Length)
summary(mlr_iris)




mlr_diagnostics(mlr_iris)



pca_iris <- pca(df_iris[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])

summary(pca_iris)

pca_analysis(pca_iris, "iris")




pca_biplot(pca_iris, "iris", 0.7)
par(cex = 1)





df_wsc = read.csv('\\Wholesale_customers_data.csv')




data_overview(df_wsc, "Wholesale customers")



summary_stats(df_wsc$Milk, "Milk (annual spending (m.u.))")




distribution_visualization(df_wsc$Milk, "Milk", "annual spending (m.u.)")




categorical_var_analysis(df_wsc$Region, "Region")
  



correlation(df_wsc$Fresh, df_wsc$Milk, "Fresh", "Milk")




scatter_plot(df_wsc$Milk,df_wsc$Fresh, "Milk", "Fresh" )



mlr_wsc <- multiple_reg(df_wsc$Fresh, df_wsc$Milk, df_wsc$Grocery)
summary(mlr_wsc)



mlr_diagnostics(mlr_wsc)





pca_wsc <- pca(df_wsc[,c("Fresh", "Milk", "Grocery", "Frozen", "Detergents_Paper", "Delicassen")])

summary(pca_wsc)

pca_analysis(pca_wsc, "Wholesale customers")




pca_biplot(pca_wsc, "Wholesale customers", 0.7)
par(cex = 1)





df_ccs = read_excel('\\Concrete_Data.xls')




data_overview(df_ccs, "Concrete compressive strength")



summary_stats(df_ccs$`Concrete compressive strength(MPa, megapascals)`, "Concrete compressive strength(MPa, megapascals)")




distribution_visualization(df_ccs$`Concrete compressive strength(MPa, megapascals)`, "Compressive strength", "Mpa")




categorical_var_analysis(df_ccs$`Age (day)`, "Age")
  



correlation(df_ccs$`Concrete compressive strength(MPa, megapascals)`, df_ccs$`Cement (component 1)(kg in a m^3 mixture)`, "Compressive strength", "Cement")




scatter_plot(df_ccs$`Cement (component 1)(kg in a m^3 mixture)`,df_ccs$`Concrete compressive strength(MPa, megapascals)`, "Cement", "Compressive strength" )



mlr_ccs <- multiple_reg(df_ccs$`Concrete compressive strength(MPa, megapascals)`, df_ccs$`Cement (component 1)(kg in a m^3 mixture)`, df_ccs$`Fine Aggregate (component 7)(kg in a m^3 mixture)`)

summary(mlr_ccs)




mlr_diagnostics(mlr_ccs)





pca_ccs <- pca(df_ccs[,c("Cement (component 1)(kg in a m^3 mixture)"            
,"Blast Furnace Slag (component 2)(kg in a m^3 mixture)"
,"Fly Ash (component 3)(kg in a m^3 mixture)"           
,"Water  (component 4)(kg in a m^3 mixture)"            
,"Superplasticizer (component 5)(kg in a m^3 mixture)"  
,"Coarse Aggregate  (component 6)(kg in a m^3 mixture)" 
,"Fine Aggregate (component 7)(kg in a m^3 mixture)"    
,"Concrete compressive strength(MPa, megapascals)")])

summary(pca_ccs)

pca_analysis(pca_ccs, "Concrete Compressive strength")


pca_biplot(pca_ccs, "Concrete Compressive strength", 0.65)
par(cex = 1)




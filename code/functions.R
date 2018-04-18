# Logistic Regression ------------


#' Update the list of parameters for the model building. Used in conjunction with the
#' "LogisticModel" and "LogsticModelInt" functions
#' 
#' @param input an existing list of input parameters
#' @param add a list of parameters to be added to the model (optional)
#' @param remove a list of values to be removed from the model (optional)
#'
#' @return An updateded model parameter list
#'  
ParameterUpdate <- function(input, add = NULL, remove = NULL){
  ParList <- c(input, add)
  ParList <- ParList[!ParList %in% c(remove)]
  return(ParList)
}

#' Function to build logistic model from a list of input parameters
#'
#' @param variables a list of predictor variables
#' @param df model dataframe
#'
LogisticModel <- function(variables, Dataframe){
  
  Formula <- formula(paste("Status.Summary ~ ", paste(variables, collapse=" + ")))
  GlmModel <- glm(Formula , data = Dataframe, family = binomial())
  return(GlmModel)
}


# Basic Diagnostics ---------------

#' Prints results for the Chi Squared, Psuedo R sqaured values, Variance Inflation 
#' Factors and Durbin Watson Test
#'
#' @param Model A glm object
#'
LogisticDiagnostics <- function(Model){
  ChiSquared(Model)
  LogisticPseudoR2s(Model)
  VIFcheck(Model)
  DurbinWatsonCheck(Model)
}

#' Creates an odds table for parameters within the regression model
#'
#' @param model  A glm object
#
OddsTable <- function(Model, round = 3){
  odds <- exp(Model$coefficients)
  # Compute Confidence Intervals for Logistic Model parameters
  CI <- suppressMessages(exp(confint(Model)))
  results <- cbind(odds, CI) # Create Results Table
  results <- as.data.frame(cbind(odds, CI)) # Create Results Table
  row.names(results) <- matchNames(row.names(results))
  results <-  round(results, digits = round)
  return(results) 
}


#' Calculates the Chi Squared statistics for a model and compares against the null model. 
#' 
#' @param model A glm object
#'
ChiSquared <- function(Model){
  
  modelChi <- Model$null.deviance - Model$deviance
  chidf <- Model$df.null - Model$df.residual  # Degrees of Freedom 
  chisq.prob <- 1 - pchisq(modelChi, chidf) 
  Null.deviance <- cbind(modelChi, chidf, chisq.prob)
  
  cat("Chi Squared Test \n")
  cat("Chi Squared              ", round(modelChi, 3), "\n")
  cat("Df                       ", round(chidf, 3), "\n")
  cat("Chi Squared p            ", round(chisq.prob, 3), "\n \n")
  
}

#' Calculates the R squared values (Hosmer and Lemeshow, Cox and Snell and Nagelkerke) values for a logistic regression model.
#' 
#' @param model A glm object
#'
LogisticPseudoR2s <- function(Model) {
  dev <- Model$deviance
  nullDev <- Model$null.deviance
  modelN <-  length(Model$fitted.values)
  R.hl <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.hl, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n \n")  }

#' Calculate the Durbin-Watson statistic to detect the presence of autocorrelation in the residuals from a regression analysis.
#' 
#' @param model A glm object
#'
DurbinWatsonCheck <- function(Model){
  DW <- car::durbinWatsonTest(Model)
  # Check results
  b <-  (DW$p > 0.05)  # check bootstrapped p-values
  d <- (0 < DW$dw & DW$dw < 4)  # Check DurbinWatsonRange
  # Print Results
  cat("Durbin Watson Results:", "\n", "\n")
  print(DW)
  cat("\n","Is p-value greater than 0.05:            ", b)
  cat("\n","Is DW ~ 2 (range of 0 to 4 acceptable):  ", d) }

#' Calculates variance-inflation and generalized variance-inflation factors for linear and generalized linear models, and returns any which are above 10.
#' 
#' @param model an object that responds to coef, vcov, and model.matrix, such as an lm or glm object
#'
VIFcheck <- function(Model){
  vifresults <- car::vif(Model)
  # Check VIF for values above 10
  suspectvif <- vifresults[order(vifresults, decreasing = TRUE) & vifresults > 10]
  
  # Print Results
  cat("Variance Inflation Factors: \n")
  print(vifresults)
  cat("\n","Checking for potential issues: ")
  
  if (length(suspectvif) > 1){  # Checks if empty
    return(suspectvif) }
  else
    return(cat("No apparent issues with collinearity","\n", "\n"))
}

#' Fit a LOESS curve to a dataset
#'
#' @param x the parameter name for the model
#' @param DegreeValue the degree of polynomials to be used
#' @param SpanValue the parameter ?? which controls the degree of smoothing
#' 
loessfit <- function(x, DegreeValue, SpanValue){
  Function <- formula(paste("Planning_Status_Code ~ ", x))
  lfit <-loess(Function, data = fulldatared, degree = DegreeValue, span = SpanValue)
  lgpred<-log(predict(lfit) / (1 - predict(lfit)))
  plot(formula(paste("lgpred ~ fulldatared$", x)))
}


#' Takes a list of variables, and  updates the list to include the log transformations (Int)
#' 
#' @param PredictorVariables a list of varaibles to be assessed
#' @param outcome the outcome variable
#' @param df a datafram containing the observations
#'
LogisticModelInt <- function(PredictorVariables, outcome, df){
  # Load Data
  fulldata <- df[, c(outcome, PredictorVariables)]
  
  # Calculation the logarithmic transformations
  for (i in PredictorVariables){
    data <- df[ ,eval(i)]
    suppressWarnings(int <- log(data)* data)
    int <- as.data.frame(int)
    names(int) <- paste(i,"Int", sep="_")
    
    fulldata <- cbind(fulldata, int)
    rm(i, int)
  }
  
  # Formulate Logistic Regressio Model
  intNames <- paste(PredictorVariables, "Int", sep ="_")
  variableInt <- c(PredictorVariables, intNames)
  Formula <- formula(paste(outcome,  "~", paste(variableInt, collapse=" + ")))
  GlmModel <- glm(Formula , data = fulldata, family = binomial())
  
  # Extract significance values
  Significance <- summary(GlmModel)$'coefficients'[, 4]
  Significance <- Significance[Significance < 0.05 ]
  
  # Return list of signficant parameters
  cat("Statistically significant parameters from Logarithmic Transformations: \n")
  cat(names(Significance), sep = ", ")
}


#'  Divides a set of numeric variables into disjoint or hierarchical cluster
#' 
VariableCluster <- function(PredictorVariables, datasource){
  
  Formula <- formula(paste("Status.Summary ~ ", paste(PredictorVariables, collapse=" + ")))
  
  return(plot(Hmisc::varclus(Formula, data = datasource)))
}

# Presenting Model Results --------------------------

LogisticResultsTable <- function(Model, roundby = 3){
  # Formats a logistic regression results table including significance indicators,
  # Odds ratios and confidence intervals
  # Base Table
  ResultsTable <- as.data.frame(summary(Model)$coefficients)
  ResultsTable <- round(ResultsTable, roundby)
  ResultsTable <- ResultsTable[,c(1,2,3,4)] 
  
  # Add Significance Stars
  pval <- ResultsTable[,4]
  symp <- symnum(pval, corr = FALSE,
                 cutpoints = c(0,  .001,.01,.05, .1, 1),
                 symbols = c("***","**","*","."," "))
  ResultsTable$Signif <- symp
  
  # Add Odd Ratio
  odds <- exp(Model$coefficients)
  CI <- suppressMessages(exp(confint(Model)))
  OddsResults <- cbind(odds, CI) # Create Results Table
  OddsResults <- round(OddsResults, roundby)
  ResultsTable <- cbind(ResultsTable, OddsResults)
  
  # Make row names a column
  ResultsNames <- row.names(ResultsTable)
  ResultsTable <- cbind(ResultsNames, ResultsTable)
  row.names(ResultsTable) <- NULL
  ResultsTable <- ResultsTable[-1,]
  
  # Rename Columns
  names(ResultsTable) <- c("Variable", "Estimate", "Std. Error", "z value", "Pr", "Sig.", "Odds Ratio", "OR \n 2.5% CI", "OR \n 97.5% CI")
  row.names(ResultsTable) <- NULL
  
  
  return(ResultsTable)
}

LogisticOddsPlot <- function(Model, Sort = FALSE, Title){
  
  a <- LogisticOddsTable(Model, Sort)
  b <- LogOddsPlotGraph(a, Title, Sort)
  return(b)
}

LogisticOddsTable <- function(Model, Sort = FALSE){
  # Produces an odds table with confidence intervals for a logistic model
  #
  ModelDF <-  broom::tidy(Model)
  ModelDF$odds <- exp(Model$coefficients)
  CI <- suppressMessages(exp(confint(Model)))
  CI <- as.data.frame(CI)
  names(CI) <- c("ci_lower", "ci_upper")
  ModelDF <- cbind(ModelDF, CI)
  ModelDF <- ModelDF[ModelDF$term != "(Intercept)", ]
  
  # Reorder values. Default order is as listed into model
  if (Sort == "Score"){ 
    ModelDF <- ModelDF[order(ModelDF$odds),]
    ModelDF$term <- factor(ModelDF$term, levels = ModelDF$term[order(ModelDF$odds)])
  }
  
  # Define whether the CI values are greater or less than one
  checkvalue <- function(x, y){
    if (x> 1 & y > 1){return("Positive")} 
    else if (x < 1 & y < 1) {return("Negative")}
    else return("Uncertain")
  }
  
  # Apply function to data
  ModelDF$Relationship <- mapply(ModelDF$ci_lower, ModelDF$ci_upper, FUN = checkvalue)
  
  return(ModelDF)
}


LogOddsPlotGraph <- function(OddsTable, PlotTitle, Sort = FALSE){
  # Produces boxplots for estimated values from a regression model
  #
  # Args:
  #   OddsTable: a formatted odds table from the function "LogisticOddsTable"
  #   PlotTitle: the title of the resulting plot
  #   plotColour: the output colour of the boxplots
  #   Sort: reorder the plot by variable fit
  #
  ModelDF <- OddsTable
  
  # Rename Variables
  names <- read.csv("VariableDisplayNames.csv", stringsAsFactors = FALSE)
  ModelDF$term <- names[match(x = ModelDF$term, table = names$Label, nomatch = ""), 2]
  
  Terms <- ModelDF$term
  
  suppressWarnings(library(ggplot2))
  
  # Determine Max vales for axes
  Ymax <- max(c(ModelDF$ci_upper, ModelDF$ci_upper))
  Ymax <- round(Ymax + 0.5, 0)
  Ymin <- min(c(ModelDF$ci_upper, ModelDF$ci_upper))
  Ymin <- round(Ymin - 0.5, 0)
  
  offset = 1 # Defines where barplots start from
  linebreak = 0.1
  Uncertain <- "#C7C7C7"
  Negative <- "#fc8d59"
  Positive <- "#91cf60"
  
  # Plot Graph
  plotlogodds <- ggplot(ModelDF, aes(x = term, y = odds - offset, fill = Relationship)) +
    # Add Data
    geom_bar(position=position_dodge(), stat="identity", colour = "black", size = 0.3) +
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)
    ) +
    geom_hline(yintercept = 0) +
    # Labels and axes
    labs(y = "Odds Ratio", x = "") +
    labs(title = PlotTitle) +
    scale_y_continuous(labels = seq(Ymin - offset - 0.1, Ymax + 0.1, linebreak) + offset,
                       breaks = seq(Ymin - offset- 0.1, Ymax + 0.1, linebreak)
    ) +
    scale_x_discrete(limits = rev(Terms)) +
    scale_fill_manual(values = c("Uncertain" = Uncertain, "Negative" = Negative, "Positive" = Positive)) + 
    coord_flip() +
    # Theme Settings
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank()
    )
  
  return(plotlogodds)
}




LogResults <- function(Model){
  # Function to summarise a logistic regression model
  
  dev <- round(Model$deviance, 2)
  nullDev <- round(Model$null.deviance, 2)
  modelN <-  length(Model$fitted.values)
  R.hl <-  1 -  dev / nullDev
  R.cs <- 1- exp (-(nullDev - dev) / modelN)
  R.n <- round(R.cs / ( 1 - ( exp (-(nullDev / modelN)))), 3)
  modelChi <- Model$null.deviance - Model$deviance
  chidf <- Model$df.null - Model$df.residual
  
  c(
    Observations = length(fitted(Model)),
    Parameters = length(coef(Model)) %>% as.character(),
    Deviance = format(dev, nsmall=1),
    R.n = format(R.n, nsmall = 3),
    "Chi Square" = round(modelChi, 0),
    "Degrees of Freedom" = round(chidf, 0),
    p = format(round(1- pchisq(modelChi, chidf),5), nsmall=3),
    "Residual Deviance" = round(Model$df.residual, 0),
    AIC = round(AIC(Model), 0))
}

LogResultsTable <- function(...){
  # Takes a list of logistic regression models and creates a summary table
  
  lapply(list(...), LogResults) %>% 
    do.call(cbind, .) %>%
    as.data.frame() %>%
    set_colnames(paste("Model", seq(1, length(list(...)))))
}





Segmented_LogisticModelsComplete <- function(df, split_category, variables_list, outcome_variable, linebreak = 0.1, limits= 1){
  
  data <- Segmented_Dataset(df, by = split_category)
  models <- Segmented_LogisticModels(data, variables_list, outcome_variable)
  oddstable <- Segmented_OddsTable(LogisticModelList = models)
  plot <- Segmented_OddsPlotGroupedCustom(oddstable, linebreak, limits)
  
  return(list(data = data, models = models, oddstable = oddstable, plot = plot))
}




FindBestModel <- function(df, variableList, direction = "backward", steps = 1000){
  
  LogModel <- LogisticModel(variableList, df)
  Dataframe <- df[, c(which(names(df) == "Status.Summary"), which(names(df) %in% variableList))]
  Dataframe <- Dataframe[complete.cases(Dataframe),]
  
  # Find Best Model
  BestModel <- step(LogModel, direction = direction, trace = FALSE, steps = steps)
  return(BestModel)
  
}


# Segmented Models -------------------

# Name:       Functions Segmenting Model
# Date:       16 December 2016  
# Author:     Michael Harper 

# Functions used to assess the impact of segmenting the logistic regression model into
# seperate sub-models based on a particular characteristic

# ---------------------------------------------------------------------------------------
# Primary Functions
# ---------------------------------------------------------------------------------------

Segmented_FullOddsPlot <- function(dataset, by, predictors, outcome, Title, linebreak){
  # Combines the stages outlined in the subfunctions below to produce a faceted box plot 
  # for the odds ratios of logistic regression models
  #
  #
  # Segment the dataset by the given category
  SegDatasets <- Segmented_Dataset(dataset, by)
  
  # Build Logistic Regression Models for the segmented models
  SegLogisticModels <- Segmented_LogisticModels(SegDatasets, predictors, outcome)
  
  # Build Odds Tables for the list of logistic regression models
  LogisticModelsOddsTable <- Segmented_OddsTable(SegLogisticModels)
  
  # Plot the Faceted Graph
  FacetedBoxPlot <- Segmented_OddsPlot(LogisticModelsOddsTable, Title, linebreak)
  
  return(FacetedBoxPlot)
}


# ---------------------------------------------------------------------------------------
# Sub Functions: 
# These functions are called by the master functions above or can be used individually
# ---------------------------------------------------------------------------------------

Segmented_Dataset <- function(dataset, by){
  # Creates data subsets for a full dataset based on a parameter within the dataset
  #
  # Args:
  #   dataset: the full datatable to be split
  #   by: the name of the column for the dataset to be split using
  #
  # Returns:
  #   A list of dataframes for each subset
  #
  columntoSplit <- dataset[[by]] 
  variableLevels <- levels(columntoSplit)
  SegmentedDatasets <- NULL # for results
  
  for (i in 1:length(variableLevels)){
    SegmentedDataset <- dataset[columntoSplit == variableLevels[i], ]
    SegmentedDatasets[[variableLevels[i]]] <- SegmentedDataset # Add dataset to results
  }
  return(SegmentedDatasets)
}


Segmented_LogisticModels <- function(SegmentedDatasets, predictors, outcome){
  # Combines the SplitDatasetbyVariable function and "Logstic Model" function to create
  # seperate logistic regression models
  # Args:
  #   dataset: the full dataframe to be split
  #   variableName: the name of the column for the dataset to be split using
  #
  # Returns:
  #   A list of logistic regression models
  #
  LogisticModels <- NULL # Empty dataframe for results
  
  for (i in 1:length(SegmentedDatasets)){
    datasetname <- (names(SegmentedDatasets[i]))
    ModelDataset <- SegmentedDatasets[[datasetname]]
    
    Formula <- formula(paste(outcome, "~ ", paste(predictors, collapse=" + ")))
    GlmModel <- glm(Formula , data = ModelDataset, family = binomial())
    LogisticModels[[datasetname]] <- GlmModel
    
  }
  return(LogisticModels)
}


Segmented_OddsTable <- function(LogisticModelList){
  # Splits the 
  #
  # Args:
  #   dataset: the input dataset to be split
  #   splitVariableName: the name of the parameter which will be used to segment the model
  #   predictorVariableList: the list of parameters to be used within the GLM
  #   outcomeVariable: the binary outcome variable 
  #
  # Returns: a single odds table which contains the statistics for the segmented model
  #
  # Split dataset into segments and build logistic models into a list
  OddsTables <- NULL
  
  for (i in 1:length(LogisticModelList)){
    datasetname <- (names(LogisticModelList[i]))
    ModelDataset <- LogisticModelList[[datasetname]]
    
    # Calculate Log odds
    ModelDF <- LogisticOddsTable(ModelDataset, Sort = FALSE)
    ModelDF$Facet <- datasetname
    OddsTables[[datasetname]] <- ModelDF
  }
  return(OddsTables)
}


Segmented_OddsPlot <- function(OddsTables, PlotTitle, linebreak){
  # Plots a faceted odds ratio plot for a list of segmented odds tables
  # 
  # Args:
  #   OddsTables: a list of odds tables as produced from the function "OddsTableSegmented"
  #
  suppressWarnings(library(ggplot2))
  
  Combined <- NULL # Blank table for results
  
  # Load reference names
  names <- read.csv("VariableDisplayNames.csv")
  
  for (i in 1:length(OddsTables)){ # Loop combines models into a single table
    datasetname <- (names(OddsTables[i])) # Extract name of the submodel
    ModelDataset <- OddsTables[[datasetname]] # Extract the data from the list
    # Rename parameters
    ModelDataset$term <- names[match(x = ModelDataset$term, table = names$Label, nomatch = ""), 2]
    Combined <- rbind(ModelDataset, Combined)
  }
  
  linebrk = linebreak # spacing between markers
  
  # Determine Max vales for axes
  Ymax <- max(c(Combined$ci_upper, Combined$ci_upper))
  Ymax <- round(Ymax + linebrk, 1)
  Ymin <- min(c(Combined$ci_upper, Combined$ci_upper))
  Ymin <- round(Ymin - linebrk, 1)
  
  # Produces axes labels starting from ymin and ending at Ymax.
  # Code structure forces the sequence to always stop at 0
  axisValues <- c( -rev(seq(0, abs(Ymin), linebrk)),  seq(linebrk, Ymax, by = linebrk))
  
  offset <- 1 # Defines where barplots start from
  MixedColour <- "grey51"
  Negative <- "coral3"
  Positive <- "palegreen4"
  
  # Plot Graph
  plotOdds <- ggplot(Combined, aes(x = term, y = odds - offset, fill = Relationship)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2, position=position_dodge(.9)) +
    labs(y = "Odds Ratio", x = "Term") +
    labs(title = PlotTitle) +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_blank(),
          axis.title.y=element_blank()) +
    scale_y_continuous(labels = axisValues + offset, breaks = axisValues) +
    scale_x_discrete(limits = rev(ModelDataset$term)) +
    scale_fill_manual(values = c("Mixed" = MixedColour, "Negative" = Negative,
                                 "Positive" = Positive)) + 
    geom_hline(yintercept = 0, size = 0.5) +
    facet_grid(. ~ Facet) +
    coord_flip() 
  
  return(plotOdds)
}


# Define Custom Plotting Function
Segmented_OddsPlotGroupedCustom <- function(OddsTables, linebreak = 0.2, scale = 1){
  # Plots a faceted odds ratio plot for a list of segmented odds tables
  # 
  # Args:
  #   OddsTables: a list of odds tables as produced from the function "OddsTableSegmented"
  #
  suppressWarnings(library(ggplot2))
  
  Combined <- NULL # Blank table for results
  
  # Load reference names
  names <- read.csv("VariableDisplayNames.csv")
  
  for (i in 1:length(OddsTables)){ # Loop combines models into a single table
    datasetname <- (names(OddsTables[i])) # Extract name of the submodel
    ModelDataset <- OddsTables[[datasetname]] # Extract the data from the list
    
    # Rename parameters
    ModelDataset$term <- names[match(x = ModelDataset$term, table = names$Label, nomatch = ""), 2]
    Combined <- rbind(ModelDataset, Combined)
  }
  
  Terms <- ModelDataset$term
  linebrk <-linebreak # spacing between markers
  
  # Determine Max vales for axes
  Ymax <- 1 + scale
  Ymin <- 1 - scale
  
  # Crop error bars if they exceed the limits of the graph. Otherwise the scale has to be massive to get them on
  Combined$ci_lower[Combined$ci_lower <= Ymin] <- Ymin
  Combined$ci_upper[Combined$ci_upper >= Ymax] <- Ymax
  
  # axis values produces labels which always return 1
  axisValues <- c( -rev(seq(0, abs(Ymin), linebrk)),  seq(linebrk, Ymax, by = linebrk))
  offset <- 1 # Defines where barplots start from
  spacing <- 0.7
  #windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  plotOdds <- ggplot(Combined, aes(x = term, y = odds - offset, fill = Facet, width = spacing)) +
    geom_hline(yintercept = 0) +
    geom_bar(position=position_dodge(), stat="identity",colour = "black") + 
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(spacing),
                  size = 0.4,
                  colour = "grey20",
                  linetype = 1
    ) +
    
    # Axes
    labs(y = "Odds Ratio") +
    scale_y_continuous(labels = seq( from = Ymin, to =  Ymax, by = linebreak),
                       breaks = seq(from = Ymin - offset, to = Ymax - offset, by = linebreak),
                       limits = c(Ymin, Ymax) - offset,
                       expand=c(0,0)
    ) +
    scale_x_discrete(limits = rev(Terms)) +
    
    # Theme
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          axis.title.y=element_blank()
    ) +
    coord_flip()
  
  return(plotOdds)
}


FiltNumericColumns <- function(dataframe){
  nums <- sapply(dataframe, is.numeric)
  return(dataframe[, nums])
}

#' Produces summary statistics for an entire numeric dataframe
#' 
#' @param dataframe the dataframe
#' 
#' @return a dataframe of summary statistics 
#' 
SummariseDataframe <- function(dataframe){
  # Produces summary statistics for rows in a dataframe
  lapply(dataframe, function(x) my.summary(x, na.rm= TRUE)) %>%
    do.call(rbind, .) %>%
    as.data.frame()
}


#' Produces a summary of a column within a dataframe
#' 
#' @param x a vector of values
#' @param rounded the number of decimal places to return
#' 
#' @return a list of summary statistics for the variable
#' 
my.summary <- function(x, rounded = 1, ...){
  round(c(mean=mean(x, ...),
          sd=sd(x, ...),
          median=median(x, ...),
          min=min(x, ...),
          max=max(x,...), 
          skew = psych::skew(x,...),
          n=length(x[!is.na(x)])), digits = rounded)
}


#' Matches variable names with their full descriptive name. 
#' Used when plotting or showing results in a table
#' 
#' This function is designed to work with the PhD project
#' 
#' @param inputNames a list of names which are to looked up
#' 
matchNames <- function(inputNames, path = "VariableDisplayNames.csv"){
  names <- read.csv(path, stringsAsFactors = FALSE)
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 2])
}

#' Matches variable names with their full descriptive name. 
#' Used when plotting or showing results in a table
#' 
#' This function is designed to work with the PhD project
#' 
#' @param inputNames a list of names which are to looked up
#' 
matchUnits <- function(inputNames, path = "VariableDisplayNames.csv"){
  names <- read.csv(path, stringsAsFactors = FALSE)
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 3])
}

#' Matches variable names with their full descriptive name. 
#' Used when plotting or showing results in a table
#' 
#' This function is designed to work with the PhD project
#' 
#' @param inputNames a list of names which are to looked up
#' 
matchCategory <- function(inputNames, path = "VariableDisplayNames.csv"){
  names <- read.csv(path, stringsAsFactors = FALSE)
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 4])
}

#' Checks the row names against the variable full names
#' 
#' This function is designed to work with the PhD project
#' 
#' @param dataframe a list of names which are to looked up
#' @param dropNA a TRUE/FALSE selection whether names should be dropped if empty
#' 
matchNamesColumns <- function(dataframe, dropNA = TRUE){
  ReplacementNames <- matchNames(names(dataframe))
  names(dataframe) <- ReplacementNames
  if(dropNA){dataframe <- dataframe[,-c(which(ReplacementNames == ""))]}
  return(dataframe)
}

#' Creates a 2 way frequency table from a dataframe and calculates the percentage of
#' a specified category
#'
#' @param rows the variable to counted in the rows
#' @param   columns the variable to be counted in the columns
#' @param SumPercentage the parameter to be summed in the column
#' @param inputDataframe the dataframe containing the row and column parameters
#'
TwoWayFrequency <- function(Rows, Columns, SumPercentage, inputDataframe){
  
  InputFormula <- paste("~ ",(paste(Rows, Columns, sep = " + ")))
  a <- xtabs( InputFormula , data = inputDataframe) # produces base count table
  
  # Create New columns
  Total <- rowSums(a)
  Percentage <- a[,SumPercentage] / Total
  Percentage <- round(Percentage, 2) * 100 # Convert into percentage
  
  # Merge and format data
  results <- cbind(a, Total, Percentage)
  totalcol <- ncol(a) + 1
  results <- as.data.frame(results)
  results <- plyr::rename(results, replace = c("Percentage" = paste("Percentage ", SumPercentage, sep="")))
  return(results)
}



#' Produces a scatter plot for the logistic regression variables
#' 
#' @param df the dataframe which contains the data
#' @param c the column reference to search
#' @param bins the number of bins in which the data should be grouped
#' 
ScatterPlotOdds <- function(df, variable, quantiles = 20){
  temp <- data.frame(Value = df[[variable]], Status = df$Planning_Status_Summary)
  temp <- temp[complete.cases(temp),]
  temp$Bin <- Hmisc::cut2(temp[,1], g = quantiles, levels.mean = TRUE)  %>% as.character() %>% as.numeric() 
  names(temp) <- c("Value", "PlanningStatus", "Bin")
  temp$PlanningStatus <- temp$PlanningStatus * 100
  
  aggdata <-aggregate(temp, by=list(temp$Bin),FUN=mean, na.rm=TRUE) %>% .[c(4,3)]
  
  
  ggplot(aggdata, aes(x = Bin, y = PlanningStatus)) +
    geom_smooth(method = "loess", color = "steelblue1", alpha=0.5) + 
    geom_point(size = 0.5) +
    geom_point(data = temp, aes(Value, PlanningStatus), size = 1, alpha = 0.2, color = "steelblue1") +
    scale_y_continuous(limits = (c(0,100))) +
    scale_x_continuous(limits = c(max(0, floor(min(temp$Bin))), max(temp$Bin))) +
    labs(title = paste(matchNames(variable)), 
         x = matchUnits(variable), y = "Planning Success") +
    theme(plot.title = element_text(size=10),
          axis.text = element_text(size=6),
          axis.title.x = element_text(size=8),
          axis.title.y = element_text(size=8))
}

LogResultsTable <- function(...){
  # Takes a list of logistic regression models and creates a summary table
  
  lapply(list(...), LogResults) %>% 
    do.call(cbind, .) %>%
    as.data.frame() %>%
    set_colnames(paste("Model", seq(1, length(list(...)))))
}



Segmented_LogisticModelsComplete <- function(df, split_category, variables_list, outcome_variable, linebreak = 0.1, limits= 1){
  
  data <- Segmented_Dataset(df, by = split_category)
  models <- Segmented_LogisticModels(data, variables_list, outcome_variable)
  oddstable <- Segmented_OddsTable(LogisticModelList = models)
  plot <- Segmented_OddsPlotGroupedCustom(oddstable, linebreak, limits)
  
  return(list(data = data, models = models, oddstable = oddstable, plot = plot))
}


Segmented_Dataset <- function(dataset, by){
  # Creates data subsets for a full dataset based on a parameter within the dataset
  #
  # Args:
  #   dataset: the full datatable to be split
  #   by: the name of the column for the dataset to be split using
  #
  # Returns:
  #   A list of dataframes for each subset
  #
  columntoSplit <- dataset[[by]] 
  variableLevels <- levels(columntoSplit)
  SegmentedDatasets <- NULL # for results
  
  for (i in 1:length(variableLevels)){
    SegmentedDataset <- dataset[columntoSplit == variableLevels[i], ]
    SegmentedDatasets[[variableLevels[i]]] <- SegmentedDataset # Add dataset to results
  }
  return(SegmentedDatasets)
}


Segmented_LogisticModels <- function(SegmentedDatasets, predictors, outcome){
  # Combines the SplitDatasetbyVariable function and "Logstic Model" function to create
  # seperate logistic regression models
  # Args:
  #   dataset: the full dataframe to be split
  #   variableName: the name of the column for the dataset to be split using
  #
  # Returns:
  #   A list of logistic regression models
  #
  LogisticModels <- NULL # Empty dataframe for results
  
  for (i in 1:length(SegmentedDatasets)){
    datasetname <- (names(SegmentedDatasets[i]))
    ModelDataset <- SegmentedDatasets[[datasetname]]
    
    Formula <- formula(paste(outcome, "~ ", paste(predictors, collapse=" + ")))
    GlmModel <- glm(Formula , data = ModelDataset, family = binomial())
    LogisticModels[[datasetname]] <- GlmModel
    
  }
  return(LogisticModels)
}


Segmented_OddsTable <- function(LogisticModelList){
  # Splits the 
  #
  # Args:
  #   dataset: the input dataset to be split
  #   splitVariableName: the name of the parameter which will be used to segment the model
  #   predictorVariableList: the list of parameters to be used within the GLM
  #   outcomeVariable: the binary outcome variable 
  #
  # Returns: a single odds table which contains the statistics for the segmented model
  #
  # Split dataset into segments and build logistic models into a list
  OddsTables <- NULL
  
  for (i in 1:length(LogisticModelList)){
    datasetname <- (names(LogisticModelList[i]))
    ModelDataset <- LogisticModelList[[datasetname]]
    
    # Calculate Log odds
    ModelDF <- LogisticOddsTable(ModelDataset, Sort = FALSE)
    ModelDF$Facet <- datasetname
    OddsTables[[datasetname]] <- ModelDF
  }
  return(OddsTables)
}


Segmented_OddsPlot <- function(OddsTables, PlotTitle, linebreak){
  # Plots a faceted odds ratio plot for a list of segmented odds tables
  # 
  # Args:
  #   OddsTables: a list of odds tables as produced from the function "OddsTableSegmented"
  #
  suppressWarnings(library(ggplot2))
  
  Combined <- NULL # Blank table for results
  
  # Load reference names
  names <- read.csv("VariableDisplayNames.csv")
  
  for (i in 1:length(OddsTables)){ # Loop combines models into a single table
    datasetname <- (names(OddsTables[i])) # Extract name of the submodel
    ModelDataset <- OddsTables[[datasetname]] # Extract the data from the list
    # Rename parameters
    ModelDataset$term <- names[match(x = ModelDataset$term, table = names$Label, nomatch = ""), 2]
    Combined <- rbind(ModelDataset, Combined)
  }
  
  linebrk = linebreak # spacing between markers
  
  # Determine Max vales for axes
  Ymax <- max(c(Combined$ci_upper, Combined$ci_upper))
  Ymax <- round(Ymax + linebrk, 1)
  Ymin <- min(c(Combined$ci_upper, Combined$ci_upper))
  Ymin <- round(Ymin - linebrk, 1)
  
  # Produces axes labels starting from ymin and ending at Ymax.
  # Code structure forces the sequence to always stop at 0
  axisValues <- c( -rev(seq(0, abs(Ymin), linebrk)),  seq(linebrk, Ymax, by = linebrk))
  
  offset <- 1 # Defines where barplots start from
  MixedColour <- "grey51"
  Negative <- "coral3"
  Positive <- "palegreen4"
  
  # Plot Graph
  plotOdds <- ggplot(Combined, aes(x = term, y = odds - offset, fill = Relationship)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2, position=position_dodge(.9)) +
    labs(y = "Odds Ratio", x = "Term") +
    labs(title = PlotTitle) +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_blank(),
          axis.title.y=element_blank()) +
    scale_y_continuous(labels = axisValues + offset, breaks = axisValues) +
    scale_x_discrete(limits = rev(ModelDataset$term)) +
    scale_fill_manual(values = c("Mixed" = MixedColour, "Negative" = Negative,
                                 "Positive" = Positive)) + 
    geom_hline(yintercept = 0, size = 0.5) +
    facet_grid(. ~ Facet) +
    coord_flip() 
  
  return(plotOdds)
}


# Define Custom Plotting Function
Segmented_OddsPlotGroupedCustom <- function(OddsTables, linebreak = 0.2, scale = 1){
  # Plots a faceted odds ratio plot for a list of segmented odds tables
  # 
  # Args:
  #   OddsTables: a list of odds tables as produced from the function "OddsTableSegmented"
  #
  suppressWarnings(library(ggplot2))
  
  Combined <- NULL # Blank table for results
  
  # Load reference names
  names <- read.csv("VariableDisplayNames.csv")
  
  for (i in 1:length(OddsTables)){ # Loop combines models into a single table
    datasetname <- (names(OddsTables[i])) # Extract name of the submodel
    ModelDataset <- OddsTables[[datasetname]] # Extract the data from the list
    
    # Rename parameters
    ModelDataset$term <- names[match(x = ModelDataset$term, table = names$Label, nomatch = ""), 2]
    Combined <- rbind(ModelDataset, Combined)
  }
  
  Terms <- ModelDataset$term
  linebrk <-linebreak # spacing between markers
  
  # Determine Max vales for axes
  Ymax <- 1 + scale
  Ymin <- 1 - scale
  
  # Crop error bars if they exceed the limits of the graph. Otherwise the scale has to be massive to get them on
  Combined$ci_lower[Combined$ci_lower <= Ymin] <- Ymin
  Combined$ci_upper[Combined$ci_upper >= Ymax] <- Ymax
  
  # axis values produces labels which always return 1
  axisValues <- c( -rev(seq(0, abs(Ymin), linebrk)),  seq(linebrk, Ymax, by = linebrk))
  offset <- 1 # Defines where barplots start from
  spacing <- 0.7
  #windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  plotOdds <- ggplot(Combined, aes(x = term, y = odds - offset, fill = Facet, width = spacing)) +
    geom_hline(yintercept = 0) +
    geom_bar(position=position_dodge(), stat="identity",colour = "black") + 
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(spacing),
                  size = 0.4,
                  colour = "grey20",
                  linetype = 1
    ) +
    
    # Axes
    labs(y = "Odds Ratio") +
    scale_y_continuous(labels = seq( from = Ymin, to =  Ymax, by = linebreak),
                       breaks = seq(from = Ymin - offset, to = Ymax - offset, by = linebreak),
                       limits = c(Ymin, Ymax) - offset,
                       expand=c(0,0)
    ) +
    scale_x_discrete(limits = rev(Terms)) +
    
    # Theme
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          axis.title.y=element_blank()
    ) +
    coord_flip()
  
  return(plotOdds)
}



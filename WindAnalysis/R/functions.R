#' @import stats
NULL

# Logistic Regression ------------

#' Update the list of parameters for the model building. Used in conjunction with the
#' "LogisticModel" and "LogsticModelInt" functions
#'
#' @param input an existing list of input parameters
#' @param add a list of parameters to be added to the model (optional)
#' @param remove a list of values to be removed from the model (optional)
#' @return An updateded model parameter list
#' @export
#'
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
#' @export
#'
LogisticModel <- function(variables, df){

  Formula <- formula(paste("Status.Summary ~ ", paste(variables, collapse=" + ")))
  GlmModel <- stats::glm(Formula , data = df, family = stats::binomial())
  return(GlmModel)
}


# Basic Diagnostics ---------------

#' Logistic Regression Model Diagnostics
#'
#' Prints results for the Chi Squared, Psuedo R sqaured values, Variance Inflation
#' Factors and Durbin Watson Test
#'
#' @param Model A glm object
#' @return  A printout of diagnostics
#' @export
#'
LogisticDiagnostics <- function(Model){
  ChiSquared(Model)
  LogisticPseudoR2s(Model)
  VIFcheck(Model)
  DurbinWatsonCheck(Model)
}

#' Build Odds Table
#'
#' Creates an odds table for parameters within the regression model
#'
#' @param Model  A glm object
#' @param round the number of decimal places the figure should be printed to
#' @export
#'
OddsTable <- function(Model, round = 3){
  odds <- exp(Model$coefficients)
  # Compute Confidence Intervals for Logistic Model parameters
  CI <- suppressMessages(exp(stats::confint(Model)))
  results <- cbind(odds, CI) # Create Results Table
  results <- as.data.frame(cbind(odds, CI)) # Create Results Table
  row.names(results) <- matchNames(row.names(results))
  results <-  round(results, digits = round)
  return(results)
}


#' Calculate Chi squared statistics
#'
#' Internal function used to calculates the Chi Squared statistics for a model
#'
#' @param Model A glm object
#' @export
#'
ChiSquared <- function(Model){

  modelChi <- Model$null.deviance - Model$deviance
  chidf <- Model$df.null - Model$df.residual  # Degrees of Freedom
  chisq.prob <- 1 - stats::pchisq(modelChi, chidf)
  Null.deviance <- cbind(modelChi, chidf, chisq.prob)

  cat("Chi Squared Test \n")
  cat("Chi Squared              ", round(modelChi, 3), "\n")
  cat("Df                       ", round(chidf, 3), "\n")
  cat("Chi Squared p            ", round(chisq.prob, 3), "\n \n")

}

#' Calculate Logistic Model R-squared values
#'
#' Calculates the R squared values (Hosmer and Lemeshow, Cox and Snell and Nagelkerke) values for a logistic regression model.
#'
#' @param Model A glm object
#' @export
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

#' Calculates Durbin-Watson statistic for glm model
#'
#' Calculate the Durbin-Watson statistic to detect the presence of
#'  autocorrelation in the residuals from a regression analysis.
#'
#' @param Model A glm object
#' @export
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

#' Calculate Variance Inflation Factor (VIF)
#'
#' Calculates variance-inflation and generalized variance-inflation factors for
#'    linear and generalized linear models, and returns any which are above 10.
#'
#' @param Model an object that responds to coef, vcov, and model.matrix, such as an lm or glm object
#' @export
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
#' @export
#'
loessfit <- function(x, DegreeValue, SpanValue){
  Function <- stats:: formula(paste("Planning_Status_Code ~ ", x))
  lfit <-stats::loess(Function, data = fulldatared, degree = DegreeValue, span = SpanValue)
  lgpred<-log(stats::predict(lfit) / (1 - stats::predict(lfit)))
  graphics::plot(stats:: formula(paste("lgpred ~ fulldatared$", x)))
}


#' Takes a list of variables, and  updates the list to include the log transformations (Int)
#'
#' @param PredictorVariables a list of varaibles to be assessed
#' @param outcome the outcome variable
#' @param df a datafram containing the observations
#' @export
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
  Formula <- stats:: formula(paste(outcome,  "~", paste(variableInt, collapse=" + ")))
  GlmModel <- glm(Formula , data = fulldata, family = stats::binomial())

  # Extract significance values
  Significance <- summary(GlmModel)$'coefficients'[, 4]
  Significance <- Significance[Significance < 0.05 ]

  # Return list of signficant parameters
  cat("Statistically significant parameters from Logarithmic Transformations: \n")
  cat(names(Significance), sep = ", ")
}


#'  Divides a set of numeric variables into disjoint or hierarchical cluster
#'   which can be used to diagnose collinearity between variables
#'
#' @param PredictorVariables Parameter names to be considered
#' @param df The dataframe containing the parameters
#' @export
#'
VariableCluster <- function(PredictorVariables, df){

  Formula <- formula(paste("Status.Summary ~ ", paste(PredictorVariables, collapse=" + ")))

  return(graphics::plot(Hmisc::varclus(Formula, data = df)))
}

# Presenting Model Results --------------------------


#' Creates a formatted odds table of the results using ggplot
#'
#' @param Model a logistic regression object
#' @param  roundby the number of decimal places to display results. Default 3
#' @import ggplot2
#' @export
#'
LogisticResultsTable <- function(Model, roundby = 3){

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
  CI <- suppressMessages(exp(stats::confint(Model)))
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

#' Function builds the
#'
#' @param Model a glmmodel
#' @param Sort indicates whether the parameters should be sorted
#' @param Title a character string of the title of the of ggplot
#' @import ggplot2
#' @export
#'
LogisticOddsPlot <- function(Model, Sort = FALSE, Title){

  a <- LogisticOddsTable(Model, Sort)
  b <- LogOddsPlotGraph(a, Title, Sort)
  return(b)
}

#' Builds a Logistic Odds Table
#'
#' Produces an odds table with confidence intervals for a logistic model
#'
#' @param Model a glmmodel
#' @param Sort boolean indicator whether the parameters should be sorted. Default FALSE
#' @export
#'
LogisticOddsTable <- function(Model, Sort = FALSE){

  #
  ModelDF <-  broom::tidy(Model)
  ModelDF$odds <- exp(Model$coefficients)
  CI <- suppressMessages(exp(stats::confint(Model)))
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


#' Produces boxplots for estimated values from a regression model
#'
#' @param OddsTable a formatted odds table from the function "LogisticOddsTable"
#' @param PlotTitle the title of the resulting plot
#' @param Sort reorder the plot by variable fit
#' @import ggplot2
#' @export
#'
LogOddsPlotGraph <- function(OddsTable, PlotTitle, Sort = FALSE){

  ModelDF <- OddsTable

  # Rename Variables
  names <- utils::read.csv("VariableDisplayNames.csv", stringsAsFactors = FALSE)
  ModelDF$term <- names[match(x = ModelDF$term, table = names$Label, nomatch = ""), 2]

  Terms <- ModelDF$term

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

#' Summarise results for a logistic odds model
#'
#' @param Model a glm object
#' @export
#'
LogResults <- function(Model){

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
    Parameters = length(stats::coef(Model)) %>% as.character(),
    Deviance = format(dev, nsmall=1),
    R.n = format(R.n, nsmall = 3),
    "Chi Square" = round(modelChi, 0),
    "Degrees of Freedom" = round(chidf, 0),
    p = format(round(1- stats::pchisq(modelChi, chidf),5), nsmall=3),
    "Residual Deviance" = round(Model$df.residual, 0),
    AIC = round(AIC(Model), 0))
}

#' Takes a list of logistic regression models and creates a summary table
#'
#' @param ... a list of glm objects
#' @export
#'
LogResultsTable <- function(...){

  lapply(list(...), LogResults) %>%
    do.call(cbind, .) %>%
    as.data.frame() %>%
    set_colnames(paste("Model", seq(1, length(list(...)))))
}



#  Segmented Logistic Regression Models -------------------------------------------------------------------------------------


#' Splits a dataset and forms a segmented Odds plot
#'
#' This function splits data based on a categorical variable, builds separate
#'   glm models for each dataset and compares the results using a Odds Plot
#'
#' @param df a dataframe to be split
#' @param split_category the column to be used to split the dataset by
#' @param predictors a list of variables to be used within the regression model
#' @param outcome the outcome variable
#' @param Title a string to be used within the ggplot title
#' @param linebreak the spacing to be used between gridlines in the plot
#' @export
#'
Segmented_FullOddsPlot <- function(df, split_category, predictors, outcome, Title, linebreak){

  SegDatasets <- Segmented_Dataset(df, split_category)

  # Build Logistic Regression Models for the segmented models
  SegLogisticModels <- Segmented_LogisticModels(SegDatasets, predictors, outcome)

  # Build Odds Tables for the list of logistic regression models
  LogisticModelsOddsTable <- Segmented_OddsTable(SegLogisticModels)

  # Plot the Faceted Graph
  FacetedBoxPlot <- Segmented_OddsPlot(LogisticModelsOddsTable, Title, linebreak)

  return(FacetedBoxPlot)
}


#' Returns the a list of results for split logistic regresion model.
#'
#' This function splits data based on a categorical variable, builds separate
#'   glm models for each dataset and compares the results using a Odds Plot.
#'   This returns the full stages of the analysis 1) dataset 2) glm models
#'   3) Summary Odds table 4) logistic regresion plot.
#'
#' @param df a dataframe to be split
#' @param split_category the column to be used to split the dataset by
#' @param variables_list a list of variables to be used within the regression model
#' @param outcome_variable the outcome variable
#' @param linebreak the spacing to be used between gridlines in the plot
#' @param limits the extent of the plot
#' @export
#'
Segmented_LogisticModelsComplete <- function(df, split_category, variables_list, outcome_variable, linebreak = 0.1, limits= 1){

  data <- Segmented_Dataset(df, by = split_category)
  models <- Segmented_LogisticModels(data, variables_list, outcome_variable)
  oddstable <- Segmented_OddsTable(LogisticModelList = models)
  plot <- Segmented_OddsPlotGroupedCustom(oddstable, linebreak, limits)

  return(list(data = data, models = models, oddstable = oddstable, plot = plot))
}


#' Runs a stepwise optimisation to select the best fitting parameters
#'
#' @param df a dataframe
#' @param variableList a list of parameters from the dataframe
#' @param direction "forward" or "backwards. Default "backward"
#' @param steps the number of steps for the model to assess
#' @export
#'
FindBestModel <- function(df, variableList, direction = "backward", steps = 1000){

  LogModel <- LogisticModel(variableList, df)
  Dataframe <- df[, c(which(names(df) == "Status.Summary"), which(names(df) %in% variableList))]
  Dataframe <- Dataframe[stats::complete.cases(Dataframe),]

  # Find Best Model
  BestModel <- stats::step(LogModel, direction = direction, trace = FALSE, steps = steps)
  return(BestModel)

}


#' Splits a dataset based on a categorical variable
#'
#' @param df a dataframe
#' @param by the column used to split the data
#' @return a list of dataframes
#' @export
#'
Segmented_Dataset <- function(df, by){

    columntoSplit <- df[[by]]
  variableLevels <- levels(columntoSplit)
  SegmentedDatasets <- NULL # for results

  for (i in 1:length(variableLevels)){
    SegmentedDataset <- df[columntoSplit == variableLevels[i], ]
    SegmentedDatasets[[variableLevels[i]]] <- SegmentedDataset # Add dataset to results
  }
  return(SegmentedDatasets)
}

#' Build Segmented Logistic Regression Models
#'
#' Combines the SplitDatasetbyVariable function and "Logstic Model"
#'
#' @param SegmentedDatasets the full dataframe to be split
#' @param predictors the name of the column for the dataset to be split using
#' @param outcome the outcome variable
#' @return a list of logistic regression models
#' @export
#'
Segmented_LogisticModels <- function(SegmentedDatasets, predictors, outcome){

  LogisticModels <- NULL # Empty dataframe for results

  for (i in 1:length(SegmentedDatasets)){
    datasetname <- (names(SegmentedDatasets[i]))
    ModelDataset <- SegmentedDatasets[[datasetname]]

    Formula <- formula(paste(outcome, "~ ", paste(predictors, collapse=" + ")))
    GlmModel <- stats::glm(Formula , data = ModelDataset, family = stats::binomial())
    LogisticModels[[datasetname]] <- GlmModel

  }
  return(LogisticModels)
}


#' Builds Segmented Regression Odds Tables
#'
#' Split dataset into segments and build logistic models into a list
#'
#' @param LogisticModelList a list of logistic regression models
#' @return a single odds table which contains the statistics for the segmented model
#' @export
#'
Segmented_OddsTable <- function(LogisticModelList){

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

#' Builds an odds plot for a segmented logistic regression model
#'
#' @param OddsTables a list of odds as produced from the function "OddsTableSegmented"
#' @param PlotTitle the title for the ggplot object
#' @param linebreak numeric. Spacing between minor line breaks in plot
#' @export
#
Segmented_OddsPlot <- function(OddsTables, PlotTitle, linebreak){

  requireNamespace("ggplot2")

  Combined <- NULL # Blank table for results

  # Load reference names
  names <- utils::read.csv("VariableDisplayNames.csv")

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


#' Plots a faceted odds ratio plot for a list of segmented odds tables
#'
#' @param OddsTables a list of odds as produced from the function "OddsTableSegmented"
#' @param scale the limits of the plot
#' @param linebreak numeric. Spacing between minor line breaks in plot
#' @export
#
Segmented_OddsPlotGroupedCustom <- function(OddsTables, linebreak = 0.2, scale = 1){

  requireNamespace("ggplot2")

  Combined <- NULL # Blank table for results

  # Load reference names
  names <- utils::read.csv("VariableDisplayNames.csv")

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

#' Produces summary statistics for an entire numeric dataframe
#'
#' @param dataframe the dataframe
#' @return a dataframe of summary statistics
#' @export
#'
FiltNumericColumns <- function(dataframe){
  nums <- sapply(dataframe, is.numeric)
  return(dataframe[, nums])
}

#' Summarise a Dataframe
#'
#' Produces summary statistics for rows in a dataframe
#'
#' @param dataframe a datatable
#' @param ... additional arguments parsed to `my.sumarry`
#' @export
SummariseDataframe <- function(dataframe){

  lapply(dataframe, function(x) my.summary(x, na.rm= TRUE)) %>%
    do.call(rbind, .) %>%
    as.data.frame()
}


#' Produces a summary of a column within a dataframe
#'
#' @param x a vector of values
#' @param rounded the number of decimal places to return
#' @param ... additional arguments parsed by summary statistic calculations
#'
#' @return a list of summary statistics for the variable
#' @export
#'
my.summary <- function(x, rounded = 1, ...){
  round(c(mean=mean(x, ...),
          sd=sd(x, ...),
          median=stats::median(x, ...),
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
#' @param path the filepath of the lookup file
#' @export
#'
matchNames <- function(inputNames, path = "VariableDisplayNames.csv"){
  names <- utils::read.csv(path, stringsAsFactors = FALSE)
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 2])
}

#' Matches variable names with their full descriptive name.
#' Used when plotting or showing results in a table
#'
#' This function is designed to work with the PhD project
#'
#' @param inputNames a list of names which are to looked up
#' @param path the filepath of the lookup file
#'
matchUnits <- function(inputNames, path = "VariableDisplayNames.csv"){
  names <- utils::read.csv(path, stringsAsFactors = FALSE)
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 3])
}

#' Matches variable names with their full descriptive name.
#' Used when plotting or showing results in a table
#'
#' This function is designed to work with the PhD project
#'
#' @param inputNames a list of names which are to looked up
#' @param path the filepath of the lookup file
#' @export
#'
matchCategory <- function(inputNames, path = "VariableDisplayNames.csv"){
  names <- utils::read.csv(path, stringsAsFactors = FALSE)
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 4])
}

#' Checks the row names against the variable full names
#'
#' This function is designed to work with the PhD project
#'
#' @param dataframe a list of names which are to looked up
#' @param dropNA a TRUE/FALSE selection whether names should be dropped if empty
#' @export
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
#' @param Rows the variable to counted in the rows
#' @param Columns the variable to be counted in the columns
#' @param SumPercentage the parameter to be summed in the column
#' @param inputDataframe the dataframe containing the row and column parameters
#' @export
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
#' @param variable the column reference to search
#' @param quantiles the number of bins in which the data should be grouped
#' @export
#'
ScatterPlotOdds <- function(df, variable, quantiles = 20){
  temp <- data.frame(Value = df[[variable]], Status = df$Planning_Status_Summary)
  temp <- temp[stats::complete.cases(temp),]
  temp$Bin <- Hmisc::cut2(temp[,1], g = quantiles, levels.mean = TRUE)  %>% as.character() %>% as.numeric()
  names(temp) <- c("Value", "PlanningStatus", "Bin")
  temp$PlanningStatus <- temp$PlanningStatus * 100

  aggdata <-stats::aggregate(temp, by=list(temp$Bin),FUN=mean, na.rm=TRUE) %>% .[c(4,3)]


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


# ---- Reporting -----

#' # Save a file from DiagrammeR as a PDF figure which can be included in a knitr report.
#' Saves PDF output in the same directory as the ".gv" file specified.
#'
#' @param filepath The input location of the .gz file
#' @param saveSuffix An optional term to be added to the end of the PDF.
#' @export
#'
grViz_pdf <- function(filepath, saveSuffix = ""){

  capture.output({
    g <- DiagrammeR::grViz(filepath)
    pdfFile <- gsub(pattern = ".gv", replacement = "", x = filepath) %>% paste0(".pdf")
    DiagrammeRsvg::export_svg(g) %>% charToRaw %>% rsvg::rsvg_pdf(pdfFile, width = 2000)
  },  file='NUL')
  knitr::include_graphics(pdfFile)
}


#' Function will update the name with the statistic of your choice
#'
#' @param df a dataframe
#' @param category the category being named
#' @param count_col the column being aggregated against
#' @param stat the statistics. "sd" is standard deviation, "mean" or "count".
#' @param dp decimal places of returned value
#' @export
#'
AddNameStat <- function(df, category, count_col, stat = c("sd","mean","count"), dp= 0){

  # Create temporary data frame for analysis
  temp <- data.frame(ref = df[[category]], comp = df[[count_col]])

  # Aggregate the variables and calculate statistics
  agg_stats <- plyr::ddply(temp, "ref", summarize,
                           sd = sd(comp),
                           mean = mean(comp),
                           count = length(comp))

  # Dictionary used to replace stat name with correct symbol for plot
  labelName <- plyr::mapvalues(stat,
                               from=c("sd","mean","count"),
                               to=c("\u03C3", "x", "n"))

  # Updates the name based on the selected variable
  agg_stats$join <- paste0(agg_stats$ref, ": \n(", labelName," = ",
                           round(agg_stats[[stat]], dp), ")")

  # Map the names
  name_map <- setNames(agg_stats$join, as.factor(agg_stats$ref))
  return(name_map[as.character(df[[category]])])
}


#' A theme for use within the publication
#'
#' @param base_size the font size of the plot Defaults to 14
#' @param base_family the font family of the plot. Defaults to "helvetica"
#' @import ggthemes
#' @export
#'
theme_Publication <- function(base_size=14, base_family="helvetica") {

  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))

}

#' A fill scale fill for the publication
#'
#' @param ... option arguments passed to `discrete_scale`
#' @export
#' @import scales
#'
scale_fill_Publication <- function(...){

  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}


#' A colour scale fill for the publication
#'
#' @param ... option arguments passed to `discrete_scale`
#' @export
#' @import scales
#'
#'
scale_colour_Publication <- function(...){
  discrete_scale("colour","Publication", manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

#' Runs an external Knitr script
#'
#' @param x A filepath to an external knitr script
#' @param ... options to be passed to the purl function
#' @export
#'
#'
ksource <- function(x, ...) {
  source(knitr::purl(x, output = tempfile()), ...)
}

# CROSS VALIDATION FUNCTIONS ----------------------
# Functions used to internally validate the logistic regression model


#' Assesses the accuracy of a logistic regression model
#'
#' @param dataframe a data frame
#' @param outcomeVariable a list of the outcome variables
#' @param predictorVariables a string of the column name for the outcome variable
#' @param iterations number of iterations to run for the model accuracy. Defaults 300
#' @param foldSize size of data proportion used to assess model accuracy. Defaults 0.05
#' @param fullstats TRUE/FALSE. Should the full results be returned? Default FALSE
#' @export
#'
ModelAccuracy <- function(dataframe, outcomeVariable, predictorVariables, iterations = 300, foldSize = 0.05, fullstats = FALSE){

  reducedDataframe <- ReducedDataframe(dataframe, outcomeVariable, predictorVariables)
  modelAccuracy <- CrossValidation(reducedDataframe, iterations, foldSize, fullstats)
  return(modelAccuracy)

}


#' Reduces a dataframe to a list of parameters for use within regression modelling
#'
#' @param dataframe a dataframe
#' @param outcomeVariable the outcome variable
#' @param inputVariables a list of input variables
#' @export
#'
ReducedDataframe <- function(dataframe, outcomeVariable, inputVariables){

  reducedDataFrame <- dataframe[, c(outcomeVariable, inputVariables)]
  reducedDataFrame <- reducedDataFrame[complete.cases(reducedDataFrame),]
  return(reducedDataFrame)
}


#' Assess the model accuracy
#'
#' Wrapper for the ModelAccuracy function to extract the variables from the model
#'   Would probably be better to rewrite the original function
#'
#' @param Model the glm object
#' @param Dataframe the full dataframe used to build the model
#' @param ... additional arguments passed to the `ModelAccuracy` function.
#' @export
#'
ModelAccuracyFromModel <- function(Model, Dataframe, ...){

  parameters <- names(coef(Model))[-1]
  accuracy <- invisible(ModelAccuracy(Dataframe, "Status.Summary", parameters, ...))
  return(accuracy)
}



#' Function for internally validating a glm model.
#'
#' @param dataframe the dataframe which contains the parameters to be modelled. The first column must contain the outcome variable
#' @param iterations the number of iterations for the function to run
#' @param foldSize the size of the test dataset expressed as a value between 0 and 1
#' @param fullstats TRUE/FALSE. Should the full stats be exported?
#' @export
#'
CrossValidation <- function(dataframe, iterations, foldSize, fullstats= FALSE){

  # Create Blank Results
  fpr <- NULL  # False positive rate
  fnr <- NULL  # False negative rate
  acc <- NULL  # Accuracy

  k <- iterations # Number of iterations
  set.seed(123)
  OutcomeVariable <- names(dataframe)[1]

  for(i in 1:k)
  {
    # Train-test splitting
    smp_size <- floor((1- foldSize) * nrow(dataframe))
    index <- sample(seq_len(nrow(dataframe)), size=smp_size)
    train <- dataframe[index, ]  # 95% of samples -> fitting
    test <- dataframe[-index, ]  # 5% of samples -> testing

    # Fitting
    Formula <- formula(paste(OutcomeVariable, "~.", sep = ""))
    model <- glm(Formula, family = binomial, data = train)

    # Predict results
    results_prob <- predict.glm(model, subset(test, select = c(2:ncol(test))), type = 'response')

    results <- ifelse(results_prob > 0.5, 1,0) %>% as.numeric() %>% factor(levels = c("1", "0"))  # User defined cutoff points
    answers <- ifelse(test[, 1] == "Approved", 0, 1) %>% as.numeric()
    answers <- factor(answers, levels = c("1", "0"))

    misClasificError <- mean(answers != results)  # Accuracy calculation
    acc[i] <- 1 - misClasificError  # Collecting results

    # Confusion matrix
    cm <- caret::confusionMatrix(data = results, reference = answers)
    fpr[i] <- cm$table[2] / (nrow(dataframe) - smp_size)
    fnr[i] <- cm$table[3] / (nrow(dataframe) - smp_size)
  }

  # Return Results
  results<- list(MeanAccuracy = scales::percent(mean(acc)),
                 MeanFPR = mean(fpr),
                 MeanFNR = mean(fnr),
                 Accuracy  = round(acc, 3),
                 FPR = round(fpr, 3),
                 FNR = round(fnr, 3)
  )

  if (fullstats == FALSE){
    return(results$MeanAccuracy)
  } else {
    return(results)
  }
}







context("Relational Event Modeling")

# does not test:


#### baseline tests ####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
options$actorData <- ""
options$syncAnalysisBox <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -8.42918896411138, 0, 0, 0.00928196673781619, -908.125314624275
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11607, 218889.192612882, "Null deviance", "", 11606, 218889.192612882,
                                      "Residual deviance", "", 1, 0, "Chi^2", 1, "", 218891.192612882,
                                      "AIC", "", "", 218891.192957561, "AICC", "", "", 218898.551976525,
                                      "BIC", ""))
})

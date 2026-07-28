#' I commented out some lines of code, which can be uncommented if one ever wants to run the tests manually
#'
#'
context("Relational Event Modeling")


# ----  baseline test tie model directed ----
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.54993551239156, 0, 0, 0.0319112823136298, -205.254538129105
                                      ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 14828.0733463371, "Null deviance", "", 981, 14828.0733463371,
                                       "Residual deviance", "", 1, 0, "Chi^2", 1, "", 14830.0733463371,
                                       "AIC", "", "", 14830.0774279697, "AICC", "", "", 14834.9629376454,
                                       "BIC", ""))
})

# ---- standard tie modeling ----
# tie model directed with effects
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
options$timepointInputUpper <- "Inf"
options$regularization <- ""

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor1.csv"), value = "#"),
                              list(actorData = testthat::test_path("team4_attributes_actor2.csv"), value = "#2"))
options$dyadDataList <- list(list(dyadData = testthat::test_path("team4_social_dyadic.csv"), value = "#"),
                             list(dyadData = testthat::test_path("team4_advice_dyadic.csv"), value = "#2"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTableActors <- list(
  list(maximum = TRUE, minimum = TRUE, value = "gender"),
  list(difference = TRUE, value = "age")
)

options$exogenousEffectsTableDyads <- list(
  list(tie = TRUE, value = "team4_advice_dyadic"),
  list(tie = TRUE, value = "team4_social_dyadic")
)

options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "maximum('gender')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std", value = "minimum('gender')"),
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_advice_dyadic')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "prop",
                                       endogenousEffectsConsiderType = "separate"),
                                  list(value = "isp", translatedName = "Incoming shared partners", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "separate"),
                                  list(value = "otp", translatedName = "Outgoing two-path", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = TRUE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "ignore"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "minimum('gender') : Incoming shared partners(type)"),
  list(includeInteractionEffect = TRUE, value = "difference('age') : Inertia(type)"),
  list(includeInteractionEffect = TRUE, value = "tie('team4_advice_dyadic') : Inertia(type)")
)

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options, makeTests = FALSE)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.88407044272748, 6.09335175875961e-136, 0, 0.274359761473974,
                                       -25.0913997218229, "inertia", 1.88014750597345, 0.0182339778758246,
                                       0.000115686762064415, 0.48770513706164, 3.8550906338635, "Incoming shared partners(type)",
                                       0.0024711240140328, 5.44071158836114e-57, 0, 0.000151515339309625,
                                       16.3093982780384, "Outgoing two-path", 0.549168198430819, 7.21984382223754e-89,
                                       0, 0.0270294744961108, 20.3173834737274, "Maximum_gender", 0.099688251477181,
                                       0.953313262331482, 0.354685041312339, 0.107708194149353, 0.925540087868792,
                                       "Minimum_gender", -0.0630437964206211, 0.923054753551795, 0.165812310337666,
                                       0.045493174366773, -1.38578583047101, "Difference_age", -0.0508301982020613,
                                       5.61540523443909e-09, 2.08941752788405e-11, 0.00758701122438156,
                                       -6.6996339795457, "Tie_team4_advice_dyadic", 0.0704279751468146,
                                       0.930703439328457, 0.193008766982217, 0.0541033744953719, 1.30172980527932,
                                       "Tie_team4_social_dyadic", -0.32866300114638, 0.00167701581729122,
                                       9.2145162144952e-06, 0.0741095000393228, -4.43482955588676,
                                       "Minimum_gender:Incoming shared partners(type)", 0.000332975746486192,
                                       0.716945508262184, 0.0248993806755768, 0.000148453702129382,
                                       2.24296020718967, "Difference_age:inertia", -0.0208068235829708,
                                       0.929423498825845, 0.187919097443327, 0.0158015311722801, -1.31675996181125,
                                       "Tie_team4_advice_dyadic:inertia", 0.401300713777363, 0.238565112093028,
                                       0.00240602563391024, 0.132227940774127, 3.03491615635813))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 14828.0733463371, "Null deviance", "", 970, 13699.9152188104,
                                       "Residual deviance", "", 12, 1128.15812752666, "Chi^2", 0, "",
                                       13723.9152188104, "AIC", "", "", 13724.2372002345, "AICC", "",
                                       "", 13782.5903145107, "BIC", ""))
})



#### tie model directed changed estimation options ####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$eventHistory <- "window"
options$eventHistorySingleInput <- 100
options$timepointInputLower <- 2
options$timepointInputUpper <- "200"
options$regularization <- ""

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.90807937519436, 0, 0, 0.0708881204999232, -97.4504518736936
                                      ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(199, 3147.41559133758, "Null deviance", "", 198, 3147.41559133758,
                                       "Residual deviance", "", 1, 0, "Chi^2", 1, "", 3149.41559133758,
                                       "AIC", "", "", 3149.4358959061, "AICC", "", "", 3152.7088961623,
                                       "BIC", ""))
})


# ---- model options ----
##### tie undirected #####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$timepointInputUpper <- "Inf"
options$eventHistory <- "decay"
options$eventHistorySingleInput <- 100
options$eventDirection <- "undirected"
options$syncAnalysisBox <- TRUE
options$regularization <- ""

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor2.csv"), value = "#"))
options$dyadDataList <- list(list(dyadData = testthat::test_path("team4_social_dyadic.csv"), value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "age")
)
options$exogenousEffectsTableDyads <- list(
  list(tie = TRUE, value = "team4_social_dyadic")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "difference('age') : Inertia")
)

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.11610846175515, 0, 0, 0.14089616908991, -43.4086214072454,
                                       "Inertia", 0.666373581088053, 7.23723535768428e-45, 0, 0.0459662256218572,
                                       14.4970262855601, "Difference_age", -0.0261036293115428, 0.0134619798355373,
                                       8.34598340577752e-05, 0.00663498396541122, -3.9342415064789,
                                       "Tie_team4_social_dyadic", -0.0188887437311464, 0.967626581993817,
                                       0.758440715945587, 0.0614210066145066, -0.30752904864775, "Difference_age:Inertia",
                                       0.00872948929277308, 0.541598626306569, 0.0104529240471627,
                                       0.00340932441607323, 2.56047481184776))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 13466.7322837173, "Null deviance", "", 977, 12822.6070983531,
                                       "Residual deviance", "", 5, 644.12518536419, "Chi^2", 0, "",
                                       12832.6070983531, "AIC", "", "", 12832.668573763, "AICC", "",
                                       "", 12857.0550548949, "BIC", ""))
})


# tie undirected, active riskset
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$timepointInputUpper <- "Inf"
options$eventHistory <- "decay"
options$eventHistorySingleInput <- 100
options$eventDirection <- "undirected"
options$syncAnalysisBox <- TRUE
options$regularization <- ""

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor2.csv"), value = "#"))
options$dyadDataList <- list(list(dyadData = testthat::test_path("team4_social_dyadic.csv"), value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "age")
)
options$exogenousEffectsTableDyads <- list(
  list(tie = TRUE, value = "team4_social_dyadic")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "difference('age') : Inertia")
)
options$timepointInputUpper <- "Inf"
options$eventHistory <- "full"
options$eventDirection <- "directed"
options$riskset <- "active"

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.21928141744167, 0, 0, 0.149021868748825, -41.734018434061,
                                       "Inertia", 0.0239114870517698, 7.06244696270498e-26, 0, 0.00215853335076441,
                                       11.0776546692234, "Difference_age", -0.0175203941402824, 0.495921427312757,
                                       0.00851311980828329, 0.00665918612620721, -2.6310113290468,
                                       "Tie_team4_social_dyadic", 0.0635843525338618, 0.949093676540691,
                                       0.308158804437228, 0.0623930797986709, 1.0190930266471, "Difference_age:Inertia",
                                       0.000949044259578596, 0.00026002149991298, 1.31676818981852e-06,
                                       0.000196196776262225, 4.8372061848262))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 13466.7322837173, "Null deviance", "", 977, 13138.4550723062,
                                       "Residual deviance", "", 5, 328.277211411112, "Chi^2", 0, "",
                                       13148.4550723062, "AIC", "", "", 13148.516547716, "AICC", "",
                                       "", 13172.903028848, "BIC", ""))
})

# tie directed, ordinal
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$timepointInputUpper <- "Inf"
options$eventDirection <- "directed"
options$syncAnalysisBox <- TRUE
options$eventSequence <- "orderOnly"
options$regularization <- ""

options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "gender")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('gender')")
)
options$endogenousEffects <- list(list(value = "indegreeSender", translatedName = "Indegree sender", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"),
                                  list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "ignore"),
                                  list(value = "osp", translatedName = "Outgoing shared partners", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = TRUE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore")
                                  )

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor1.csv"), value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Indegree sender", -0.00899648221870555, 1.65808084405486e-36,
                                       0, 0.000686645551139249, -13.1020760329387, "Inertia", 0.613439118380534,
                                       5.98558781454482e-222, 0, 0.0191544656340011, 32.0259061308198,
                                       "Outgoing shared partners", -0.161775474740192, 9.42515590805944e-20,
                                       0, 0.0166411165558782, -9.72143150352783, "Difference_gender",
                                       -0.138359885936909, 0.700906660788373, 0.0227649374093819, 0.060754597570509,
                                       -2.27735663587162))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 8399.37225774746, "Null deviance", "", 978, 7358.39288076112,
                                       "Residual deviance", "", 4, 1040.97937698634, "Chi^2", 0, "",
                                       7366.39288076112, "AIC", "", "", 7366.43382241926, "AICC", "",
                                       "", 7385.95124599454, "BIC", ""))
})


# tie undirected, active riskset, ordinal
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$timepointInputUpper <- "Inf"
options$eventHistory <- "decay"
options$eventHistorySingleInput <- 100
options$eventDirection <- "undirected"
options$syncAnalysisBox <- TRUE
options$regularization <- ""

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor2.csv"), value = "#"))
options$dyadDataList <- list(list(dyadData = testthat::test_path("team4_social_dyadic.csv"), value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "age")
)
options$exogenousEffectsTableDyads <- list(
  list(tie = TRUE, value = "team4_social_dyadic")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "difference('age') : Inertia")
)
options$timepointInputUpper <- "Inf"
options$eventHistory <- "full"
options$eventDirection <- "directed"
options$riskset <- "active"
options$eventSequence <- "timeSensitive"

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.21928141744167, 0, 0, 0.149021868748825, -41.734018434061,
                                       "Inertia", 0.0239114870517698, 7.06244696270498e-26, 0, 0.00215853335076441,
                                       11.0776546692234, "Difference_age", -0.0175203941402824, 0.495921427312757,
                                       0.00851311980828329, 0.00665918612620721, -2.6310113290468,
                                       "Tie_team4_social_dyadic", 0.0635843525338618, 0.949093676540691,
                                       0.308158804437228, 0.0623930797986709, 1.0190930266471, "Difference_age:Inertia",
                                       0.000949044259578596, 0.00026002149991298, 1.31676818981852e-06,
                                       0.000196196776262225, 4.8372061848262))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 13466.7322837173, "Null deviance", "", 977, 13138.4550723062,
                                       "Residual deviance", "", 5, 328.277211411112, "Chi^2", 0, "",
                                       13148.4550723062, "AIC", "", "", 13148.516547716, "AICC", "",
                                       "", 13172.903028848, "BIC", ""))
})



#### actor model ####
# basic input
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$orientation <- "actor"
options$regularization <- ""

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Coefficient estimates sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -7.80841571987397, 0, 0, 0.093658581157968, -83.371065665665
                                      ))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 2008.31878413225, "Null deviance", "", 113, 2008.31878413225,
                                       "Residual deviance", "", 1, 0, "Chi^2", 1, "", 2010.31878413225,
                                       "AIC", "", "", 2010.35449841796, "AICC", "", "", 2013.05498258064,
                                       "BIC", ""))
})

# actor model with effects
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "weight"
options$typeVariable <- "setting"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$orientation <- "actor"
options$regularization <- "horseshoe"
options$regularizationCiLevel <- .95
options$regularizationIterations <- 1000
options$regularizationSetSeed <- TRUE
options$regularizationSeed <- 1234

options$exogenousEffectsTableActors <- list(
  list(average = TRUE, value = "age"),
  list(difference = TRUE, value = "age")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std", value = "average('age')"),
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')")
)
options$exogenousEffectsTableSender <- list(
  list(send = TRUE, value = "extraversion"),
  list( send = TRUE, value = "sex")
  )

options$specifiedExogenousEffectsSender <- list(
  list(exogenousEffectsAbsoluteSender = FALSE, exogenousEffectsScalingSender = "none", value = "send('sex')"),
  list(exogenousEffectsAbsoluteSender = FALSE, exogenousEffectsScalingSender = "std", value = "send('extraversion')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))
options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender", includeEndoEffectSender = TRUE,
                                             endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                             endogenousEffectsConsiderTypeSender = "ignore"),
                                        list(value = "outdegreeSender", translatedNameSender = "Outdegree sender", includeEndoEffectSender = TRUE,
                                            endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "prop",
                                            endogenousEffectsConsiderTypeSender = "ignore")
                                       )
options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "average('age') : Inertia"),
  list(includeInteractionEffect = TRUE, value = "difference('age') : Inertia")
)
options$interactionEffectsSender <- list(
  list(includeInteractionEffectSender = TRUE, value = "send('extraversion') : Indegree sender")
)

options$actorDataList <- list(list(actorData = testthat::test_path("history_info_actor.csv"), value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options, makeTests = F)

test_that("Coefficient Estimates Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Inertia", -0.0951754789409397, 0.877933695547456, 0.374044174961988,
                                       0.107068240393298, -0.88892353690812, "Average_age", -0.0924604558162358,
                                       0.899967118954679, 0.558401275697253, 0.157993238336427, -0.585217802924913,
                                       "Difference_age", -0.640854428123168, 0.682803428824876, 0.0735110642527186,
                                       0.35809033214251, -1.78964459690614, "Average_age:Inertia",
                                       -0.0147611138441488, 0.913547574498423, 0.885555107374967, 0.102557013375509,
                                       -0.143930808418743, "Difference_age:Inertia", -0.0986600699876799,
                                       0.903718760043568, 0.611698475616632, 0.194346448970213, -0.507650489681965
                                      ))
})

test_that("Coefficient Estimates Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -8.24293268074641, 2.82332813049861e-266, 0, 0.235259413400152,
                                       -35.0376317003138, "Indegree sender", 0.0173806455843033, 0.846279339724174,
                                       0.249737135467444, 0.0151006302672603, 1.15098808968168, "Outdegree sender",
                                       1.7531500945068, 0.789651064073216, 0.148212544308283, 1.21251927121958,
                                       1.4458740047434, "Send_sex", 0.595652484221158, 0.474073720636972,
                                       0.0261844872160457, 0.267893845310236, 2.22346460976496, "Send_extraversion",
                                       -0.164499099985981, 0.885106844440097, 0.419118032674118, 0.203600010381127,
                                       -0.80795231629924, "Send_extraversion:Indegree sender", 0.0164719985461306,
                                       0.873170499440953, 0.348853243943556, 0.0175829614166462, 0.936815941058495
                                      ))
})

test_that("Model Fit Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 500.967203632658, "Null deviance", "", 109, 489.612687157003,
                                       "Residual deviance", "", 5, 11.3545164756547, "Chi^2", 0.044786812826805,
                                       "", 499.612687157003, "AIC", "", "", 500.168242712558, "AICC",
                                       "", "", 513.293679398975, "BIC", ""))
})

test_that("Model Fit Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 2008.31878413225, "Null deviance", "", 108, 1997.64470453795,
                                       "Residual deviance", "", 6, 10.6740795942965, "Chi^2", 0.0989869876222471,
                                       "", 2009.64470453795, "AIC", "", "", 2010.42975126692, "AICC",
                                       "", "", 2026.06189522832, "BIC", ""))
})

test_that("Regularization Results Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regContainer"]][["collection"]][["mainContainer_regContainer_regTableReceiver"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Inertia", -0.0951754789409397, "FALSE", -0.223637472278997, -0.0397362999699926,
                                       -0.0397362999699926, -0.00444679564861779, 0.0901455011903437,
                                       "Average_age", -0.0924604558162358, "FALSE", -0.323841937685539,
                                       -0.0720283463217621, -0.0720283463217621, -0.00608017838336934,
                                       0.0907049033422867, "Difference_age", -0.640854428123168, "FALSE",
                                       -1.00293493227272, -0.274220170561488, -0.274220170561488, -0.0184661447401417,
                                       0.104915715300126, "Average_age:Inertia", -0.0147611138441488,
                                       "FALSE", -0.141597212380919, -0.00707682335775935, -0.00707682335775935,
                                       0.000158933390875268, 0.117123812887903, "Difference_age:Inertia",
                                       -0.0986600699876799, "FALSE", -0.402696974747945, -0.0912630192153079,
                                       -0.0912630192153079, -0.0081997253971901, 0.0979716328670142
                                      ))
})

test_that("Regularization Results Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regContainer"]][["collection"]][["mainContainer_regContainer_regTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -8.24293268074641, "TRUE", -8.48230803572206, -7.99524158232376,
                                       -7.99524158232376, -7.90770509269842, -7.62724036700257, "Indegree sender",
                                       0.0173806455843033, "FALSE", -0.0166523244466993, 0.00859966530309672,
                                       0.00859966530309672, 0.00193871945022288, 0.042178884605473,
                                       "Outdegree sender", 1.7531500945068, "FALSE", -0.427393199970551,
                                       0.714901307362825, 0.714901307362825, 0.0545021403064673, 3.08830088566605,
                                       "Send_sex", 0.595652484221158, "FALSE", -0.066256217993435,
                                       0.333066750498852, 0.333066750498852, 0.0339563038308422, 0.95998678636594,
                                       "Send_extraversion", -0.164499099985981, "FALSE", -0.466266061599046,
                                       -0.121381621417571, -0.121381621417571, -0.00784405777929769,
                                       0.102358276052897, "Send_extraversion:Indegree sender", 0.0164719985461306,
                                       "FALSE", -0.00893261234981526, 0.0124524679074123, 0.0124524679074123,
                                       0.00959234599227139, 0.0422569496824215))
})


# actor model, riskset active
# just use the same effects as before, plus:
options$riskset <- "active"
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options, makeTests = F)

test_that("Model Fit Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 500.967203632658, "Null deviance", "", 109, 413.490088076064,
                                       "Residual deviance", "", 5, 87.4771155565934, "Chi^2", 0, "",
                                       423.490088076064, "AIC", "", "", 424.04564363162, "AICC", "",
                                       "", 437.171080318037, "BIC", ""))
})

test_that("Model Fit Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 2008.31878413225, "Null deviance", "", 108, 1997.64470453795,
                                       "Residual deviance", "", 6, 10.6740795942965, "Chi^2", 0.0989869876222471,
                                       "", 2009.64470453795, "AIC", "", "", 2010.42975126692, "AICC",
                                       "", "", 2026.06189522832, "BIC", ""))
})


# actor model, ordinal, simultaneous events split

options <- jaspTools::analysisOptions("relationalEventModeling")
options$orientation <- "actor"
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$timepointInputUpper <- "Inf"
options$syncAnalysisBox <- TRUE
options$eventSequence <- "orderOnly"
options$regularization <- ""

options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "age")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')")
)
options$exogenousEffectsTableSender <- list(
  list(send = TRUE, value = "age")
)

options$specifiedExogenousEffectsSender <- list(
  list(exogenousEffectsAbsoluteSender = FALSE, exogenousEffectsScalingSender = "none", value = "send('age')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))
options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender", includeEndoEffectSender = TRUE,
                                             endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                             endogenousEffectsConsiderTypeSender = "ignore")
)

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor2.csv"), value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Indegree sender", -0.0106304185782523, 3.19587202417955e-31,
                                       0, 0.000875811109424194, -12.1377982807746, "Send_age", -0.0260349320763536,
                                       1.87785360293575e-06, 8.06095812322383e-09, 0.00451432729288288,
                                       -5.76717867076215))
})

test_that("Model fit receiver model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 4084.0231878592, "Null deviance", "", 980, 3669.93096303438,
                                       "Residual deviance", "", 2, 414.092224824817, "Chi^2", 0, "",
                                       3673.93096303438, "AIC", "", "", 3673.9432204399, "AICC", "",
                                       "", 3683.71014565109, "BIC", ""))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 4315.34906988834, "Null deviance", "", 980, 4137.77031957953,
                                       "Residual deviance", "", 2, 177.578750308802, "Chi^2", 0, "",
                                       4141.77031957953, "AIC", "", "", 4141.78257698505, "AICC", "",
                                       "", 4151.54950219624, "BIC", ""))
})



# ---- input ----
# dyadic attributes format test
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""

options$actorDataList <- list(list(actorData = testthat::test_path("history_info_actor.csv"), value = "#"))
options$dyadDataList <- list(list(dyadData = testthat::test_path("history_wide_dyadic.csv"), value = "#"),
                             list(dyadData = testthat::test_path("history_long_dyadic1.csv"), value = "#2"),
                             list(dyadData = testthat::test_path("history_long_dyadic2.csv"), value = "#4"))
options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "age"),
  list(average = TRUE, value = "extraversion"))
options$exogenousEffectsTableDyads <- list(
  list(tie = TRUE, value = "dy1")
)
options$specifiedExogenousEffects <- list(list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none",
                                               value = "difference('age')"),
                                          list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std",
                                               value = "average('extraversion')"),
                                          list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std",
                                               value = "tie('dy1')"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options, makeTests = F)

test_that("Coefficient Estimates Tie Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.8087303973617, 0, 0, 0.117843767764239, -83.235037231543,
                                       "Difference_age", -0.633477782461078, 0.38082265400407, 0.0168847450782184,
                                       0.265141340527932, -2.38920788889329, "Average_extraversion",
                                       -0.069991249612869, 0.890509383102476, 0.460660690685024, 0.0948700253806424,
                                       -0.73775936426755, "Tie_dy1", -0.175079898356758, 0.778025834179373,
                                       0.13554664038534, 0.117300020660646, -1.49258199078474))
})

test_that("Model Fit Tie Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 2509.2859877649, "Null deviance", "", 110, 2503.13315001429,
                                       "Residual deviance", "", 4, 6.15283775061653, "Chi^2", 0.188021304839229,
                                       "", 2511.13315001429, "AIC", "", "", 2511.50012249135, "AICC",
                                       "", "", 2522.07794380786, "BIC", ""))
})



# .txt actor attributes data
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""

options$exogenousEffectsTableActors <- list(
  list(difference = TRUE, value = "age")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')")
)
options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor2.txt"), value = "#"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.34043941277379, 0, 0, 0.0514639341263393, -123.201607502617,
                                       "Difference_age", -0.021461498186301, 0.000205470295488, 1.03086977887301e-06,
                                       0.00439276003011727, -4.88565230951804))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(982, 14828.0733463371, "Null deviance", "", 980, 14804.0459719474,
                                       "Residual deviance", "", 2, 24.0273743896578, "Chi^2", 6.06068823116868e-06,
                                       "", 14808.0459719474, "AIC", "", "", 14808.0582293529, "AICC",
                                       "", "", 14817.8251545641, "BIC", ""))
})



# ---- plots ----
# tie model
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"),
                                  list(value = "indegreeSender", translatedName = "Indegree sender", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))
options$diagnosticPlots <- TRUE
options$diagnosticPlotWaitTime <- TRUE
options$residualPlotSelect <- list(list(includePlotEffect = TRUE, value = "Indegree sender"),
                                   list(includePlotEffect = TRUE, value = "Inertia"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)


test_that("Indegree sender plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainer_indegreeSender"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "indegree-sender")
})

test_that("Inertia plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainer_inertia"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "inertia")
})

test_that("Waiting times fit plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_waitingTimePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "waiting-times-fit-tie")
})



# actor model
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$orientation <- "actor"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"),
                                  list(value = "indegreeReceiver", translatedName = "Indegree receiver", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))

options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender", includeEndoEffectSender = TRUE,
                                       endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                       endogenousEffectsConsiderTypeSender = "ignore"),
                                  list(value = "outdegreeSender", translatedNameSender = "Outdegree sender", includeEndoEffectSender = TRUE,
                                       endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                       endogenousEffectsConsiderTypeSender = "ignore"))
options$diagnosticPlots <- TRUE
options$diagnosticPlotWaitTime <- TRUE
options$residualPlotSelect <- list(list(includePlotEffect = TRUE, value = "Indegree receiver"),
                                   list(includePlotEffect = TRUE, value = "Inertia"),
                                   list(includePlotEffect = TRUE, value = "Indegree sender"),
                                   list(includePlotEffect = TRUE, value = "Outdegree sender"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options, makeTests = FALSE)

test_that("titleless-plot-4 matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainerReceiver"]][["collection"]][["mainContainer_plotContainer_residualsContainerReceiver_indegreeReceiver"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-4")
})

test_that("titleless-plot-5 matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainerReceiver"]][["collection"]][["mainContainer_plotContainer_residualsContainerReceiver_inertia"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-5")
})

test_that("titleless-plot-6 matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainerSender"]][["collection"]][["mainContainer_plotContainer_residualsContainerSender_indegreeSender"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-6")
})

test_that("titleless-plot-7 matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_residualsContainerSender"]][["collection"]][["mainContainer_plotContainer_residualsContainerSender_outdegreeSender"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "titleless-plot-7")
})

test_that("Waiting Times Fit plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_waitingTimePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "waiting-times-fit-actor")
})


# regularization and manual riskset
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$orientation <- "tie"
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"),
                                  list(value = "indegreeSender", translatedName = "Indegree sender", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))
options$regularization <- "horseshoe"
options$regularizationSetSeed <- TRUE
options$regularizationSeed <- 1234
options$regularizationIterations <- 2000
options$regularizationCiLevel <- .95
options$riskset <- "manual"
options$dyadInclude <- testthat::test_path("history_dyads_include.csv")

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options, makeTests = FALSE)


test_that("Regularization results tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regContainer"]][["collection"]][["mainContainer_regContainer_regTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.67540496073896, "TRUE", -9.99167301450269, -9.65533760523644,
                                       -9.65533760523644, -9.62832610729822, -9.32954281063711, "Inertia",
                                       -0.443038714289726, "TRUE", -0.688927301045705, -0.412285433580491,
                                       -0.412285433580491, -0.433311831677773, -0.124385200787328,
                                       "Indegree sender", 0.0450959526179004, "FALSE", -0.00768602278777249,
                                       0.0386953874755496, 0.0386953874755496, 0.0424774152268278,
                                       0.0859399422115661))
})

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.67540496073896, 0, 0, 0.164272303768048, -58.898577172209,
                                       "Inertia", -0.443038714289726, 0.0627306157497622, 0.00144737072301959,
                                       0.139100158367428, -3.18503385969881, "Indegree sender", 0.0450959526179004,
                                       0.657645232531809, 0.0640003088401224, 0.0243475301747488, 1.85217770731711
                                      ))
})


# ---- consider type: multi-type (two event types via 'setting') ----
# separate: one effect per event type
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$typeVariable <- "setting"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "separate"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Coefficient estimates separate-type tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.97902818590901, 0, 0, 0.11629032024679, -85.8113397979435,
                                       "Inertia (social)", -0.153264307209122, 0.891767036544162, 0.471541869247075,
                                       0.212875196001556, -0.719972594684077, "Inertia (work)", 0.0132587783146718,
                                       0.913935948316998, 0.916997538344354, 0.127223405338997, 0.104216502296434
                                      ))
})

test_that("Model fit separate-type tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 2509.2859877649, "Null deviance", "", 111, 2508.71573074288,
                                       "Residual deviance", "", 3, 0.570257022026453, "Chi^2", 0.903206288213203,
                                       "", 2514.71573074288, "AIC", "", "", 2514.93391256106, "AICC",
                                       "", "", 2522.92432608806, "BIC", ""))
})

# interact: one effect per ordered pair of event types (requires extend_riskset_by_type)
options$extendRisksetByType <- TRUE
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "interact"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Coefficient estimates interact-type tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -10.6639834266878, 0, 0, 0.116002932980178, -91.9285672588123,
                                       "Inertia (social <unicode> social)", -0.366690246694737, 0.856809568666721,
                                       0.281856205654616, 0.340739878626238, -1.07615888158769, "Inertia (social <unicode> work)",
                                       -0.00708432215006572, 0.914332274209219, 0.9779678998228, 0.256523563347494,
                                       -0.0276166526677672, "Inertia (work <unicode> social)", -0.291798316200179,
                                       0.82336308047711, 0.197928144873102, 0.226643549851728, -1.28747681719191,
                                       "Inertia (work <unicode> work)", 0.186062992881817, 0.81667481394368,
                                       0.18609819682583, 0.140721200102433, 1.32221010584318))
})

test_that("Model fit interact-type tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(114, 2667.32354493257, "Null deviance", "", 109, 2661.34382157553,
                                       "Residual deviance", "", 5, 5.97972335704026, "Chi^2", 0.308196927446234,
                                       "", 2671.34382157553, "AIC", "", "", 2671.89937713109, "AICC",
                                       "", "", 2685.0248138175, "BIC", ""))
})


# ---- interaction with a type-considered (multi-type) endogenous effect ----
# regression test: interactions with a "separate"/"interact" endo effect used to crash
# (the interaction picker only ever offers a generic "(type)" placeholder, which the old
# matching logic couldn't resolve once that effect was expanded into multiple type slices)
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$typeVariable <- "setting"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$extendRisksetByType <- TRUE

options$actorDataList <- list(list(actorData = testthat::test_path("history_info_actor.csv"), value = "#"))
options$exogenousEffectsTableActors <- list(list(minimum = TRUE, value = "age"))
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "minimum('age')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "interact"))
options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "Inertia(type) : minimum('age')")
)

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Coefficient estimates for an interaction with a multi-type endo effect match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -10.6745406924782, 0, 0, 0.117769975912614, -90.638896796572,
                                       "Inertia (social <unicode> social)", -0.36180436670133, 0.858876502325773,
                                       0.289011822189708, 0.341230906336063, -1.06029190200317, "Inertia (social <unicode> work)",
                                       -0.00821996133617336, 0.914322567685501, 0.974640734800902,
                                       0.258583037567496, -0.0317884785231814, "Inertia (work <unicode> social)",
                                       -0.296814282858049, 0.819491314122479, 0.190934953464081, 0.226953765724053,
                                       -1.30781827704475, "Inertia (work <unicode> work)", 0.179400292880255,
                                       0.831752820881112, 0.21461836477658, 0.144565052221205, 1.24096584979437,
                                       "Minimum_age", 0.431113658629683, 0.892297496210068, 0.47629048162079,
                                       0.605257419371765, 0.712281493512567, "Inertia (social <unicode> social):Minimum_age",
                                       0.0470134791013786, 0.914164681717325, 0.943414024755239, 0.662352378450391,
                                       0.07097955805846))
})


# ---- recall diagnostics plot ----
# tie model
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))
options$diagnosticPlots <- TRUE
options$diagnosticPlotWaitTime <- FALSE
options$diagnosticPlotRecall <- TRUE

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Recall plot matches (tie model)", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_recallContainer"]][["collection"]][["mainContainer_plotContainer_recallContainer_tie"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "recall-tie")
})

# actor model
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$orientation <- "actor"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "ignore"))
options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender",
                                             includeEndoEffectSender = TRUE, endogenousEffectsScalingSender = "none",
                                             endogenousEffectsConsiderTypeSender = "ignore"))
options$diagnosticPlots <- TRUE
options$diagnosticPlotWaitTime <- FALSE
options$diagnosticPlotRecall <- TRUE

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Recall plot matches (actor model, sender)", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_recallContainer"]][["collection"]][["mainContainer_plotContainer_recallContainer_sender"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "recall-sender")
})

test_that("Recall plot matches (actor model, receiver)", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_recallContainer"]][["collection"]][["mainContainer_plotContainer_recallContainer_receiver"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "recall-receiver")
})


# ---- fitting window: model must start at the second event and be origin-invariant ----
# remstats >= 4.0 fits from event 2 onwards (event 1 only initialises the statistics). Fitting from
# event 1 makes the likelihood depend on the absolute value of the first timestamp, because remify
# uses origin = 0 for numeric time. history_events.csv starts at t = 238, which makes that visible.
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "ignore"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

# same data, every timestamp shifted by a constant: the fit must not move
shiftedEvents <- read.csv(testthat::test_path("history_events.csv"))
shiftedEvents$time <- shiftedEvents$time + 2e5
shiftedPath <- file.path(tempdir(), "history_events_shifted.csv")
write.csv(shiftedEvents, shiftedPath, row.names = FALSE)

set.seed(1)
resultsShifted <- jaspTools::runAnalysis("relationalEventModeling", shiftedPath, options)

test_that("Model is fitted from the second event onwards", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  nEvents <- nrow(read.csv(testthat::test_path("history_events.csv")))
  testthat::expect_equal(table[[1]][["df"]], nEvents - 1)
})

test_that("Fit does not depend on the origin of the time variable", {
  coefs <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  coefsShifted <- resultsShifted[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  testthat::expect_equal(coefsShifted, coefs)
})

# ---- riskset = active_saturated ----
# remstimate < 3.1.0 only mapped dyad ids for riskset "active"/"manual"; for "active_saturated" it
# fell back to the full-riskset ids and read past the end of the statistics array, which crashed the
# JASP engine. Fixed upstream in remstimate 3.1.0; this pins that all three risksets stay fittable.
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$regularization <- ""
options$riskset <- "active_saturated"
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "ignore"),
                                  list(value = "reciprocity", translatedName = "Reciprocity", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "ignore"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options)

test_that("Coefficient estimates with a saturated active riskset match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.95786279706687, 0, 0, 0.0958044817882727, -103.939425496541,
                                      "Inertia", -0.0783886655111873, 0.886645341764227, 0.430174879687953,
                                      0.0993654678258922, -0.78889243140826, "Reciprocity", -0.207144100561539,
                                      0.634896473847828, 0.0567592142183457, 0.108727901524946, -1.90516047542785))
})

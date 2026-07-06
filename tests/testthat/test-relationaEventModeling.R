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
                                 list("baseline", -6.54993551239805, 0, 0, 0.0318950466054745, -205.359019957469
                                      ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14843.1732173746, "Null deviance", "", 982, 14843.1732173746,
                                       "Residual deviance", "", 1, 0, "Chi^2", 1, "", 14845.1732173746,
                                       "AIC", "", "", 14845.1772948466, "AICC", "", "", 14850.0638264948,
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
                                       endogenousEffectsConsiderType = "yes"),
                                  list(value = "isp", translatedName = "Incoming shared partners", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "yes"),
                                  list(value = "otp", translatedName = "Outgoing two-path", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = TRUE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "no"))

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
                                 list("baseline", -6.88595246779805, 3.03922657251031e-136, 0, 0.274131837041246,
                                       -25.1191271401358, "inertia", 1.88339131479427, 0.0176565924986628,
                                       0.00011167675521162, 0.487456409623567, 3.8637122778808, "Incoming shared partners(type)",
                                       0.00246893299362001, 6.36319464212989e-57, 0, 0.000151469921259041,
                                       16.2998235761785, "Outgoing two-path", 0.548437129786952, 9.25959421377496e-89,
                                       0, 0.0270097447111817, 20.3051578477121, "Maximum_gender", 0.100763723664104,
                                       0.95290777832206, 0.349357762828112, 0.107672445610558, 0.935835747880731,
                                       "Minimum_gender", -0.0638405197076946, 0.921234730367901, 0.160222623389529,
                                       0.045459990178547, -1.40432321821797, "Difference_age", -0.0504755710128331,
                                       7.35785239487092e-09, 2.75228728696675e-11, 0.00757976865463581,
                                       -6.65924955136488, "Tie_team4_advice_dyadic", 0.0704995509650402,
                                       0.930528913930139, 0.1921671695405, 0.0540559972400868, 1.30419480843023,
                                       "Tie_team4_social_dyadic", -0.329123396384468, 0.00161925116883589,
                                       8.87734402388318e-06, 0.0740792747984182, -4.44285391940008,
                                       "Minimum_gender:Incoming shared partners(type)", 0.000332691847022364,
                                       0.717712045463997, 0.0249936286676695, 0.000148423676426288,
                                       2.24150118790239, "Difference_age:inertia", -0.0211894821303297,
                                       0.927244639035214, 0.179666629475185, 0.0157920431000981, -1.34178218714449,
                                       "Tie_team4_advice_dyadic:inertia", 0.400701189194525, 0.240409416383031,
                                       0.0024314400872143, 0.132168472755773, 3.03174562616729))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14843.1732173746, "Null deviance", "", 971, 13716.1648389327,
                                       "Residual deviance", "", 12, 1127.00837844189, "Chi^2", 0, "",
                                       13740.1648389327, "AIC", "", "", 13740.4864884173, "AICC", "",
                                       "", 13798.8521483745, "BIC", ""))
})



#### tie model directed changed estimation options ####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$eventHistory <- "window"
options$eventHistorySingleInput <- 100
options$timepointInputLower <- 1
options$timepointInputUpper <- "200"
options$regularization <- ""

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.90657310391815, 0, 0, 0.0707106781177702, -97.6736935320447
                                      ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(200, 3162.62924157727, "Null deviance", "", 199, 3162.62924157727,
                                       "Residual deviance", "", 1, 0, "Chi^2", 1, "", 3164.62924157727,
                                       "AIC", "", "", 3164.64944359747, "AICC", "", "", 3167.92755894381,
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
                                       endogenousEffectsConsiderType = "no"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "difference('age') : Inertia")
)

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.11882527397944, 0, 0, 0.140905252936895, -43.4251040784106,
                                       "Inertia", 0.667829359107085, 4.48862037121954e-45, 0, 0.0459621819049167,
                                       14.5299751105951, "Difference_age", -0.0256805126865358, 0.0169660768767438,
                                       0.000106973323243054, 0.0066285889547125, -3.87420503247204,
                                       "Tie_team4_social_dyadic", -0.0192108195907097, 0.967591960375716,
                                       0.754484362166199, 0.0614290386026578, -0.312731894030954, "Difference_age:Inertia",
                                       0.00855827046240732, 0.572663615873849, 0.0120387379758933,
                                       0.00340830239110172, 2.51100679468787))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13480.4458603938, "Null deviance", "", 978, 12837.1317409289,
                                       "Residual deviance", "", 5, 643.314119464889, "Chi^2", 0, "",
                                       12847.1317409289, "AIC", "", "", 12847.1931534161, "AICC", "",
                                       "", 12871.5847865296, "BIC", ""))
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
  list(tie = TRUE, value = "social")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))

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
                                 list("baseline", -6.22203447023904, 0, 0, 0.149039941892949, -41.747429522672,
                                       "Inertia", 0.0239790778456563, 4.81819403921846e-26, 0, 0.00215791211304842,
                                       11.1121661075352, "Difference_age", -0.0171239709681093, 0.533372080531808,
                                       0.0100656841561921, 0.00665379477234879, -2.57356464303219,
                                       "Tie_team4_social_dyadic", 0.0633179099939921, 0.949339257584778,
                                       0.31030450382567, 0.062407798420916, 1.01458329882009, "Difference_age:Inertia",
                                       0.000939544879591987, 0.000326269577327712, 1.66657319922159e-06,
                                       0.000196141045745112, 4.79014923175713))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13480.4458603938, "Null deviance", "", 978, 13152.7947502167,
                                       "Residual deviance", "", 5, 327.651110177098, "Chi^2", 0, "",
                                       13162.7947502167, "AIC", "", "", 13162.8561627039, "AICC", "",
                                       "", 13187.2477958174, "BIC", ""))
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
                                       endogenousEffectsConsiderType = "no"),
                                  list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "std",
                                       endogenousEffectsConsiderType = "no"),
                                  list(value = "osp", translatedName = "Outgoing shared partners", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = TRUE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no")
                                  )

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor1.csv"), value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Indegree sender", -0.00899775776081751, 1.60929952598435e-36,
                                      0, 0.000686621439620137, -13.1043938356999, "Inertia", 0.61320667850748,
                                      6.04445877505199e-222, 0, 0.0191473809788657, 32.0256164111592,
                                      "Outgoing shared partners", -0.161756761814537, 9.48592275180902e-20,
                                      0, 0.016640233636505, -9.72082275694003, "Difference_gender",
                                      -0.135984064525107, 0.717954271087487, 0.0250281735917186, 0.0606809595676603,
                                      -2.24096760324765))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 8407.92558998549, "Null deviance", "", 979, 7367.07281942246,
                                      "Residual deviance", "", 4, 1040.85277056303, "Chi^2", 0, "",
                                      7375.07281942246, "AIC", "", "", 7375.11371921796, "AICC", "",
                                      "", 7394.63525590305, "BIC", ""))
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
  list(tie = TRUE, value = "social")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))

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
                                 list("baseline", -6.22203447023904, 0, 0, 0.149039941892949, -41.747429522672,
                                       "Inertia", 0.0239790778456563, 4.81819403921846e-26, 0, 0.00215791211304842,
                                       11.1121661075352, "Difference_age", -0.0171239709681093, 0.533372080531808,
                                       0.0100656841561921, 0.00665379477234879, -2.57356464303219,
                                       "Tie_team4_social_dyadic", 0.0633179099939921, 0.949339257584778,
                                       0.31030450382567, 0.062407798420916, 1.01458329882009, "Difference_age:Inertia",
                                       0.000939544879591987, 0.000326269577327712, 1.66657319922159e-06,
                                       0.000196141045745112, 4.79014923175713))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13480.4458603938, "Null deviance", "", 978, 13152.7947502167,
                                       "Residual deviance", "", 5, 327.651110177098, "Chi^2", 0, "",
                                       13162.7947502167, "AIC", "", "", 13162.8561627039, "AICC", "",
                                       "", 13187.2477958174, "BIC", ""))
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
                                 list("baseline", -7.80812869551183, 0, 0, 0.0932504808238319, -83.7328518473046
                                      ))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2025.86959996871, "Null deviance", "", 114, 2025.86959996871,
                                       "Residual deviance", "", 1, 0, "Chi^2", 1, "", 2027.86959996871,
                                       "AIC", "", "", 2027.90499819879, "AICC", "", "", 2030.61453209707,
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
                                       endogenousEffectsConsiderType = "no"))
options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender", includeEndoEffectSender = TRUE,
                                             endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                             endogenousEffectsConsiderTypeSender = "no"),
                                        list(value = "outdegreeSender", translatedNameSender = "Outdegree sender", includeEndoEffectSender = TRUE,
                                            endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "prop",
                                            endogenousEffectsConsiderTypeSender = "no")
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
                                 list("Inertia", -0.0957215642664434, 0.877892401619444, 0.371186287457273,
                                      0.107040751981119, -0.894253473511915, "Average_age", -0.0962082753309049,
                                      0.899077141533991, 0.542510102149272, 0.157971852966924, -0.609021629638342,
                                      "Difference_age", -0.649335634931579, 0.674237554917298, 0.0696986090874485,
                                      0.357984667994287, -1.81386437181701, "Average_age:Inertia",
                                      -0.0133837060290562, 0.914035360319271, 0.896063450679119, 0.102450552304395,
                                      -0.130635762599809, "Difference_age:Inertia", -0.0964302037038356,
                                      0.90456936218298, 0.619327774712083, 0.194101378918122, -0.49680329032857
                                 ))
})

test_that("Coefficient Estimates Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -8.20897166098803, 8.72574251427079e-273, 0, 0.231479947966985,
                                       -35.4629925100848, "Indegree sender", 0.016584907101278, 0.853386956539206,
                                       0.268949022884598, 0.015002366293712, 1.10548607976791, "Outdegree sender",
                                       1.46555813105886, 0.837707614111703, 0.226549755041969, 1.211911764898,
                                       1.20929441689363, "Send_sex", 0.595647701823807, 0.475797377290406,
                                       0.0262617123480431, 0.268029716937427, 2.22231963168049, "Send_extraversion",
                                       -0.241188510478177, 0.839437510112121, 0.230652278242039, 0.201211706776616,
                                       -1.19868030713513, "Send_extraversion:Indegree sender", 0.0222555201643948,
                                       0.824816956395237, 0.199467102018404, 0.0173455055719926, 1.28307128737316
                                      ))
})

test_that("Model Fit Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 505.36165278733, "Null deviance", "", 110, 493.732984145275,
                                       "Residual deviance", "", 5, 11.6286686420546, "Chi^2", 0.0402457931007782,
                                       "", 503.732984145275, "AIC", "", "", 504.283442860872, "AICC",
                                       "", "", 517.457644787092, "BIC", ""))
})

test_that("Model Fit Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2025.86959996871, "Null deviance", "", 109, 2014.58901695407,
                                       "Residual deviance", "", 6, 11.2805830146383, "Chi^2", 0.0800822367971277,
                                       "", 2026.58901695407, "AIC", "", "", 2027.36679473185, "AICC",
                                       "", "", 2043.05860972425, "BIC", ""))
})

test_that("Regularization Results Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regContainer"]][["collection"]][["mainContainer_regContainer_regTableReceiver"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Inertia", -0.0957215642664434, "FALSE", -0.224579668670249, -0.04035636665249,
                                      -0.04035636665249, -0.00416152747171972, 0.09037515619135, "Average_age",
                                      -0.0962082753309049, "FALSE", -0.327688367620372, -0.0745376492806999,
                                      -0.0745376492806999, -0.00783625042911029, 0.0877219491953564,
                                      "Difference_age", -0.649335634931579, "FALSE", -1.01603606009306,
                                      -0.283773726185557, -0.283773726185557, -0.0196298290624105,
                                      0.104391134876241, "Average_age:Inertia", -0.0133837060290562,
                                      "FALSE", -0.141675915592962, -0.00653747901853176, -0.00653747901853176,
                                      -0.000183198903951316, 0.119658871471872, "Difference_age:Inertia",
                                      -0.0964302037038356, "FALSE", -0.400348387522549, -0.0907860320348994,
                                      -0.0907860320348994, -0.00678565631124418, 0.102066790269673
                                 ))
})

test_that("Regularization Results Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regContainer"]][["collection"]][["mainContainer_regContainer_regTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -8.20897166098803, "TRUE", -8.46889256085392, -7.98650613390461,
                                       -7.98650613390461, -7.89902266505521, -7.62614475456054, "Indegree sender",
                                       0.016584907101278, "FALSE", -0.017389296815788, 0.0083420199288467,
                                       0.0083420199288467, 0.00189210690787694, 0.0418306452360446,
                                       "Outdegree sender", 1.46555813105886, "FALSE", -0.516789489166073,
                                       0.619432798760609, 0.619432798760609, 0.037392906143182, 2.93555517525255,
                                       "Send_sex", 0.595647701823807, "FALSE", -0.0703157586104887,
                                       0.334949914933375, 0.334949914933375, 0.0349494193400559, 0.965950521564457,
                                       "Send_extraversion", -0.241188510478177, "FALSE", -0.544628683738782,
                                       -0.172977998953413, -0.172977998953413, -0.0154585763035013,
                                       0.0665521070609683, "Send_extraversion:Indegree sender", 0.0222555201643948,
                                       "FALSE", -0.00670793839562496, 0.0163964648039465, 0.0163964648039465,
                                       0.0148734207687219, 0.0472558349091708))
})


# actor model, risket active, simultaneous events split
# just use the same effects as before, plus:
options$riskset <- "active"
options$simultaneousEvents <- "split"
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options, makeTests = F)

test_that("Model Fit Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 505.36165278733, "Null deviance", "", 110, 417.694471146251,
                                       "Residual deviance", "", 5, 87.6671816410787, "Chi^2", 0, "",
                                       427.694471146251, "AIC", "", "", 428.244929861848, "AICC", "",
                                       "", 441.419131788067, "BIC", ""))
})

test_that("Model Fit Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2025.86959996871, "Null deviance", "", 109, 2014.58901695407,
                                       "Residual deviance", "", 6, 11.2805830146383, "Chi^2", 0.0800822367971277,
                                       "", 2026.58901695407, "AIC", "", "", 2027.36679473185, "AICC",
                                       "", "", 2043.05860972425, "BIC", ""))
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
                                       endogenousEffectsConsiderType = "no"))
options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender", includeEndoEffectSender = TRUE,
                                             endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                             endogenousEffectsConsiderTypeSender = "no")
)

options$actorDataList <- list(list(actorData = testthat::test_path("team4_attributes_actor2.csv"), value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("team4_events.csv"), options)

test_that("Coefficient estimates sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Indegree sender", -0.0106027975755074, 4.2311881579719e-31, 0,
                                       0.000875201111704419, -12.1146984775407, "Send_age", -0.0257657622963597,
                                       2.56671168915153e-06, 1.11115676659779e-08, 0.00451016100371022,
                                       -5.7128253903051))
})

test_that("Model fit receiver model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 4088.18207094256, "Null deviance", "", 981, 3674.13224406993,
                                       "Residual deviance", "", 2, 414.049826872626, "Chi^2", 0, "",
                                       3678.13224406993, "AIC", "", "", 3678.14448896789, "AICC", "",
                                       "", 3687.91346231023, "BIC", ""))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 4319.74351904301, "Null deviance", "", 981, 4142.80426686032,
                                       "Residual deviance", "", 2, 176.939252182685, "Chi^2", 0, "",
                                       4146.80426686032, "AIC", "", "", 4146.81651175828, "AICC", "",
                                       "", 4156.58548510062, "BIC", ""))
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
                                 list("baseline", -9.80548940677429, 0, 0, 0.117157168927935, -83.6951720197831,
                                       "Difference_age", -0.644818961281669, 0.353308581769778, 0.0146840387631091,
                                       0.264261479246842, -2.4400792848032, "Average_extraversion",
                                       -0.0747614281322239, 0.886821528501677, 0.428245645888951, 0.0943722336001686,
                                       -0.792197294481439, "Tie_dy1", -0.173578797778321, 0.78102070110212,
                                       0.137860129178866, 0.116982058734749, -1.48380700131037))
})

test_that("Model Fit Tie Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2531.23125275603, "Null deviance", "", 111, 2524.78387062065,
                                       "Residual deviance", "", 4, 6.44738213538722, "Chi^2", 0.168136074300923,
                                       "", 2532.78387062065, "AIC", "", "", 2533.14750698428, "AICC",
                                       "", "", 2543.7635991341, "BIC", ""))
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
                                 list("baseline", -6.34253269860022, 0, 0, 0.0514741387296739, -123.217849878154,
                                       "Difference_age", -0.0212339040876801, 0.000260332255338324,
                                       1.317707411852e-06, 0.00438983282163195, -4.83706440551562
                                      ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14843.1732173746, "Null deviance", "", 981, 14819.6255460111,
                                       "Residual deviance", "", 2, 23.5476713635326, "Chi^2", 7.70350081014115e-06,
                                       "", 14823.6255460111, "AIC", "", "", 14823.637790909, "AICC",
                                       "", "", 14833.4067642514, "BIC", ""))
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
                                       endogenousEffectsConsiderType = "no"),
                                  list(value = "indegreeSender", translatedName = "Indegree sender", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))
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
                                       endogenousEffectsConsiderType = "no"),
                                  list(value = "indegreeReceiver", translatedName = "Indegree receiver", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))

options$endogenousEffectsSender <- list(list(value = "indegreeSender", translatedNameSender = "Indegree sender", includeEndoEffectSender = TRUE,
                                       endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                       endogenousEffectsConsiderTypeSender = "no"),
                                  list(value = "outdegreeSender", translatedNameSender = "Outdegree sender", includeEndoEffectSender = TRUE,
                                       endogenousEffectsUniqueSender = FALSE, endogenousEffectsScalingSender = "none",
                                       endogenousEffectsConsiderTypeSender = "no"))
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
                                       endogenousEffectsConsiderType = "no"),
                                  list(value = "indegreeSender", translatedName = "Indegree sender", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))
options$regularization <- "horseshoe"
options$regularizationSetSeed <- TRUE
options$regularizationSeed <- 1234
options$regularizationIterations <- 2000
options$regularizationCiLevel <- .95
options$riskset <- "manual"
options$dyadExclude <- testthat::test_path("history_dyads_exclude.csv")

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", testthat::test_path("history_events.csv"), options, makeTests = FALSE)


test_that("Regularization results tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regContainer"]][["collection"]][["mainContainer_regContainer_regTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -10.0414967533405, "TRUE", -10.3466444743103, -10.0218523434971,
                                       -10.0218523434971, -9.99243243440296, -9.71200972837774, "Inertia",
                                       -0.10430614783501, "FALSE", -0.314704950213094, -0.0756411748973929,
                                       -0.0756411748973929, -0.00720552597994455, 0.132863995864512,
                                       "Indegree sender", 0.0233443436371193, "FALSE", -0.0248414627068577,
                                       0.0172281161853238, 0.0172281161853238, 0.00547294531529529,
                                       0.0614184609536507))
})

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -10.0414967533405, 0, 0, 0.163523423201926, -61.4070850323427,
                                       "Inertia", -0.10430614783501, 0.882170358453617, 0.396589888696021,
                                       0.123042108488384, -0.847727246521116, "Indegree sender", 0.0233443436371193,
                                       0.869023505646079, 0.327129558483129, 0.0238228979869976, 0.979912001044562
                                      ))
})


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
                                 list("baseline", -6.54902255758944, 0, 0, 0.0318950466054745, -205.330396239815
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14841.3783482209, "Null deviance", "", 982, 14841.3783482209,
                                      "Residual deviance", "", 1, 0, "Chi^2", 1, "", 14843.3783482209,
                                      "AIC", "", "", 14843.3824256928, "AICC", "", "", 14848.268957341,
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
                                 list("baseline", -6.88475275793919, 3.33526529777755e-136, 0, 0.274124460866219,
                                      -25.115426533567, "inertia", 1.88250646559702, 0.017745426874788,
                                      0.000112283249319001, 0.487394228643815, 3.86238973496903, "Incoming shared partners(type)",
                                      0.00246714037268166, 8.07815856571961e-57, 0, 0.000151496075868399,
                                      16.2851767515404, "Outgoing two-path", 0.548141969955606, 1.1386620272385e-88,
                                      0, 0.0270087574969223, 20.2949717334486, "Maximum_gender", 0.100879051153746,
                                      0.95286296745226, 0.34880912165211, 0.107673036053342, 0.936901705862271,
                                      "Minimum_gender", -0.0638520839673557, 0.92121528351021, 0.160165837632509,
                                      0.0454620478911208, -1.40451402717885, "Difference_age", -0.0504775789231556,
                                      7.34023126128528e-09, 2.74555933543752e-11, 0.00757966035890176,
                                      -6.65960960425798, "Tie_team4_advice_dyadic", 0.0704451427792313, 0.930610542375215,
                                      0.192497711193019, 0.0540544447615987, 1.30322572158353, "Tie_team4_social_dyadic",
                                      -0.32902102119001, 0.00163051723589137, 8.94209742918761e-06,
                                      0.0740823011543191, -4.44129051154383, "Minimum_gender:Incoming shared partners(type)",
                                      0.000332546512347564, 0.718327007121658, 0.0250814633853964,
                                      0.0001484486065012, 2.24014573248875, "Difference_age:inertia",
                                      -0.021141692719793, 0.927509998927023, 0.180621951462581, 0.0157910229947912,
                                      -1.33884250100622, "Tie_team4_advice_dyadic:inertia", 0.400494575515731,
                                      0.241111485389019, 0.00244166556512715, 0.132155556716913, 3.030478516871
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14841.3783482209, "Null deviance", "", 971, 13715.2158350503,
                                      "Residual deviance", "", 12, 1126.16251317057, "Chi^2", 0, "",
                                      13739.2158350503, "AIC", "", "", 13739.5374845348, "AICC", "",
                                      "", 13797.9031444921, "BIC", ""))
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
                                 list("baseline", -6.90342845978138, 0, 0, 0.0707106781178174, -97.6292215481085
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(200, 3161.37138392202, "Null deviance", "", 199, 3161.37138392202,
                                      "Residual deviance", "", 1, 0, "Chi^2", 1, "", 3163.37138392202,
                                      "AIC", "", "", 3163.39158594223, "AICC", "", "", 3166.66970128857,
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
                                 list("baseline", -6.1178345770935, 0, 0, 0.140905030989789, -43.4181415249597,
                                      "Inertia", 96.2698809101359, 5.53075302451979e-45, 0, 6.63216720995604,
                                      14.5155991793479, "Difference_age", -0.025687998673645, 0.0169093376176443,
                                      0.000106587709038264, 0.00662901640414771, -3.87508449331522,
                                      "Tie_team4_social_dyadic", -0.0191427510932347, 0.967602672815231, 0.755316084231341,
                                      0.061426353844977, -0.311637430760512, "Difference_age:Inertia",
                                      1.23617613625278, 0.571071308168138, 0.0119507417843097, 0.49179597027741,
                                      2.51359549683883))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13478.65099124, "Null deviance", "", 978, 12836.0866321316,
                                      "Residual deviance", "", 5, 642.564359108463, "Chi^2", 0, "",
                                      12846.0866321316, "AIC", "", "", 12846.1480446188, "AICC", "",
                                      "", 12870.5396777323, "BIC", ""))
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
                                 list("baseline", -6.22074972197566, 0, 0, 0.149032198824019, -41.7409779300195,
                                      "Inertia", 0.0239520866999468, 5.71722601343895e-26, 0, 0.00215847583035702,
                                      11.0967592794333, "Difference_age", -0.0171354953828436, 0.532356227155608,
                                      0.01001966488604, 0.00665417392215236, -2.57514991091501, "Tie_team4_social_dyadic",
                                      0.0632856607903381, 0.949359765876912, 0.310505056214087, 0.0624018729605998,
                                      1.01416284139895, "Difference_age:Inertia", 0.000939363588278229,
                                      0.000329869375873128, 1.68571494585379e-06, 0.000196197075701293,
                                      4.78785723446966))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13478.65099124, "Null deviance", "", 978, 13151.6297126877,
                                      "Residual deviance", "", 5, 327.021278552349, "Chi^2", 0, "",
                                      13161.6297126877, "AIC", "", "", 13161.6911251749, "AICC", "",
                                      "", 13186.0827582884, "BIC", ""))
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
                                 list("baseline", -6.22074972197566, 0, 0, 0.149032198824019, -41.7409779300195,
                                      "Inertia", 0.0239520866999468, 5.71722601343895e-26, 0, 0.00215847583035702,
                                      11.0967592794333, "Difference_age", -0.0171354953828436, 0.532356227155608,
                                      0.01001966488604, 0.00665417392215236, -2.57514991091501, "Tie_team4_social_dyadic",
                                      0.0632856607903381, 0.949359765876912, 0.310505056214087, 0.0624018729605998,
                                      1.01416284139895, "Difference_age:Inertia", 0.000939363588278229,
                                      0.000329869375873128, 1.68571494585379e-06, 0.000196197075701293,
                                      4.78785723446966))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13478.65099124, "Null deviance", "", 978, 13151.6297126877,
                                      "Residual deviance", "", 5, 327.021278552349, "Chi^2", 0, "",
                                      13161.6297126877, "AIC", "", "", 13161.6911251749, "AICC", "",
                                      "", 13186.0827582884, "BIC", ""))
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
                                 list("baseline", -7.7997176797298, 0, 0, 0.093250480823861, -83.6426537517006
                                 ))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2023.9350663387, "Null deviance", "", 114, 2023.9350663387,
                                      "Residual deviance", "", 1, 0, "Chi^2", 1, "", 2025.9350663387,
                                      "AIC", "", "", 2025.97046456879, "AICC", "", "", 2028.67999846706,
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
                                 list("baseline", -8.1798138638532, 4.85428266522845e-268, 0, 0.232688249930558,
                                      -35.1535321026924, "Indegree sender", 0.0147522810237776, 0.869568375479053,
                                      0.329557342157186, 0.0151304533567232, 0.975005882240961, "Outdegree sender",
                                      1.42884305466272, 0.84305762993606, 0.239655568941801, 1.2151595232938,
                                      1.17584813127227, "Send_sex", 0.587979904353113, 0.491254964324703,
                                      0.0282147161456476, 0.267959320444268, 2.19428793661016, "Send_extraversion",
                                      -0.242033657565874, 0.840730520864494, 0.233801313644987, 0.20328307104546,
                                      -1.19062377561066, "Send_extraversion:Indegree sender", 0.0226893068623272,
                                      0.822814640002037, 0.195741907398593, 0.017537240841064, 1.29377859766854
                                 ))
})

test_that("Model Fit Receiver Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 4.39444915467244, "Null deviance", "", 110, 493.732984145275,
                                      "Residual deviance", "", 5, -489.338534990603, "Chi^2", 1, "",
                                      503.732984145275, "AIC", "", "", 504.283442860872, "AICC", "",
                                      "", 517.457644787092, "BIC", ""))
})

test_that("Model Fit Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2023.9350663387, "Null deviance", "", 109, 2012.95385403721,
                                      "Residual deviance", "", 6, 10.9812123014881, "Chi^2", 0.0889588273440796,
                                      "", 2024.95385403721, "AIC", "", "", 2025.73163181498, "AICC",
                                      "", "", 2041.42344680739, "BIC", ""))
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
                                 list("baseline", -8.1798138638532, "TRUE", -8.44195567747698, -7.96244374889441,
                                      -7.96244374889441, -7.86305776662492, -7.60307064535268, "Indegree sender",
                                      0.0147522810237776, "FALSE", -0.0186109097351843, 0.00681053742738997,
                                      0.00681053742738997, 0.00140931065003133, 0.0399329083450789,
                                      "Outdegree sender", 1.42884305466272, "FALSE", -0.515876375075361,
                                      0.603002709641844, 0.603002709641844, 0.0346983648140132, 2.88892214555355,
                                      "Send_sex", 0.587979904353113, "FALSE", -0.0732321706186625,
                                      0.327478934744132, 0.327478934744132, 0.0355508799587188, 0.958911430974557,
                                      "Send_extraversion", -0.242033657565874, "FALSE", -0.547738467696973,
                                      -0.172931697797102, -0.172931697797102, -0.0148009337897842,
                                      0.0681210268128113, "Send_extraversion:Indegree sender", 0.0226893068623272,
                                      "FALSE", -0.00662229891650218, 0.0166965715241437, 0.0166965715241437,
                                      0.0153348092300569, 0.0476481278239918))
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
                                 list(115, 277.974918588083, "Null deviance", "", 110, 240.22772188864,
                                      "Residual deviance", "", 5, 37.7471966994438, "Chi^2", 4.24116054276169e-07,
                                      "", 250.22772188864, "AIC", "", "", 250.778180604236, "AICC",
                                      "", "", 263.952382530456, "BIC", ""))
})

test_that("Model Fit Sender Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2023.9350663387, "Null deviance", "", 109, 2012.95385403721,
                                      "Residual deviance", "", 6, 10.9812123014881, "Chi^2", 0.0889588273440796,
                                      "", 2024.95385403721, "AIC", "", "", 2025.73163181498, "AICC",
                                      "", "", 2041.42344680739, "BIC", ""))
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
                                 list("baseline", 1.17921879893501, 4.58698398297698e-08, 1.788067471864e-10,
                                      0.184873401300191, 6.37852060189146, "Indegree sender", -0.00990059228120915,
                                      6.34051665507703e-33, 0, 0.000794805873525519, -12.4566169060794,
                                      "Send_age", -0.025902563085422, 2.55425752872077e-06, 1.10560927080172e-08,
                                      0.00453343166542433, -5.71367674580301))
})

test_that("Model fit receiver model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 4.15888308335967, "Null deviance", "", 981, 3674.13224406993,
                                      "Residual deviance", "", 2, -3669.97336098657, "Chi^2", 1, "",
                                      3678.13224406993, "AIC", "", "", 3678.14448896789, "AICC", "",
                                      "", 3687.91346231023, "BIC", ""))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 4319.74351904301, "Null deviance", "", 980, 4143.74687228237,
                                      "Residual deviance", "", 3, 175.996646760644, "Chi^2", 0, "",
                                      4149.74687228237, "AIC", "", "", 4149.7713870934, "AICC", "",
                                      "", 4164.41869964281, "BIC", ""))
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
                                 list("baseline", -9.79753342876852, 0, 0, 0.117242482943909, -83.5664102529785,
                                      "Difference_age", -0.642620457277501, 0.359563709448936, 0.0151456250041615,
                                      0.264575017499634, -2.42887806774269, "Average_extraversion",
                                      -0.0691502776164443, 0.8913117364744, 0.46388217030698, 0.0944070652757011,
                                      -0.732469306343775, "Tie_dy1", -0.173130178357087, 0.78239546669586,
                                      0.139309465706688, 0.117109034013645, -1.47836740192832))
})

test_that("Model Fit Tie Model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2529.29671912602, "Null deviance", "", 111, 2522.9399589589,
                                      "Residual deviance", "", 4, 6.35676016712205, "Chi^2", 0.174042378999372,
                                      "", 2530.9399589589, "AIC", "", "", 2531.30359532254, "AICC",
                                      "", "", 2541.91968747236, "BIC", ""))
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
                                 list("baseline", -6.34161974379161, 0, 0, 0.0514741387296736, -123.200113693905,
                                      "Difference_age", -0.021233904087681, 0.000260332255338065,
                                      1.317707411852e-06, 0.00438983282163195, -4.83706440551583
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14841.3783482209, "Null deviance", "", 981, 14817.8306768573,
                                      "Residual deviance", "", 2, 23.5476713635398, "Chi^2", 7.70350081003013e-06,
                                      "", 14821.8306768573, "AIC", "", "", 14821.8429217553, "AICC",
                                      "", "", 14831.6118950976, "BIC", ""))
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
                                 list("baseline", -9.86384358356178, "TRUE", -10.1625302129706, -9.84318313761637,
                                      -9.84318313761637, -9.80289902782461, -9.53951591639268, "Inertia",
                                      0.0401278305929245, "FALSE", -0.177162355620214, 0.0317062978884972,
                                      0.0317062978884972, 0.00647003474545849, 0.26269240663871, "Indegree sender",
                                      0.0115272367578098, "FALSE", -0.0331947142707422, 0.00901076175607919,
                                      0.00901076175607919, 0.00333218999209164, 0.0536675308980435
                                 ))
})

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.86384358356178, 0, 0, 0.163824619270752, -60.2097757191174,
                                      "Inertia", 0.0401278305929245, 0.910465024912607, 0.744419920394006,
                                      0.123089291749678, 0.326005861456504, "Indegree sender", 0.0115272367578098,
                                      0.905272249878339, 0.631172103900244, 0.0240111869251122, 0.480077756829005
                                 ))
})


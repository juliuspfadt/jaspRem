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

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = F)


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

options$actorDataList <- list(list(actorData = "team4_attributes_actor1.csv", value = "#"),
                              list(actorData = "team4_attributes_actor2.csv", value = "#2"))
options$dyadDataList <- list(list(dyadData = "team4_social_dyadic.csv", value = "#"),
                             list(dyadData = "team4_advice_dyadic.csv", value = "#2"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTable <- list(
  list(maximum = TRUE, minimum = TRUE, value = "gender"),
  list(difference = TRUE, value = "age"),
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
  list(includeInteractionEffect = TRUE, value = "minimum('gender') * Incoming shared partners(type)"),
  list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia(type)"),
  list(includeInteractionEffect = TRUE, value = "tie('team4_advice_dyadic') * Inertia(type)")
)

# options$actorDataList <- list(list(actorData = "tests/testthat/team4_attributes_actor1.csv", value = "#"),
#                               list(actorData = "tests/testthat/team4_attributes_actor2.csv", value = "#2"))
# options$dyadDataList <- list(list(dyadData = "tests/testthat/team4_social_dyadic.csv", value = "#"),
#                              list(dyadData = "tests/testthat/team4_advice_dyadic.csv", value = "#2"))
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = F)

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)

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

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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

options$actorDataList <- list(list(actorData = "team4_attributes_actor2.csv", value = "#"))
options$dyadDataList <- list(list(dyadData = "team4_social_dyadic.csv", value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTable <- list(
  list(difference = TRUE, value = "age"),
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
  list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia")
)

# options$actorDataList <- list(list(actorData = "tests/testthat/team4_attributes_actor2.csv", value = "#"))
# options$dyadDataList <- list(list(dyadData = "tests/testthat/team4_social_dyadic.csv", value = "#"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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

options$actorDataList <- list(list(actorData = "team4_attributes_actor2.csv", value = "#"))
options$dyadDataList <- list(list(dyadData = "team4_social_dyadic.csv", value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTable <- list(
  list(difference = TRUE, value = "age"),
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
  list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia")
)
options$timepointInputUpper <- "Inf"
options$eventHistory <- "full"
options$eventDirection <- "directed"
options$riskset <- "active"

# options$actorDataList <- list(list(actorData = "tests/testthat/team4_attributes_actor2.csv", value = "#"))
# options$dyadDataList <- list(list(dyadData = "tests/testthat/team4_social_dyadic.csv", value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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

options$exogenousEffectsTable <- list(
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

options$actorDataList <- list(list(actorData = "team4_attributes_actor1.csv", value = "#"))
# options$actorDataList <- list(list(actorData = "tests/testthat/team4_attributes_actor1.csv", value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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

options$actorDataList <- list(list(actorData = "team4_attributes_actor2.csv", value = "#"))
options$dyadDataList <- list(list(dyadData = "team4_social_dyadic.csv", value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTable <- list(
  list(average = FALSE, difference = TRUE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "age"),
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = TRUE, value = "social")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('team4_social_dyadic')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia")
)
options$timepointInputUpper <- "Inf"
options$eventHistory <- "full"
options$eventDirection <- "directed"
options$riskset <- "active"
options$eventSequence <- "timeSensitive"

# options$actorDataList <- list(list(actorData = "tests/testthat/team4_attributes_actor2.csv", value = "#"))
# options$dyadDataList <- list(list(dyadData = "tests/testthat/team4_social_dyadic.csv", value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", history, options, makeTests = T)

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
options$exogenousEffectsTable <- list(
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
  list(includeInteractionEffect = TRUE, value = "average('age') * Inertia"),
  list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia")
)
options$interactionEffectsSender <- list(
  list(includeInteractionEffectSender = TRUE, value = "send('extraversion') * Indegree sender")
)
options$actorDataList <- list(list(actorData = "history_info_actor.csv", value = "#"))

# options$actorDataList <- list(list(actorData = "tests/testthat/info.csv", value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history_events.csv", options)

# results <- jaspTools::runAnalysis("relationalEventModeling", history, options, makeTests = T)

test_that("Coefficient estimates receiver model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Inertia", -0.0957215457734518, 0.877892416806052, 0.371186372200697,
                                      0.107040750264209, -0.894253315089645, "Average_age", -0.0962082886374941,
                                      0.899077137112637, 0.542510049117684, 0.157971854062944, -0.609021709646834,
                                      "Difference_age", -0.649335581452957, 0.674237611033644, 0.0696986307781589,
                                      0.357984666310104, -1.81386423096253, "Average_age:Inertia",
                                      -0.0133836559772642, 0.914035365181425, 0.896063825409504, 0.102450540645495,
                                      -0.130635288920291, "Difference_age:Inertia", -0.0964301418523927,
                                      0.904569374251566, 0.619327973178754, 0.194101364367049, -0.49680300891673
                                 ))
})

test_that("Coefficient estimates sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -8.18014042071102, 4.57533968411268e-265, 0, 0.233997956829992,
                                      -34.9581702828892, "Indegree sender", 0.0158870237167222, 0.859492136284046,
                                      0.289321560099512, 0.0149932590030345, 1.05961110346368, "Outdegree sender",
                                      1.38135802157882, 0.852568294397778, 0.266406733321369, 1.2429264125482,
                                      1.11137554696164, "Send_sex", 0.582721603768429, 0.498133063191506,
                                      0.0291307872578681, 0.267093721240829, 2.1817121011355, "Send_extraversion",
                                      -0.246614172404979, 0.837918576291459, 0.227043454994804, 0.204149132181351,
                                      -1.2080098983027, "Send_extraversion:Indegree sender", 0.0202109445565804,
                                      0.851583936917184, 0.263404485101041, 0.0180716161634362, 1.11838057945656
                                 ))
})

test_that("Model fit receiver model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 4.39444915467244, "Null deviance", "", 110, 493.732984145276,
                                      "Residual deviance", "", 5, -489.338534990603, "Chi^2", 1, "",
                                      503.732984145276, "AIC", "", "", 504.283442860872, "AICC", "",
                                      "", 517.457644787092, "BIC", ""))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2023.9350663387, "Null deviance", "", 109, 2013.14898442893,
                                      "Residual deviance", "", 6, 10.7860819097721, "Chi^2", 0.0952171386617836,
                                      "", 2025.14898442893, "AIC", "", "", 2025.9267622067, "AICC",
                                      "", "", 2041.61857719911, "BIC", ""))
})


# actor model, risket active, simultaneous events split
# just use the same effects as before, plus:
options$riskset <- "active"
options$simultaneousEvents <- "split"
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history_events.csv", options)

test_that("Model fit receiver model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 277.974918588083, "Null deviance", "", 110, 240.22772188864,
                                      "Residual deviance", "", 5, 37.7471966994438, "Chi^2", 4.24116054276169e-07,
                                      "", 250.22772188864, "AIC", "", "", 250.778180604236, "AICC",
                                      "", "", 263.952382530456, "BIC", ""))
})

test_that("Model fit sender model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableSender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2023.93506633869, "Null deviance", "", 109, 2013.14898442893,
                                      "Residual deviance", "", 6, 10.7860819097682, "Chi^2", 0.0952171386619114,
                                      "", 2025.14898442893, "AIC", "", "", 2025.9267622067, "AICC",
                                      "", "", 2041.61857719911, "BIC", ""))
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

options$exogenousEffectsTable <- list(
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

options$actorDataList <- list(list(actorData = "team4_attributes_actor2.csv", value = "#"))
# options$actorDataList <- list(list(actorData = "tests/testthat/team4_attributes_actor2.csv", value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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
options$actorDataList <- list(list(actorData = "history_info_actor.csv", value = "#"))
options$dyadDataList <- list(list(dyadData = "history_wide_dyadic.csv", value = "#"),
                             list(dyadData = "history_long_dyadic1.csv", value = "#2"),
                             list(dyadData = "history_long_dyadic2.csv", value = "#4"))
options$exogenousEffectsTable <- list(
  list(difference = TRUE, value = "age"),
  list(average = TRUE, value = "extraversion"),
  list(tie = TRUE, value = "dy1")
)
options$specifiedExogenousEffects <- list(list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none",
                                               value = "difference('age')"),
                                          list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std",
                                               value = "average('extraversion')"),
                                          list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std",
                                               value = "tie('dy1')"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", history, options, makeTests = T)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -9.79803048708652, 0, 0, 0.117105319895939, -83.6685344081137,
                                      "Difference_age", -0.642779091893944, 0.356352068570571, 0.0149072417032137,
                                      0.264015676794453, -2.43462471508604, "Average_extraversion",
                                      -0.0766459056664178, 0.884397621070177, 0.411164261337275, 0.0932606089633795,
                                      -0.821846506454984, "Tie_dy1", -0.17196305136877, 0.784490110995608,
                                      0.141563800062397, 0.116982268793839, -1.46999244536644))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(115, 2529.29671912602, "Null deviance", "", 111, 2522.80475785775,
                                      "Residual deviance", "", 4, 6.49196126827746, "Chi^2", 0.165297593633206,
                                      "", 2530.80475785775, "AIC", "", "", 2531.16839422138, "AICC",
                                      "", "", 2541.7844863712, "BIC", ""))
})



# .txt actor attributes data
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$exogenousEffectsTable <- list(
  list(difference = TRUE, value = "age")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')")
)
options$actorDataList <- list(list(actorData = "team4_attributes_actor2.txt", value = "#"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

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
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)


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
  jaspTools::expect_equal_plots(testPlot, "waiting-times-fit")
})



# actor model
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$orientation <- "actor"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
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
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_events.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

### TODO when Giuseppe is ready


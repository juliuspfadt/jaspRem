


context("Relational Event Modeling")

# does not test:


#### baseline test tie model directed ####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
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

#### tie model directed with effects ####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
options$timepointInputUpper <- "Inf"

options$actorDataList <- list(list(actorData = "attributes1.csv", value = "#"),
                              list(actorData = "attributes2.csv", value = "#2"))
options$dyadDataList <- list(list(dyadData = "social.csv", value = "#"),
                             list(dyadData = "advice.csv", value = "#2"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTable <- list(
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = TRUE, minimum = TRUE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "gender"),
  list(average = FALSE, difference = TRUE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "age"),
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = TRUE, value = "advice"),
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = TRUE, value = "social")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "maximum('gender')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std", value = "minimum('gender')"),
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('advice')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('social')")
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
  list(includeInteractionEffect = TRUE, value = "tie('advice') * Inertia(type)")
)

# options$actorDataList <- list(list(actorData = "tests/testthat/attributes1.csv", value = "#"),
#                               list(actorData = "tests/testthat/attributes2.csv", value = "#2"))
# options$dyadDataList <- list(list(dyadData = "tests/testthat/social.csv", value = "#"),
#                              list(dyadData = "tests/testthat/advice.csv", value = "#2"))
# set.seed(1)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = F)

results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)

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
                                      -6.65960960425798, "Tie_advice", 0.0704451427792313, 0.930610542375215,
                                      0.192497711193019, 0.0540544447615987, 1.30322572158353, "Tie_social",
                                      -0.32902102119001, 0.00163051723589137, 8.94209742918761e-06,
                                      0.0740823011543191, -4.44129051154383, "Minimum_gender:Incoming shared partners(type)",
                                      0.000332546512347564, 0.718327007121658, 0.0250814633853964,
                                      0.0001484486065012, 2.24014573248875, "Difference_age:inertia",
                                      -0.021141692719793, 0.927509998927023, 0.180621951462581, 0.0157910229947912,
                                      -1.33884250100622, "Tie_advice:inertia", 0.400494575515731,
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
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
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

# TODO: BSIR is broken
# BSIR and interval window
# options <- jaspTools::analysisOptions("relationalEventModeling")
# options$timeVariable <- "time"
# options$actorVariableSender <- "actor1"
# options$actorVariableReceiver <- "actor2"
# options$syncAnalysisBox <- TRUE
# options$method <- "BSIR"
# options$eventHistory <- "interval"
# options$eventHistoryIntervalInputLower <- 50
# options$eventHistoryIntervalInputUpper <- 100
# options$timepointInputLower <- 1
# options$timepointInputUpper <- "200"
# options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
#                                        endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
#                                        endogenousEffectsConsiderType = "no"))
# set.seed(1)
# # results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = F)
#
# test_that("Coefficient estimates tie model table results match", {
#   table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list("baseline", -7.15198275739228, 0.0795393109438783, 0, -7.30811669991729,
#                                       -7.15040793617075, -7.0020148776776, "Inertia", 1.68248335791643,
#                                       0.113421904446645, 5.18241861461358e-47, 1.4635571359845, 1.67556754609801,
#                                       1.89428089512612))
# })
#
# test_that("Model fit tie model table results match", {
#   table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(3076.77214310402, "BIC"))
# })



#### model options ####
# tie undirected #
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$timepointInputUpper <- "Inf"
options$eventHistory <- "decay"
options$eventHistorySingleInput <- 100
options$eventDirection <- "undirected"
options$syncAnalysisBox <- TRUE

options$actorDataList <- list(list(actorData = "attributes2.csv", value = "#"))
options$dyadDataList <- list(list(dyadData = "social.csv", value = "#"))

options$syncAnalysisBox <- TRUE
options$exogenousEffectsTable <- list(
  list(average = FALSE, difference = TRUE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "age"),
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = TRUE, value = "social")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('social')")
)

options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))

options$interactionEffects <- list(
  list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia")
)

# options$actorDataList <- list(list(actorData = "tests/testthat/attributes2.csv", value = "#"))
# options$dyadDataList <- list(list(dyadData = "tests/testthat/social.csv", value = "#"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.11783457709347, 0, 0, 0.140905030989795, -43.4181415249578,
                                      "Inertia", 96.2698809101322, 5.53075302460193e-45, 0, 6.63216720995626,
                                      14.5155991793469, "Difference_age", -0.0256879986736469, 0.0169093376176363,
                                      0.000106587709038264, 0.00662901640414798, -3.87508449331534,
                                      "Tie_social", -0.0191427510932326, 0.967602672815231, 0.755316084231374,
                                      0.0614263538449788, -0.311637430760468, "Difference_age:Inertia",
                                      1.236176136253, 0.571071308167902, 0.0119507417842968, 0.491795970277425,
                                      2.51359549683921))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13478.65099124, "Null deviance", "", 978, 12836.0866321316,
                                      "Residual deviance", "", 5, 642.564359108472, "Chi^2", 0, "",
                                      12846.0866321316, "AIC", "", "", 12846.1480446188, "AICC", "",
                                      "", 12870.5396777323, "BIC", ""))
  })



# # tie undirected , active riskset # FAILS with remstimate
# options <- jaspTools::analysisOptions("relationalEventModeling")
# options$timeVariable <- "time"
# options$actorVariableSender <- "actor1"
# options$actorVariableReceiver <- "actor2"
# options$timepointInputUpper <- "Inf"
# options$eventHistory <- "decay"
# options$eventHistorySingleInput <- 100
# options$eventDirection <- "undirected"
# options$syncAnalysisBox <- TRUE
#
# options$actorDataList <- list(list(actorData = "attributes2.csv", value = "#"))
# options$dyadDataList <- list(list(dyadData = "social.csv", value = "#"))
#
# options$syncAnalysisBox <- TRUE
# options$exogenousEffectsTable <- list(
#   list(average = FALSE, difference = TRUE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "age"),
#   list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = TRUE, value = "social")
# )
# options$specifiedExogenousEffects <- list(
#   list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')"),
#   list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "none", value = "tie('social')")
# )
#
# options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
#                                        endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
#                                        endogenousEffectsConsiderType = "no"))
#
# options$interactionEffects <- list(
#   list(includeInteractionEffect = TRUE, value = "difference('age') * Inertia")
# )
# options$timepointInputUpper <- "Inf"
# options$eventHistory <- "full"
# options$eventDirection <- "directed"
# options$riskset <- "active"
#
# # options$actorDataList <- list(list(actorData = "tests/testthat/attributes2.csv", value = "#"))
# # options$dyadDataList <- list(list(dyadData = "tests/testthat/social.csv", value = "#"))
#
# set.seed(1)
# results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
# # results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)
#
#
# test_that("Coefficient estimates tie model table results match", {
#   table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list("baseline", -6.52166655849015, 5.04891549570117e-240, 0, 0.195845568243134,
#                                       -33.3000466489687, "Inertia", 0.0313931376360234, 1.15126461850371e-14,
#                                       0, 0.0037235495344607, 8.43097086408715, "Difference_age", -0.00701130789106741,
#                                       0.954615113967713, 0.371586871699675, 0.00784697232226769, -0.893504858067502,
#                                       "Tie_social", 0.146265791330681, 0.91593649670018, 0.145970709698504,
#                                       0.100601544579648, 1.45391198457078, "Difference_age:Inertia",
#                                       0.000539918874242037, 0.83002654012185, 0.0537972774065809,
#                                       0.000279973342856817, 1.92846529149085))
# })
#
# test_that("Model fit tie model table results match", {
#   table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTable"]][["data"]]
#   jaspTools::expect_equal_tables(table,
#                                  list(983, 9441.55126411736, "Null deviance", "", 978, 9179.59793153212,
#                                       "Residual deviance", "", 5, 261.953332585244, "Chi^2", 0, "",
#                                       9189.59793153212, "AIC", "", "", 9189.65934401932, "AICC", "",
#                                       "", 9214.05097713285, "BIC", ""))
# })


# tie directed, ordinal #
# FAILS with remstimate
# options <- jaspTools::analysisOptions("relationalEventModeling")
# options$timeVariable <- "time"
# options$actorVariableSender <- "actor1"
# options$actorVariableReceiver <- "actor2"
# options$timepointInputUpper <- "Inf"
# options$eventDirection <- "directed"
# options$syncAnalysisBox <- TRUE
# options$eventSequence <- "ordinal"
#
# set.seed(1)
# results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options)


# actor model #
# basic input
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$orientation <- "actor"

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history.csv", options)
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
  list(average = TRUE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "age"),
  list(average = FALSE, difference = TRUE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE, tie = FALSE, value = "age")
)
options$specifiedExogenousEffects <- list(
  list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std", value = "average('age')"),
  list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none", value = "difference('age')")
)
options$exogenousEffectsTableSender <- list(
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = TRUE, tie = FALSE, value = "extraversion"),
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE, send = TRUE, tie = FALSE, value = "sex")
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
options$actorDataList <- list(list(actorData = "info.csv", value = "#"))

# options$actorDataList <- list(list(actorData = "tests/testthat/info.csv", value = "#"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history.csv", options)

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

# actor model, risket active # fails

# actor model, ordinal #



# input:
# dyadic attributes format test
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$timepointInputUpper <- "Inf"
options$actorDataList <- list(list(actorData = "info.csv", value = "#"))
options$dyadDataList <- list(list(dyadData = "wideDyad.csv", value = "#"),
                             list(dyadData = "longDyad1.csv", value = "#2"),
                             list(dyadData = "longDyad2.csv", value = "#4"))
options$exogenousEffectsTable <- list(
  list(average = FALSE, difference = TRUE, event = FALSE,
       maximum = FALSE, minimum = FALSE, receive = FALSE, same = FALSE,
       send = FALSE, tie = FALSE, value = "age"),
  list(average = TRUE, difference = FALSE, event = FALSE, maximum = FALSE,
       minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE,
       tie = FALSE, value = "extraversion"),
  list(average = FALSE, difference = FALSE, event = FALSE, maximum = FALSE,
       minimum = FALSE, receive = FALSE, same = FALSE, send = FALSE,
       tie = TRUE, value = "dy1")
)
options$specifiedExogenousEffects <- list(list(exogenousEffectsAbsolute = TRUE, exogenousEffectsScaling = "none",
                                               value = "difference('age')"),
                                          list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std",
                                               value = "average('extraversion')"),
                                          list(exogenousEffectsAbsolute = FALSE, exogenousEffectsScaling = "std",
                                               value = "tie('dy1')"))

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "history.csv", options)
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

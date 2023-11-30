


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
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.54902255758944, 0, 0, 0.0318950466054745, -205.330396239815
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
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
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
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
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
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
# results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.90342845978138, 0, 0, 0.0707106781178174, -97.6292215481085
                                 ))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 14841.3783482209, "Null deviance", "", 982, 3161.37138392202,
                                      "Residual deviance", "", 1, 11680.0069642989, "Chi^2", 0, "",
                                      3163.37138392202, "AIC", "", "", 3163.37546139399, "AICC", "",
                                      "", 3168.26199304217, "BIC", ""))
})

# BSIR and interval window
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$syncAnalysisBox <- TRUE
options$method <- "BSIR"
options$eventHistory <- "interval"
options$eventHistoryIntervalInputLower <- 50
options$eventHistoryIntervalInputUpper <- 100
options$timepointInputLower <- 1
options$timepointInputUpper <- "200"
options$endogenousEffects <- list(list(value = "inertia", translatedName = "Inertia", includeEndoEffect = TRUE,
                                       endogenousEffectsUnique = FALSE, endogenousEffectsScaling = "none",
                                       endogenousEffectsConsiderType = "no"))
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -7.15198275739228, 0.0795393109438783, 0, -7.30811669991729,
                                      -7.15040793617075, -7.0020148776776, "Inertia", 1.68248335791643,
                                      0.113421904446645, 5.18241861461358e-47, 1.4635571359845, 1.67556754609801,
                                      1.89428089512612))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3076.77214310402, "BIC"))
})



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

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)
# results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = T)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
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
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 13478.65099124, "Null deviance", "", 978, 12836.0866321316,
                                      "Residual deviance", "", 5, 642.564359108472, "Chi^2", 0, "",
                                      12846.0866321316, "AIC", "", "", 12846.1480446188, "AICC", "",
                                      "", 12870.5396777323, "BIC", ""))
  })



# tie directed , active riskset #
options$timepointInputUpper <- "Inf"
options$eventHistory <- "full"
options$eventDirection <- "directed"
options$riskset <- "active"

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red.csv", options)

test_that("Coefficient estimates tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_coefficientsContainer"]][["collection"]][["mainContainer_coefficientsContainer_coefficientsTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("baseline", -6.52166655849015, 5.04891549570117e-240, 0, 0.195845568243134,
                                      -33.3000466489687, "Inertia", 0.0313931376360234, 1.15126461850371e-14,
                                      0, 0.0037235495344607, 8.43097086408715, "Difference_age", -0.00701130789106741,
                                      0.954615113967713, 0.371586871699675, 0.00784697232226769, -0.893504858067502,
                                      "Tie_social", 0.146265791330681, 0.91593649670018, 0.145970709698504,
                                      0.100601544579648, 1.45391198457078, "Difference_age:Inertia",
                                      0.000539918874242037, 0.83002654012185, 0.0537972774065809,
                                      0.000279973342856817, 1.92846529149085))
})

test_that("Model fit tie model table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_modelFitContainer"]][["collection"]][["mainContainer_modelFitContainer_modelFitTableTie"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(983, 9441.55126411736, "Null deviance", "", 978, 9179.59793153212,
                                      "Residual deviance", "", 5, 261.953332585244, "Chi^2", 0, "",
                                      9189.59793153212, "AIC", "", "", 9189.65934401932, "AICC", "",
                                      "", 9214.05097713285, "BIC", ""))
})


# tie directed, ordinal #
#### HERE ####
# not working
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

# actor model, risket active #

# actor model, ordinal #

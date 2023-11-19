


context("Relational Event Modeling")

# does not test:


#### baseline test tie model directed ####
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
options$actorData <- ""
options$syncAnalysisBox <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red", options)

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


#### so there seems to be an issue with inertia being capitalized, why?
#' also the both endo effect in the interaction translates incorrectly
#'
options <- jaspTools::analysisOptions("relationalEventModeling")
options$timeVariable <- "time"
options$actorVariableSender <- "actor1"
options$actorVariableReceiver <- "actor2"
options$weightVariable <- "duration"
options$typeVariable <- "sensor"
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

set.seed(1)
results <- jaspTools::runAnalysis("relationalEventModeling", "team4_red", options)
results <- jaspTools::runAnalysis("relationalEventModeling", edges, options, makeTests = F)

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

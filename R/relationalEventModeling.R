#' TODO:
#' - lots of qml conditioning? how to change the rows of the componentlist depending on the effect? even possible?
#' - state things,
#'  - retrigger exoEffects feedback somehow
#' - other model types in qml... undirected, riskset, etc.
#' - effects such as "both_male"
#' - Bruno: maybe it makes sense to also work with R sources for the different qml elements depending on model type
#' - finish the storing and reusing of the statsobject in storage




# Copyright (C) 2013-2022 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.


# Main function ----

relationalEventModeling <- function(jaspResults, dataset, options) {

  # sink(file="~/Downloads/log.txt")
  # on.exit(sink(NULL))

  .feedbackEndoEffects(jaspResults, options)

  ready <- (options[["timeVariable"]] != "") && (length(options[["actorVariables"]]) > 1)

  if (!ready) return()

  options <- .getExogenousEffects(options)

  .feedbackExoEffects(jaspResults, options)

  dataset <- .remReadData(dataset, options)

  .remErrorHandling(dataset, options)
  .remMainContainer(jaspResults, options)
  .remRemify(jaspResults, dataset, options)

  .remRemstats(jaspResults, dataset, options)
  # .remRemstatsBeta(jaspResults, dataset, options)

  .remRemstimate(jaspResults, dataset, options)

  .remModelFitTable(jaspResults, options)
  .remCoefficientsTable(jaspResults, options)

  return()
}

.getExogenousEffects <- function(options) {

  # if (!is.null(options[["exoEffects"]]))
  #   return(options)

  exoTable <- options$exogenousEffectsTable
  exoNames <- exoTable[[1]]$levels
  exoEffectsList <- c("Average", "Difference", "Event", "Maximum", "Minimum", "Receive", "Same", "Send", "Tie")
  exoInds <- lapply(exoTable, function(x) which(x[["values"]] == 1))

  if (length(unlist(exoInds)) == 0)
    return(options)

  options[["exoEffects"]] <- list()
  options[["exoEffects"]][["variableNames"]] <- jaspBase::encodeColNames(c(exoNames[sort(unique(unlist(exoInds)))], "time_y", "name"))
  exoEffectsNames <- c("Average", "Difference", "Event", "Maximum", "Minimum", "Receive", "Same", "Send", "Tie")
  names(exoInds) <- exoEffectsNames
  exoInds[sapply(exoInds, function(x) length(x) == 0)] <- NULL
  options[["exoEffects"]][["list"]] <- lapply(exoInds, function(x) exoNames[x])

  return(options)

}


.remReadData <- function(dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  if (!is.null(options[["exoEffects"]])) {
    if (options$weightVariable == "") {
      variables  <- c(options$timeVariable, options$actorVariables,
                      options$exoEffects$variableNames)
    } else {
      variables  <- c(options$timeVariable, options$actorVariables, options$weightVariable,
                      options$exoEffects$variableNames)
    }

  } else {
    if (options$weightVariable == "") {
      variables  <- c(options$timeVariable, options$actorVariables)
    } else {
      variables  <- c(options$timeVariable, options$actorVariables, options$weightVariable)
    }
  }

  dataset <- .readDataSetToEnd(columns = variables)
  return(dataset)
}

.feedbackExoEffects <- function(jaspResults, options) {
###########################################################################
### this still needs some work regarding the option to exit at the beginning
###########################################################################
  if (!(length(options$exoEffects) > 0))
    return()
      # || !is.null(jaspResults[["sourceTopics"]])) return()

  exo <- options$exoEffects
  exoList <- exo$list
  # lapply(exoList, function(x))

  exoNames <- names(exoList)
  outExoList <- list()
  for (i in 1:length(exoList)) {
    if (!is.null(exoList[[i]])) {
      nam1 <- tolower(exoNames[i])
      vars <- exoList[[i]]
      tmp <- as.list(paste0(nam1, "('", vars, "')"))
      outExoList <- append(outExoList, tmp)
    }
  }

  jaspResults[["specifiedEffectsFromR"]] <- createJaspQmlSource("specifiedEffectsFromR", outExoList)


  endoIndex <- grep("specifiedEndogenousEffects", names(options))
  outEndoList <- list()
  for (ii in seq_len(length(options[[endoIndex]]))) {
    outEndoList <- append(outEndoList,
                          paste0(options[[endoIndex]][[ii]][["variable"]], "('", options[[endoIndex]][[ii]][["endogenousEffectScaling"]], "')"))
  }

  interTmp <- combn(c(unlist(outExoList), unlist(outEndoList)), m = 2)
  inters <- as.list(paste0(interTmp[1, ], " * ", interTmp[2, ]))

  jaspResults[["possibleInteractionEffectsFromR"]] <- createJaspQmlSource("possibleInteractionEffectsFromR", inters)

  return()
}



.remErrorHandling <- function(dataset, options) {

  .hasErrors(dataset = dataset,
             type = 'infinity',
             exitAnalysisIfErrors = TRUE)
}

.remMainContainer <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]])) return()

  ###############################################
  # TODO: Dependencies ##########################
  ###############################################
  mainContainer <- createJaspContainer()
  mainContainer$dependOn(c("timeVariable", "actorVariables", "weightVariable"))
  jaspResults[["mainContainer"]] <- mainContainer

  # mainContainer$setError("ERROR")


  return()
}

.remRemify <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remifyResultState"]]$object))
    return()

  # the first three columns should always contain the time and actors variables.
  # the following column needs to be named "weight"
  # what though if the actors are switched? that shouldnt be an issue, should it?
  if ("weight" %in% colnames(dataset)) {
    dt <- cbind(dataset[, 1:3], dataset[, "weight"])
  } else {
    dt <- dataset[, 1:3]
  }

  # is there any other naAction even possible?
  if (anyNA(dataset) & options$naAction == "listwise") {
    dt <- dt[complete.cases(dt), ]
  }

  rehObject <- try(remify::remify(edgelist = dt,
                                 directed = options[["eventDirection"]] == "directed",
                                 ordinal = options[["eventSequence"]] == "orderOnly",
                                 model = options[["orientation"]],
                                 riskset = options[["riskset"]]))

  if (isTryError(rehObject)) {
    errmsg <- gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(rehObject))
    jaspResults[["mainContainer"]]$setError(errmsg)
  }


  remifyResultState <- createJaspState(rehObject)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "naAction", "weightVariable"))

  jaspResults[["mainContainer"]][["remifyResultState"]] <- remifyResultState

  return()
}


.remRemstats <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object))
    return()

  if (jaspResults[["mainContainer"]]$getError()) return() # doesnt make sense to continue if there is already an error

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

  effects <- .translateEffects(options)

  effectsText <- Reduce(paste, deparse(effects))
  effectsText <- gsub("+", "+\n", effectsText, fixed = TRUE)

  # show the effects to the output
  outText <- createJaspHtml(text = gettextf("The effects were specified as: \n%s",
                                            effectsText))
  outText$position <- 0.5
  jaspResults[["mainContainer"]][["effectsCall"]] <- outText

  dtExo <- .separateCovariateData(dataset, options)

  statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = effects, attr_data = dtExo))

  if (isTryError(statsObject)) {
    errmsg <- gettextf("Remstats failed. Internal error message: %s", .remExtractErrorMessage(statsObject))
    jaspResults[["mainContainer"]]$setError(errmsg)
  }

  remstatsResultState <- createJaspState(statsObject)
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  remstatsResultState$dependOn(c("eventDirection", "eventSequence",
                                 "orientation", "riskset", "naAction",
                                 endoDepend, "specifiedExogenousEffects",
                                 "interactionEffects"))

  jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState

  return()
}




.remRemstimate <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    return()

  if (jaspResults[["mainContainer"]]$getError()) return() # doesnt make sense to continue if there is already an error

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object
  statsObject <- jaspResults[["mainContainer"]][["remstatsResultState"]]$object

  fit <- try(remstimate::remstimate(reh = rehObject, stats = statsObject,
                                method = options[["method"]]))

  if (isTryError(fit)) {
    errmsg <- gettextf("Remstimate failed. Internal error message: %s", .extractErrorMessage(fit))
    jaspResults[["mainContainer"]]$setError(errmsg)
  }

  remstimateResultState <- createJaspState(fit)
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  remstimateResultState$dependOn(c("eventDirection", "eventSequence",
                                   "orientation", "riskset", "naAction", "method",
                                   endoDepend, "specifiedExogenousEffects",
                                   "interactionEffects"))

  jaspResults[["mainContainer"]][["remstimateResultState"]] <- remstimateResultState
}


.remModelFitTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["modelFitTable"]])) return()

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object

  modelFitTable <- createJaspTable(title = gettext("Model Fit Table"))
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  modelFitTable$dependOn(c("eventDirection", "eventSequence",
                           "orientation", "riskset", "naAction", "method",
                           endoDepend, "specifiedExogenousEffects",
                           "interactionEffects"))
  modelFitTable$position <- 1

  modelFitTable$addColumnInfo(name = "fitmeasure",     title = gettext("Statistic"), type= "string")
  modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
  modelFitTable$addColumnInfo(name = "df",   title = gettext("df"),  type= "number")
  modelFitTable$addColumnInfo(name = "pvalue",   title = gettext("p"),     type= "number")

  jaspResults[["mainContainer"]][["modelFitTable"]] <- modelFitTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  footnote <- ""
  if (!is.null(remResults)){
    modelFitTable$addFootnote(footnote)
  }

  if (!is.null(remResults)){

    res <- summary(remResults)
    dtFill <- data.frame(fitmeasure = c("Null deviance", "Residual deviance", "Chi^2", "AIC", "AICC", "BIC"))
    dtFill$estimate <- c(res$null.deviance, res$residual.deviance, res$model.deviance, res$AIC, res$AICC, res$BIC)
    dtFill$df <- c(res$df.null, res$df.residual, res$df.model, NA_real_, NA_real_, NA_real_)
    dtFill$pvalue <- c(NA_real_, NA_real_, res$chiP, NA_real_, NA_real_, NA_real_)

    modelFitTable$setData(dtFill)
  }

  return()
}


.remCoefficientsTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["coefficientsTable"]])) return()

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object


  coefficientsTable <- createJaspTable(title = gettext("Coefficients Table"))
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  coefficientsTable$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "naAction", "method",
                               endoDepend, "specifiedExogenousEffects",
                               "interactionEffects"))
  coefficientsTable$position <- 2

  coefficientsTable$addColumnInfo(name = "coef",     title = gettext("Coefficient"), type= "string")
  coefficientsTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
  # coefficientsTable$addColumnInfo(name = "exp", title = "exp(Estimate)",    type= "number")
  coefficientsTable$addColumnInfo(name = "stdErr",   title = gettext("Std. Error"),  type= "number")
  coefficientsTable$addColumnInfo(name = "zValue",   title = gettext("z-value"),     type= "number")
  coefficientsTable$addColumnInfo(name = "prZ",      title = gettext("p"),     type= "number")
  coefficientsTable$addColumnInfo(name = "pr0",      title = gettext("p(=0)"),       type= "number")

  jaspResults[["mainContainer"]][["coefficientsTable"]] <- coefficientsTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  footnote <- ""
  if (!is.null(remResults)){
    coefficientsTable$addFootnote(footnote)
  }

  if (!is.null(remResults)){

    res <- summary(remResults)
    dtFill <- data.frame(res$coefsTab)
    colnames(dtFill) <- c("estimate", "stdErr", "zValue", "prZ", "pr0")
    rwnames <- rownames(res$coefsTab)
    # check for the endoNames
    endoNames <- .endoEffectsMatching(options)
    for (ii in 1:nrow(endoNames)) {
      ind <- grep(endoNames[ii, "endoEffectsR"], rwnames)
      if (length(ind) > 0) {
        rwnames[ind] <- gsub(endoNames[ii, "endoEffectsR"], endoNames[ii, "endoEffectsJasp"], rwnames[ind])
      }
    }
    # rwnames <- tolower(rwnames)
    dtFill$coef <- rwnames
    # dtFill$exp <- exp(dtFill$estimate)

    coefficientsTable$setData(dtFill)
  }

  return()
}



.endoEffectsMatching <- function(options) {

  if (options[["orientation"]] == "tie" && options[["eventDirection"]] == "directed") {
    endoEffectsJasp <- gettext(c("In degree receiver", "In degree sender", "Fixed effects for event type", "Inertia",
                               "Incoming shared partners", "Incoming two-path", "Outgoing shared partners",
                               "Outgoing two-path", "Out deregee receiver", "Out degree sender", "Pshift AB-AB",
                               "Pshift AB-AY", "Pshift AB-BA", "Pshift AB-BY", "Pshift AB-XA", "Pshift AB-XB", "Pshift AB-XY",
                               "Recency continue", "Recency receive of receiver", "Recency receive of sender",
                               "Recency send of receiver", "Recency send of sender", "Reciprocity", "Recency rank receive",
                               "Recency rank send", "Total degree dyad", "Total degree receiver", "Total degree sender",
                               "User statistics"))

    endoEffectsR <- c("indegreeReceiver", "indegreeSender", "FEtype", "inertia", "isp", "itp", "osp", "otp",
                      "outdegreeReceiver", "outdegreeSender", "psABAB", "psABAY", "psABBA", "psABBY", "psABXA",
                      "psABXB", "psABXY", "recencyContinue", "recencyReceiveReceiver", "recencyReceiveSender",
                      "recencySendReceiver", "recencySendSender", "reciprocity", "rrankReceive", "rrankSend",
                      "totaldegreeDyad", "totaldegreeReceiver", "totaldegreeSender", "userStat")

  } else if (options[["orientation"]] == "tie" && options[["eventDirection"]] == "undirected") {
    endoEffectsJasp <- gettext(c("Degree difference",	"Degree maximum", "Degree minimum",	"Fixed effects for event type",
                                 "Inertia",	"Pshift AB-AB", "Pshift AB-AY",	"Shared partners",	"Unique shared partners",
                                 "Recency continue", "Total degree dyad", "User statistics"))

    endoEffectsR <- c("degreeDiff", "degreeMax", "degreeMin", "FEtype", "inertia", "psABAB",
                      "psABAY", "sp", "spUnique", "recencyContinue", "totaldegreeDyad", "userStat")

  } else if (options[["orientation"]] == "actor" && options[["actorDirection"]] == "sender") {
    endoEffectsJasp <- gettext(c("Degree difference",	"Degree maximum", "Degree minimum",	"Fixed effects for event type",
                                 "Inertia",	"Pshift AB-AB", "Pshift AB-AY",	"Shared partners",	"Unique shared partners",
                                 "Recency continue", "Total degree dyad", "User statistics"))

    endoEffectsR <- c("degreeDiff", "degreeMax", "degreeMin", "FEtype", "inertia", "psABAB",
                      "psABAY", "sp", "spUnique", "recencyContinue", "totaldegreeDyad", "userStat")

  } else if (options[["orientation"]] == "actor" && options[["actorDirection"]] == "receiver") {
    endoEffectsJasp <- gettext(c("Degree difference",	"Degree maximum", "Degree minimum",	"Fixed effects for event type",
                                 "Inertia",	"Pshift AB-AB", "Pshift AB-AY",	"Shared partners",	"Unique shared partners",
                                 "Recency continue", "Total degree dyad", "User statistics"))

    endoEffectsR <- c("degreeDiff", "degreeMax", "degreeMin", "FEtype", "inertia", "psABAB",
                      "psABAY", "sp", "spUnique", "recencyContinue", "totaldegreeDyad", "userStat")

  }


  out <- cbind(endoEffectsJasp, endoEffectsR)
  return(out)

}

.endoNoScalingList <- function() {
  noScaling <- c("psABAB", "psABAY", "psABBA", "psABBY", "psABXA", "psABXB", "psABXY",
                 "recencyContinue", "recencyReceiveReceiver", "recencySendReceiver", "recencySendSender",
                 "recencyRankReceiver", "recencyRankSend")
  return(noScaling)
}


.translateEffects <- function(options) {

  effects <- "~1"
  # endogenous effects:
  endoIndex <- grep("specifiedEndogenousEffects", names(options))
  if (length(options[[endoIndex]]) > 0) {
    endoEffects <- sapply(options[[endoIndex]], function(x) x[["variable"]])
    # endoEffectsJaspSave <- endoEffects
    endoScaling <- sapply(options[[endoIndex]], function(x) x[["endogenousEffectScaling"]])
    endoEffectsJaspSave <- paste0(endoEffects, "('", endoScaling, "')")
    endoNames <- .endoEffectsMatching(options)
    endoEffects <- endoNames[which(endoNames %in% endoEffects, arr.ind = TRUE), "endoEffectsR"]
    endoNoScalingNames <- .endoNoScalingList()
    endoMatch <- which(!is.na(match(endoEffects, endoNoScalingNames)))
    if (length(endoMatch) > 0) {
      endoEffects[-endoMatch] <- paste0(endoEffects[-endoMatch], "(scaling = ", "'", endoScaling[-endoMatch], "'", ")")
      endoEffects[endoMatch] <- paste0(endoEffects[endoMatch], "()")
    } else {
      endoEffects <- paste0(endoEffects, "(scaling = ", "'", endoScaling, "'", ")")
    }

    endoEffectsRSave <- endoEffects
    endoEffects <- paste(endoEffects, collapse = " + ")
    effects <- paste(effects, "+", endoEffects)
  }

  # exogenous effects
  exoIndex <- grep("specifiedExogenousEffects", names(options))
  if (length(exoIndex) > 0 && length(options[[exoIndex]]) > 0) {
    exoEffects <- sapply(options[[exoIndex]], function(x) x[["value"]])
    exoEffectsSave1 <- exoEffects
    exoEffects <- gsub(")", "", exoEffects)
    exoScaling <- sapply(options[[exoIndex]], function(x) x[["exoEffectsScaling"]])
    # exoAbsolute <- sapply(options[[exoIndex]], function(x) x[["absolute"]])
    exoEffects <- paste0(exoEffects, ", scaling = '", exoScaling, "')")#, absolute = ", exoAbsolute, ")")
    # event is a special case, has no scaling:
    eventIndex <- grep("event", exoEffects)
    if (length(eventIndex) > 0) {
      for (ev in 1:length(eventIndex)) {
        eventName <- options[["exoEffects"]][["list"]][["Event"]][ev]
        exoEffects[eventIndex[ev]] <- gsub("scaling.[^)]*", "placeHolder", exoEffects[eventIndex[ev]])
        exoEffects[eventIndex[ev]] <- gsub("'","", exoEffects[eventIndex[ev]], fixed = TRUE)
        exoEffects[eventIndex[ev]] <- gsub("placeHolder", paste0("'", eventName, "'"), exoEffects[eventIndex[ev]], fixed = TRUE)
      }
    }
    exoEffectsSave2 <- exoEffects
    exoEffects <- paste0(exoEffects, collapse = " + ")
    effects <- paste(effects, "+", exoEffects)
  }

  # interactions
  interIndex <- grep("interactionEffects", names(options))
  if (length(interIndex) > 0 && length(options[[interIndex]]) > 0) {
    interEffects <- sapply(options[[interIndex]], function(x) if (x[["include"]]) x[["value"]] else NULL)
    interEffects[sapply(interEffects, is.null)] <- NULL
    if (length(interEffects) > 0) {
      interEffects <- unlist(interEffects)


      # replace the endo effects
      if (length(options[[endoIndex]]) > 0) {
        for (ee in 1:length(endoEffectsJaspSave)) {
          ind <- grep(endoEffectsJaspSave[ee], interEffects, fixed = TRUE)
          if (length(ind) > 0) {
            interEffects <- gsub(endoEffectsJaspSave[ee], endoEffectsRSave[ee], interEffects, fixed = TRUE)
          }
        }
      }


      # replace the exo effects
      if (length(options[[endoIndex]]) > 0) {
        for (eee in 1:length(exoEffectsSave1)) {
          ind <- grep(exoEffectsSave1[eee], interEffects, fixed = TRUE)
          if (length(ind) > 0) {
            interEffects <- gsub(exoEffectsSave1[eee], exoEffectsSave2[eee], interEffects, fixed = TRUE)
          }
        }
      }

      interEffects <- gsub(" * ", ":", interEffects, fixed = TRUE)
      interEffects <- paste0(interEffects, collapse = " + ")
      effects <- paste0(effects, " + ", interEffects)
    }
  }

  effects <- eval(parse(text = effects))

  return(effects)
}



.feedbackEndoEffects <- function(jaspResults, options) {

  if (!is.null(jaspResults[["endoEffectsFromR"]])) return()

  if (options[["orientation"]] == "tie" && options[["eventDirection"]] == "directed") {

    endos <- .endoEffectsMatching(options)
    specs <- endos[, "endoEffectsJasp"]
    specs <- as.list(specs)

  } else if (options[["orientation"]] == "tie" && options[["eventDirection"]] == "undirected") {

    endos <- .endoEffectsMatching(options)
    specs <- endos[, "endoEffectsJasp"]
    specs <- as.list(specs)

  } else if (options[["orientation"]] == "actor" && options[["actorDirection"]] == "sender") {

    endos <- .endoEffectsMatching(options)
    specs <- endos[, "endoEffectsJasp"]
    specs <- as.list(specs)

  } else if (options[["orientation"]] == "actor" && options[["actorDirection"]] == "receiver") {

    endos <- .endoEffectsMatching(options)
    specs <- endos[, "endoEffectsJasp"]
    specs <- as.list(specs)
  }

  src <- createJaspQmlSource("endoEffectsFromR", specs)
  src$dependOn(c("eventDirection", "orientation"))
  jaspResults[["endoEffectsFromR"]] <- src

  return()

}


.remExtractErrorMessage <- function (error) {
  stopifnot(length(error) == 1)
  if (isTryError(error)) {
    msg <- error[1]
    return(trimws(msg))
  }
  else if (is.character(error)) {
    return(trimws(error))
  }
  else {
    stop("Do not know what to do with an object of class `",
         class(error)[1], "`; The class of the `error` object should be `try-error` or `character`!",
         domain = NA)
  }
}

.separateCovariateData <- function(dataset, options) {

  exoVariables <- unique(unlist(options[["exoEffects"]][["variableNames"]]))

  if (!is.null(exoVariables)) {
    dtCv <- dataset[, exoVariables]
    colnames(dtCv) <- jaspBase::decodeColNames(colnames(dtCv))
    colnames(dtCv) <- gsub("time_y", "time", colnames(dtCv))

    # if a event effect is specified we need the variable to be present
    exoList <- options[["exoEffects"]][["list"]]
    if (!is.null(exoList[["Event"]])) {
      eventVariables <- jaspBase::decodeColNames(exoList[["Event"]])
      for (ii in 1:length(eventVariables)) {
        assign(eventVariables[ii], dtCv[, eventVariables[ii]], pos = 1) # dont know why pos=1 is working....
      }
      # drop the event variables
      dtCv <- dtCv[, !(names(dtCv) %in% eventVariables)]
    }

    ##############
    # so in theory the exo data should now only contain the covariates for each person, givne
    # that we eliminiated the event things that are part of the event data
    # Or are there other variables that I forget about?
    ##############

    # unfactor the data
    indx <- sapply(dtCv, is.factor)
    dtCv[indx] <- lapply(dtCv[indx], function(x) as.numeric(as.character(x)))

    dtCvP <- dtCv[complete.cases(dtCv), ]

  } else {
    dtCvP <- NULL
  }
  return(dtCvP)
}

.matchEffectsForStats <- function(formulaOld, newEffects) {

  formulaOld <- as.character(formulaOld)
  newEffects <- as.character(newEffects)

  form <- strsplit(formulaOld, split = " + ", fixed = TRUE)
  form[[1]] <- NULL
  form <- unlist(form)

  effs <- strsplit(newEffects, split = " + ", fixed = TRUE)
  effs[[1]] <- NULL
  effs <- unlist(effs)

  dupInd <- match(form, effs)
  dupInd <- dupInd[complete.cases(dupInd)]

  effsOut <- effs[-dupInd]

  effsOut <- paste0(effsOut, collapse = " + ")
  effsOut <- paste0("~", effsOut)
  effsOut <- eval(parse(text = effsOut))

  return(effsOut)

}



.remRemstatsBeta <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) &&
      !is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) return()

  if (jaspResults[["mainContainer"]]$getError()) return() # doesnt make sense to continue if there is already an error

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

  effects <- .translateEffects(options)

  effectsText <- Reduce(paste, deparse(effects))
  effectsText <- gsub("+", "+\n", effectsText, fixed = TRUE)

  # show the effects to the output
  outText <- createJaspHtml(text = gettextf("The effects were specified as: \n%s",
                                            effectsText))
  outText$position <- 0.5
  jaspResults[["mainContainer"]][["effectsCall"]] <- outText

  dtExo <- .separateCovariateData(dataset, options)

  # check if there is already a statsObject in storage
  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object)) {
    statsObjectOld <- jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object
    formulaOld <- attr(statsObjectOld, "formula")
    effsOut <- .matchEffectsForStats(formulaOld, effects)
    statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = effsOut, attr_data = dtExo))
    statsObjectCombined <- try(remstats::bind_remstats(statsObjectOld, statsObject))
    jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object <- statsObjectCombined

    # now take the slices out of the combined statsObject and pass them on

    print(effects)
    print(dimnames(statsObjectCombined))
    # remstatsResultState <- createJaspState(statsObject)
    # jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState


  } else {
    # in the first round both states are created
    statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = effects, attr_data = dtExo))
    remstatsResultState <- createJaspState(statsObject)
    jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState
    remstatsResultStateStorage <- createJaspState(statsObject)
    jaspResults[["mainContainer"]][["remstatsResultStateStorage"]] <- remstatsResultStateStorage
  }


  if (isTryError(statsObject)) {
    errmsg <- gettextf("Remstats failed. Internal error message: %s", .remExtractErrorMessage(statsObject))
    jaspResults[["mainContainer"]]$setError(errmsg)
  }

  print("here")
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  remstatsResultState$dependOn(c("eventDirection", "eventSequence",
                                 "orientation", "riskset", "naAction",
                                 endoDepend, "specifiedExogenousEffects",
                                 "interactionEffects"))
  return()

  #' To sum this up, the remaining difficulty is to use specific subparts of the stats array by name or by number.
  #' The names look like this, which is a bit hard to replicate
  #' [1] "baseline"                   "indegreeReceiver"
  #' [3] "inertia"                    "event_JaspColumn_3_Encoded"
  #' However, the thing about using a subarray is that it has to be a new object that has to be assigned a class
  #' in order for it to be used in remstimate. Given that the whole motivation for this is to do it with large networks
  #' this means we use a ton of memory, even though we get a speedup. I wonder if that will work at all.
  #'
  #' or maybe try putting the storage elemtn in the state but never delete it, aka no dependencies
  #' and then also put the current statsObject in another state andpass that on, and always delete when options change
}



#' # Leave this for now unless the same exo effect should be specified with different scaling
# .feedbackInterEffectsWindow <- function(jaspResults, options) {
#   ###########################################################################
#   ### this still needs some work regarding the option to exit at the beginning
#   ###########################################################################
#   if (length(options$exoEffects) <= 1)
#     return()
#   # || !is.null(jaspResults[["sourceTopics"]])) return()
#
#   # exogenous effects
#   exoIndex <- grep("specifiedExoEffects", names(options))
#   if (!is.null(options[[exoIndex]])) {
#     exoEffects <- sapply(options[[exoIndex]], function(x) x[["value"]])
#     exoScaling <- sapply(options[[exoIndex]], function(x) x[["exoEffectsScaling"]])
#     exoEffects <- paste0(exoEffects, ", scaling = '", exoScaling, "', absolute = ", exoAbsolute, ")")
#   }
#
#
#   # jaspResults[["specifiedEffectsFromR"]] <- createJaspQmlSource("specifiedEffectsFromR", outExoList)
#
#
#   endoIndex <- grep("specifiedEndogenousEffects", names(options))
#   outEndoList <- list()
#   for (ii in seq_len(length(options[[endoIndex]]))) {
#     outEndoList <- append(outEndoList,
#                           paste0(options[[endoIndex]][[ii]][["variable"]], " (", options[[endoIndex]][[ii]][["endogenousEffectScaling"]], ")"))
#   }
#
#   interTmp <- combn(c(unlist(outExoList), unlist(outEndoList)), m = 2)
#   inters <- as.list(paste0(interTmp[1, ], " * ", interTmp[2, ]))
#
#   jaspResults[["possibleInteractionEffectsFromR"]] <- createJaspQmlSource("possibleInteractionEffectsFromR", inters)
#
#   return()
# }

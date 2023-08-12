#' TODO:
#' - separate the daydic data from the covariates data for now
#' - lots of qml conditioning? how to change the rows of the componentlist depending on the effect? even possible?
#' - state things,
#'  - retrigger exoEffects feedback somehow
#' - other model types in qml... undirected, riskset, etc.
#' - effects such as "both_male"
#' maybe it makes sense to also work with R sources for the different qml elements depending on model type




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

frequentistRelationalEventModeling <- function(jaspResults, dataset, options) {

  # sink(file="~/Downloads/log.txt")
  # on.exit(sink(NULL))

  ready <- (length(options[["timeVariable"]]) > 0) && (length(options[["actorVariables"]]) > 1)

  if (!ready) return()

  options <- .getExogenousEffects(options)

  .feedbackEffectsWindow(jaspResults, options)

  dataset <- .remReadData(dataset, options)

  # .feedbackInterEffectsWindow(jaspResults, options)

  .remErrorHandling(dataset, options)
  .remMainContainer(jaspResults)
  .remRemify(jaspResults, dataset, options)

  .remRemstats(jaspResults, dataset, options)
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

.feedbackEffectsWindow <- function(jaspResults, options) {
###########################################################################
### this still needs some work regarding the option to exit at the beginning
###########################################################################
  if (length(options$exoEffects) <= 1)
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

.remMainContainer <- function(jaspResults) {

  if (!is.null(jaspResults[["mainContainer"]])) return()

  ###############################################
  # TODO: Dependencies ##########################
  ###############################################
  mainContainer <- createJaspContainer()
  mainContainer$dependOn(c("timeVariable", "actorVariables", "weightVariable"))
  jaspResults[["mainContainer"]] <- mainContainer



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
                                 model = options[["modelOrientation"]],
                                 riskset = options[["riskset"]]))

  if (isTryError(rehObject)) {
    errmsg <- gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(rehObject))
    jaspResults[["mainContainer"]]$setError(errmsg)
  }


  remifyResultState <- createJaspState(rehObject)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "modelOrientation", "riskset", "naAction", "weightVariable"))

  jaspResults[["mainContainer"]][["remifyResultState"]] <- remifyResultState

  return()
}

.remRemstats <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object))
    return()

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

  effects <- .translateEffects(options)

  effectsText <- Reduce(paste, deparse(effects))
  effectsText <- gsub("+", "+\n", effectsText, fixed = TRUE)

  # print the effects to the output
  outText <- createJaspHtml(text = gettextf("The effects were specified as: \n%s",
                                            effectsText))
  outText$position <- 0.5
  jaspResults[["mainContainer"]][["effectsCall"]] <- outText

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


    # unfactor the data
    indx <- sapply(dtCv, is.factor)
    dtCv[indx] <- lapply(dtCv[indx], function(x) as.numeric(as.character(x)))

    # this whole block needs some rework, it seems the NAs are not found in JASP.
    firstNA <- min(which(is.na(dtCv), arr.ind = TRUE)[, "row"])
    nr <- nrow(dtCv)
    # are all entries between first NA and last row NA, then it is a person variable:
    colnams <- list()
    for (cc in 1:ncol(dtCv)) {
      if (all(is.na(dtCv[firstNA:nr, cc]))) {
        colnams <- append(colnams, colnames(dtCv)[cc])
      }
    }

    dtCvP <- dtCv[, unlist(colnams)]

  } else {
    dtCvP <- NULL
  }


  statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = effects, attr_data = dtCvP))

  if (isTryError(statsObject)) {
    errmsg <- gettextf("Remstats failed. Internal error message: %s", .extractErrorMessage(statsObject))
    jaspResults[["mainContainer"]]$setError(errmsg)
  }

  remstatsResultState <- createJaspState(statsObject)
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  remstatsResultState$dependOn(c("eventDirection", "eventSequence",
                                 "modelOrientation", "riskset", "naAction",
                                 endoDepend, "specifiedExogenousEffects"))

  jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState

  return()
}


.remRemstimate <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    return()

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
                                   "modelOrientation", "riskset", "naAction", "method",
                                   endoDepend, "specifiedExogenousEffects"))

  jaspResults[["mainContainer"]][["remstimateResultState"]] <- remstimateResultState
}


.remModelFitTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["modelFitTable"]])) return()

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object

  modelFitTable <- createJaspTable(title = gettext("Model Fit Table"))
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  modelFitTable$dependOn(c("eventDirection", "eventSequence",
                           "modelOrientation", "riskset", "naAction", "method",
                           endoDepend, "specifiedExogenousEffects"))
  modelFitTable$position <- 1

  modelFitTable$addColumnInfo(name = "fitmeasure",     title = gettext("Statistic"), type= "string")
  modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
  modelFitTable$addColumnInfo(name = "df",   title = gettext("df"),  type= "number")
  modelFitTable$addColumnInfo(name = "pvalue",   title = gettext("p"),     type= "number")


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

  jaspResults[["mainContainer"]][["modelFitTable"]] <- modelFitTable

  return()
}


.remCoefficientsTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["coefficientsTable"]])) return()

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object


  coefficientsTable <- createJaspTable(title = gettext("Coefficients Table"))
  endoDepend <- names(options)[grep("specifiedEndogenousEffects", names(options))]
  coefficientsTable$dependOn(c("eventDirection", "eventSequence",
                               "modelOrientation", "riskset", "naAction", "method",
                               endoDepend, "specifiedExogenousEffects"))
  coefficientsTable$position <- 2

  coefficientsTable$addColumnInfo(name = "coef",     title = gettext("Coefficient"), type= "string")
  coefficientsTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
  coefficientsTable$addColumnInfo(name = "stdErr",   title = gettext("Std. Error"),  type= "number")
  coefficientsTable$addColumnInfo(name = "zValue",   title = gettext("z-value"),     type= "number")
  coefficientsTable$addColumnInfo(name = "prZ",      title = gettext("p"),     type= "number")
  coefficientsTable$addColumnInfo(name = "pr0",      title = gettext("p(=0)"),       type= "number")


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
    endoNames <- .endoEffectsMatching()
    for (ii in 1:nrow(endoNames)) {
      ind <- grep(endoNames[ii, "R_name"], rwnames)
      if (length(ind) > 0) {
        rwnames[ind] <- gsub(endoNames[ii, "R_name"], endoNames[ii, "JASP_name"], rwnames[ind])
      }
    }
    rwnames <- tolower(rwnames)
    dtFill$coef <- rwnames

    coefficientsTable$setData(dtFill)
  }

  jaspResults[["mainContainer"]][["coefficientsTable"]] <- coefficientsTable

  return()
}






# once Bruno fixes the VariablesList form, we dont need this anymore, since the values
# instead of the labels will be given to R
.endoEffectsMatching <- function() {

  effs <- c(
    "In degree receiver",						 "indegreeReceiver",
    "In degree sender",							 "indegreeSender"					,
    "Fixed effects for event type",	 "FEtype"						,
    "Inertia",												 "inertia"					,
    "Incoming shared partners",			 "isp"										,
    "Incoming two-path",							 "itp"										,
    "Outgoing shared partners",			 "osp"										,
    "Outgoing two-path",							 "otp"										,
    "Out deregee receiver",					 "outdegreeReceiver"			,
    "Out degree sender",							 "outdegreeSender"				,
    "Pshift AB-AB",									 "psABAB"						,
    "Pshift AB-AY",									 "psABAY"						,
    "Pshift AB-BA",									 "psABBA"									,
    "Pshift AB-BY",									 "psABBY"									,
    "Pshift AB-XA",									 "psABXA"									,
    "Pshift AB-XB",									 "psABXB"									,
    "Pshift AB-XY",									 "psABXY"									,
    "Recency continue",							 "recencyContinue"	,
    "Recency receive of receiver", 	 "recencyReceiveReceiver"	,
    "Recency receive of sender",			 "recencyReceiveSender"		,
    "Recency send of receiver",			 "recencySendReceiver"		,
    "Recency send of sender",				 "recencySendSender"			,
    "Reciprocity",										 "reciprocity"						,
    "Recency rank receive",					 "rrankReceive"						,
    "Recency rank send",							 "rrankSend"							,
    "Total degree dyad",							 "totaldegreeDyad"	,
    "Total degree receiver",					 "totaldegreeReceiver"		,
    "Total degree sender",						 "totaldegreeSender"			,
    "User statistics",								 "userStat"					)
  out <- matrix(effs, ncol = 2, nrow = length(effs)/2, byrow = TRUE)
  colnames(out) <- c("JASP_name", "R_name")
  return(out)

}


.translateEffects <- function(options) {

  effects <- NULL
  # endogenous effects:
  endoIndex <- grep("specifiedEndogenousEffects", names(options))
  if (length(options[[endoIndex]]) > 0) {
    endoEffects <- sapply(options[[endoIndex]], function(x) x[["variable"]])
    # endoEffectsJaspSave <- endoEffects
    endoScaling <- sapply(options[[endoIndex]], function(x) x[["endogenousEffectScaling"]])
    endoEffectsJaspSave <- paste0(endoEffects, "('", endoScaling, "')")
    endoNames <- .endoEffectsMatching()
    endoEffects <- endoNames[which(endoNames %in% endoEffects, arr.ind = TRUE), "R_name"]
    endoEffects <- paste0(endoEffects, "(scaling = ", "'", endoScaling, "'", ")")
    endoEffectsRSave <- endoEffects
    endoEffects <- paste(endoEffects, collapse = " + ")
    effects <- paste(effects, endoEffects)
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
    effects <- paste0(effects, " + ", exoEffects)
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

  if (!is.null(effects)) {
    effects <- paste0("~", effects)
    effects <- eval(parse(text = effects))
  } else {
    effects <- eval(parse(text = "~1"))
  }

  return(effects)
}



# Leave this for now unless the same exo effect should be specified with different scaling
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

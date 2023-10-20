

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

  .remUploadActorData(jaspResults, options)
  .remUploadDyadData(jaspResults, options)

  .feedbackExoTableVariables(jaspResults, options)
  .getExogenousEffects(jaspResults, options)
  .feedbackInteractionEffects(jaspResults, options)


  ready <- (options[["timeVariable"]] != "") && (length(options[["actorVariables"]]) > 1) &&
    options[["syncAnalysisBox"]]

  if (!ready) return()


  dataset <- .remReadData(jaspResults, dataset, options)

  .remErrorHandling(jaspResults, dataset, options)
  .remMainContainer(jaspResults, options)

  .remRemify(jaspResults, dataset, options)
  .remRemstats(jaspResults, dataset, options)
  .remRemstimate(jaspResults, dataset, options)

  .remModelFitTable(jaspResults, options)
  .remCoefficientsTable(jaspResults, options)

  return()
}

.remMainContainer <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]])) return()
  mainContainer <- createJaspContainer()
  mainContainer$dependOn(c("timeVariable", "actorVariables", "weightVariable"))
  jaspResults[["mainContainer"]] <- mainContainer

  # mainContainer$setError("ERROR")


  return()
}

.remUploadActorData <- function(jaspResults, options) {

  if (options[["actorData"]] == "") return()

  if (!is.null(jaspResults[["actorDataState"]])) return()

  actorDt <- read.csv(options[["actorData"]])

  actorDataState <- createJaspState(actorDt)
  actorDataState$dependOn("actorData")
  jaspResults[["actorDataState"]] <- actorDataState


  return()
}


.remUploadDyadData <- function(jaspResults, options) {

  dyadDataPaths <- sapply(options[["dyadDataList"]], function(x) x[["dyadData"]])
  if (all(dyadDataPaths == "")) return()

  if (!is.null(jaspResults[["dyadDataState"]]$object)) return()

  dyadOut <- list()
  for (i in 1:length(dyadDataPaths)) {

    if (dyadDataPaths[i] != "") {
      dyadDt <- read.csv(options[["dyadDataList"]][[i]][["dyadData"]], row.names = NULL, check.names = FALSE)
      rownames(dyadDt) <- colnames(dyadDt)
      attrName <- basename(options[["dyadDataList"]][[i]][["dyadData"]])
      attrName <- gsub("\\..*","", attrName)

      dyadOut[[attrName]] <- dyadDt

    } else {
      dyadOut[[i]] <- NULL
    }
  }

  dyadDataState <- createJaspState(dyadOut)
  dyadDataState$dependOn("dyadDataList")
  jaspResults[["dyadDataState"]] <- dyadDataState


  return()
}


# -------- Effects preparation and handling ----------

.feedbackExoTableVariables <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoTableVariablesR"]])) return()

  vars <- jaspBase::decodeColNames(options[["allVariablesHidden"]])
  vars <- vars[!grepl("weight", vars)]
  vars <- as.list(vars)

  if (!is.null(jaspResults[["actorDataState"]])) {
    actorDt <- jaspResults[["actorDataState"]]$object
    actorVars <- colnames(actorDt)
    actorVars <- actorVars[!grepl("time", actorVars)]
    actorVars <- actorVars[!grepl("name", actorVars)]
    actorVars <- as.list(actorVars)
    vars <- append(vars, actorVars)
  }

  if (!is.null(jaspResults[["dyadDataState"]])) {
    dyadDataList <- jaspResults[["dyadDataState"]]$object
    dyadVars <- names(dyadDataList)
    vars <- append(vars, dyadVars)
  }

  src <- createJaspQmlSource("exoTableVariablesR", vars)
  src$dependOn(c("allVariablesHidden", "actorData", "dyadDataList"))
  jaspResults[["exoTableVariablesR"]] <- src

  return()
}

.getExogenousEffects <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoEffectsState"]])) return()

  exoTable <- options[["exogenousEffectsTable"]]

  if (length(exoTable) == 0) return()

  varNames <- sapply(exoTable, function(x) x[["value"]])
  exoEffectsList <- c("Average", "Difference", "Event", "Maximum", "Minimum", "Receive", "Same", "Send", "Tie")

  exoInds <- vector("list", length(varNames))
  names(exoInds) <- varNames
  for (i in 1:length(exoTable)) {
    exoInds[[i]] <- which(sapply(exoTable[[i]], function(x) isTRUE(x)))
  }

  if (length(unlist(exoInds)) == 0)
    return()

  exoInds[sapply(exoInds, function(x) length(x) == 0)] <- NULL

  specExoEffects <- list()
  specExoEffects[["variableNames"]] <- jaspBase::encodeColNames(names(exoInds))
  specExoEffects[["list"]] <- exoInds

  exoEffectsState <- createJaspState(specExoEffects)
  exoEffectsState$dependOn("exogenousEffectsTable")
  jaspResults[["exoEffectsState"]] <- exoEffectsState

  # fill the rSource for the specified exo effects
  exoVarNames <- names(exoInds)
  exoEffNames <- sapply(exoInds, names)
  exoEffectsForQml <- as.list(paste0(exoEffNames, "('", exoVarNames, "')"))
  specifiedExoEffectsFromR <- createJaspQmlSource("specifiedExoEffectsFromR", exoEffectsForQml)
  jaspResults[["specifiedExoEffectsFromR"]] <- specifiedExoEffectsFromR

  return()

}

.feedbackInteractionEffects <- function(jaspResults, options) {

  if (!is.null(jaspResults[["possibleInteractionEffectsFromR"]]))
    return()

  outExoList <- NULL
  if (!is.null(jaspResults[["exoEffectsState"]])) {
    exoOut <- jaspResults[["exoEffectsState"]]$object
    exoEffects <- exoOut[["list"]]
    exoVarNames <- names(exoEffects)
    exoEffNames <- sapply(exoEffects, names)
    outExoList <- as.list(paste0(exoEffNames, "('", exoVarNames, "')"))
  }


  # handle the endo effects to add to the interactions field
  endos <- options[["endogenousEffects"]]
  endosSave <- lapply(endos, function(x) {
    if (x[["includeEndoEffect"]]) x[["value"]] else NULL
  })
  specEndos <- which(!sapply(endosSave, is.null))
  outEndoList <- lapply(endos[specEndos], function(x) x[["value"]])

  if ((length(outExoList) + length(outEndoList)) < 2) return()

  combList <- c(unlist(outExoList), unlist(outEndoList) == 0)
  interTmp <- combn(c(unlist(outExoList), unlist(outEndoList)), m = 2)
  inters <- as.list(paste0(interTmp[1, ], " * ", interTmp[2, ]))

  possibleInteractionEffectsFromR <- createJaspQmlSource("possibleInteractionEffectsFromR", inters)
  possibleInteractionEffectsFromR$dependOn(c("endogenousEffects", "specifiedExogenousEffects"))
  jaspResults[["possibleInteractionEffectsFromR"]] <- possibleInteractionEffectsFromR

  return()
}


# ----------- Main analysis -------------
.remReadData <- function(jaspResults, dataset, options) {

  if (!is.null(dataset))
    return(dataset)

  if (options$weightVariable == "") {
    variables  <- c(options$timeVariable, options$actorVariables)
  } else {
    variables  <- c(options$timeVariable, options$actorVariables, options$weightVariable)
  }

  exoEffects <- jaspResults[["exoEffectsState"]][["object"]][["list"]]
  if (!is.null(exoEffects)) {
    tmp1 <- lapply(exoEffects, function(x) names(x) == "event")
    tmp2 <- unlist(lapply(tmp1, any))
    eventNames <- names(tmp2[tmp2])
    variables <- c(variables, jaspBase::encodeColNames(eventNames))
  }

  dataset <- .readDataSetToEnd(columns = variables)
  return(dataset)
}


.remErrorHandling <- function(jaspResults, dataset, options) {

  .hasErrors(dataset = dataset,
             type = 'infinity',
             exitAnalysisIfErrors = TRUE)

  evnames <- jaspBase::decodeColNames(colnames(dataset))
  evnames <- evnames[! evnames == "time"]

  if (!is.null(jaspResults[["actorDataState"]]$object)) {
    attrnames <- colnames(jaspResults[["actorDataState"]]$object)
    if (length(c(evnames, attrnames)) != length(unique(c(evnames, attrnames)))) {
      .quitAnalysis(gettext("Duplicate variable names have been detected, please rename them"))
    }
  }

  if (!is.null(jaspResults[["dyadDataState"]]$object)) {
    dyadnames <- names(jaspResults[["dyadDataState"]]$object)
    if (length(c(evnames, dyadnames)) != length(unique(c(evnames, dyadnames)))) {
      .quitAnalysis(gettext("Duplicate variable names have been detected, please rename them"))
    }
  }

  if (!is.null(jaspResults[["actorDataState"]]$object) && !is.null(jaspResults[["dyadDataState"]]$object)) {
    if (length(c(evnames, attrnames, dyadnames)) != length(unique(c(evnames, attrnames, dyadnames)))) {
      .quitAnalysis(gettext("Duplicate variable names have been detected, please rename them"))
    }
  }


  # first three variables should be time actors and maybe weight
  # seperator variables should be there
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
  if (anyNA(dataset)) {
    dt <- dt[complete.cases(dt), ]
  }

  if (is.factor(dt[, 1]) || is.character(dt[, 1])) {
    dt[, 1] <- as.character(dt[, 1])
    dt[, 1] <- as.POSIXct(dt[, 1])
  }

  if (options[["eventDirection"]] == "undirected" && options[["orientation"]] == "tie") {
    directed <- FALSE
  } else {
    directed <- TRUE
  }
  rehObject <- try(remify::remify(edgelist = dt,
                                  directed = directed,
                                  ordinal = options[["eventSequence"]] == "orderOnly",
                                  model = options[["orientation"]],
                                  riskset = options[["riskset"]]))




  remifyResultState <- createJaspState(rehObject)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "naAction", "weightVariable"))

  jaspResults[["mainContainer"]][["remifyResultState"]] <- remifyResultState


  return()
}


.remRemstats <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) &&
      !is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) return()

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

  if (isTryError(rehObject)) {
    return()
  }

  # produce the effects text:
  effectsObj <- .translateEffects(jaspResults, options)
  effectsForm <- effectsObj$effects
  effectsText <- Reduce(paste, deparse(effectsForm))
  effectsText <- gsub("+", "+\n", effectsText, fixed = TRUE)

  # show the effects above the output
  outText <- createJaspHtml(text = gettextf("The effects were specified as: \n%s",
                                            effectsText))
  outText$position <- 0.5
  jaspResults[["mainContainer"]][["effectsCall"]] <- outText

  # prepare the data for remstats, aka, assign the attributes to objects so they are present for remstats
  dtExo <- .prepareCovariateData(jaspResults, dataset, options)

  # do the model conditions once here:


  # check if there is already a statsObject in storage, if null we are in the first round of calculations
  # or maybe the user did not choose to save the samples
  if (is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) ||
      !options[["oldEffectsSaved"]]) {

    # this weird thing makes sure the correct effects are filled in
    conds[[which(!sapply(conds, is.null))]] <- effectsForm
    # in the first round both states are created
    statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = conds[[1]], sender_effects = conds[[2]],
                                          receiver_effects = conds[[3]], attr_actors = dtExo))

    if (!isTryError(statsObject)) {
      # specify a new attribute to save the formula, since the actor oriented object does not have the same
      # formula structure as the tie oriented
      attr(statsObject, "formulaJasp") <- effectsForm
      if (options[["orientation"]] == "actor") {
        if (options[["actorDirection"]] == "sender") {
          dimnames(statsObject[["sender_stats"]])[[3]] <- effectsObj$dimNames
        } else {
          dimnames(statsObject[["receiver_stats"]])[[3]] <- effectsObj$dimNames
        }
      } else {
        dimnames(statsObject)[[3]] <- effectsObj$dimNames
      }
    }

    remstatsResultState <- createJaspState(statsObject)

    remstatsResultStateStorage <- createJaspState(statsObject)
    jaspResults[["mainContainer"]][["remstatsResultStateStorage"]] <- remstatsResultStateStorage

  } else {

    statsObjectOld <- jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object
    formulaOld <- attr(statsObjectOld, "formulaJasp")
    dimNamesOld <- dimnames(statsObjectOld)[[3]]
    # match the current specified effects with the effects that have been calculated in the past
    # return the "new" effects,
    effsOut <- .matchEffectsForStats(formulaOld, effectsObj, dimNamesOld)


    if (is.null(effsOut$form)) { # if no new effects, we can just continue with the old object and calculate nothing new
      statsObjectCombined <- statsObject <- statsObjectOld

    } else { # some effects that have not been calculated in the past

      # this weird thing makes sure the correct effects are filled in
      conds[[which(!sapply(conds, is.null))]] <- effsOut$form

      statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = conds[[1]], sender_effects = conds[[2]],
                                          receiver_effects = conds[[3]], attr_actors = dtExo))

      # add the jasp-detailed dimnames to the whole thing.
      if (!isTryError(statsObject)) {
        attr(statsObject, "formulaJasp") <- effsOut$form

        if (options[["orientation"]] == "actor") {
          if (options[["actorDirection"]] == "sender") {
            dimnames(statsObject[["sender_stats"]])[[3]] <- effsOut$dimNamesNew
          } else {
            dimnames(statsObject[["receiver_stats"]])[[3]] <- effsOut$dimNamesNew
          }
        } else {
          dimnames(statsObject)[[3]] <- effsOut$dimNamesNew
        }
      }

      # bind the current and the old statsobject together, remstats should by itself take care of duplicates
      # and leave deal with those, aka, not bind them twice
      statsObjectCombined <- remstats::bind_remstats(statsObjectOld, statsObject)
      attr(statsObjectCombined, "formulaJasp") <- effsOut$formComb
    }

    jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object <- statsObjectCombined

    # now just continue with the statsObject array slices that are required for the current analysis

    if (options[["orientation"]] == "actor") {
      if (options[["actorDirection"]] == "sender") {
        sliced <- statsObjectCombined[["sender_stats"]][, , effectsObj$dimNames]
      } else { # receiver
        sliced <- statsObjectCombined[["receiver_stats"]][, , effectsObj$dimNames]
      }
      attr(sliced, "actors") <- attr(statsObject, "actors")
    } else {
      sliced <- statsObjectCombined[, , effectsObj$dimNames]
      attr(sliced, "riskset") <- attr(statsObject, "riskset")
    }

    attr(sliced, "class") <- attr(statsObject, "class")
    attr(sliced, "model") <- attr(statsObject, "model")
    attr(sliced, "formula") <- effectsForm

    remstatsResultState <- createJaspState(sliced)


  }

  remstatsResultState$dependOn(c("eventDirection", "eventSequence",
                                 "orientation", "riskset", "naAction",
                                 "endogenousEffects", "specifiedExogenousEffects",
                                 "interactionEffects", "oldEffectsSaved"))
  jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState

  return()

}


.remRemstimate <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    return()

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object
  statsObject <- jaspResults[["mainContainer"]][["remstatsResultState"]]$object

  if (isTryError(statsObject)) {
    return()
  }

  fit <- try(remstimate::remstimate(reh = rehObject, stats = statsObject, method = options[["method"]]))

  remstimateResultState <- createJaspState(fit)
  remstimateResultState$dependOn(c("eventDirection", "eventSequence",
                                   "orientation", "riskset", "naAction",
                                   "endogenousEffects", "specifiedExogenousEffects",
                                   "interactionEffects", "method"))

  jaspResults[["mainContainer"]][["remstimateResultState"]] <- remstimateResultState

}


# -------------- Output functions ---------------
.remModelFitTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["modelFitTable"]])) return()

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object

  modelFitTable <- createJaspTable(title = gettext("Model Fit Table"))
  modelFitTable$dependOn(c("eventDirection", "eventSequence",
                           "orientation", "riskset", "naAction",
                           "endogenousEffects", "specifiedExogenousEffects",
                           "interactionEffects", "method"))
  modelFitTable$position <- 1

  jaspResults[["mainContainer"]][["modelFitTable"]] <- modelFitTable

  if (isTryError(jaspResults[["mainContainer"]][["remifyResultState"]]$object)) {
    errmsg <- gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(jaspResults[["mainContainer"]][["remifyResultState"]]$object))
    modelFitTable$setError(errmsg)
    return()
  }

  if (isTryError(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) {
    errmsg <- gettextf("Remstats failed. Internal error message: %s", .extractErrorMessage(jaspResults[["mainContainer"]][["remstatsResultState"]]$object))
    modelFitTable$setError(errmsg)
    return()
  }

  if (isTryError(jaspResults[["mainContainer"]][["remstimateResultState"]]$object)) {
    errmsg <- gettextf("Remstimate failed. Internal error message: %s", .extractErrorMessage(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    modelFitTable$setError(errmsg)
    return()
  }


  if (!is.null(remResults)) {

    res <- summary(remResults)
    if (options[["orientation"]] == "actor") {
      if (options[["actorDirection"]] == "sender") {
        npar <- nrow(res[["coefsTab"]][["sender_model"]])
        res <- res[["sender_model"]]
      } else {
        npar <- nrow(res[["coefsTab"]][["receiver_model"]])
        res <- res[["receiver_model"]]
      }
    }

    if (options[["method"]] == "MLE") {

      modelFitTable$addColumnInfo(name = "fitmeasure",     title = gettext("Statistic"), type= "string")
      modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
      modelFitTable$addColumnInfo(name = "df",   title = gettext("df"),  type= "number")
      modelFitTable$addColumnInfo(name = "pvalue",   title = gettext("p"),     type= "number")

      footnote <- ""
      if (!is.null(remResults)){
        modelFitTable$addFootnote(footnote)
      }

      dtFill <- data.frame(fitmeasure = c("Null deviance", "Residual deviance", "Chi^2", "AIC", "AICC", "BIC"))
      dtFill$estimate <- c(res$null.deviance, res$residual.deviance, res$model.deviance, res$AIC, res$AICC, res$BIC)
      dtFill$df <- c(res$df.null, res$df.residual, res$df.model, NA_real_, NA_real_, NA_real_)
      dtFill$pvalue <- c(NA_real_, NA_real_, res$chiP, NA_real_, NA_real_, NA_real_)

      modelFitTable$setData(dtFill)

    } else { # method = BSIR

      modelFitTable$addColumnInfo(name = "fitmeasure",     title = gettext("statistic"), type= "string")
      modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")

      footnote <- ""
      if (!is.null(remResults)){
        modelFitTable$addFootnote(footnote)
      }

      reh <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

      npar <- nrow(res$coefsTab)
      N <- reh$M
      BIC <- -2 * res$loglik + npar * log(N)

      dtFill <- data.frame(fitmeasure = "BIC")
      dtFill$estimate <- BIC
      modelFitTable$setData(dtFill)

    }

  }

  return()
}


.remCoefficientsTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["coefficientsTable"]])) return()

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object


  coefficientsTable <- createJaspTable(title = gettext("Coefficients Table"))
  coefficientsTable$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "naAction",
                               "endogenousEffects", "specifiedExogenousEffects",
                               "interactionEffects", "method"))
  coefficientsTable$position <- 2

  jaspResults[["mainContainer"]][["coefficientsTable"]] <- coefficientsTable

  if (isTryError(jaspResults[["mainContainer"]][["remifyResultState"]]$object)) {
    errmsg <- gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(jaspResults[["mainContainer"]][["remifyResultState"]]$object))
    coefficientsTable$setError(errmsg)
    return()
  }

  if (isTryError(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) {
    errmsg <- gettextf("Remstats failed. Internal error message: %s", .extractErrorMessage(jaspResults[["mainContainer"]][["remstatsResultState"]]$object))
    coefficientsTable$setError(errmsg)
    return()
  }

  if (isTryError(jaspResults[["mainContainer"]][["remstimateResultState"]]$object)) {
    errmsg <- gettextf("Remstimate failed. Internal error message: %s", .extractErrorMessage(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    coefficientsTable$setError(errmsg)
    return()
  }

  if (!is.null(remResults)) {

    ctab <- summary(remResults)[["coefsTab"]]

    if (options[["orientation"]] == "actor") {
      if (options[["actorDirection"]] == "sender") {
        ctab <- ctab[["sender_model"]]
      } else {
        ctab <- ctab[["receiver_model"]]
      }
    }

    dtFill <- data.frame(ctab)
    rwnames <- rownames(ctab)
    coefNames <- .transformCoefficientNames(rwnames, options, jaspResults)
    dtFill$coef <- coefNames
    dtFill <- dtFill[, c(ncol(dtFill), 1:(ncol(dtFill) - 1))]

    if (options[["method"]] == "MLE") {

      coefficientsTable$addColumnInfo(name = "coef",     title = gettext("Coefficient"), type= "string")
      coefficientsTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
      # coefficientsTable$addColumnInfo(name = "exp", title = "exp(Estimate)",    type= "number")
      coefficientsTable$addColumnInfo(name = "stdErr",   title = gettext("Std. Error"),  type= "number")
      coefficientsTable$addColumnInfo(name = "zValue",   title = gettext("z-value"),     type= "number")
      coefficientsTable$addColumnInfo(name = "prZ",      title = gettext("p"),     type= "number")
      coefficientsTable$addColumnInfo(name = "pr0",      title = gettext("p(=0)"),       type= "number")

      colnames(dtFill) <- c("coef", "estimate", "stdErr", "zValue", "prZ", "pr0")

    } else { # method = BSIR
      coefficientsTable$addColumnInfo(name = "coef",     title = gettext("Coefficient"), type= "string")
      coefficientsTable$addColumnInfo(name = "postMode",     title = gettext("Posterior Mode"), type= "number")
      coefficientsTable$addColumnInfo(name = "postSD", title = gettext("Posterior SD"),    type= "number")
      coefficientsTable$addColumnInfo(name = "q2.5",   title = gettext("2.5% Quantile"),  type= "number")
      coefficientsTable$addColumnInfo(name = "q50",   title = gettext("50% Quantile"),     type= "number")
      coefficientsTable$addColumnInfo(name = "q97.5",      title = gettext("97.5% Quantile"),     type= "number")
      coefficientsTable$addColumnInfo(name = "pr0",      title = gettext("p(=0|y)"),       type= "number")

      colnames(dtFill) <- c("postMode", "postSD", "q2.5", "q50", "q97.5", "pr0")

    }

    footnote <- ""
    if (!is.null(remResults)){
      coefficientsTable$addFootnote(footnote)
    }

    coefficientsTable$setData(dtFill)
  }

  return()
}




# ------------- Helper functions ----------------

.translateEffects <- function(jaspResults, options) {

  effects <- "~1"
  # endogenous effects:
  endoEffectsSave <- NULL
  endos <- options[["endogenousEffects"]]
  endosSave <- lapply(endos, function(x) {
    if (x[["includeEndoEffect"]]) x[["value"]] else NULL
      })
  specEndos <- which(!sapply(endosSave, is.null))

  if (length(specEndos) > 0) {
    endoObj <- .processEndoEffects(endos[specEndos])
    print(str(endoObj))
    # we need the R and translated jasp names of the endo effects later
    endosMatrix <- matrix(c(endosR, endosJasp), nrow = length(endosR), ncol = 2)
    endosState <- createJaspState(endosMatrix)
    jaspResults[["mainContainer"]][["endoEffectsState"]] <- endosState

  }


  # exogenous effects
  exoEffectsSave2 <- NULL
  exoDims <- c() # also save the dimnames to later assign to the statsObject slices
  if (length(options[["specifiedExogenousEffects"]]) > 0) {
    exos <- options[["specifiedExogenousEffects"]]
    exoEffects <- sapply(exos, function(x) x[["value"]])
    exoEffectsSave1 <- exoEffects
    exoEffects <- gsub(")", "", exoEffects)
    exoScaling <- sapply(exos, function(x) x[["exogenousEffectsScaling"]])
    exoAbsolute <- sapply(exos, function(x) x[["absolute"]])

    for (ii in 1:length(exoEffects)) {

      #  create the proper dimname for the effect
      dimstmp <- exoEffectsSave1[ii]
      dimstmp <- gsub("('", "_", dimstmp, fixed = TRUE)
      dimstmp <- gsub("')", "", dimstmp, fixed = TRUE)

      if (!(exoScaling[ii] %in% c("none", ""))) {
        exoEffects[ii] <- paste0(exoEffects[ii], ", scaling = '", exoScaling[ii], "'")
        dimstmp <- paste0(dimstmp, ".", exoScaling[ii])
      }

      if (exoAbsolute[ii]) {
        exoEffects[ii] <- paste0(exoEffects[ii], ", absolute = ", exoAbsolute[ii])
        dimstmp <- paste0(dimstmp, ".", "absolute")
      }

      # deal with the event effects
      if (startsWith(exoEffects[ii], "event")) {
        ma <- regexpr("'(.*?)'", exoEffects[ii])
        eventName <- gsub("'", "", regmatches(exoEffects[ii], ma), fixed = TRUE)
        exoEffects[ii] <- sub("\\(", paste0("(", eventName, ", "), exoEffects[ii])
      }

      # deal with the tie effects
      if (startsWith(exoEffects[ii], "tie")) {
        ma <- regexpr("'(.*?)'", exoEffects[ii])
        tieName <- gsub("'", "", regmatches(exoEffects[ii], ma), fixed = TRUE)
        exoEffects[ii] <- sub("(\\'.*?)\\'", paste0("\\1', ", tieName), exoEffects[ii])
      }

      exoDims <- append(exoDims, dimstmp)
    }

    exoEffects <- paste0(exoEffects, ")")

    exoEffectsSave2 <- exoEffects
    exoEffects <- paste0(exoEffects, collapse = " + ")
    effects <- paste(effects, "+", exoEffects)
  }

  # interactions
  interEffectsSave <- NULL
  interDims <- c()
  if (length(options[["interactionEffects"]]) > 0) {

    interEffects <- sapply(options[["interactionEffects"]], function(x) if (x[["includeInteractionEffect"]]) x[["value"]] else NULL)
    interEffects[sapply(interEffects, is.null)] <- NULL

    if (length(interEffects) > 0) {

      interEffects <- unlist(interEffects)
      interDims[1:length(interEffects)] <- interEffects

      # work the endo effects
      if (length(specEndos) > 0) {
        for (ee in 1:length(endosR)) {
          ind <- grep(endosJasp[ee], interEffects, fixed = TRUE)
          if (length(ind) > 0) {
            interEffects[ind] <- gsub(endosJasp[ee], endoEffectsSave[ee], interEffects[ind], fixed = TRUE)
            interDims[ind] <- gsub(endosJasp[ee], endoDims[ee], interDims[ind], fixed = TRUE)
          }
        }
      }

      # work the exo effects
      if (length(options[["specifiedExogenousEffects"]]) > 0) {
        for (eee in 1:length(exos)) {
          ind <- grep(exoEffectsSave1[eee], interEffects, fixed = TRUE)
          if (length(ind) > 0) {
            interEffects[ind] <- gsub(exoEffectsSave1[eee], exoEffectsSave2[eee], interEffects[ind], fixed = TRUE)
            interDims[ind] <- gsub(exoEffectsSave1[eee], exoDims[eee], interDims[ind], fixed = TRUE)
          }
        }
      }

      interEffects <- gsub(" * ", ":", interEffects, fixed = TRUE)
      interEffectsSave <- interEffects
      interEffects <- paste0(interEffects, collapse = " + ")

      interDims <- gsub(" * ", ":", interDims, fixed = TRUE)

      effects <- paste0(effects, " + ", interEffects)
    }
  }

  effects <- eval(parse(text = effects))

  dimNms <- c("baseline", endoDims, exoDims, interDims)

  return(list(effects = effects, dimNames = dimNms))
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

.prepareCovariateData <- function(jaspResults, dataset, options) {

  exoVariablesEnc <- jaspBase::encodeColNames(unique(unlist(jaspResults[["exoEffectsState"]][["object"]][["variableNames"]])))
  exoVariablesDec <- jaspBase::decodeColNames(unique(unlist(jaspResults[["exoEffectsState"]][["object"]][["variableNames"]])))
  exoEffects <- jaspResults[["exoEffectsState"]][["object"]][["list"]]

  evnames <- jaspBase::decodeColNames(colnames(dataset))
  evnames <- evnames[! evnames == "time"]
  if (!is.null(jaspResults[["actorDataState"]]$object)) {
    attrnames <- colnames(jaspResults[["actorDataState"]]$object)
  }

  if (!is.null(jaspResults[["dyadDataState"]]$object)) {
    dyadnames <- names(jaspResults[["dyadDataState"]]$object)
  }

  if (!is.null(exoVariablesEnc)) { # exo effects specified

    exoList <- jaspResults[["exoEffectsState"]][["object"]][["list"]]


    # for the event and tie effects we need the event related columns form the main data, the actor attributes data
    # and dyad attributes data to be present in the environment:

    # first the event effect
    tmp1 <- lapply(exoList, function(x) names(x) == "event")
    tmp2 <- unlist(lapply(tmp1, any))
    eventNames <- names(tmp2[tmp2])

    colnames(dataset) <- jaspBase::decodeColNames(colnames(dataset))

    if (length(eventNames) > 0) {
      eventVariables <- jaspBase::decodeColNames(eventNames)
      for (ii in 1:length(eventVariables)) {
        assign(eventVariables[ii], dataset[, eventVariables[ii]], pos = 1) # dont know why pos=1 is working....
      }
    }

    # dyadic attributes data
    if (!is.null(jaspResults[["dyadDataState"]]$object)) {

      dyInds <- which(dyadnames %in% exoVariablesDec)
      if (length(dyInds) > 0) {
        for (iii in 1:length(dyInds)) {
          assign(dyadnames[iii], as.matrix(jaspResults[["dyadDataState"]][["object"]][[dyadnames[iii]]]), pos = 1)
        }
      }
    }

    # the actor attributes data
    if (!is.null(jaspResults[["actorDataState"]]$object)) {
      # is there any exo effects specified for a variable in the attributes data
      if (any(!is.na(match(attrnames, exoVariablesDec)))) {
        dt <- jaspResults[["actorDataState"]]$object
        return(dt)
      }
    }

  }
  return()
}

# match the effects formula from an old remstats object and the current one
# put out the formula that is all new, aka, oldFormula minus effects
# also transform the effects to the names of the statsObject slices, as remstats puts them out
# to use later for matching with the slices of the old object
.matchEffectsForStats <- function(formulaOld, effects, dimNamesOld) {

  newEffects <- effects$effects
  formulaOldc <- as.character(formulaOld)
  newEffectsc <- as.character(newEffects)

  if (length(newEffectsc[-1]) == 1 && newEffectsc[-1] == "1") { # if there is only a baseline effect in the current specification?
    return(list(form = NULL, slices = "baseline"))
  }

  form <- strsplit(formulaOldc, split = " + ", fixed = TRUE)
  form[[1]] <- NULL
  form <- unlist(form)

  effs <- strsplit(newEffectsc, split = " + ", fixed = TRUE)
  effs[[1]] <- NULL
  effs <- unlist(effs)

  dupInd <- match(form, effs)
  dupInd <- dupInd[complete.cases(dupInd)]

  effsOutForm <- effs[-dupInd]

  if (length(effsOutForm) > 0) {
    effsOutForm <- paste0(effsOutForm, collapse = " + ")
    effsOutForm <- paste0("~", effsOutForm)
    effsOutForm <- eval(parse(text = effsOutForm))

    # also export the combination of the old formula and new formula to attach to the combined object
    formComb <- c(form, effs)
    formCombOut <- paste0(formComb, collapse = " + ")
    formCombOut <- paste0("~", formCombOut)
    formCombOut <- eval(parse(text = formCombOut))

    # now export the new dimnames to assign to the new statsObejct
    dimNamesOut <- c("baseline", setdiff(effects$dimNames, dimNamesOld))

  } else {
    effsOutForm <- NULL
    formCombOut <- formulaOld
    dimNamesOut <- NULL
  }


  return(list(form = effsOutForm, formComb = formCombOut, dimNamesNew = dimNamesOut))

}



.transformCoefficientNames <- function(coefNames, options, jaspResults) {

  exoList <- c("average", "difference", "event", "maximum", "minimum", "receive",
               "same", "send", "tie")

  # capitalize the exo effects first letters
  for (i in 1:length(exoList)) {
    inds <- grep(exoList[i], coefNames)
    newName <- paste(toupper(substr(exoList[i], 1, 1)), substr(exoList[i], 2, nchar(exoList[i])), sep="")
    coefNames[inds] <- gsub(exoList[i], newName, coefNames[inds])
  }

  # transform the R specific endo effect names to be more readable
  # first get the proper endo effects
  if (!is.null(jaspResults[["mainContainer"]][["endoEffectsState"]])) {
    endos <- jaspResults[["mainContainer"]][["endoEffectsState"]]$object
    for (ii in 1:nrow(endos)) {
      inds <- grep(endos[ii, 1], coefNames)
      coefNames[inds] <- gsub(endos[ii, 1], endos[ii, 2], coefNames[inds])
    }
  }


  # now remove everything after the first period in each name, looks cleaner
  # however:
  # interactions are tricky
  indsIA <- grep(":", coefNames)
  splitlist <- strsplit(coefNames[indsIA], ":")
  coefNames[indsIA] <- sapply(splitlist, function(x) {
    x <- gsub("\\.(.*)", "", x)
    paste0(x, collapse = ":")
  })

  # now remove for the remaining effects
  coefNames <- gsub("\\.(.*)", "", coefNames)

  return(coefNames)

}


.processEndoEffects <- function(endos) {

  endoDims <- c() # also save the dimnames to later assign to the statsObject slices
  endosR <- sapply(endos, function(x) x[["value"]])
  endosJasp <- sapply(endos, function(x) x[["translatedName"]])
  endoScaling <- sapply(endos, function(x) x[["endogenousEffectsScaling"]])
  endoType <- sapply(endos, function(x) x[["endogenousEffectsConsiderType"]])
  endoUnique <- sapply(endos, function(x) x[["endogenousEffectsUnique"]])
  endoEffects <- paste0(endosR, "(")


  for (i in 1:length(endosR)) {

    # create the proper dimname
    dimstmp <- endosR[i]

    if (!(endoScaling[i] %in% c("none", ""))) {
      endoEffects[i] <- paste0(endoEffects[i], "scaling = '", endoScaling[i], "', ")
      dimstmp <- paste0(dimstmp, ".", endoScaling[i])
    }
    if (endoUnique[i]) {
      endoEffects[i] <- paste0(endoEffects[i], "unique = ", endoUnique[i], ", ")
      dimstmp <- paste0(dimstmp, ".unique")

    }
    if (endoType[i])  {
      endoEffects[i] <- paste0(endoEffects[i], "consider_type = ", endoType[i], ", ")
      dimstmp <- paste0(dimstmp, ".type")
    }
    endoDims <- append(endoDims, dimstmp)
  }
  endoEffects <- paste0(endoEffects, ")")
  endoEffects <- gsub(", )", ")", endoEffects)
  endoEffectsSave <- endoEffects

  endoEffects <- paste(endoEffects, collapse = " + ")
  effects <- paste(effects, "+", endoEffects)

  return(list(effects = effects, effectsSave = endoEffectsSave, dims = endoDims))
}

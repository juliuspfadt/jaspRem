

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

  .remUploadActorData(jaspResults, options)
  .remUploadDyadData(jaspResults, options)
  .remUploadDyadIncludeData(jaspResults, options)

  # fill the qml componentsLists
  .feedbackExoTableVariablesEvents(jaspResults, options)
  .feedbackExoTableVariablesActors(jaspResults, options)
  .feedbackExoTableVariablesDyads(jaspResults, options)

  # process the exo effects from the table
  .exoEffectsSpecified(jaspResults, options)

  .feedbackInteractionEffects(jaspResults, options)
  .feedbackForPlotEffects(jaspResults, options)

  # create a container for the main results that passes down dependencies to everything saved withiin
  .remMainContainer(jaspResults, options)

  ready <- (options[["timeVariable"]] != "") && (options[["actorVariableSender"]] != "") && (options[["actorVariableReceiver"]] != "")

  if (ready && !options[["syncAnalysisBox"]]) {
    syncText <- createJaspHtml(text = gettext("<b>Check the 'Sync Analysis' box to run the analysis</b>"))
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn(c("syncAnalysisBox", "timeVariable", "actorVariableSender", "actorVariableReceiver"))
    syncText$position <- 0.01
  }

  if (ready && options[["syncAnalysisBox"]]) {

    dataset <- .remHandleData(jaspResults, dataset, options)

    # # for debugging
    # saveRDS(dataset, "~/Downloads/dataset.rds")
    # saveRDS(options, "~/Downloads/options.rds")

    .remErrorHandling(jaspResults, dataset, options)

    .remRemify(jaspResults, dataset, options)
    .remCheckActorAttributes(jaspResults, dataset, options)
    .remRemstats(jaspResults, dataset, options)
    .remRemstimate(jaspResults, dataset, options)
    .remRegularization(jaspResults, dataset, options)
  }

  .remModelFitTable(jaspResults, options, ready)
  .remCoefficientsTable(jaspResults, options, ready)
  .remRegularizationTable(jaspResults, options, ready)
  .remDiagnosticPlot(jaspResults, options, ready)

  return()
}

.remMainContainer <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]])) return()
  mainContainer <- createJaspContainer()
  mainContainer$dependOn(c("timeVariable", "actorVariableSender", "actorVariableReceiver", "weightVariable", "typeVariable",
                           "syncAnalysisBox", "eventDirection", "eventSequence", "orientation", "riskset",
                           "endogenousEffects", "specifiedExogenousEffects",
                           "interactionEffects", "endogenousEffectsSender", "specifiedExogenousEffectsSender",
                           "interactionEffectsSender", "eventHistory", "eventHistorySingleInput",
                           "eventHistoryIntervalInputLower", "eventHistoryIntervalInputUpper", "timepointInputLower",
                           "timepointInputUpper", "dyadInclude", "extendRisksetByType",
                           # the exogenous effect tables decide which covariates (and therefore which data
                           # columns) enter the model, the upload lists decide what those covariates contain
                           "exogenousEffectsTableActors", "exogenousEffectsTableSender",
                           "exogenousEffectsTableEvents", "exogenousEffectsTableDyads",
                           "actorDataList", "dyadDataList"))
  jaspResults[["mainContainer"]] <- mainContainer

  return()
}

.remUploadActorData <- function(jaspResults, options) {

  actorDataPaths <- sapply(options[["actorDataList"]], function(x) x[["actorData"]])
  if (all(actorDataPaths == "")) return()

  if (!is.null(jaspResults[["actorDataState"]]$object)) return()

  actorOut <- list()
  for (i in 1:length(actorDataPaths)) {

    if (actorDataPaths[i] != "") {
      bsName <- basename(options[["actorDataList"]][[i]][["actorData"]])
      actorDataName <- gsub("\\..*","", bsName)
      ending <- sub(".*(\\..*)", "\\1", bsName)

      if (ending == ".csv") {
        actorDt <- read.csv(options[["actorDataList"]][[i]][["actorData"]], row.names = NULL, check.names = FALSE)
      } else if (ending == ".txt") {
        actorDt <- read.table(options[["actorDataList"]][[i]][["actorData"]], row.names = NULL, check.names = FALSE)
      }

      actorOut[[actorDataName]] <- actorDt

    } else {
      actorOut[[i]] <- NULL
    }
  }

  actorDataState <- createJaspState(actorOut)
  actorDataState$dependOn("actorDataList")
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

      bsName <- basename(options[["dyadDataList"]][[i]][["dyadData"]])
      attrName <- gsub("\\..*","", bsName)
      ending <- sub(".*(\\..*)", "\\1", bsName)

      if (ending == ".csv") {
        dyadDt <- read.csv(options[["dyadDataList"]][[i]][["dyadData"]], row.names = NULL, check.names = FALSE)
      } else if (ending == ".txt") {
        dyadDt <- read.delim(options[["dyadDataList"]][[i]][["dyadData"]], row.names = NULL, check.names = FALSE)
      }

      # this is only necessary for the wide format...
      if (ncol(dyadDt) == nrow(dyadDt)) {
        rownames(dyadDt) <- colnames(dyadDt)
      }

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


.remUploadDyadIncludeData <- function(jaspResults, options) {

  if (!is.null(jaspResults[["dyadIncludeState"]]$object) || options[["riskset"]] != "manual" ||
      is.null(options[["dyadInclude"]])) return()

  if (options[["dyadInclude"]] == "") return()

  bsName <- basename(options[["dyadInclude"]])
  attrName <- gsub("\\..*","", bsName)
  ending <- sub(".*(\\..*)", "\\1", bsName)

  if (ending == ".csv") {
    dyadDt <- read.csv(options[["dyadInclude"]], row.names = NULL, check.names = FALSE)
  } else if (ending == ".txt") {
    dyadDt <- read.delim(options[["dyadInclude"]], row.names = NULL, check.names = FALSE)
  }

  cnames <- colnames(dyadDt)
  if (!all(c("actor1", "actor2") %in% cnames)) {
    .quitAnalysis(gettext("The columns in the dyads-to-include data file should be named 'actor1' and 'actor2'"))
  }

  dyadDt <- dyadDt[, c("actor1", "actor2")]
  dyadIncludeState <- createJaspState(dyadDt)
  dyadIncludeState$dependOn(c("riskset", "dyadInclude"))
  jaspResults[["dyadIncludeState"]] <- dyadIncludeState

  return()
}


# -------- Effects preparation and handling ----------

.feedbackExoTableVariablesEvents <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoTableVariablesEventsFromR"]])) return()

  vars <- jaspBase::decodeColNames(options[["allVariablesHidden"]])
  vars <- as.list(vars)

  src <- createJaspQmlSource("exoTableVariablesEventsFromR", vars)
  src$dependOn("allVariablesHidden")
  jaspResults[["exoTableVariablesEventsFromR"]] <- src

  return()
}

.feedbackExoTableVariablesActors <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoTableVariablesActorsFromR"]])) return()

  vars <- list()
  if (!is.null(jaspResults[["actorDataState"]])) {
    actorDataList <- jaspResults[["actorDataState"]]$object
    for (i in 1:length(actorDataList)) {
      actnms <- colnames(actorDataList[[i]])
      actnms <- actnms[!grepl("time", actnms)]
      actnms <- actnms[!grepl("name", actnms)]
      vars <- append(vars, actnms)
    }
  }

  src <- createJaspQmlSource("exoTableVariablesActorsFromR", vars)
  src$dependOn("actorDataList")
  jaspResults[["exoTableVariablesActorsFromR"]] <- src

  return()
}

.feedbackExoTableVariablesDyads <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoTableVariablesDyadsFromR"]])) return()

  vars <- list()

  if (!is.null(jaspResults[["dyadDataState"]])) {
    dyadDataList <- jaspResults[["dyadDataState"]]$object
    dyadVars <- names(dyadDataList)
    # create a list that records the variable names and where to find them,
    # so we dont have to do the checking for wide and long format and recording the names multiple times
    dyadFind <- list()
    # so now we need to distinguish between long and wide format
    for (ii in 1:length(dyadDataList)) {
      if (ncol(dyadDataList[[ii]]) == nrow(dyadDataList[[ii]])) { # wide format
        vars <- append(vars, dyadVars[ii])
        dyadFind$name[[ii]] <- dyadFind$file[[ii]] <- dyadVars[ii]
      } else {
        longNames <- colnames(dyadDataList[[ii]])[-c(1, 2)] # actor1 and actor2 are in cols 1 and 2
        vars <- append(vars, longNames)
        dyadFind$name[[ii]] <- longNames
        dyadFind$file[[ii]] <- dyadVars[[ii]]
      }
    }
    dyadFindState <- createJaspState(dyadFind)
    dyadFindState$dependOn("dyadDataList")
    jaspResults[["dyadFindState"]] <- dyadFindState
  }

  src <- createJaspQmlSource("exoTableVariablesDyadsFromR", vars)
  src$dependOn("dyadDataList")
  jaspResults[["exoTableVariablesDyadsFromR"]] <- src

  return()
}


.feedbackInteractionEffects <- function(jaspResults, options) {

  if (!is.null(jaspResults[["possibleInteractionEffectsFromR"]]) &&
      !is.null(jaspResults[["possibleInteractionEffectsFromRSender"]]))
    return()

  outExoList <- outExoListSender <- NULL
  if (!is.null(jaspResults[["exoEffectsState"]])) {
    exoOut <- jaspResults[["exoEffectsState"]]$object

    exoEffects <- exoOut[["list"]]
    exoEffNames <- lapply(exoEffects, names)
    if (length(exoEffNames) > 0) {
      outExoList <- list()
      for (iii in 1:length(exoEffNames)) {
        nm <- names(exoEffNames[iii])
        tmp <- paste0(exoEffNames[[iii]], "('", nm, "')")
        outExoList <- append(outExoList, tmp)
      }
    }

    if (options[["orientation"]] == "actor") {
      # sender
      exoEffectsSender<- exoOut[["listSender"]]
      exoEffNamesSender <- lapply(exoEffectsSender, names)
      if (length(exoEffNamesSender) > 0) {
        outExoListSender <- list()
        for (iii in 1:length(exoEffNamesSender)) {
          nm <- names(exoEffNamesSender[iii])
          tmp <- paste0(exoEffNamesSender[[iii]], "('", nm, "')")
          outExoListSender <- append(outExoListSender, tmp)
        }
      }
    }
  }

  # handle the endo effects to add to the interactions field
  endos <- options[["endogenousEffects"]]
  endosSave <- lapply(endos, function(x) {
    if (x[["includeEndoEffect"]]) x[["value"]] else NULL
  })
  specEndos <- which(!sapply(endosSave, is.null))
  outEndoList <- lapply(endos[specEndos], function(x) x[["translatedName"]])
  # type-considered effects (separate/interact) are offered as a single "(type)" term
  endoType <- sapply(endos[specEndos], function(x) {
    x[["endogenousEffectsConsiderType"]]
  })
  indType <- which(endoType %in% c("separate", "interact"))
  if (length(indType) > 0)
    outEndoList[indType] <- paste0(outEndoList[indType], "(type)")

  outEndoListX <- outEndoList

  combListSender <- NULL
  # the full effect list is also used for the residual plot picker, so it is built regardless of
  # whether there are enough effects to form an interaction
  combList <- c(unlist(outEndoListX), unlist(outExoList))
  if (length(combList) >= 2) {
    interTmp <- combn(combList, m = 2)
    inters <- as.list(paste0(interTmp[1, ], " : ", interTmp[2, ]))

    possibleInteractionEffectsFromR <- createJaspQmlSource("possibleInteractionEffectsFromR", inters)
    possibleInteractionEffectsFromR$dependOn(c("endogenousEffects", "specifiedExogenousEffects"))
    jaspResults[["possibleInteractionEffectsFromR"]] <- possibleInteractionEffectsFromR
  }


  if (options[["orientation"]] == "actor") {

    endosSender <- options[["endogenousEffectsSender"]]
    endosSaveSender <- lapply(endosSender, function(x) {
      if (x[["includeEndoEffectSender"]]) x[["value"]] else NULL
    })
    specEndosSender <- which(!sapply(endosSaveSender, is.null))
    outEndoListSender <- lapply(endosSender[specEndosSender], function(x) x[["translatedNameSender"]])
    # type-considered sender effects (separate/interact) are offered as a single "(type)" term
    endoTypeSender <- sapply(endosSender[specEndosSender], function(x) {
      x[["endogenousEffectsConsiderTypeSender"]]
    })
    indTypeSender <- which(endoTypeSender %in% c("separate", "interact"))
    if (length(indTypeSender) > 0)
      outEndoListSender[indTypeSender] <- paste0(outEndoListSender[indTypeSender], "(type)")
    outEndoListXSender <- outEndoListSender

    combListSender <- c(unlist(outEndoListXSender), unlist(outExoListSender))
    if (length(combListSender) >= 2) {

      interTmpSender <- combn(combListSender, m = 2)
      intersSender <- as.list(paste0(interTmpSender[1, ], " : ", interTmpSender[2, ]))

      possibleInteractionEffectsFromRSender <- createJaspQmlSource("possibleInteractionEffectsFromRSender", intersSender)
      possibleInteractionEffectsFromRSender$dependOn(c("endogenousEffectsSender", "specifiedExogenousEffectsSender"))
      jaspResults[["possibleInteractionEffectsFromRSender"]] <- possibleInteractionEffectsFromRSender
    }

  }

  # so lets save the effects for using them later too source in the plot effects;
  # stored unconditionally so that shrinking the model clears the previous, larger list
  saveState <- createJaspState(c(combList, combListSender))
  saveState$dependOn(c("orientation", "endogenousEffects", "specifiedExogenousEffects",
                       "endogenousEffectsSender", "specifiedExogenousEffectsSender"))
  jaspResults[["savedEffects"]] <- saveState


  return()
}


.feedbackForPlotEffects <- function(jaspResults, options) {

  if (!is.null(jaspResults[["effectsForPlot"]])) return()

  if (is.null(jaspResults[["savedEffects"]])) return()

  savedEffects <- jaspResults[["savedEffects"]]$object

  # add the interaction effects that are checked to the saved effects
  if (length(options[["interactionEffects"]]) > 1) {
    iaEffects <- lapply(options[["interactionEffects"]], function(x) {
      if (x[["includeInteractionEffect"]]) x[["value"]] else NULL
    })

    iaEffects <- unlist(iaEffects[which(!sapply(iaEffects, is.null))])

    savedEffects <- c(savedEffects, iaEffects)

  }
  if (length(options[["interactionEffectsSender"]]) > 1) {
    iaEffectsSender <- lapply(options[["interactionEffectsSender"]], function(x) {
      if(x[["includeInteractionEffectSender"]]) x[["value"]] else NULL
    })
    iaEffectsSender <- unlist(iaEffectsSender[which(!sapply(iaEffectsSender, is.null))])
    savedEffects <- c(savedEffects, iaEffectsSender)
  }
  savedEffects <- as.list(savedEffects)

  savedSource <- createJaspQmlSource("effectsForPlot", savedEffects)
  savedSource$dependOn(c("orientation", "endogenousEffects", "specifiedExogenousEffects",
                         "endogenousEffectsSender", "specifiedExogenousEffectsSender",
                         "interactionEffects", "interactionEffectsSender"))
  jaspResults[["effectsForPlot"]] <- savedSource

}


.exoEffectsSpecified <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoEffectsState"]])) {
    return()
  }

  exoEventsOut <- .exogenousEffectsHelper(options[["exogenousEffectsTableEvents"]])
  exoActorsOut <- .exogenousEffectsHelper(options[["exogenousEffectsTableActors"]])
  exoDyadsOut <- .exogenousEffectsHelper(options[["exogenousEffectsTableDyads"]])

  # Merge the specifiedEffects$variableNames
  mergedVariableNames <- c(
    exoEventsOut$specifiedEffects$variableNames,
    exoActorsOut$specifiedEffects$variableNames,
    exoDyadsOut$specifiedEffects$variableNames
  )

  # Merge the specifiedEffects$list elements
  mergedList <- c(
    exoEventsOut$specifiedEffects$list,
    exoActorsOut$specifiedEffects$list,
    exoDyadsOut$specifiedEffects$list
  )

  # Merge the qmlNames vectors
  mergedQmlNames <- c(
    exoEventsOut$qmlNames,
    exoActorsOut$qmlNames,
    exoDyadsOut$qmlNames
  )

  # Combine everything into a new merged list
  exoMerged <- list(
    specifiedEffects = list(
      variableNames = mergedVariableNames,
      list = mergedList
    ),
    qmlNames = mergedQmlNames
  )
  specExoEffects <- exoMerged$specifiedEffects

  if (options[["orientation"]] == "actor") {

    exoOutSender <- .exogenousEffectsHelper(options[["exogenousEffectsTableSender"]])
    if (!is.null(exoOutSender)) {
      specExoEffectsSender <- exoOutSender$specifiedEffects
      names(specExoEffectsSender) <- paste0(names(specExoEffectsSender), "Sender")
      specExoEffects <- append(specExoEffects, specExoEffectsSender)
    }

  }

  if (length(specExoEffects) > 0) {
    exoEffectsState <- createJaspState(specExoEffects)
    exoEffectsState$dependOn(c("orientation",
                               "exogenousEffectsTableEvents",
                               "exogenousEffectsTableActors",
                               "exogenousEffectsTableDyads",
                               "exogenousEffectsTableSender"))
    jaspResults[["exoEffectsState"]] <- exoEffectsState
  }

  return()

}



# ----------- Main analysis -------------
.remHandleData <- function(jaspResults, dataset, options) {

  variables <- c(options$timeVariable, options$actorVariableSender, options$actorVariableReceiver)
  if (options$weightVariable != "") {
    variables  <- c(variables, options$weightVariable)
  }
  if (options$typeVariable != "") {
    variables  <- c(variables, options$typeVariable)
  }

  exoEffects <- jaspResults[["exoEffectsState"]][["object"]][["list"]]

  if (options[["orientation"]] == "actor") {
    exoEffects <- append(exoEffects, jaspResults[["exoEffectsState"]][["object"]][["listSender"]])
  }

  if (length(exoEffects) > 0) {
    tmp1 <- lapply(exoEffects, function(x) names(x) == "event")
    tmp2 <- unlist(lapply(tmp1, any))
    eventNames <- unique(names(tmp2[tmp2]))
    eventIndices <- which(decodeColNames(colnames(dataset)) %in% eventNames)
    variables <- c(variables, colnames(dataset)[eventIndices])
  }

  dataset <- dataset[, variables]

  # listwise deletion happens here (and only here) so that remify, remstats and the covariate
  # objects all see the exact same event sequence
  if (anyNA(dataset)) {
    dataset <- dataset[complete.cases(dataset), ]
  }

  colnames(dataset)[1:3] <- jaspBase::encodeColNames(c("time", "actor1", "actor2"))

  return(dataset)
}



.remErrorHandling <- function(jaspResults, dataset, options) {

  .hasErrors(dataset = dataset,
             type = 'infinity',
             exitAnalysisIfErrors = TRUE)

  evnames <- jaspBase::decodeColNames(colnames(dataset))
  evnames <- evnames[! evnames == "time"]

  if (!is.null(jaspResults[["actorDataState"]]$object)) {
    actorDataList <- jaspResults[["actorDataState"]]$object
    attrnames <- c()
    for (i in 1:length(actorDataList)) {
      actnms <- colnames(actorDataList[[i]])
      if (!("name" %in% actnms))
        .quitAnalysis(gettextf("The actor attributes data file  %1$s does not contain a 'name' variable", names(actorDataList)[i]))
      actnms <- actnms[!grepl("time", actnms)]
      actnms <- actnms[!grepl("name", actnms)]
      attrnames <- c(attrnames, actnms)
    }

    if (length(attrnames) != length(unique(attrnames)))
      .quitAnalysis(gettext("Duplicate variable names in the actor attributes have been detected, please rename them."))

    if (length(c(evnames, attrnames)) != length(unique(c(evnames, attrnames))))
      .quitAnalysis(gettext("Duplicate variable names have been detected, please rename them."))

    if (length(actorDataList) > 1) {
      nrows <- sapply(actorDataList, nrow)
      if (length(nrows) == length(unique(nrows)))
        .quitAnalysis(gettext("The actor attributes data frames differ in row length. Please align."))
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

}



.remRemify <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["remifyResultState"]]$object))
    return()

  # incomplete rows were already dropped in .remHandleData

  # could be the time is in a weird format:
  if (is.factor(dataset[, 1]) || is.character(dataset[, 1])) {
    dataset[, 1] <- as.character(dataset[, 1])
    dataset[, 1] <- as.POSIXct(dataset[, 1])
  }

  # handle the weight and type variables
  colnames(dataset) <- jaspBase::decodeColNames(colnames(dataset))

  if (options[["weightVariable"]] != "") {
    colnames(dataset)[colnames(dataset) == jaspBase::decodeColNames(options[["weightVariable"]])] <- "weight"
  }

  if (options[["typeVariable"]] != "") {
    colnames(dataset)[colnames(dataset) == jaspBase::decodeColNames(options[["typeVariable"]])] <- "type"
  }

  if (options[["eventDirection"]] == "undirected" && options[["orientation"]] == "tie") {
    directed <- FALSE
  } else {
    directed <- TRUE
  }

  if (!is.null(jaspResults[["dyadIncludeState"]][["object"]])) {
    # remify >= 4.0's manual.riskset is the data.frame of dyads that make up the risk
    # set over the whole sequence, so the uploaded file *is* the risk set (NA acts as a
    # wildcard for any actor). remify additionally re-adds all observed dyads.
    manualRiskset <- jaspResults[["dyadIncludeState"]][["object"]]
    manualRiskset <- data.frame(lapply(manualRiskset, as.character), stringsAsFactors = FALSE)
  } else {
    manualRiskset <- NULL
  }

  # a manual riskset without an uploaded include file is equivalent to the full one,
  # but remify errors on riskset = "manual" without a manual_riskset data.frame
  risksetType <- options[["riskset"]]
  if (risksetType == "manual" && is.null(manualRiskset))
    risksetType <- "full"

  # extend_riskset_by_type only applies when a type variable is present
  extendByType <- options[["extendRisksetByType"]] && options[["typeVariable"]] != ""

  rehObject <- try(remify::remify(edgelist = dataset,
                                  directed = directed,
                                  ordinal = options[["eventSequence"]] == "orderOnly",
                                  model = options[["orientation"]],
                                  riskset = risksetType,
                                  manual_riskset = manualRiskset,
                                  extend_riskset_by_type = extendByType))

  if (isTryError(rehObject)) {
    jaspResults[["mainContainer"]]$setError(gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(rehObject)))
  }

  remifyResultState <- createJaspState(rehObject)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "weightVariable",
                               "timeVariable", "actorVariableSender", "actorVariableReceiver",
                               "typeVariable", "syncAnalysisBox", "dyadInclude", "extendRisksetByType",
                               # these decide which columns .remHandleData keeps and therefore which
                               # rows survive listwise deletion, so they change the event sequence
                               "exogenousEffectsTableActors", "exogenousEffectsTableSender",
                               "exogenousEffectsTableEvents", "exogenousEffectsTableDyads"))

  jaspResults[["remifyResultState"]] <- remifyResultState

  return()
}


# some extra error handling for the actor attributes data
# see if those really contain all the actor names and if they are in the correct order
# we do this check here, because remify automatically extracts the actor names
.remCheckActorAttributes <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["actorDataState"]]$object)) return()
  if (!is.null(jaspResults[["actorDataStateNew"]]$object)) return()

  orderedActorDataList <- list()
  actorDataList <- jaspResults[["actorDataState"]]$object
  rehObject <- jaspResults[["remifyResultState"]]$object

  if ((length(actorDataList) > 0) && !isTryError(rehObject)) {
    actorNames <- rehObject[["meta"]][["dictionary"]][["actors"]][["actorName"]]
    for (i in 1:length(actorDataList)) {
      nameVar <- actorDataList[[i]][, "name"]
      if (!all(actorNames %in% nameVar)) {
        .quitAnalysis(gettextf("The actor attributes data file %1$s does not contain all actor names",
                               names(actorDataList)[i]))
      }
      # keep the actors in the covariates object that are in the rehObject, aka, the event data
      actorsToKeep <- nameVar %in% actorNames
      orderedActorDataList[[i]] <- actorDataList[[i]][actorsToKeep, ]
    }
    names(orderedActorDataList) <- names(actorDataList)
  }

  actorDataStateNew <- createJaspState(orderedActorDataList)
  actorDataStateNew$dependOn("actorDataList")
  jaspResults[["actorDataStateNew"]] <- actorDataStateNew

  return()
}


.remRemstats <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) return()

  rehObject <- jaspResults[["remifyResultState"]]$object

  if (isTryError(rehObject)) {
    return()
  }

  # this function translates the options from the interface into a formula string for remstats
  # it also produces the detailed effectnames for later use
  ties <- senders <- receivers <- NULL
  if (options[["orientation"]] == "tie") {
    effectsObj <- .translateEffects(jaspResults, options)
    ties <- effectsObj$effects
    senders <- NULL
    receivers <- NULL
  } else {
    effectsObj <- .translateEffects(jaspResults, options, receiver = TRUE)
    effectsObjSender <- .translateEffects(jaspResults, options, sender = "Sender")
    ties <- NULL
    receivers <- effectsObj$effects
    senders <- effectsObjSender$effects
  }

  # prepare the data for remstats, aka, assign the attributes to objects so they are present for remstats;
  # they are removed again as soon as remstats has resolved the formula, so that a later run can never
  # silently resolve an effect against this run's data
  covariateObjects <- .prepareCovariateData(jaspResults, dataset, options)
  on.exit(rm(list = covariateObjects, envir = globalenv()), add = TRUE)

  # prepare some more options for remstats
  memoryValues <- switch(options[["eventHistory"]],
                         "window" = options[["eventHistorySingleInput"]],
                         "interval" = c(options[["eventHistoryIntervalInputLower"]], options[["eventHistoryIntervalInputUpper"]]),
                         "full" = NULL,
                         "decay" = options[["eventHistorySingleInput"]])

  # remstats >= 4.0 no longer has a method argument for treating simultaneous events;
  # statistics are always computed per unique time point (the former "join"/"pt" behavior)

  # when model is ordinal, and no effects are specified, there is no baseline so we need an error
  if (all(sapply(c(ties, senders, receivers), is.null))) {
    jaspResults[["mainContainer"]]$setError(gettext("No effects were specified."))
    return()
  }

  # remstats >= 4.1 renamed the fitting window arguments start/stop to first/last (event indices,
  # first = 2 by default because the first event only initialises the statistics)
  statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = ties, sender_effects = senders,
                                        receiver_effects = receivers,
                                        memory = options[["eventHistory"]], memory_value = memoryValues,
                                        first = options[["timepointInputLower"]],
                                        last = as.numeric(options[["timepointInputUpper"]])))

  if (isTryError(statsObject)) {
    jaspResults[["mainContainer"]]$setError(gettextf("Remstats failed. Internal error message: %s",
                                                     .extractErrorMessage(statsObject)))
    return()
  }

  # specify a new attribute to save the formula, since the actor oriented object does not have the same
  # formula structure as the tie oriented
  attr(statsObject, "formulaJasp") <- effectsObj$effects
  if (options[["orientation"]] == "tie") {
    dimnames(statsObject)[[3]] <- effectsObj$dimNames

  } else {
    attr(statsObject, "formulaJaspSender") <- effectsObjSender$effects
    dimnames(statsObject[["sender_stats"]])[[3]] <- effectsObjSender$dimNames
    # somehow the baseline is only part of the sender model
    dimnames(statsObject[["receiver_stats"]])[[3]] <- effectsObj$dimNames
  }

  jaspResults[["mainContainer"]][["remstatsResultState"]] <- createJaspState(statsObject)

  return()

}


.remRemstimate <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    return()

  rehObject <- jaspResults[["remifyResultState"]]$object
  statsObject <- jaspResults[["mainContainer"]][["remstatsResultState"]]$object

  if (jaspResults[["mainContainer"]]$getError()) return()

  # the method group is hidden in the UI so this is always MLE; guard legacy .jasp files
  # that still carry method = "BSIR" so they fail with a clear message
  if (options[["method"]] != "MLE") {
    jaspResults[["mainContainer"]]$setError(gettextf("The estimation method '%s' is not currently available. Please use maximum likelihood estimation (MLE).",
                                                     options[["method"]]))
    return()
  }

  # remstimate >= 3.1 replaced method = "MLE" with approach = "frequentist" ("method" still
  # works but is deprecated)
  fit <- try(remstimate::remstimate(reh = rehObject, stats = statsObject, approach = "frequentist"))

  if (isTryError(fit)) { # try error
    jaspResults[["mainContainer"]]$setError(gettextf("Remstimate failed. Internal error message: %s", .extractErrorMessage(fit)))
    return()
  }

  remstimateResultState <- createJaspState(fit)
  remstimateResultState$dependOn("method")

  jaspResults[["mainContainer"]][["remstimateResultState"]] <- remstimateResultState

  return()
}

.remRegularization <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["regResultsState"]]$object)) return()
  if (jaspResults[["mainContainer"]]$getError()) return()
  if (options[["regularization"]] == "") return()

  remstimateResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object

  if (options[["regularizationSetSeed"]]) set.seed(options[["regularizationSeed"]])
  if (options[["orientation"]] == "tie") {
    coefs <- remstimateResults$coefficients
    ecov <- remstimateResults$vcov
    regOut <- try(shrinkem::shrinkem(x = coefs,
                             Sigma = ecov,
                             type = options[["regularization"]],
                             iterations = options[["regularizationIterations"]],
                             cred.level = options[["regularizationCiLevel"]]))
  } else {
    regOut <- list()
    if (!is.null(remstimateResults$receiver_model)) {
      coefsRec <- remstimateResults$receiver_model$coefficients
      ecovRec <- remstimateResults$receiver_model$vcov
      regOut$recReg <- try(shrinkem::shrinkem(x = coefsRec,
                                              Sigma = ecovRec,
                                              type = options[["regularization"]],
                                              iterations = options[["regularizationIterations"]],
                                              cred.level = options[["regularizationCiLevel"]]))
    }

    coefsSend <- remstimateResults$sender_model$coefficients
    ecovSend <- remstimateResults$sender_model$vcov
    regOut$sendReg <- try(shrinkem::shrinkem(x = coefsSend,
                                       Sigma = ecovSend,
                                       type = options[["regularization"]],
                                       iterations = options[["regularizationIterations"]],
                                       cred.level = options[["regularizationCiLevel"]]))
  }

  regState <- createJaspState(regOut)
  regState$dependOn(c("regularization", "regularizationIterations", "regularizationCiLevel",
                      "regularizationSetSeed", "regularizationSeed"))
  jaspResults[["mainContainer"]][["regResultsState"]] <- regState

  return()
}


# -------------- Output functions ---------------
.remModelFitTable <- function(jaspResults, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["modelFitContainer"]])) return()

  modelFitContainer <- createJaspContainer()
  modelFitContainer$dependOn("method")
  jaspResults[["mainContainer"]][["modelFitContainer"]] <- modelFitContainer

  modelFitTable <- .createEmptyModelFitTable(options)
  modelFitTable$title <- gettext("Model Fit Table")
  modelFitTable$position <- 1

  modelFitContainer[["modelFitTable"]] <- modelFitTable

  if (ready && options[["syncAnalysisBox"]] && !jaspResults[["mainContainer"]]$getError()) { # create empty table

    remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object

    if (options[["orientation"]] == "tie") {

      res <- summary(remResults)

      modelFitTable$title <- gettext("Model Fit Tie Model")

    } else { # actor model

      resTmp <- summary(remResults)
      res <- resTmp[["receiver_model"]]
      modelFitTable$title <- gettext("Model Fit Receiver Model")

      resSend <- resTmp[["sender_model"]]
      if (!is.null(resSend)) {

        modelFitTableSender <- .createEmptyModelFitTable(options)
        modelFitTableSender$title <- gettext("Model Fit Sender Model")
        modelFitTableSender$position <- 1.1
        modelFitContainer[["modelFitTableSender"]] <- modelFitTableSender

        # in theory, here we would have to distinguish between MLE
        if (options[["method"]] == "MLE") {
          dtFillSender <- data.frame(fitmeasure = c("Null deviance", "Residual deviance", "Chi^2", "AIC", "AICC", "BIC"))
          dtFillSender$estimate <- c(resSend$null.deviance, resSend$residual.deviance, resSend$model.deviance, resSend$AIC, resSend$AICC, resSend$BIC)
          dtFillSender$df <- c(resSend$df.null, resSend$df.residual, resSend$df.model, NA_real_, NA_real_, NA_real_)
          dtFillSender$pvalue <- c(NA_real_, NA_real_, resSend$chiP, NA_real_, NA_real_, NA_real_)

        } else if (options[["method"]] == "BSIR") {

          # BSIR would be:
          # we need N for calculating the BIC for BSIR method
          N <- jaspResults[["remifyResultState"]]$object$M
          BIC <- -2 * res$loglik + npar * log(N)

          dtFillSender <- data.frame(fitmeasure = "BIC")
          dtFillSender$estimate <- BIC
        }
        modelFitTableSender$setData(dtFillSender)
      }
    }

    # fill a data frame:
    # in theory, here we would have to distinguish between MLE
    if (!is.null(res)) {
      if (options[["method"]] == "MLE") {
        dtFill <- data.frame(fitmeasure = c("Null deviance", "Residual deviance", "Chi^2", "AIC", "AICC", "BIC"))
        dtFill$estimate <- c(res$null.deviance, res$residual.deviance, res$model.deviance, res$AIC, res$AICC, res$BIC)
        dtFill$df <- c(res$df.null, res$df.residual, res$df.model, NA_real_, NA_real_, NA_real_)
        dtFill$pvalue <- c(NA_real_, NA_real_, res$chiP, NA_real_, NA_real_, NA_real_)

      } else if (options[["method"]] == "BSIR") {
        # BSIR would be:
        # we need N for calculating the BIC for BSIR method
        N <- jaspResults[["remifyResultState"]]$object$M
        BIC <- -2 * res$loglik + npar * log(N)

        dtFill <- data.frame(fitmeasure = "BIC")
        dtFill$estimate <- BIC
      }

      # fill the table
      modelFitTable$setData(dtFill)
    }

  }


  # add model options to footnote
  eventSeq <- switch(options[["eventSequence"]],
                     "orderOnly" = gettext("ordered only"),
                     "timeSensitive" = gettext("time-sensitive"))
  if (options[["orientation"]] == "tie") {
    modelFitTable$addFootnote(gettextf("The model is %1$s-oriented %2$s with %3$s riskset. The event sequence is %4$s.",
                                       options[["orientation"]], options[["eventDirection"]], options[["riskset"]], eventSeq))
  } else {
    modelFitTable$addFootnote(gettextf("The model is %1$s-oriented with %2$s riskset. The event sequence is %3$s.",
                                       options[["orientation"]], options[["riskset"]], eventSeq))
  }


  # add statistics options in footnote
  eventHistory <- switch(options[["eventHistory"]],
                         "window" = gettextf("a window with %s time units", options[["eventHistorySingleInput"]]),
                         "interval" = gettextf("an interval from %1$s to %2$s",
                                               options[["eventHistoryIntervalInputLower"]],
                                               options[["eventHistoryIntervalInputUpper"]]),
                         "decay" = gettextf("decaying with half-life time of %s", options[["eventHistorySingleInput"]]),
                         options[["eventHistory"]])
  modelFitTable$addFootnote(gettextf("Event history is considered as %1$s.", eventHistory))

  return()
}


.remCoefficientsTable <- function(jaspResults, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["coefficientsContainer"]])) return()

  coefficientsContainer <- createJaspContainer()
  coefficientsContainer$dependOn("method")
  jaspResults[["mainContainer"]][["coefficientsContainer"]] <- coefficientsContainer

  coefficientsTable <- .createEmptyCoefficientsTable(options)
  coefficientsTable$title <- gettext("Coefficient Estimates")

  coefficientsTable$position <- 2
  coefficientsContainer[["coefficientsTable"]] <- coefficientsTable

  if (ready && options[["syncAnalysisBox"]] && !jaspResults[["mainContainer"]]$getError()) { # empty table if we are not ready

    remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object
    ctab <- summary(remResults)[["coefsTab"]]

    if (options[["orientation"]] == "tie") {

      coefficientsTable$title <- gettext("Coefficient Estimates Tie Model")

    } else {

      ctabSend <- ctab[["sender_model"]]
      ctab <- ctab[["receiver_model"]]

      coefficientsTable$title <- gettext("Coefficient Estimates Receiver Model")

      if (!is.null(ctabSend)) {

        coefficientsTableSender <- .createEmptyCoefficientsTable(options)
        coefficientsTableSender$title <- gettext("Coefficient Estimates Sender Model")
        coefficientsTableSender$position <- 3
        coefficientsContainer[["coefficientsTableSender"]] <- coefficientsTableSender

        rwnamesSend <- rownames(ctabSend)
        coefNamesSend <- .transformCoefficientNames(rwnamesSend, options, jaspResults, sender = "Sender")
        # fill a data frame to fill the table
        dtFillSender <- data.frame(ctabSend)
        dtFillSender$coef <- coefNamesSend
        dtFillSender <- dtFillSender[, c(ncol(dtFillSender), 1:(ncol(dtFillSender) - 1))]
        if (options[["method"]] == "MLE") {
          colnames(dtFillSender) <- c("coef", "estimate", "stdErr", "zValue", "prZ", "pr0")

        } else { # method = BSIR
          colnames(dtFillSender) <- c("coef", "estimate", "stdErr", "q2.5", "q50", "q97.5", "pr0")
        }
        # fill the table
        coefficientsTableSender$setData(dtFillSender)
      }
    }

    # fill a data frame
    if (!is.null(ctab)) {
      dtFill <- data.frame(ctab)
      rwnames <- rownames(ctab)
      coefNames <- .transformCoefficientNames(rwnames, options, jaspResults)
      dtFill$coef <- coefNames
      dtFill <- dtFill[, c(ncol(dtFill), 1:(ncol(dtFill) - 1))]
      if (options[["method"]] == "MLE") {
        colnames(dtFill) <- c("coef", "estimate", "stdErr", "zValue", "prZ", "pr0")

      } else { # method = BSIR
        colnames(dtFill) <- c("coef", "estimate", "stdErr", "q2.5", "q50", "q97.5", "pr0")
      }

      # fill the table
      coefficientsTable$setData(dtFill)
    }


  }

  return()
}

.remRegularizationTable <- function(jaspResults, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["regContainer"]])) return()
  if (!ready) return()
  if (!options[["syncAnalysisBox"]]) return()
  if (jaspResults[["mainContainer"]]$getError()) return()
  if (options[["regularization"]] == "") return()


  regContainer <- createJaspContainer()
  regContainer$dependOn(optionsFromObject = jaspResults[["mainContainer"]][["regResultsState"]])
  jaspResults[["mainContainer"]][["regContainer"]] <- regContainer

  ci <- format(100 * options[["regularizationCiLevel"]], digits = 3, drop0trailing = TRUE)

  regTable <- .createRegularizationTable(ci)
  regTable$position <- 2.5
  regResults <- jaspResults[["mainContainer"]][["regResultsState"]]$object

  if (options[["orientation"]] == "tie") {

    if (isTryError(regResults)) {
      regContainer$setError(gettext("Regularization failed"))
      return()
    }
    regTable$title <- gettext("Regularization Results Tie Model")
    dt <- regResults$estimates
    rwnames <- rownames(dt)
    dt$coef <- .transformCoefficientNames(rwnames, options, jaspResults, sender = "")
    regTable$setData(dt)
    regContainer[["regTable"]] <- regTable

  } else {

    if (!is.null(regResults$recReg)) {
      if (isTryError(regResults$recReg) ) {
        regContainer$setError(gettext("Regularization failed"))
        return()
      }
      regTableReceiver <- .createRegularizationTable(ci)
      regTableReceiver$title <- gettext("Regularization Results Receiver Model")
      regTableReceiver$position <- 2.4
      dtRec <- regResults$recReg$estimates
      rwnames <- rownames(dtRec)
      dtRec$coef <- .transformCoefficientNames(rwnames, options, jaspResults, sender = "")
      regTableReceiver$setData(dtRec)
      regContainer[["regTableReceiver"]] <- regTableReceiver
    }

    if (isTryError(regResults$sendReg) ) {
      regContainer$setError(gettext("Regularization failed"))
      return()
    }

    regTableSender <- regTable
    regTableSender$position <- 2.5
    regTableSender$title <- gettext("Regularization Results Sender Model")
    dtSend <- regResults$sendReg$estimates
    rwnames <- rownames(dtSend)
    dtSend$coef <- .transformCoefficientNames(rwnames, options, jaspResults, sender = "Sender")
    regTableSender$setData(dtSend)
    regContainer[["regTableSender"]] <- regTableSender

  }

  return()
}


.remDiagnosticPlot <- function(jaspResults, options, ready) {

  if (!is.null(jaspResults[["mainContainer"]][["plotContainer"]])) return()

  plotContainer <- createJaspContainer()
  plotContainer$dependOn(c("method", "diagnosticPlots", "diagnosticPlotWaitTime", "residualPlotSelect"))
  jaspResults[["mainContainer"]][["plotContainer"]] <- plotContainer


  if (ready && options[["syncAnalysisBox"]] && !jaspResults[["mainContainer"]]$getError()
      && options[["diagnosticPlots"]]) {

    selected <- character(0)
    if (length(options[["residualPlotSelect"]]) > 0) {
      selected <- lapply(options[["residualPlotSelect"]], function(x) {
        if (x[["includePlotEffect"]]) x[["value"]] else NULL
      })
      selected <- unlist(selected[which(!sapply(selected, is.null))])
    }

    if (options[["diagnosticPlotWaitTime"]] || length(selected) > 0 || isTRUE(options[["diagnosticPlotRecall"]])) {

      rehObject <- jaspResults[["remifyResultState"]]$object
      statsObject <- jaspResults[["mainContainer"]][["remstatsResultState"]]$object
      remstimateObject <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object

      diagnos <- remstimate::diagnostics(remstimateObject, rehObject, statsObject)

      if (options[["diagnosticPlotWaitTime"]]) {
        # TODO: plot function for reuse?

        plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 1, effects = NULL,
                                  send_effects = NULL, rec_effects = NULL)
        waitPlot <- createJaspPlot(plot = NULL, title = gettext("Waiting Times Fit"),
                                   height = 400, width = 500)
        waitPlot$dependOn("diagnosticPlotWaitTime")
        waitPlot$position <- 1
        plotContainer[["waitingTimePlot"]] <- waitPlot
        if (isTryError(plotObj)) {
          waitPlot$setError(gettextf("Waiting time diagnostics plot failed with error: %1$s", .extractErrorMessage(plotObj)))
        } else {
          waitPlot$plotObject <- plotObj
        }
      }

      if (isTRUE(options[["diagnosticPlotRecall"]])) {

        recallList <- if (options[["orientation"]] == "tie") {
          list(list(recall = diagnos$recall, role = "", key = "tie", title = gettext("Recall")))
        } else {
          list(
            list(recall = diagnos$sender_model$recall, role = gettext("Sender"),
                 key = "sender", title = gettext("Recall (Sender Model)")),
            list(recall = diagnos$receiver_model$recall, role = gettext("Receiver"),
                 key = "receiver", title = gettext("Recall (Receiver Model)"))
          )
        }

        recallContainer <- createJaspContainer(gettext("Recall Diagnostics"))
        recallContainer$dependOn("diagnosticPlotRecall")
        plotContainer[["recallContainer"]] <- recallContainer

        pos <- 1
        for (rc in recallList) {
          if (is.null(rc$recall) || is.null(rc$recall$per_event)) next

          plotObj <- try(.plotRecallHelper(rc$recall, rc$role))
          recallPlot <- createJaspPlot(plot = NULL, title = rc$title, height = 500, width = 700)
          recallPlot$position <- pos
          pos <- pos + 1
          recallContainer[[rc$key]] <- recallPlot

          if (isTryError(plotObj)) {
            recallPlot$setError(gettextf("Recall diagnostics plot failed with error: %1$s", .extractErrorMessage(plotObj)))
          } else {
            recallPlot$plotObject <- plotObj
          }
        }
      }

      if (length(selected) > 0) {
        residualsPlotContainer <- createJaspContainer(gettext("Schoenfeld's Residuals Fit"))
        residualsPlotContainer$dependOn("residualPlotSelect")
        jaspResults[["mainContainer"]][["plotContainer"]][["residualsContainer"]] <- residualsPlotContainer

        if (options[["orientation"]] == "tie") {

          toPlotTie <- .matchJaspPlotEffects(selected, attr(remstimateObject$coefficients, "name"))
          for (pp in seq_len(length(toPlotTie$coef))) {
            plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 2, effects = toPlotTie$coef[pp],
                                      send_effects = NULL, rec_effects = NULL)
            residualsPlot <- createJaspPlot(plot = NULL, title = toPlotTie$label[pp], height = 400)
            residualsPlot$position <- pp
            residualsPlotContainer[[toPlotTie$coef[pp]]] <- residualsPlot

            if (isTryError(plotObj)) {
              residualsPlot$setError(gettextf("Residual diagnostics plot failed with error: %1$s", .extractErrorMessage(plotObj)))
            } else {
              residualsPlot$plotObject <- plotObj
            }
          }

        } else {
          ##### TODO: the proper titles
          residualsPlotContainerReceiver <- createJaspContainer(gettext("Schoenfeld's Residuals Fit Receiver Model"))
          residualsPlotContainerReceiver$dependOn("residualPlotSelect")
          jaspResults[["mainContainer"]][["plotContainer"]][["residualsContainerReceiver"]] <- residualsPlotContainerReceiver

          toPlotReceiver <- .matchJaspPlotEffects(selected, attr(remstimateObject$receiver_model$coefficients, "name"))
          if (length(toPlotReceiver$coef) > 0) {
            for (pp in seq_len(length(toPlotReceiver$coef))) {
              plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 2, effects = NULL,
                                        send_effects = NULL, rec_effects = toPlotReceiver$coef[pp])
              residualsPlot <- createJaspPlot(plot = NULL, title = "", height = 400)
              residualsPlot$position <- pp
              residualsPlotContainerReceiver[[toPlotReceiver$coef[pp]]] <- residualsPlot

              if (isTryError(plotObj)) {
                residualsPlot$setError(gettextf("Residual diagnostics plot failed with error: %1$s", .extractErrorMessage(plotObj)))
              } else {
                residualsPlot$plotObject <- plotObj
              }
            }
          }

          residualsPlotContainerSender <- createJaspContainer(gettext("Schoenfeld's Residuals Fit Sender Model"))
          residualsPlotContainerSender$dependOn("residualPlotSelect")
          jaspResults[["mainContainer"]][["plotContainer"]][["residualsContainerSender"]] <- residualsPlotContainerSender

          toPlotSender <- .matchJaspPlotEffects(selected, attr(remstimateObject$sender_model$coefficients, "name"))

          if (length(toPlotSender$coef) > 0) {
            for (pp in seq_len(length(toPlotSender$coef))) {
              plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 2, effects = NULL,
                                        send_effects = toPlotSender$coef[pp], rec_effects = NULL)
              residualsPlot <- createJaspPlot(plot = NULL, title = "", height = 400)
              residualsPlot$position <- pp
              residualsPlotContainerSender[[toPlotSender$coef[pp]]] <- residualsPlot

              if (isTryError(plotObj)) {
                residualsPlot$setError(gettextf("Residual diagnostics plot failed with error: %1$s", .extractErrorMessage(plotObj)))
              } else {
                residualsPlot$plotObject <- plotObj
              }
            }
          }
        }


      }
    }

  }

  return()
}


# ------------- Helper functions ----------------
# the following code processes the model effects
.translateEffects <- function(jaspResults, options, sender = "", receiver = FALSE) {

  # event types (in the order remify/remstats use them) drive how consider_type effects
  # expand; interact is only possible with extend_riskset_by_type on
  reh <- jaspResults[["remifyResultState"]]$object
  typeLevels <- character(0)
  if (!isTryError(reh) && !is.null(reh[["meta"]][["dictionary"]][["types"]]))
    typeLevels <- reh[["meta"]][["dictionary"]][["types"]][["typeName"]]
  canInteract <- isTRUE(options[["extendRisksetByType"]]) && options[["typeVariable"]] != ""

  effects <- "~ 1"
  # endogenous effects:
  endos <- options[[paste0("endogenousEffects", sender)]]
  endosSave <- lapply(endos, function(x) {
    if (x[[paste0("includeEndoEffect", sender)]]) x[["value"]] else NULL
      })
  specEndos <- which(!sapply(endosSave, is.null))

  endoObj <- NULL
  if (length(specEndos) > 0) {
    endoObj <- .processEndoEffects(endos[specEndos], sender, typeLevels, canInteract)
    effects <- paste(effects, "+", endoObj$effects)

    # we need the R and translated jasp names of the endo effects in the coefficients table later
    endosMatrix <- matrix(c(endoObj$rNames, endoObj$jaspNames), nrow = length(endoObj$rNames), ncol = 2)
    endosState <- createJaspState(endosMatrix)
    jaspResults[["mainContainer"]][[paste0("endoEffectsState", sender)]] <- endosState

  }

  # exogenous effects
  exoObj <- NULL
  if (length(options[[paste0("specifiedExogenousEffects", sender)]]) > 0) {
    exos <- options[[paste0("specifiedExogenousEffects", sender)]]
    exoObj <- .processExoEffects(exos, sender, jaspResults)
    effects <- paste(effects, "+", exoObj$effects)
  }

  # interactions
  interObj <- NULL
  # does the interactions table even have elements:
  if (length(options[[paste0("interactionEffects", sender)]]) > 0) {

    interEffects <- sapply(options[[paste0("interactionEffects", sender)]],
                           function(x) if (x[[paste0("includeInteractionEffect", sender)]]) x[["value"]] else NULL)
    interEffects[sapply(interEffects, is.null)] <- NULL

    # are any of the interactions in the table included, aka, checked?
    if (length(interEffects) > 0) {
      interObj <- .processInterEffects(interEffects, endoObj, exoObj)
      effects <- paste0(effects, " + ", interObj$effects)
    }
  }

  # the receiver (choice) model never has a baseline; the tie and sender (rate)
  # models have no baseline when the model is ordinal (remstats >= 4.0 ordinal
  # stats objects omit the baseline slice entirely)
  recText <- if (receiver) "receiver" else tolower(sender)
  pos     <- if (receiver) 0.1 else 0.2
  if (receiver || options[["eventSequence"]] == "orderOnly") {
    effects <- sub("1 + ", "", effects, fixed = TRUE)
    if (effects == "~ 1") {
      effects <- NULL
    } else {
      effects <- eval(parse(text = effects))
    }
    dimNms <- c(endoObj$dims, exoObj$dims, interObj$dims)
  } else {
    effects <- eval(parse(text = effects))
    dimNms <- c("baseline", endoObj$dims, exoObj$dims, interObj$dims)
  }

  # print the effects text in the output window:
  effectsText <- Reduce(paste, deparse(effects))
  effectsText <- gsub("+", "+\n", effectsText, fixed = TRUE)

  outText <- createJaspHtml(text = gettextf("The %1$s effects were specified as: \n%2$s", recText, effectsText))
  outText$position <- pos
  jaspResults[["mainContainer"]][[paste0("effectsCall", sender)]] <- outText

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

# for remstats covariate variables need to be present in the environment for the corresponding effects to work:
# returns the names of the objects that were put there, so the caller can clean them up again
.prepareCovariateData <- function(jaspResults, dataset, options) {

  assignedNames <- character(0)

  exoVariablesEnc <- jaspBase::encodeColNames(unique(unlist(jaspResults[["exoEffectsState"]][["object"]][["variableNames"]])))
  exoVariablesDec <- jaspBase::decodeColNames(unique(unlist(jaspResults[["exoEffectsState"]][["object"]][["variableNames"]])))
  exoEffects <- jaspResults[["exoEffectsState"]][["object"]][["list"]]

  if (options[["orientation"]] == "actor") {
    exoVariablesEnc <- c(exoVariablesEnc,
                         jaspBase::encodeColNames(unique(unlist(jaspResults[["exoEffectsState"]][["object"]][["variableNamesSender"]]))))
    exoVariablesDec <- c(exoVariablesDec,
                         jaspBase::decodeColNames(unique(unlist(jaspResults[["exoEffectsState"]][["object"]][["variableNamesSender"]]))))
    exoEffects <- append(exoEffects, jaspResults[["exoEffectsState"]][["object"]][["listSender"]])
  }

  if (!is.null(exoVariablesDec)) { # exo effects specified

    # for the event and tie effects we need the event related columns from the main data, the actor attributes data
    # and dyad attributes data to be present in the environment:

    # first the event effect
    tmp1 <- lapply(exoEffects, function(x) names(x) == "event")
    tmp2 <- unlist(lapply(tmp1, any))
    # use unique, since an actor oriented model could have duplicate effects
    eventNames <- unique(names(tmp2[tmp2]))
    exoVariablesDec <- unique(exoVariablesDec)

    colnames(dataset) <- jaspBase::decodeColNames(colnames(dataset))

    if (length(eventNames) > 0) {
      eventVariables <- jaspBase::decodeColNames(eventNames)

      for (ii in 1:length(eventVariables)) {
        # this makes the variable with the according name "present" in the environment so remstats can find it
        assign(eventVariables[ii], dataset[, eventVariables[ii]], pos = 1) # dont know why pos=1 is working....
        assignedNames <- c(assignedNames, eventVariables[ii])
      }
    }

    # dyadic attributes data
    if (!is.null(jaspResults[["dyadDataState"]]$object)) {
      dyadObj <- jaspResults[["dyadFindState"]]$object
      dyadVarNames <- dyadObj[["name"]]
      dyadFileNames <- dyadObj[["file"]]

      # dyInds records the match between specified exoEffects and dyad attributes variables
      dyInds <- lapply(dyadVarNames, function(x) which(x %in% exoVariablesDec))
      if (length(unlist(dyInds)) > 0) {
        for (iii in 1:length(dyInds)) {
          if (length(dyInds[[iii]]) > 0) {# because there are some integer(0) elements sometimes
            # seems like remstats needs as.matrix()
            dtObj <- jaspResults[["dyadDataState"]][["object"]][[dyadFileNames[[iii]]]]
            if (ncol(dtObj) == nrow(dtObj)) dtObj <- as.matrix(dtObj) # for wide format we need to transform into matrix
            assign(dyadFileNames[[iii]], dtObj, pos = 1)
            assignedNames <- c(assignedNames, dyadFileNames[[iii]])
          }
        }
      }
    }

    # the actor attributes data
    if (!is.null(jaspResults[["actorDataStateNew"]]$object)) {

      actorDataList <- jaspResults[["actorDataStateNew"]]$object

      actorVarNames <- lapply(actorDataList, colnames)
      actorDataNames <- names(actorDataList)
      # is there any exo effects specified for a variable in the attributes data
      actInds <- lapply(actorVarNames, function(x) which(x %in% exoVariablesDec))

      if (length(unlist(actInds)) > 0) {
        for (a in 1:length(actInds)) {
          if (length(actInds[[a]]) > 0) {# because there are some integer(0) elements sometimes
            assign(actorDataNames[[a]], jaspResults[["actorDataStateNew"]][["object"]][[actorDataNames[[a]]]], pos = 1)
            assignedNames <- c(assignedNames, actorDataNames[[a]])
          }
        }
      }
    }
  }

  return(unique(assignedNames))
}



.transformCoefficientNames <- function(coefNames, options, jaspResults, sender = "") {

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
  if (!is.null(jaspResults[["mainContainer"]][[paste0("endoEffectsState", sender)]])) {
    endos <- jaspResults[["mainContainer"]][[paste0("endoEffectsState", sender)]]$object
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

  # now remove the period in the remaining effects
  coefNames <- gsub("\\.(.*)", "", coefNames)

  return(coefNames)

}


.processEndoEffects <- function(endos, sender = "", typeLevels = character(0), canInteract = FALSE) {

  # consider_type values (remstats >= 4.0): "ignore", "separate" (one stat per event
  # type) and "interact" (one stat per ordered pair of types, needs extend_riskset_by_type).
  # remstats collapses separate/interact to ignore when there are < 2 event types and
  # interact to separate without extend_riskset_by_type, so we mirror that when building
  # the dimnames/labels, expanding an effect into as many slices as remstats returns.
  nL <- length(typeLevels)

  endoEffects <- character(0)   # remstats formula terms (one per effect)
  endoDims    <- character(0)   # dimnames assigned to the stats slices (expanded by type)
  endoR       <- character(0)   # r-side names, aligned with endoDims
  endoJasp    <- character(0)   # display names, aligned with endoDims
  endoSave    <- character(0)   # per-slice copy of the formula term (for interactions)

  # one entry per effect (not expanded by slice): matches the generic "(type)" placeholder
  # the interaction picker offers (.feedbackInteractionEffects); remstats always collapses a
  # type-considered effect used in an interaction to a single slice, regardless of type count
  endoGeneric     <- character(0)
  endoGenericTerm <- character(0)
  endoGenericDim  <- character(0)

  for (i in seq_along(endos)) {
    base  <- endos[[i]][["value"]]
    jbase <- endos[[i]][[paste0("translatedName", sender)]]
    scal  <- endos[[i]][[paste0("endogenousEffectsScaling", sender)]]
    uniq  <- endos[[i]][[paste0("endogenousEffectsUnique", sender)]]
    ctype <- endos[[i]][[paste0("endogenousEffectsConsiderType", sender)]]

    stem <- base
    args <- character(0)
    if (!(scal %in% c("none", ""))) {
      args <- c(args, paste0("scaling = '", scal, "'"))
      stem <- paste0(stem, ".", scal)
    }
    if (isTRUE(uniq)) {
      args <- c(args, "unique = TRUE")
      stem <- paste0(stem, ".unique")
    }

    # resolve the effective consider_type; interact needs extend_riskset_by_type and >1 type
    effType <- "ignore"
    if (ctype %in% c("separate", "interact"))
      effType <- if (ctype == "interact" && canInteract && nL > 1) "interact" else "separate"
    if (effType != "ignore")
      args <- c(args, paste0("consider_type = '", effType, "'"))

    term <- paste0(base, "(", paste(args, collapse = ", "), ")")
    endoEffects <- c(endoEffects, term)

    if (effType == "separate" && nL > 1) {
      d <- paste0(stem, ".", typeLevels)
      j <- paste0(jbase, " (", typeLevels, ")")
      r <- d
    } else if (effType == "interact" && nL > 1) {
      g <- expand.grid(inner = typeLevels, outer = typeLevels, stringsAsFactors = FALSE)
      d <- paste0(stem, ".", g$outer, ".", g$inner)
      j <- paste0(jbase, " (", g$outer, " × ", g$inner, ")")
      r <- d
    } else if (effType != "ignore") {
      # a single event type: remstats returns one collapsed slice, keep the legacy label
      d <- paste0(stem, ".type")
      j <- paste0(jbase, "(type)")
      r <- paste0(base, ".type")
    } else {
      d <- stem
      j <- jbase
      r <- base
    }
    endoDims <- c(endoDims, d)
    endoR    <- c(endoR, r)
    endoJasp <- c(endoJasp, j)
    endoSave <- c(endoSave, rep(term, length(d)))

    generic <- if (effType != "ignore") paste0(jbase, "(type)") else jbase
    endoGeneric     <- c(endoGeneric, generic)
    endoGenericTerm <- c(endoGenericTerm, term)
    endoGenericDim  <- c(endoGenericDim, d[1])
  }

  endoEffectsStr <- paste(endoEffects, collapse = " + ")

  return(list(effects = endoEffectsStr, effectsSave = endoSave, dims = endoDims,
              jaspNames = endoJasp, rNames = endoR,
              generic = endoGeneric, genericTerm = endoGenericTerm, genericDim = endoGenericDim))
}


.processExoEffects <- function(exos, sender = "", jaspResults) {

  exoDims <- c() # also save the dimnames to later assign to the statsObject slices
  exoEffects <- sapply(exos, function(x) x[["value"]])
  exoEffectsSave1 <- exoEffects
  exoEffects <- gsub(")", "", exoEffects)
  exoScaling <- sapply(exos, function(x) {
    x[[paste0("exogenousEffectsScaling", sender)]]
    })
  exoAbsolute <- sapply(exos, function(x) {
    x[[paste0("exogenousEffectsAbsolute", sender)]]
    })

  # prepare the dyad data for the possible tie effects:
  dyadObj <- jaspResults[["dyadFindState"]]$object
  dyadVarNames <- dyadObj[["name"]]
  dyadFileNames <- dyadObj[["file"]]

  # prepare the actors data for possible effects
  actorDataList <- jaspResults[["actorDataStateNew"]]$object
  if (!is.null(actorDataList)) {
    actorVarNames <- lapply(actorDataList, colnames)
    actorDataNames <- names(actorDataList)
  }

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

    } else if (startsWith(exoEffects[ii], "tie")) {
      # deal with the tie effects
      # we need to write the filename into the effect string
      ma <- regexpr("'(.*?)'", exoEffects[ii])
      tieName <- gsub("'", "", regmatches(exoEffects[ii], ma), fixed = TRUE)
      ind <- grep(tieName, dyadVarNames)

      if (length(ind) > 0) {
        dtName <- dyadFileNames[[ind]]
        exoEffects[ii] <- sub("(\\'.*?)\\'", paste0("\\1', ", dtName), exoEffects[ii])
      }

    } else { # everything that is not event and tie is also in the attr actors object (if that exists)
      if (!is.null(actorDataList)) {
        ma <- regexpr("'(.*?)'", exoEffects[ii])
        effVarName <- gsub("'", "", regmatches(exoEffects[ii], ma), fixed = TRUE)
        ind <- grep(effVarName, actorVarNames)
        if (length(ind) > 0) {
          dtName <- actorDataNames[[ind]]
          exoEffects[ii] <- sub("(\\'.*?)\\'", paste0("\\1', ", dtName), exoEffects[ii])
        }
      }
    }

    exoDims <- append(exoDims, dimstmp)
  }

  exoEffects <- paste0(exoEffects, ")")
  exoEffectsSave2 <- exoEffects
  exoEffects <- paste0(exoEffects, collapse = " + ")

  return(list(effects = exoEffects, dims = exoDims, saveShort = exoEffectsSave1, saveLong = exoEffectsSave2))
}


.processInterEffects <- function(interEffects, endoObj, exoObj) {

  interDims <- c()

  interEffects <- unlist(interEffects)
  interDims[1:length(interEffects)] <- interEffects

  # work the endo effects: match against the generic "(type)" placeholder the interaction
  # picker offers (.feedbackInteractionEffects) rather than the (possibly type-expanded)
  # per-slice jaspNames -- remstats collapses a type-considered effect used in an interaction
  # to a single slice regardless of how many event types it has
  if (length(endoObj$generic) > 0) {
    interTmps <- strsplit(interEffects, " : ", fixed = TRUE)
    interDimsTmps <- strsplit(interDims, " : ", fixed = TRUE)
    for (ii in 1:length(interTmps)) {
      for (ee in 1:length(endoObj$generic)) {
        ind <- which(endoObj$generic[ee] == interTmps[[ii]])
        if (length(ind) > 0) {
          interTmps[[ii]][ind] <- gsub(endoObj$generic[ee], endoObj$genericTerm[ee], interTmps[[ii]][ind], fixed = TRUE)
          interDimsTmps[[ii]][ind] <- gsub(endoObj$generic[ee], endoObj$genericDim[ee], interDimsTmps[[ii]][ind], fixed = TRUE)
        }
      }
    }
    interEffects <- sapply(interTmps, function(x) paste0(x, collapse = " : "))
    interDims <- sapply(interDimsTmps, function(x) paste0(x, collapse = " : "))
  }

  # work the exo effects
  if (length(exoObj$saveShort) > 0) {
    for (eee in 1:length(exoObj$saveShort)) {
      ind <- grep(exoObj$saveShort[eee], interEffects, fixed = TRUE)
      if (length(ind) > 0) {
        interEffects[ind] <- gsub(exoObj$saveShort[eee], exoObj$saveLong[eee], interEffects[ind], fixed = TRUE)
        interDims[ind] <- gsub(exoObj$saveShort[eee], exoObj$dims[eee], interDims[ind], fixed = TRUE)
      }
    }
  }

  interEffects <- gsub(" : ", ":", interEffects, fixed = TRUE)
  interEffects <- paste0(interEffects, collapse = " + ")

  interDims <- gsub(" : ", ":", interDims, fixed = TRUE)

  return(list(effects = interEffects, dims = interDims))

}


.exogenousEffectsHelper <- function(exoTable) {

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

  exoEffNames <- lapply(exoInds, names)
  exoEffectsForQml <- list()
  for (i in 1:length(exoEffNames)) {
    nm <- names(exoEffNames[i])
    tmp <- paste0(exoEffNames[[i]], "('", nm, "')")
    exoEffectsForQml <- append(exoEffectsForQml, tmp)
  }

  return(list(specifiedEffects = specExoEffects, qmlNames = exoEffectsForQml))
}


.createEmptyModelFitTable <- function(options) {

  modelFitTable <- createJaspTable()
  modelFitTable$addColumnInfo(name = "fitmeasure", title = gettext("Statistic"), type= "string")
  modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type = "number")

  if (options[["method"]] == "MLE") {

    modelFitTable$addColumnInfo(name = "df",   title = gettext("df"), type= "number")
    modelFitTable$addColumnInfo(name = "pvalue",   title = gettext("p"), type= "number")

  }

  return(modelFitTable)
}


.createEmptyCoefficientsTable <- function(options) {

  coefficientsTable <- createJaspTable()
  coefficientsTable$addColumnInfo(name = "coef", title = gettext("Coefficient"), type= "string")
  coefficientsTable$addColumnInfo(name = "estimate", title = gettext("Estimate"), type= "number")

  if (options[["method"]] == "MLE") {
    coefficientsTable$addColumnInfo(name = "stdErr",   title = gettext("Std. Error"),  type= "number")
    coefficientsTable$addColumnInfo(name = "zValue",   title = gettext("z-value"),     type= "number")
    coefficientsTable$addColumnInfo(name = "prZ",      title = gettext("p"),     type= "number")
    coefficientsTable$addColumnInfo(name = "pr0",      title = gettext("p(=0)"),       type= "number")

  } else { # method = BSIR
    coefficientsTable$addColumnInfo(name = "stdErr", title = gettext("Posterior SD"),    type= "number")
    coefficientsTable$addColumnInfo(name = "q2.5",   title = gettextf("2.5%% Quantile"),  type= "number")
    coefficientsTable$addColumnInfo(name = "q50",   title = gettextf("50%% Quantile"),     type= "number")
    coefficientsTable$addColumnInfo(name = "q97.5",      title = gettextf("97.5%% Quantile"),     type= "number")
    coefficientsTable$addColumnInfo(name = "pr0",      title = gettext("p(=0|y)"),       type= "number")
  }

  return(coefficientsTable)
}

.createRegularizationTable <- function(ci) {

  regTable <- createJaspTable()
  regTable$addColumnInfo(name = "coef", title = gettext("Coefficient"), type = "string")
  regTable$addColumnInfo(name = "est", title = gettext("Estimate"), type = "number")
  regTable$addColumnInfo(name = "shrunk.mean", title = gettext("Shrunk Mean"), type = "number")
  regTable$addColumnInfo(name = "shrunk.median", title = gettext("Shrunk Median"), type = "number")
  regTable$addColumnInfo(name = "shrunk.mode", title = gettext("Shrunk Mode"), type = "number")
  regTable$addColumnInfo(name = "shrunk.lower", title = gettext("Lower"),
                         type = "number", overtitle = gettextf("Shrunk %s%% CI", ci))
  regTable$addColumnInfo(name = "shrunk.upper", title = gettext("Upper"),
                         type = "number", overtitle = gettextf("Shrunk %s%% CI", ci))
  regTable$addColumnInfo(name = "nonzero", title = gettext("≠0"), type = "string")

  return(regTable)
}


.plotFunHelper <- function(fit, reh, diagnos, wh, effects, send_effects, rec_effects) {

  plotFun <- function() {
    remstimate:::plot.remstimate(fit, reh = reh, diagnostics = diagnos, which = wh, effects = effects,
                                 sender_effects = send_effects, receiver_effects = rec_effects)
  }
  return(plotFun)
}


# ggplot2 plot of the predictive recall of observed events (remstimate::diagnostics()$recall):
# the relative rank the model assigns each observed event over time, 0 = bottom, 1 = top
.plotRecallHelper <- function(recall, role = "") {

  pe <- recall$per_event
  if (is.null(pe) || nrow(pe) == 0L) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0, label = gettext("Recall (no data)")) +
             ggplot2::theme_void())
  }

  hasTime <- "time" %in% names(pe) && any(is.finite(pe$time))
  med <- stats::median(pe$rel_rank, na.rm = TRUE)

  ord <- order(pe$event)
  x <- pe$event[ord]
  y <- pe$rel_rank[ord]
  k <- max(3L, min(length(y) - 1L, round(length(y) * 0.1)))
  if (k %% 2 == 0) k <- k + 1L
  ym <- tryCatch(stats::runmed(y, k = k), error = function(e) NULL)
  smoothDf <- NULL
  if (!is.null(ym)) {
    sm <- tryCatch(stats::smooth.spline(x, ym), error = function(e) NULL)
    smoothDf <- if (!is.null(sm)) data.frame(x = sm$x, y = sm$y) else data.frame(x = x, y = ym)
  }

  rankLabel    <- gettext("Rank")
  medianLabel  <- gettext("Median rank")
  smoothLabel  <- gettext("Smoothed trend")
  legendLabels <- c(rankLabel, medianLabel, smoothLabel)

  xRange     <- range(pe$event)
  xBreaksAll <- jaspGraphs::getPrettyAxisBreaks(pe$event)
  xBreaks    <- xBreaksAll[xBreaksAll >= xRange[1] & xBreaksAll <= xRange[2]]
  yBreaks    <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))

  xScale <- if (hasTime) {
    timeAtBreaks <- round(stats::approx(pe$event, pe$time, xout = xBreaks, rule = 2)$y, 1)
    ggplot2::scale_x_continuous(name = gettext("Event"), breaks = xBreaks, limits = xRange,
                                sec.axis = ggplot2::dup_axis(breaks = xBreaks, labels = timeAtBreaks,
                                                             name = gettext("Time")))
  } else {
    ggplot2::scale_x_continuous(name = gettext("Event"), breaks = xBreaks, limits = xRange)
  }

  mainLabel <- if (nzchar(role))
    gettextf("%1$s: recall (median rank = %2$.3f)", role, med)
  else
    gettextf("Recall (median rank = %1$.3f)", med)

  p <- ggplot2::ggplot(pe, ggplot2::aes(x = event, y = rel_rank)) +
    ggplot2::geom_point(ggplot2::aes(colour = rankLabel), shape = 16, alpha = 0.35, size = 2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = med, colour = medianLabel), linetype = "dashed", linewidth = 0.7)

  if (!is.null(smoothDf))
    p <- p + ggplot2::geom_line(data = smoothDf, ggplot2::aes(x = x, y = y, colour = smoothLabel), linewidth = 1.2)

  p <- p +
    ggplot2::scale_colour_manual(name = NULL, breaks = legendLabels,
                                 values = stats::setNames(c("black", "blue", "firebrick"), legendLabels)) +
    xScale +
    ggplot2::scale_y_continuous(name = gettext("Relative rank (0 = bottom, 1 = top)"),
                                breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::ggtitle(mainLabel) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw(legend.position = "bottom")

  return(p)
}





.matchJaspPlotEffects <- function(jaspNames, remstimateNames) {

  # align the jaspNames to match them with rnames
  jNames <- gsub("('", "_", jaspNames, fixed = TRUE)
  jNames <- gsub("')", "", jNames, fixed = TRUE)
  jNames <- gsub(" : ", ":", jNames, fixed = TRUE)
  # because the "of" is not part of the remstimate outputted names
  jNames <- gsub(" of ", "", jNames, fixed = TRUE)
  jNames <- gsub(" ", "", jNames, fixed = TRUE)
  jNames <- gsub("(type)", ".type", jNames, fixed = TRUE)
  jNames <- tolower(jNames)
  # because apparently remstimate abbreviates recencyrank to rrank
  jNames <- gsub("recencyrank", "rrank", jNames, fixed = TRUE)


  # align the remstimateNames
  rNames <- remstimateNames
  indsIA <- grep(":", rNames)
  splitlist <- strsplit(rNames[indsIA], ":")
  rNames[indsIA] <- sapply(splitlist, function(x) {
    x <- gsub(".std", "", x, fixed = TRUE)
    x <- gsub(".absolute", "", x, fixed = TRUE)
    x <- gsub(".prop", "", x, fixed = TRUE)
    paste0(x, collapse = ":")
  })
  # now remove the scaling suffixes from the remaining effects
  rNames <- gsub(".std", "", rNames, fixed = TRUE)
  rNames <- gsub(".absolute", "", rNames, fixed = TRUE)
  rNames <- gsub(".prop", "", rNames, fixed = TRUE)
  rNames <- tolower(rNames)

  # a type-considered effect (consider_type = separate/interact) is offered as a single
  # "(type)" picker entry, but remstats expands it into one slice per type (or type pair);
  # match every remstimate slice that shares its stem instead of a single exact name
  coefOut  <- character(0)
  labelOut <- character(0)
  for (j in seq_len(length(jNames))) {
    ind <- match(jNames[j], rNames)
    # there should always be a match, however, sometimes for interactions,
    # the order is switched, so check that
    if (is.na(ind) && grepl(":", jNames[j], fixed = TRUE)) {
      splitted <- unlist(strsplit(jNames[j], ":", fixed = TRUE))
      ind <- match(paste0(splitted[2], ":", splitted[1]), rNames)
    }

    if (!is.na(ind)) {
      coefOut  <- c(coefOut, remstimateNames[ind])
      labelOut <- c(labelOut, jaspNames[j])
    } else if (endsWith(jNames[j], ".type")) {
      stem <- sub("\\.type$", "", jNames[j])
      inds <- which(rNames == stem | startsWith(rNames, paste0(stem, ".")))
      coefOut  <- c(coefOut, remstimateNames[inds])
      labelOut <- c(labelOut, remstimateNames[inds])
    }
  }

  return(list(coef = coefOut, label = labelOut))
}




















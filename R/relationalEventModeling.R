

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
  .remUploadDyadExcludeData(jaspResults, options)

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
                           "naAction", "endogenousEffects", "specifiedExogenousEffects",
                           "interactionEffects", "endogenousEffectsSender", "exogenousEffectsSender",
                           "interactionEffectsSender", "eventHistory", "eventHistorySingleInput",
                           "eventHistoryIntervalInputLower", "eventHistoryIntervalInputUpper", "timepointInputLower",
                           "timepointInputUpper", "simultaneousEvents", "dyadExclude"))
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


.remUploadDyadExcludeData <- function(jaspResults, options) {

  if (!is.null(jaspResults[["dyadExcludeState"]]$object) || options[["riskset"]] != "manual" ||
      is.null(options[["dyadExclude"]])) return()

  if (options[["dyadExclude"]] == "") return()

  bsName <- basename(options[["dyadExclude"]])
  attrName <- gsub("\\..*","", bsName)
  ending <- sub(".*(\\..*)", "\\1", bsName)

  if (ending == ".csv") {
    dyadDt <- read.csv(options[["dyadExclude"]], row.names = NULL, check.names = FALSE)
  } else if (ending == ".txt") {
    dyadDt <- read.delim(options[["dyadExclude"]], row.names = NULL, check.names = FALSE)
  }

  cnames <- colnames(dyadDt)
  if (!all(c("actor1", "actor2") %in% cnames)) {
    .quitAnalysis(gettext("The columns in the dyad exclude data file should be named 'actor1' and 'actor2'"))
  }

  dyadDt <- dyadDt[, c("actor1", "actor2")]
  dyadExcludeState <- createJaspState(dyadDt)
  dyadExcludeState$dependOn(c("riskset", "dyadExclude"))
  jaspResults[["dyadExcludeState"]] <- dyadExcludeState

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
  # does any endo effect have type = both
  endoType <- sapply(endos[specEndos], function(x) {
    x[["endogenousEffectsConsiderType"]]
  })
  # first add the "type" string to all the type-yes instances
  indYes <- which(endoType == "yes")
  if (length(indYes) > 0)
    outEndoList[indYes] <- paste0(outEndoList[indYes], "(type)")

  outEndoListX <- outEndoList
  # now take care of the type-both instances
  indBoth <- which(endoType == "both")
  if (length(indBoth) > 0) {
    newInd <- sort(c(indBoth + seq_len(length(indBoth)), indBoth + seq_len(length(indBoth)) - 1))
    outEndoListX <- vector("character", length = length(outEndoList) + length(indBoth))
    tmp <- rep(outEndoList[indBoth], each = 2)
    tmp[seq(2, length(newInd), by = 2)] <- paste0(tmp[seq(2, length(newInd), by = 2)], "(type)")
    outEndoListX[newInd] <- tmp
    outEndoListX[-newInd] <- outEndoList[-indBoth]
  }

  combList <- combListSender <- NULL
  if ((length(outExoList) + length(outEndoListX)) >= 2) {
    combList <- c(unlist(outEndoListX), unlist(outExoList))
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
    outEndoListXSender <- outEndoListSender
    # does any endo effect have type = both
    endoTypeSender <- sapply(endosSender[specEndosSender], function(x) {
      x[["endogenousEffectsConsiderTypeSender"]]
    })
    indBothSender <- which(endoTypeSender == "both")
    if (length(indBothSender) > 0) {
      newIndSender <- sort(c(indBothSender + seq_len(length(indBothSender)), indBothSender + seq_len(length(indBothSender)) - 1))
      outEndoListXSender <- vector("character", length = length(outEndoListSender) + length(indBothSender))
      tmpSender <- rep(outEndoListSender[indBothSender], each = 2)
      tmpSender[seq(2, length(newIndSender), by = 2)] <- paste0(tmpSender[seq(2, length(newIndSender), by = 2)], "(type)")
      outEndoListXSender[newIndSender] <- tmpSender
      outEndoListXSender[-newIndSender] <- outEndoListSender[-indBothSender]
    }

    if ((length(outExoListSender) + length(outEndoListXSender)) >= 2) {

      combListSender <- c(unlist(outEndoListXSender), unlist(outExoListSender))
      interTmpSender <- combn(combListSender, m = 2)
      intersSender <- as.list(paste0(interTmpSender[1, ], " : ", interTmpSender[2, ]))

      possibleInteractionEffectsFromRSender <- createJaspQmlSource("possibleInteractionEffectsFromRSender", intersSender)
      possibleInteractionEffectsFromRSender$dependOn(c("endogenousEffectsSender", "specifiedExogenousEffectsSender"))
      jaspResults[["possibleInteractionEffectsFromRSender"]] <- possibleInteractionEffectsFromRSender
    }

  }

  # so lets save the effects for using them later too source in the plot effects
  if (!is.null(combList) || !is.null(combListSender)) {
    saveOut <- c(combList, combListSender)
    saveState <- createJaspState(saveOut)
    jaspResults[["savedEffects"]] <- saveState
  }


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
  savedSource$dependOn(c("endogenousEffects", "specifiedExogenousEffects",
                         "endogenousEffectsSender", "specifiedExogenousEffectsSender",
                         "interactionEffects", "interactionEffecsSender",
                         "includeInteractionEffect", "includeInteractionEffectSender"))
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
    exoEffectsState$dependOn(c("exogenousEffectsTableEvents",
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
    eventIndices <- which(eventNames == decodeColNames(colnames(dataset)))
    variables <- c(variables, colnames(dataset)[eventIndices])
  }

  dataset <- dataset[, variables]

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

  # is there any other naAction even possible?
  if (anyNA(dataset)) {
    dataset <- dataset[complete.cases(dataset), ]
  }

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

  if (!is.null(jaspResults[["dyadExcludeState"]][["object"]])) {
    dtExclude <- jaspResults[["dyadExcludeState"]][["object"]]
    dtExclude <- lapply(dtExclude, as.character)
    dtExclude$type <- NA

    omitDyad <- list(list(time = c(NA,NA), dyad = dtExclude))
  } else {
    omitDyad <- NULL
  }

  rehObject <- try(remify::remify(edgelist = dataset,
                                  directed = directed,
                                  ordinal = options[["eventSequence"]] == "orderOnly",
                                  model = options[["orientation"]],
                                  riskset = options[["riskset"]],
                                  omit_dyad = omitDyad))

  if (isTryError(rehObject)) {
    jaspResults[["mainContainer"]]$setError(gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(rehObject)))
  }

  remifyResultState <- createJaspState(rehObject)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "naAction", "weightVariable",
                               "timeVariable", "actorVariableSender", "actorVariableReceiver", "weightVariable",
                               "typeVariable", "syncAnalysisBox", "dyadExclude"))

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
    actorNames <- attr(rehObject, "dictionary")[["actors"]][["actorName"]]
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

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) &&
      !is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) return()

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

  # prepare the data for remstats, aka, assign the attributes to objects so they are present for remstats
  .prepareCovariateData(jaspResults, dataset, options)

  # prepare some more options for remstats
  memoryValues <- switch(options[["eventHistory"]],
                         "window" = options[["eventHistorySingleInput"]],
                         "interval" = c(options[["eventHistoryIntervalInputLower"]], options[["eventHistoryIntervalInputUpper"]]),
                         "full" = NULL,
                         "decay" = options[["eventHistorySingleInput"]])

  # how to treat simultaenous events:
  simMethod <- ifelse(options[["simultaneousEvents"]] == "join", "pt", "pe")

  # check if there is already a statsObject in storage, if null we are in the first round of calculations
  # or maybe the user did not choose to save the samples
  if (is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) ||
      !options[["oldEffectsSaved"]]) {

    # when model is ordinal, and no effects are specified, there is no baseline so we need an error
    if (all(sapply(c(ties, senders, receivers), is.null))) {
      jaspResults[["mainContainer"]]$setError(gettext("No effects were specified."))
      return()
    }

    # in the first round both states are created

    statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = ties, sender_effects = senders,
                                          receiver_effects = receivers,
                                          memory = options[["eventHistory"]], memory_value = memoryValues,
                                          start = options[["timepointInputLower"]],
                                          stop = as.numeric(options[["timepointInputUpper"]]),
                                          method = simMethod))

    if (isTryError(statsObject)) {
      jaspResults[["mainContainer"]]$setError(gettextf("Remstats failed. Internal error message: %s",
                                                       .extractErrorMessage(statsObject)))
      return()
    } else {
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
    }

    remstatsResultState <- createJaspState(statsObject)

    remstatsResultStateStorage <- createJaspState(statsObject)
    jaspResults[["mainContainer"]][["remstatsResultStateStorage"]] <- remstatsResultStateStorage

  } else {

    statsObjectOld <- jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object
    formulaOld <- attr(statsObjectOld, "formulaJasp")
    dimNamesOldSender <- formulaOldSender <- NULL

    if (options[["orientation"]] == "tie") {

      dimNamesOld <- dimnames(statsObjectOld)[[3]]
      # match the current specified effects with the effects that have been calculated in the past
      # return the "new" effects,
      effsMatched <- .matchEffectsForStats(formulaOld, effectsObj$effects, dimNamesOld, effectsObj$dimNames)
      ties <- effsMatched$form
      if (is.null(ties)) { # if no new effects, we can just continue with the old object and calculate nothing new
        statsObjectCombined <- statsObject <- statsObjectOld

      } else {

        statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = ties,
                                              memory = options[["eventHistory"]], memory_value = memoryValues,
                                              start = options[["timepointInputLower"]],
                                              stop = options[["timepointInputUpper"]],
                                              method = simMethod))

        # add the jasp-detailed dimnames to the whole thing.
        if (isTryError(statsObject)) {
          jaspResults[["mainContainer"]]$setError(gettextf("Remstats failed. Internal error message: %s",
                                                           .extractErrorMessage(statsObject)))
          return()
        } else {
          attr(statsObject, "formulaJasp") <- effsMatched$form
          dimnames(statsObject)[[3]] <- effsMatched$dimNamesNew
        }

        # bind the current and the old statsobject together, remstats should by itself take care of duplicates
        # and deal with those, aka, not bind them twice
        statsObjectCombined <- remstats::bind_remstats(statsObjectOld, statsObject)
        attr(statsObjectCombined, "formulaJasp") <- effsMatched$formComb

      }

      jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object <- statsObjectCombined

      sliced <- statsObjectCombined[, , effectsObj$dimNames, drop = FALSE]
      attr(sliced, "riskset") <- attr(statsObjectCombined, "riskset")
      attr(sliced, "class") <- attr(statsObjectCombined, "class")
      attr(sliced, "model") <- attr(statsObjectCombined, "model")
      attr(sliced, "formulaJasp") <- effectsObj$effects

    } else {

      dimNamesOld <- dimnames(statsObjectOld[["receiver_stats"]])[[3]]
      dimNamesOldSender <- dimnames(statsObjectOld[["sender_stats"]])[[3]]
      formulaOldSender <- attr(statsObjectOld, "formulaJaspSender")

      # this get the receiver effects
      effsMatched <- .matchEffectsForStats(formulaOld, effectsObj$effects, dimNamesOld, effectsObj$dimNames)
      effsMatchedSender <- .matchEffectsForStats(formulaOldSender, effectsObjSender$effects, dimNamesOldSender,
                                             effectsObjSender$dimNames)
      receiversNew <- effsMatched$form
      sendersNew <- effsMatchedSender$form

      if (is.null(receiversNew) && is.null(sendersNew)) { # if no new effects, we can just continue with the old object and calculate nothing new
        statsObjectCombined <- statsObject <- statsObjectOld

      } else {

        statsObject <- try(remstats::remstats(reh = rehObject, receiver_effects = receiversNew,
                                              sender_effects = sendersNew,
                                              memory = options[["eventHistory"]], memory_value = memoryValues,
                                              start = options[["timepointInputLower"]],
                                              stop = options[["timepointInputUpper"]],
                                              method = simMethod))

        # add the jasp-detailed dimnames to the whole thing.
        if (isTryError(statsObject)) {
          jaspResults[["mainContainer"]]$setError(gettextf("Remstats failed. Internal error message: %s",
                                                           .extractErrorMessage(statsObject)))
          return()

        } else {
          attr(statsObject, "formulaJasp") <- effsMatched$form
          attr(statsObject, "formulaJaspSender") <- effsMatchedSender$form
          if (!is.null(statsObject[["receiver_stats"]]))
            dimnames(statsObject[["receiver_stats"]])[[3]] <- effsMatched$dimNamesNew[!effsMatched$dimNamesNew == "baseline"]
          if (!is.null(statsObject[["sender_stats"]]))
            dimnames(statsObject[["sender_stats"]])[[3]] <- effsMatchedSender$dimNamesNew
        }

        # bind the current and the old statsobject together, remstats should by itself take care of duplicates
        #  aka, not bind them twice
        statsObjectCombined <- remstats::bind_remstats(statsObjectOld, statsObject)
        attr(statsObjectCombined, "formulaJasp") <- effsMatched$formComb
        attr(statsObjectCombined, "formulaJaspSender") <- effsMatchedSender$formComb

      }

      jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object <- statsObjectCombined

      sliced <- list()
      if (!is.null(receivers))
        sliced$receiver_stats <- statsObjectCombined[["receiver_stats"]][, , effectsObj$dimNames, drop = FALSE]
      if (!is.null(senders))
        sliced$sender_stats <- statsObjectCombined[["sender_stats"]][, , effectsObjSender$dimNames, drop = FALSE]
      attr(sliced, "class") <- attr(statsObjectCombined, "class")
      attr(sliced, "model") <- attr(statsObjectCombined, "model")
      attr(sliced, "actors") <- attr(statsObjectCombined, "actors")
      attr(sliced, "formulaJasp") <- effectsObj$effects
      attr(sliced, "formulaJaspSender") <- effectsObjSender$effects

    }

    remstatsResultState <- createJaspState(sliced)

  }

  remstatsResultState$dependOn("oldEffectsSaved")
  jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState

  return()

}


.remRemstimate <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    return()

  rehObject <- jaspResults[["remifyResultState"]]$object
  statsObject <- jaspResults[["mainContainer"]][["remstatsResultState"]]$object

  if (jaspResults[["mainContainer"]]$getError()) return()

  fit <- try(remstimate::remstimate(reh = rehObject, stats = statsObject, method = options[["method"]]))

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
  simEvents <- switch(options[["simultaneousEvents"]],
                      "join" = "joined",
                      options[["simultaneousEvents"]])
  eventHistory <- switch(options[["eventHistory"]],
                         "window" = gettextf("a window with %s time units", options[["eventHistorySingleInput"]]),
                         "interval" = gettextf("an interval from %1$s to %2$s",
                                               options[["eventHistoryIntervalInputLower"]],
                                               options[["eventHistoryIntervalInputUpper"]]),
                         "decay" = gettextf("decaying with half-life time of %s", options[["eventHistorySingleInput"]]),
                         options[["eventHistory"]])
  modelFitTable$addFootnote(gettextf("Simultaneous events are %1$s. Event history is considered as %2$s.",
                                     simEvents, eventHistory))

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

    if (length(options[["residualPlotSelect"]]) > 0) {
      selected <- lapply(options[["residualPlotSelect"]], function(x) {
        if (x[["includePlotEffect"]]) x[["value"]] else NULL
      })
      selected <- unlist(selected[which(!sapply(selected, is.null))])
    }

    if (options[["diagnosticPlotWaitTime"]] || length(selected) > 0) {

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

      if (length(selected) > 0) {
        residualsPlotContainer <- createJaspContainer(gettext("Schoenfeld's Residuals Fit"))
        residualsPlotContainer$dependOn("residualPlotSelect")
        jaspResults[["mainContainer"]][["plotContainer"]][["residualsContainer"]] <- residualsPlotContainer

        if (options[["orientation"]] == "tie") {

          toPlotTie <- .matchJaspPlotEffects(selected, attr(remstimateObject$coefficients, "name"))
          for (pp in seq_len(length(toPlotTie))) {
            plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 2, effects = toPlotTie[pp],
                                      send_effects = NULL, rec_effects = NULL)
            residualsPlot <- createJaspPlot(plot = NULL, title = selected[pp], height = 400)
            residualsPlot$position <- pp
            residualsPlotContainer[[toPlotTie[pp]]] <- residualsPlot

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
          toPlotReceiver<- toPlotReceiver[!is.na(toPlotReceiver)]
          if (length(toPlotReceiver) > 0) {
            for (pp in seq_len(length(toPlotReceiver))) {
              plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 2, effects = NULL,
                                        send_effects = NULL, rec_effects = toPlotReceiver[[pp]])
              residualsPlot <- createJaspPlot(plot = NULL, title = "", height = 400)
              residualsPlot$position <- pp
              residualsPlotContainerReceiver[[toPlotReceiver[pp]]] <- residualsPlot

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
          toPlotSender <- toPlotSender[!is.na(toPlotSender)]

          if (length(toPlotSender) > 0) {
            for (pp in seq_len(length(toPlotSender))) {
              plotObj <- .plotFunHelper(remstimateObject, rehObject, diagnos, wh = 2, effects = NULL,
                                        send_effects = toPlotSender[pp], rec_effects = NULL)
              residualsPlot <- createJaspPlot(plot = NULL, title = "", height = 400)
              residualsPlot$position <- pp
              residualsPlotContainerSender[[toPlotSender[pp]]] <- residualsPlot

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

  effects <- "~ 1"
  # endogenous effects:
  endos <- options[[paste0("endogenousEffects", sender)]]
  endosSave <- lapply(endos, function(x) {
    if (x[[paste0("includeEndoEffect", sender)]]) x[["value"]] else NULL
      })
  specEndos <- which(!sapply(endosSave, is.null))

  endoObj <- NULL
  if (length(specEndos) > 0) {
    endoObj <- .processEndoEffects(endos[specEndos], sender)
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

  # receiver model has no baseline, as does the model when ordinal=TRUE
  if (receiver || (options[["orientation"]] == "tie" && options[["eventSequence"]] == "orderOnly")) {
    effects <- sub("1 + ", "", effects, fixed = TRUE)
    if (effects == "~ 1") {
      effects <- NULL
    } else {
      effects <- eval(parse(text = effects))
    }
    dimNms <- c(endoObj$dims, exoObj$dims, interObj$dims)
    recText <- ifelse(receiver, "receiver", "")
    pos <- 0.1
  } else {
    effects <- eval(parse(text = effects))
    dimNms <- c("baseline", endoObj$dims, exoObj$dims, interObj$dims)
    recText <- tolower(sender)
    pos <- 0.2
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
.prepareCovariateData <- function(jaspResults, dataset, options) {

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
          }
        }
      }
    }
  }

  return()
}

# match the effects formula from an old remstats object and the current one
# put out the formula that is all new, aka, oldFormula minus effects
# also transform the effects to the names of the statsObject slices, as remstats puts them out
# to use later for matching with the slices of the old object
.matchEffectsForStats <- function(formulaOld, effects, dimNamesOld, dimNames) {

  newEffects <- effects
  formulaOldc <- as.character(formulaOld)
  newEffectsc <- as.character(newEffects)

  if (length(newEffectsc[-1]) == 1 && newEffectsc[-1] == "1") { # if there is only a baseline effect in the current specification?
    return(list(form = NULL, formComb = NULL, dimNamesNew = NULL))
  }

  form <- strsplit(formulaOldc, split = " + ", fixed = TRUE)
  form[[1]] <- NULL
  form <- unlist(form)

  effs <- strsplit(newEffectsc, split = " + ", fixed = TRUE)
  effs[[1]] <- NULL
  effs <- unlist(effs)

  dupInd <- match(form, effs)
  dupInd <- dupInd[complete.cases(dupInd)]

  effsMatchedForm <- effs[-dupInd]

  if (length(effsMatchedForm) > 0) {
    effsMatchedForm <- paste0(effsMatchedForm, collapse = " + ")
    effsMatchedForm <- paste0("~", effsMatchedForm)
    effsMatchedForm <- eval(parse(text = effsMatchedForm))

    # also export the combination of the old formula and new formula to attach to the combined object
    formComb <- c(form, effs)
    formCombOut <- paste0(formComb, collapse = " + ")
    formCombOut <- paste0("~", formCombOut)
    formCombOut <- eval(parse(text = formCombOut))

    # now export the new dimnames to assign to the new statsObejct
    dimNamesOut <- c("baseline", setdiff(dimNames, dimNamesOld))

  } else {
    effsMatchedForm <- NULL
    formCombOut <- formulaOld
    dimNamesOut <- NULL
  }

  return(list(form = effsMatchedForm, formComb = formCombOut, dimNamesNew = dimNamesOut))

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


.processEndoEffects <- function(endos, sender = "") {

  endoDims <- c() # also save the dimnames to later assign to the statsObject slices
  endosR <- sapply(endos, function(x) x[["value"]])
  endosJasp <- sapply(endos, function(x) {
    x[[paste0("translatedName", sender)]]
  })
  endoScaling <- sapply(endos, function(x) {
    x[[paste0("endogenousEffectsScaling", sender)]]
  })
  endoUnique <- sapply(endos, function(x) {
    x[[paste0("endogenousEffectsUnique", sender)]]
  })
  endoType <- sapply(endos, function(x) {
    x[[paste0("endogenousEffectsConsiderType", sender)]]
  })
  endosX <- endosR
  endoScalingX <- endoScaling
  endoUniqueX <- endoUnique
  endoTypeX <- endoType
  endosJaspX <- endosJasp

  # see if the consider_type argument is somewhere specified as "both"
  # because then we need a longer effects vector since we semi-duplicate one effect
  indBoth <- which(endoType == "both")

  if (length(indBoth) > 0) {
    # create vectors that fit the duplicated effects for the "both" consider type arg
    endosX <- vector("character", length = length(endosR) + length(indBoth))
    newInd <- sort(c(indBoth + seq_len(length(indBoth)), indBoth + seq_len(length(indBoth)) - 1))
    endosX[newInd] <- rep(endosR[indBoth], each = 2)
    endosX[-newInd] <- endosR[-indBoth]

    endoScalingX <- vector("character", length = length(endoScaling) + length(indBoth))
    endoScalingX[newInd] <- rep(endoScaling[indBoth], each = 2)
    endoScalingX[-newInd] <- endoScaling[-indBoth]

    endoUniqueX <- vector("character", length = length(endoUnique) + length(indBoth))
    endoUniqueX[newInd] <- rep(endoUnique[indBoth], each = 2)
    endoUniqueX[-newInd] <- endoUnique[-indBoth]

    endoTypeX <- vector("character", length = length(endoType) + length(indBoth))
    endoTypeX[newInd] <- c("no", "yes")
    endoTypeX[-newInd] <- endoType[-indBoth]

    endosJaspX <- vector("character", length = length(endosJasp) + length(indBoth))
    endosJaspX[newInd] <- rep(endosJasp[indBoth], each = 2)
    endosJaspX[-newInd] <- endosJasp[-indBoth]
  }

  endoEffects <- paste0(endosX, "(")

  for (i in 1:length(endosX)) {
    # create the proper dimname
    dimstmp <- endosX[i]

    if (!(endoScalingX[i] %in% c("none", ""))) {
      endoEffects[i] <- paste0(endoEffects[i], "scaling = '", endoScalingX[i], "', ")
      dimstmp <- paste0(dimstmp, ".", endoScalingX[i])
    }
    if (endoUniqueX[i]) {
      endoEffects[i] <- paste0(endoEffects[i], "unique = ", endoUniqueX[i], ", ")
      dimstmp <- paste0(dimstmp, ".unique")
    }

    # consider type is a bit special given we can also have an endo effect with consider type and one without
    if (endoTypeX[i] == "yes") {
      endoEffects[i] <- paste0(endoEffects[i], "consider_type = TRUE, ")
      dimstmp <- paste0(dimstmp, ".type")
      endosJaspX[i] <- paste0(endosJaspX[i], "(type)")
      endosX[i] <- paste0(endosX[i], ".type")
    }
    endoDims <- append(endoDims, dimstmp)
  }

  endoEffects <- paste0(endoEffects, ")")
  endoEffects <- gsub(", )", ")", endoEffects)
  endoEffectsSave <- endoEffects

  endoEffects <- paste(endoEffects, collapse = " + ")

  return(list(effects = endoEffects, effectsSave = endoEffectsSave, dims = endoDims,
              jaspNames = endosJaspX, rNames = endosX))
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

  # work the endo effects
  if (length(endoObj$rNames) > 0) {
    interTmps <- strsplit(interEffects, " : ", fixed = TRUE)
    interDimsTmps <- strsplit(interDims, " : ", fixed = TRUE)
    for (ii in 1:length(interTmps)) {
      for (ee in 1:length(endoObj$rNames)) {
        ind <- which(endoObj$jaspNames[ee] == interTmps[[ii]])
        if (length(ind) > 0) {
          interTmps[[ii]][ind] <- gsub(endoObj$jaspNames[ee], endoObj$effectsSave[ee], interTmps[[ii]][ind], fixed = TRUE)
          interDimsTmps[[ii]][ind] <- gsub(endoObj$jaspNames[ee], endoObj$dims[ee], interDimsTmps[[ii]][ind], fixed = TRUE)
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
    x <- gsub(".std", "", x)
    x <- gsub(".absolute", "", x)
    paste0(x, collapse = ":")
  })
  # now remove the period in the remaining effects
  rNames <- gsub(".std", "", rNames)
  rNames <- gsub(".absolute", "", rNames)
  rNames <- tolower(rNames)

  indVec <- c()
  for (j in seq_len(length(jNames))) {
    ind <- match(jNames[j], rNames)
    # there should always be a match, however, sometimes for interactions,
    # the order is switched, so check that
    if (is.na(ind)) {
      if (grepl(":", jNames[j])) {
        splitted <- unlist(strsplit(jNames[j], ":"))
        jNames[j] <- paste0(splitted[2], ":", splitted[1])
        ind <- grep(jNames[j], rNames)
      }
    }
    indVec <- c(indVec, ind)
  }

  return(remstimateNames[indVec])
}




















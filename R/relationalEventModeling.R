

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

  # sink("~/Downloads/log.txt")
  # on.exit(sink(NULL))

  .remUploadActorData(jaspResults, options)
  .remUploadDyadData(jaspResults, options)

  .feedbackExoTableVariables(jaspResults, options)
  .feedbackExoEffectsSpecified(jaspResults, options)
  .feedbackInteractionEffects(jaspResults, options)


  ready <- (options[["timeVariable"]] != "") && (options[["actorVariableSender"]] != "") && (options[["actorVariableReceiver"]] != "")

  if (ready && !options[["syncAnalysisBox"]]) {
    syncText <- createJaspHtml(text = gettext("<b>Check the 'Sync Analysis' box to run the analysis</b>"))
    jaspResults[["syncText"]] <- syncText
    syncText$dependOn("syncAnalysisBox")
    syncText$position <- 0.01
    return()
  }

  if (!ready) return()

  dataset <- .remReadData(jaspResults, dataset, options)

  .remErrorHandling(jaspResults, dataset, options)
  .remMainContainer(jaspResults, options)

  .remRemify(jaspResults, dataset, options)
  .remCheckActorAttributes(jaspResults, dataset, options)
  .remRemstats(jaspResults, dataset, options)
  .remRemstimate(jaspResults, dataset, options)

  .remModelFitTable(jaspResults, options)
  .remCoefficientsTable(jaspResults, options)

  return()
}

.remMainContainer <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]])) return()
  mainContainer <- createJaspContainer()
  mainContainer$dependOn(c("timeVariable", "actorVariableSender", "actorVariableReceiver", "weightVariable", "typeVariable"))
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
        actorDt <- read.delim(options[["actorDataList"]][[i]][["actorData"]], row.names = NULL, check.names = FALSE)
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

      # this is only necessary for the symmetric matrix format...
      if (isSymmetric(as.matrix(dyadDt))) {
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


# -------- Effects preparation and handling ----------

.feedbackExoTableVariables <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoTableVariablesR"]])) return()

  vars <- jaspBase::decodeColNames(options[["allVariablesHidden"]])
  vars <- vars[!grepl("weight", vars)]
  vars <- vars[!grepl("type", vars)]
  vars <- as.list(vars)

  if (!is.null(jaspResults[["actorDataState"]])) {
    actorDataList <- jaspResults[["actorDataState"]]$object
    for (i in 1:length(actorDataList)) {
      actnms <- colnames(actorDataList[[i]])
      actnms <- actnms[!grepl("time", actnms)]
      actnms <- actnms[!grepl("name", actnms)]
      vars <- append(vars, actnms)
    }
  }

  if (!is.null(jaspResults[["dyadDataState"]])) {
    dyadDataList <- jaspResults[["dyadDataState"]]$object
    dyadVars <- names(dyadDataList)
    vars <- append(vars, dyadVars)
  }

  src <- createJaspQmlSource("exoTableVariablesR", vars)
  src$dependOn(c("allVariablesHidden", "actorDataList", "dyadDataList"))
  jaspResults[["exoTableVariablesR"]] <- src

  return()
}

.feedbackExoEffectsSpecified <- function(jaspResults, options) {

  if (!is.null(jaspResults[["exoEffectsState"]])) {
    return()
  }

  exoOut <- .exogenousEffectsHelper(options[["exogenousEffectsTable"]])
  specExoEffects <- exoOut$specifiedEffects
  exoEffectsForQml <- exoOut$qmlNames

  # fill the rSource for the specified exo effects
  if (!is.null(exoEffectsForQml)) {
    specifiedExoEffectsFromR <- createJaspQmlSource("specifiedExoEffectsFromR", exoEffectsForQml)
    specifiedExoEffectsFromR$dependOn("exogenousEffectsTable")
    jaspResults[["specifiedExoEffectsFromR"]] <- specifiedExoEffectsFromR
  }


  if (options[["orientation"]] == "actor") {

    exoOutSender <- .exogenousEffectsHelper(options[["exogenousEffectsTableSender"]])
    if (!is.null(exoOutSender)) {
      specExoEffectsSender <- exoOutSender$specifiedEffects
      names(specExoEffectsSender) <- paste0(names(specExoEffectsSender), "Sender")
      specExoEffects <- append(specExoEffects, specExoEffectsSender)
      # rSource filling:
      specifiedExoEffectsFromRSender <- createJaspQmlSource("specifiedExoEffectsFromRSender", exoOutSender$qmlName)
      specifiedExoEffectsFromRSender$dependOn("exogenousEffectsTableSender")
      jaspResults[["specifiedExoEffectsFromRSender"]] <- specifiedExoEffectsFromRSender
    }

  }

  exoEffectsState <- createJaspState(specExoEffects)
  exoEffectsState$dependOn(c("exogenousEffectsTable", "exogenousEffectsTableSender"))
  jaspResults[["exoEffectsState"]] <- exoEffectsState

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

  if ((length(outExoList) + length(outEndoListX)) >= 2) {

    combList <- c(unlist(outExoList), unlist(outEndoListX) == 0)
    interTmp <- combn(c(unlist(outExoList), unlist(outEndoListX)), m = 2)
    inters <- as.list(paste0(interTmp[1, ], " * ", interTmp[2, ]))

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

      combListSender <- c(unlist(outExoListSender), unlist(outEndoListXSender) == 0)
      interTmpSender <- combn(c(unlist(outExoListSender), unlist(outEndoListXSender)), m = 2)
      intersSender <- as.list(paste0(interTmpSender[1, ], " * ", interTmpSender[2, ]))

      possibleInteractionEffectsFromRSender <- createJaspQmlSource("possibleInteractionEffectsFromRSender", intersSender)
      possibleInteractionEffectsFromRSender$dependOn(c("endogenousEffectsSender", "specifiedExogenousEffectsSender"))
      jaspResults[["possibleInteractionEffectsFromRSender"]] <- possibleInteractionEffectsFromRSender
    }

  }


  return()
}


# ----------- Main analysis -------------
.remReadData <- function(jaspResults, dataset, options) {

  if (!is.null(dataset))
    return(dataset)

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
    variables <- c(variables, jaspBase::encodeColNames(eventNames))
  }

  dataset <- .readDataSetToEnd(columns = variables)

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

  # first three variables should be time actors and maybe weight
}



.remRemify <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remifyResultState"]]$object))
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

  rehObject <- try(remify::remify(edgelist = dataset,
                                  directed = directed,
                                  ordinal = options[["eventSequence"]] == "orderOnly",
                                  model = options[["orientation"]],
                                  riskset = options[["riskset"]]))

  if (isTryError(rehObject)) {
    .quitAnalysis(gettextf("Remify failed. Internal error message: %s", .extractErrorMessage(rehObject)))
  }

  remifyResultState <- createJaspState(rehObject)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "orientation", "riskset", "naAction", "weightVariable"))

  jaspResults[["mainContainer"]][["remifyResultState"]] <- remifyResultState

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
  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object
  actorNames <- attr(rehObject, "dictionary")[["actors"]][["actorName"]]

  for (i in 1:length(actorDataList)) {
    nameVar <- actorDataList[[i]][, "name"]
    if (!all(actorNames %in% nameVar)) {
      .quitAnalysis(gettextf("The actor attributes data file %1$s does not contain all actor names",
                             names(actorDataList)[i]))
    }
    orderedActorDataList[[i]] <- actorDataList[[i]][match(actorNames, nameVar), ]
  }

  actorDataStateNew <- createJaspState(orderedActorDataList)
  actorDataStateNew$dependOn("actorDataList")
  jaspResults[["actorDataStateNew"]] <- actorDataStateNew

  return()
}


.remRemstats <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) &&
      !is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object)) return()

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

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
  dtExo <- .prepareCovariateData(jaspResults, dataset, options)


  # prepare some more options for remstats
  memoryValues <- switch(options[["eventHistory"]],
                         "window" = options[["eventHistorySingleInput"]],
                         "interval" = c(options[["eventHistoryIntervalInputLower"]], options[["eventHistoryIntervalInputUpper"]]),
                         "full" = NULL,
                         "decay" = options[["eventHistorySingleInput"]])


  # check if there is already a statsObject in storage, if null we are in the first round of calculations
  # or maybe the user did not choose to save the samples
  if (is.null(jaspResults[["mainContainer"]][["remstatsResultStateStorage"]]$object) ||
      !options[["oldEffectsSaved"]]) {

    # when model is ordinal, and no effects are specified, there is no baseline so we need an error
    if (all(sapply(c(ties, senders, receivers), is.null)))
      .quitAnalysis(gettext("No effects were specified."))
    # in the first round both states are created
    statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = ties, sender_effects = senders,
                                          receiver_effects = receivers, attr_actors = dtExo,
                                          memory = options[["eventHistory"]], memory_value = memoryValues,
                                          start = options[["timepointInputLower"]],
                                          stop = as.numeric(options[["timepointInputUpper"]])))

    if (isTryError(statsObject)) {
      .quitAnalysis(gettextf("Remstats failed. Internal error message: %s", .extractErrorMessage(statsObject)))

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

        statsObject <- try(remstats::remstats(reh = rehObject, tie_effects = ties, attr_actors = dtExo,
                                              memory = options[["eventHistory"]], memory_value = memoryValues,
                                              start = options[["timepointInputLower"]],
                                              stop = options[["timepointInputUpper"]]))

        # add the jasp-detailed dimnames to the whole thing.
        if (isTryError(statsObject)) {
          .quitAnalysis(gettextf("Remstats failed. Internal error message: %s", .extractErrorMessage(statsObject)))
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
                                              sender_effects = sendersNew, attr_actors = dtExo,
                                              memory = options[["eventHistory"]], memory_value = memoryValues,
                                              start = options[["timepointInputLower"]],
                                              stop = options[["timepointInputUpper"]]))

        # add the jasp-detailed dimnames to the whole thing.
        if (isTryError(statsObject)) {
          .quitAnalysis(gettextf("Remstats failed. Internal error message: %s", .extractErrorMessage(statsObject)))

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

  remstatsResultState$dependOn(c(.remCommonDependencies(), "oldEffectsSaved"))
  jaspResults[["mainContainer"]][["remstatsResultState"]] <- remstatsResultState

  return()

}


.remRemstimate <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstimateResultState"]]$object))
    return()

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object
  statsObject <- jaspResults[["mainContainer"]][["remstatsResultState"]]$object

  fit <- try(remstimate::remstimate(reh = rehObject, stats = statsObject, method = options[["method"]]))

  if (isTryError(fit)) { # try error
    .quitAnalysis(gettextf("Remstimate failed. Internal error message: %s", .extractErrorMessage(fit)))
  }

  remstimateResultState <- createJaspState(fit)
  remstimateResultState$dependOn(c(.remCommonDependencies(), "method"))

  jaspResults[["mainContainer"]][["remstimateResultState"]] <- remstimateResultState

  return()
}


# -------------- Output functions ---------------
.remModelFitTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["modelFitContainer"]])) return()

  modelFitContainer <- createJaspContainer()
  modelFitContainer$dependOn(c(.remCommonDependencies(), "method"))
  jaspResults[["mainContainer"]][["modelFitContainer"]] <- modelFitContainer

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object
  # we need N for calculating the BIC for BSIR method
  N <- jaspResults[["mainContainer"]][["remifyResultState"]]$object$M

  if (options[["orientation"]] == "tie") {

    res <- summary(remResults)
    modTableTie <- .modelFitTableHelper(res, method = options[["method"]],
                                        npar = ncol(res$coefsTab), N)
    modTableTie$title <- gettext("Model fit tie model")
    modTableTie$position <- 1
    modelFitContainer[["modelFitTableTie"]] <- modTableTie

  } else {

    resTmp <- summary(remResults)

    resRec <- resTmp[["receiver_model"]]
    if (!is.null(resRec)) {
      modTableRec <- .modelFitTableHelper(resRec, method = options[["method"]],
                                          npar = ncol(res$coefsTab$receiver_model), N)
      modTableRec$title <- gettext("Model fit receiver model")
      modTableRec$position <- 1
      modelFitContainer[["modelFitTableReceiver"]] <- modTableRec
    }

    resSend <- resTmp[["sender_model"]]
    if (!is.null(resSend)) {
      modTableSend <- .modelFitTableHelper(resSend, method = options[["method"]],
                                           npar = ncol(res$coefsTab$sender_model), N)
      modTableSend$title <- gettext("Model fit sender model")
      modTableSend$position <- 1.1
      modelFitContainer[["modelFitTableSender"]] <- modTableSend
    }

  }

  return()
}


.remCoefficientsTable <- function(jaspResults, options) {

  if (!is.null(jaspResults[["mainContainer"]][["coefficientsContainer"]])) return()

  coefficientsContainer <- createJaspContainer()
  coefficientsContainer$dependOn(c(.remCommonDependencies(), "method"))
  jaspResults[["mainContainer"]][["coefficientsContainer"]] <- coefficientsContainer

  remResults <- jaspResults[["mainContainer"]][["remstimateResultState"]]$object
  ctab <- summary(remResults)[["coefsTab"]]

  if (options[["orientation"]] == "tie") {

    rwnames <- rownames(ctab)
    coefNames <- .transformCoefficientNames(rwnames, options, jaspResults)

    coefTableTie <- .coefTableHelper(ctab, coefNames, options[["method"]])
    coefTableTie$title <- gettext("Coefficient estimates tie model")
    coefTableTie$position <- 2
    coefficientsContainer[["coefficientsTableTie"]] <- coefTableTie

  } else {

    ctabRec <- ctab[["receiver_model"]]
    if (!is.null(ctabRec)) {

      rwnames <- rownames(ctabRec)
      coefNames <- .transformCoefficientNames(rwnames, options, jaspResults)

      coefTableRec <- .coefTableHelper(ctabRec, coefNames, options[["method"]])
      coefTableRec$title <- gettext("Coefficient estimates receiver model")
      coefTableRec$position <- 2
      coefficientsContainer[["coefficientsTableReceiver"]] <- coefTableRec
    }

    ctabSend <- ctab[["sender_model"]]
    if (!is.null(ctabSend)) {

      rwnames <- rownames(ctabSend)
      coefNames <- .transformCoefficientNames(rwnames, options, jaspResults, sender = "Sender")

      coefTableSend <- .coefTableHelper(ctabSend, coefNames, options[["method"]])
      coefTableSend$title <- gettext("Coefficient estimates sender model")
      coefTableSend$position <- 2
      coefficientsContainer[["coefficientsTableSender"]] <- coefTableSend
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
    exoObj <- .processExoEffects(exos, sender)
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
  if (receiver || options[["eventSequence"]] == "orderOnly") {
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
        assign(eventVariables[ii], dataset[, eventVariables[ii]], pos = 1) # dont know why pos=1 is working....
      }
    }

    # dyadic attributes data
    if (!is.null(jaspResults[["dyadDataState"]]$object)) {
      dyadnames <- names(jaspResults[["dyadDataState"]]$object)
      dyInds <- which(dyadnames %in% exoVariablesDec)
      if (length(dyInds) > 0) {
        for (iii in 1:length(dyInds)) {
          assign(dyadnames[iii], as.matrix(jaspResults[["dyadDataState"]][["object"]][[dyadnames[iii]]]), pos = 1)
        }
      }
    }

    # the actor attributes data
    if (!is.null(jaspResults[["actorDataStateNew"]]$object)) {
      actorDataList <- jaspResults[["actorDataStateNew"]]$object
      dt <- data.frame(matrix(NA, nrow(actorDataList[[1]]), 0))
      for (i in 1:length(actorDataList)) {
        actnms <- colnames(actorDataList[[i]])
        if (i > 1) { # do not duplicate the time and name variable, for other attr data than the first
          actnms <- actnms[!grepl("time", actnms)]
          actnms <- actnms[!grepl("name", actnms)]
        }
        if (any(!is.na(match(actnms, exoVariablesDec)))) {
          dt <- cbind(dt, actorDataList[[i]])
        }
      }
      if (ncol(dt) == 0)
        return()
      else
        return(dt)
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

  # now remove for the remaining effects
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


.processExoEffects <- function(exos, sender = "") {

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

  return(list(effects = exoEffects, dims = exoDims, saveShort = exoEffectsSave1, saveLong = exoEffectsSave2))
}


.processInterEffects <- function(interEffects, endoObj, exoObj) {

  interDims <- c()

  interEffects <- unlist(interEffects)
  interDims[1:length(interEffects)] <- interEffects

  # work the endo effects
  if (length(endoObj$rNames) > 0) {
    interTmps <- strsplit(interEffects, " * ", fixed = TRUE)
    interDimsTmps <- strsplit(interDims, " * ", fixed = TRUE)
    for (ii in 1:length(interTmps)) {
      for (ee in 1:length(endoObj$rNames)) {
        ind <- which(endoObj$jaspNames[ee] == interTmps[[ii]])
        if (length(ind) > 0) {
          interTmps[[ii]][ind] <- gsub(endoObj$jaspNames[ee], endoObj$effectsSave[ee], interTmps[[ii]][ind], fixed = TRUE)
          interDimsTmps[[ii]][ind] <- gsub(endoObj$jaspNames[ee], endoObj$dims[ee], interDimsTmps[[ii]][ind], fixed = TRUE)
        }
      }
    }
    interEffects <- sapply(interTmps, function(x) paste0(x, collapse = " * "))
    interDims <- sapply(interDimsTmps, function(x) paste0(x, collapse = " * "))
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

  interEffects <- gsub(" * ", ":", interEffects, fixed = TRUE)
  interEffects <- paste0(interEffects, collapse = " + ")

  interDims <- gsub(" * ", ":", interDims, fixed = TRUE)

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


.modelFitTableHelper <- function(res, method, npar, N) {

  modelFitTable <- createJaspTable()

    if (method == "MLE") {

      modelFitTable$addColumnInfo(name = "fitmeasure",     title = gettext("Statistic"), type= "string")
      modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
      modelFitTable$addColumnInfo(name = "df",   title = gettext("df"),  type= "number")
      modelFitTable$addColumnInfo(name = "pvalue",   title = gettext("p"),     type= "number")

      dtFill <- data.frame(fitmeasure = c("Null deviance", "Residual deviance", "Chi^2", "AIC", "AICC", "BIC"))
      dtFill$estimate <- c(res$null.deviance, res$residual.deviance, res$model.deviance, res$AIC, res$AICC, res$BIC)
      dtFill$df <- c(res$df.null, res$df.residual, res$df.model, NA_real_, NA_real_, NA_real_)
      dtFill$pvalue <- c(NA_real_, NA_real_, res$chiP, NA_real_, NA_real_, NA_real_)

      modelFitTable$setData(dtFill)

    } else { # method = BSIR

      modelFitTable$addColumnInfo(name = "fitmeasure",     title = gettext("statistic"), type= "string")
      modelFitTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type = "number")

      BIC <- -2 * res$loglik + npar * log(N)

      dtFill <- data.frame(fitmeasure = "BIC")
      dtFill$estimate <- BIC
      modelFitTable$setData(dtFill)

    }

  return(modelFitTable)

}


.coefTableHelper <- function(ctab, coefNames, method) {

  coefficientsTable <- createJaspTable()

  dtFill <- data.frame(ctab)

  dtFill$coef <- coefNames
  dtFill <- dtFill[, c(ncol(dtFill), 1:(ncol(dtFill) - 1))]

  if (method == "MLE") {

    coefficientsTable$addColumnInfo(name = "coef",     title = gettext("Coefficient"), type= "string")
    coefficientsTable$addColumnInfo(name = "estimate", title = gettext("Estimate"),    type= "number")
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

    colnames(dtFill) <- c("coef", "postMode", "postSD", "q2.5", "q50", "q97.5", "pr0")

  }

  coefficientsTable$setData(dtFill)

  return(coefficientsTable)
}



.remCommonDependencies <- function() {
  return(c("eventDirection", "eventSequence", "orientation", "riskset",
           "naAction", "endogenousEffects", "specifiedExogenousEffects",
           "interactionEffects", "endogenousEffectsSender", "exogenousEffectsSender",
           "interactionEffectsSender", "eventHistory", "eventHistorySingleInput", "eventHistoryIntervalInputLower",
           "eventHistoryIntervalInputUpper", "timepointInputLower", "timepointInputUpper"
  ))
}

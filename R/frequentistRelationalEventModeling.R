#
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
#

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

  saveRDS(options, file = "~/Downloads/options.rds")

  .remRemstats(jaspResults, dataset, options)



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

  if (anyNA(dataset) & options$naAction == "listwise") {
    dt <- dt[complete.cases(dt), ]
  }

  remifyResult <- remify::remify(edgelist = dt,
                                 directed = options[["eventDirection"]] == "directed",
                                 ordinal = options[["eventSequence"]] == "orderOnly",
                                 model = options[["modelOrientation"]],
                                 actors = options[["actorVariables"]],
                                 riskset = options[["riskset"]])

  remifyResultState <- createJaspState(remifyResult)
  remifyResultState$dependOn(c("eventDirection", "eventSequence",
                               "modelOrientation", "riskset", "naAction"))

  jaspResults[["mainContainer"]][["remifyResultState"]] <- remifyResultState

  return()
}

.remRemstats <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["remstatsResultState"]]$object))
    return()

  rehObject <- jaspResults[["mainContainer"]][["remifyResultState"]]$object

  effects <- .translateEffects(options)



}


# once Bruno fixes the VariablesList form, we dont need this anymore, since the values
# instead of the labels will be given to R
.effectsMatching <- function() {

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
    endoNames <- .effectsMatching()
    endoEffects <- endoNames[which(endoNames %in% endoEffects, arr.ind = TRUE), "R_name"]
    endoEffects <- paste0(endoEffects, "(scaling = ", "'", endoScaling, "'", ")")
    endoEffectsRSave <- endoEffects
    endoEffects <- paste(endoEffects, collapse = " + ")
    effects <- paste(effects, endoEffects)
  }

  # exogenous effects
  exoIndex <- grep("specifiedExoEffects", names(options))
  if (length(options[[exoIndex]]) > 0) {
    exoEffects <- sapply(options[[exoIndex]], function(x) x[["value"]])
    exoEffectsSave1 <- exoEffects
    exoEffects <- gsub(")", "", exoEffects)
    exoScaling <- sapply(options[[exoIndex]], function(x) x[["exoEffectsScaling"]])
    # exoAbsolute <- sapply(options[[exoIndex]], function(x) x[["absolute"]])
    exoEffects <- paste0(exoEffects, ", scaling = '", exoScaling, "')")#, absolute = ", exoAbsolute, ")")
    exoEffectsSave2 <- exoEffects
    exoEffects <- paste0(exoEffects, collapse = " + ")
    effects <- paste0(effects, " + ", exoEffects)
  }

  # interactions
  interIndex <- grep("interactionEffects", names(options))
  if (length(options[[interIndex]]) > 0) {
    interEffects <- sapply(options[[interIndex]], function(x) if (x[["include"]]) x[["value"]] else NULL)
    interEffects[sapply(interEffects, is.null)] <- NULL
    interEffects <- unlist(interEffects)


    # replace the endo effects
    for (ee in 1:length(endoEffectsJaspSave)) {
      ind <- grep(endoEffectsJaspSave[ee], interEffects, fixed = TRUE)
      if (length(ind) > 0) {
        interEffects <- gsub(endoEffectsJaspSave[ee], endoEffectsRSave[ee], interEffects, fixed = TRUE)
      }
    }

    # replace the exo effects
    for (eee in 1:length(exoEffectsSave1)) {
      ind <- grep(exoEffectsSave1[eee], interEffects, fixed = TRUE)
      if (length(ind) > 0) {
        interEffects <- gsub(exoEffectsSave1[eee], exoEffectsSave2[eee], interEffects, fixed = TRUE)
      }
    }
    interEffects <- gsub(" * ", ":", interEffects, fixed = TRUE)
    interEffects <- paste0(interEffects, collapse = " + ")
    effects <- paste0(effects, " + ", interEffects)
  }

  if (!is.null(effects)) {
    effects <- paste0("~", effects)
    effects <- eval(parse(text = effects))
  }

  return(effects)
}

#' TODO:
#' - check data for errors and stuff


# Copyright (C) 2013-2022 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Main function ----

prepareRelationalEventModeling <- function(jaspResults, dataset, options) {

  # sink(file="~/Downloads/log.txt")
  # on.exit(sink(NULL))

  if (options[["eventData"]] == "") return()

  dataset <- .prepareMultipleDataSets(options)

  .checkData(jaspResults, dataset, options)

  .eventDataTable(jaspResults, dataset, options)
  .personDataTable(jaspResults, dataset, options)
  .dyadDataTable(jaspResults, dataset, options)

  .mergeDataAndDisplay(jaspResults, dataset, options)

  .exportData(jaspResults, dataset, options)


  return()
}


.prepareMultipleDataSets <- function(options) {

  if (options[["eventData"]] != "") {
    eventDt <- try({
      read.csv(options[["eventData"]])
    })

  } else {
    eventDt <- NULL
  }


  if (options[["personData"]] != "") {
    personDt <- try({
      read.csv(options[["personData"]])
    })

  } else {
    personDt <- NULL
  }

  # dyadic data is a bit more complicated
  dyadDataPaths <- sapply(options[["dyadDataList"]], function(x) x[["dyadData"]])

  if (any(dyadDataPaths != "")) {

    dyadOut <- list()
    for (i in 1:length(dyadDataPaths)) {

      if (dyadDataPaths[i] != "") {
        dyadDt <- read.csv(options[["dyadDataList"]][[i]][["dyadData"]], row.names = NULL, check.names = FALSE)

        if (dim(dyadDt)[1] == dim(dyadDt)[2] && all(is.na(diag(as.matrix(dyadDt))))) { # square matrix -> change to long format

          value <- c(as.matrix(dyadDt))
          rownames(dyadDt) <- colnames(dyadDt)

          rn <- rep(rownames(dyadDt), ncol(dyadDt))
          cn <- rep(colnames(dyadDt), each = nrow(dyadDt))
          dyadDf <- data.frame(rn, cn, value)
          attrName <- basename(options[["dyadDataList"]][[i]][["dyadData"]])
          attrName <- gsub("\\..*","", attrName)
          colnames(dyadDf) <- c("actor1", "actor2", attrName)
          dyadOut[[i]] <- dyadDf[complete.cases(dyadDf), ]
        } else {
          dyadOut[[i]] <- dyadDt
        }
      } else {
        dyadOut[[i]] <- NULL
      }


    }

  } else {
    dyadOut <- NULL
  }

  return(list(eventDt = eventDt, personDt = personDt, dyadDt = dyadOut))
}

.checkData <- function(jaspResults, dataset, options) {

  mainContainer <- createJaspContainer(dependencies = c("eventData", "personData", "dyadData"))
  jaspResults[["mainContainer"]] <- mainContainer

  # jaspResults[["mainContainer"]]$setError(gettext("An error has occured"))
}

.eventDataTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["eventDataTable"]])) return()

  eventDataTable <- createJaspTable(title = gettext("Event data"))
  eventDataTable$dependOn("eventData")
  jaspResults[["mainContainer"]][["eventDataTable"]] <- eventDataTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  minr <- options[["eventMinRow"]]
  maxr <- options[["eventMaxRow"]]
  nr <- nrow(dataset$eventDt)

  dt <- head(dataset$eventDt)
  eventDataTable$setData(dt)

  return()
}

.personDataTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["personDataTable"]]) ||
      options[["personData"]] == "") return()

  personDataTable <- createJaspTable(title = gettext("Person data"))
  personDataTable$dependOn("personData")
  jaspResults[["mainContainer"]][["personDataTable"]] <- personDataTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  minr <- options[["personMinRow"]]
  maxr <- options[["personMaxRow"]]
  nr <- nrow(dataset$personDt)

  dt <- head(dataset$personDt)
  personDataTable$setData(dt)

  return()

}

.dyadDataTable <- function(jaspResults, dataset, options) {
  # a shit we need one table for each dyad data cause they might not be the same size
  dyadDataPaths <- sapply(options[["dyadDataList"]], function(x) x[["dyadData"]])

  # if all tables exist, return
  if (!all(sapply(jaspResults[["mainContainer"]][["dyadDataTablesContainer"]], is.null))) return()
  # if data paths are empty return
  if (all(dyadDataPaths == "")) return()

  dyadDataTablesContainer <- createJaspContainer()
  dyadDataTablesContainer$dependOn("dyadDataList")
  jaspResults[["mainContainer"]][["dyadDataTablesContainer"]] <- dyadDataTablesContainer

  if (jaspResults[["mainContainer"]]$getError()) return()


  for (i in 1:length(dyadDataPaths)) {
    if (dyadDataPaths[i] != "") {
      dyadDataTable <- createJaspTable(title = gettextf("Dyadic data #%i", i))
      jaspResults[["mainContainer"]][["dyadDataTablesContainer"]][[gettextf("data%i", i)]] <- dyadDataTable

      dt <- head(dataset$dyadDt[[i]])
      dyadDataTable$setData(dt)

    }
  }
  return()

}


.mergeDataAndDisplay <- function(jaspResults, dataset, options) {

  if (!options[["mergeData"]]) return()

  if (is.null(dataset$eventDt)) return()

  if (!is.null(jaspResults[["mainContainer"]][["mergedDataTable"]])) return()

  evdt <- dataset$eventDt


  if (!is.null(dataset$personDt)) {
    pdt <- dataset$personDt
    newdt <- .mergeDfHelper(evdt, pdt, placeholdername = "empty0")
  } else {
    newdt <- evdt
  }

  if (!all(sapply(dataset$dyadDt, is.null))) {

    for (i in 1:length(dataset$dyadDt)) {
      ddt <- dataset$dyadDt[[i]]
      newdt <- .mergeDfHelper(newdt, ddt, paste0("empty", i), iteration = as.character(i))
    }
  }

  mergedDataTable <- createJaspTable(title = gettext("Merged data"))
  mergedDataTable$dependOn(c("mergeData", "dyadDataList", "eventData", "personData"))
  jaspResults[["mainContainer"]][["mergedDataTable"]] <- mergedDataTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  mergedDataTable$setData(head(newdt))

  mergedData <- createJaspState(newdt)
  mergedData$dependOn(c("mergeData", "dyadDataList", "eventData", "personData"))
  jaspResults[["mainContainer"]][["mergedData"]] <- mergedData


  return()
}


.exportData <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["dataSaved"]])) return()
  if (options[["savePath"]] == "") return()

  dataSaved <- createJaspState()
  dataSaved$dependOn(c("mergeData", "dyadDataList", "eventData", "personData", "savePath"))
  jaspResults[["dataSaved"]] <- dataSaved

  dt <- jaspResults[["mainContainer"]][["mergedData"]]$object

  write.csv(dt, file = options[["savePath"]], row.names = FALSE)

  dataSaved[["object"]] <- TRUE

}



.mergeDfHelper <- function(x, y, placeholdername = "NULL", iteration = NULL) {

  nx <- nrow(x)
  ny <- nrow(y)

  nomx <- names(x)
  nomy <- names(y)

  if (is.null(iteration)) {
    nomy[which(nomy %in% nomx)] <- paste0(nomy[which(nomy %in% nomx)], "_y")
  } else {
    nomy[which(nomy %in% nomx)] <- paste0(nomy[which(nomy %in% nomx)], "_", iteration)
  }

  names(y) <- nomy

  if (nx > ny) {
    y[(ny + 1):nx, ] <- NA
  } else { # this is highlyunlikely
    x[(nx + 1):ny, ] <- NA
  }

  out <- cbind(x, placeholder = NA, y)
  colnames(out) <- c(colnames(x), placeholdername, colnames(y))
  return(out)
}

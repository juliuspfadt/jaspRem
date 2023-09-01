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

  if (options[["dyadData"]] != "") {
    dyadDt <- try({
      read.csv(options[["dyadData"]], row.names = NULL, check.names = FALSE)
    })

    if (dim(dyadDt)[1] == dim(dyadDt)[2] && all(is.na(diag(as.matrix(dyadDt))))) { # square matrix -> change to long format

      value <- c(as.matrix(dyadDt))
      rownames(dyadDt) <- colnames(dyadDt)

      rn <- rep(rownames(dyadDt), ncol(dyadDt))
      cn <- rep(colnames(dyadDt), each = nrow(dyadDt))
      dyadOut <- data.frame(rn, cn, value)
      attrName <- basename(options[["dyadData"]])
      attrName <- gsub("\\..*","", attrName)
      colnames(dyadOut) <- c("actor1", "actor2", attrName)
      dyadOut <- dyadOut[complete.cases(dyadOut), ]
    } else {
      dyadOut <- dyadDt
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
  eventDataTable$dependOn(c("eventData", "eventMinRow", "eventMaxRow"))
  jaspResults[["mainContainer"]][["eventDataTable"]] <- eventDataTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  minr <- options[["eventMinRow"]]
  maxr <- options[["eventMaxRow"]]
  nr <- nrow(dataset$eventDt)

  if (minr > nr || maxr > nr || minr > maxr) {
    eventDataTable$setError("Undefined rows selected")
    return()
  }
  dt <- as.data.frame(dataset$eventDt[minr:maxr, ])
  eventDataTable$setData(dt)

  return()
}

.personDataTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["personDataTable"]]) ||
      options[["personData"]] == "") return()

  personDataTable <- createJaspTable(title = gettext("Person data"))
  personDataTable$dependOn(c("personData", "personMinRow", "personMaxRow"))
  jaspResults[["mainContainer"]][["personDataTable"]] <- personDataTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  minr <- options[["personMinRow"]]
  maxr <- options[["personMaxRow"]]
  nr <- nrow(dataset$personDt)
  if (minr > nr || maxr > nr || minr > maxr) {
    personDataTable$setError("Undefined rows selected")
    return()
  }
  dt <- as.data.frame(dataset$personDt[minr:maxr, ])
  personDataTable$setData(dt)

}

.dyadDataTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["mainContainer"]][["dyadDataTable"]]) ||
      options[["dyadData"]] == "") return()

  dyadDataTable <- createJaspTable(title = gettext("Dyadic data"))
  dyadDataTable$dependOn(c("dyadData", "dyadMinRow", "dyadMaxRow"))
  jaspResults[["mainContainer"]][["dyadDataTable"]] <- dyadDataTable

  if (jaspResults[["mainContainer"]]$getError()) return()

  minr <- options[["dyadMinRow"]]
  maxr <- options[["dyadMaxRow"]]
  nr <- nrow(dataset$dyadDt)
  if (minr > nr || maxr > nr || minr > maxr) {
    dyadDataTable$setError("Undefined rows selected")
    return()
  }
  dt <- as.data.frame(dataset$dyadDt[minr:maxr, ])
  dyadDataTable$setData(dt)

}

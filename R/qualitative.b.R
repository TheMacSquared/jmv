#' @importFrom ggplot2 ggplot aes
#' @importFrom jmvcore .
qualitativeClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "qualitativeClass",
    inherit = qualitativeBase,
    private = list(
        #### Init ----
        .init = function() {
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            if (length(vars) == 0)
                return()

            for (i in seq_along(vars)) {
                var <- vars[i]
                data <- private$.cleanData(var, splitBy)
                table <- self$results$freqs$get(var)

                if (is.null(data))
                    next

                if (is.null(splitBy))
                    private$.initSimple(table, data, var)
                else
                    private$.initGrouped(table, data, var, splitBy)
            }
        },
        .initSimple = function(table, data, var) {
            # propTestN style: level column with auto-content, count, proportion
            rowLevels <- base::levels(data[[var]])

            table$addColumn(name = 'level', title = var, type = 'text',
                content = '($key)')
            table$addColumn(name = 'counts', title = 'Liczebnosc',
                type = 'integer')
            table$addColumn(name = 'prop', title = '(%)',
                type = 'number', format = 'pc')

            for (j in seq_along(rowLevels))
                table$addRow(rowKey = rowLevels[j])
        },
        .initGrouped = function(table, data, var, splitBy) {
            # contTables style
            rowLevels <- base::levels(data[[var]])
            colLevels <- base::levels(data[[splitBy]])

            table$addColumn(name = var, title = var, type = 'text')

            subNames  <- c('[count]', '[pcCol]', '[pcRow]', '[pcTot]')
            subTitles <- c('Liczebnosc', '% w kolumnie', '% w wierszu', '% ogolem')
            visible   <- c('', '(pcCol)', '(pcRow)', '(pcTotal)')

            for (j in seq_along(subNames)) {
                v <- visible[j]
                if (v == '') next  # skip type column for counts (always visible)

                table$addColumn(
                    name = paste0('type', subNames[j]),
                    title = '', type = 'text', visible = v)
            }

            types   <- c('integer', 'number', 'number', 'number')
            formats <- c('', 'pc', 'pc', 'pc')

            for (k in seq_along(colLevels)) {
                level <- colLevels[k]
                for (j in seq_along(subNames)) {
                    v <- visible[j]
                    if (v == '') {
                        table$addColumn(
                            name = paste0(k, subNames[j]),
                            title = level,
                            superTitle = splitBy,
                            type = types[j],
                            format = formats[j])
                    } else {
                        table$addColumn(
                            name = paste0(k, subNames[j]),
                            title = level,
                            superTitle = splitBy,
                            type = types[j],
                            format = formats[j],
                            visible = v)
                    }
                }
            }

            table$addColumn(name = '.total[count]',
                title = 'Razem', type = 'integer')
            table$addColumn(name = '.total[pcCol]',
                title = 'Razem', type = 'number', format = 'pc',
                visible = '(pcCol)')
            table$addColumn(name = '.total[pcTot]',
                title = 'Razem', type = 'number', format = 'pc',
                visible = '(pcTotal)')

            values <- list()
            for (j in seq_along(subNames)) {
                if (visible[j] != '')
                    values[[paste0('type', subNames[j])]] <- subTitles[j]
            }

            for (j in seq_along(rowLevels)) {
                rowValues <- values
                rowValues[[var]] <- rowLevels[j]
                table$addRow(rowKey = j, values = rowValues)
            }

            totalValues <- values
            totalValues[[var]] <- 'Razem'
            table$addRow(rowKey = 'total', values = totalValues)
            table$addFormat(rowKey = 'total', col = 1,
                jmvcore::Cell.BEGIN_END_GROUP)
        },

        #### Run ----
        .run = function() {
            vars <- self$options$vars
            splitBy <- self$options$splitBy

            if (length(vars) == 0)
                return()

            for (i in seq_along(vars)) {
                var <- vars[i]
                data <- private$.cleanData(var, splitBy)
                table <- self$results$freqs$get(var)

                if (is.null(data))
                    next

                if (is.null(splitBy))
                    private$.fillSimple(table, data, var)
                else
                    private$.fillGrouped(table, data, var, splitBy)
            }
        },
        .fillSimple = function(table, data, var) {
            # propTestN style: setRow by rowKey
            column <- data[[var]]
            counts <- table(column)
            total <- sum(counts)

            keys <- table$rowKeys
            for (i in seq_along(keys)) {
                key <- keys[[i]]
                if (key %in% names(counts)) {
                    count <- counts[[key]]
                    table$setRow(rowKey = key, values = list(
                        counts = count,
                        prop = count / total))
                }
            }
        },
        .fillGrouped = function(table, data, var, splitBy) {
            # contTables style
            column <- data[[var]]
            groupCol <- data[[splitBy]]
            nCols <- nlevels(groupCol)

            mat <- table(column, groupCol, useNA = "no")
            total <- sum(mat)
            colTotals <- colSums(mat)
            rowTotals <- rowSums(mat)
            nRows <- nrow(mat)

            freqRowNo <- 1
            for (rowNo in seq_len(nRows)) {
                vals <- mat[rowNo, ]
                rowTotal <- sum(vals)

                counts <- as.list(vals)
                names(counts) <- paste0(1:nCols, '[count]')
                counts[['.total[count]']] <- rowTotal

                pcCol <- as.list(mat[rowNo, ] / colTotals)
                names(pcCol) <- paste0(1:nCols, '[pcCol]')
                pcCol[['.total[pcCol]']] <- unname(rowTotals[rowNo] / total)

                pcRow <- as.list(mat[rowNo, ] / rowTotal)
                names(pcRow) <- paste0(1:nCols, '[pcRow]')

                pcTot <- as.list(mat[rowNo, ] / total)
                names(pcTot) <- paste0(1:nCols, '[pcTot]')
                pcTot[['.total[pcTot]']] <- sum(mat[rowNo, ] / total)

                values <- c(counts, pcCol, pcRow, pcTot)
                table$setRow(rowNo = freqRowNo, values = values)
                freqRowNo <- freqRowNo + 1
            }

            # Total row
            counts <- as.list(colTotals)
            names(counts) <- paste0(1:nCols, '[count]')
            counts[['.total[count]']] <- as.integer(total)

            pcCol <- as.list(rep(1, nCols))
            names(pcCol) <- paste0(1:nCols, '[pcCol]')
            pcCol[['.total[pcCol]']] <- 1

            pcRow <- as.list(colTotals / total)
            names(pcRow) <- paste0(1:nCols, '[pcRow]')

            pcTot <- as.list(colTotals / total)
            names(pcTot) <- paste0(1:nCols, '[pcTot]')
            pcTot[['.total[pcTot]']] <- 1

            values <- c(counts, pcCol, pcRow, pcTot)
            table$setRow(rowKey = 'total', values = values)
        },

        #### Helpers ----
        .cleanData = function(var, splitBy) {
            data <- self$data
            column <- data[[var]]

            if (!is.factor(column))
                return(NULL)

            columns <- list()
            columns[[var]] <- as.factor(column)

            if (!is.null(splitBy))
                columns[[splitBy]] <- as.factor(data[[splitBy]])

            as.data.frame(columns)
        }
    )
)

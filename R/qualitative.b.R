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

            private$.initPlots()
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

            private$.preparePlots()
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

        #### Plots ----
        .initPlots = function() {
            if (! self$options$bar && ! self$options$mosaic)
                return()

            vars <- self$options$vars
            splitBy <- self$options$splitBy

            for (i in seq_along(vars)) {
                var <- vars[i]
                data <- private$.cleanData(var, splitBy)

                if (is.null(data))
                    next

                group <- self$results$plots$get(var)
                levels <- private$.plotLevels(data, var, splitBy)

                if (self$options$bar) {
                    size <- private$.plotSize(levels, "bar")
                    image <- jmvcore::Image$new(
                        options = self$options,
                        name = "bar",
                        renderFun = ".barPlot",
                        requiresData = TRUE,
                        width = size[1],
                        height = size[2],
                        clearWith = list("splitBy", "bar")
                    )
                    group$add(image)
                }

                if (self$options$mosaic) {
                    size <- private$.plotSize(levels, "mosaic")
                    image <- jmvcore::Image$new(
                        options = self$options,
                        name = "mosaic",
                        renderFun = ".mosaicPlot",
                        requiresData = TRUE,
                        width = size[1],
                        height = size[2],
                        clearWith = list("splitBy", "mosaic")
                    )
                    group$add(image)
                }
            }
        },
        .preparePlots = function() {
            if (! self$options$bar && ! self$options$mosaic)
                return()

            vars <- self$options$vars
            splitBy <- self$options$splitBy

            for (i in seq_along(vars)) {
                var <- vars[i]
                data <- private$.cleanData(var, splitBy)

                if (is.null(data))
                    next

                group <- self$results$plots$get(var)
                state <- list(var = var, splitBy = splitBy)

                if (self$options$bar)
                    group$get("bar")$setState(state)

                if (self$options$mosaic)
                    group$get("mosaic")$setState(state)
            }
        },
        .barPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            var <- image$state$var
            splitBy <- image$state$splitBy
            data <- private$.cleanData(var, splitBy)

            if (is.null(data) || nrow(data) == 0)
                return(FALSE)

            if (is.null(splitBy)) {
                counts <- as.data.frame(table(data[[var]], useNA = "no"))
                names(counts) <- c("level", "count")

                plot <- ggplot2::ggplot(
                    data = counts,
                    ggplot2::aes(x = level, y = count)
                ) +
                    ggplot2::geom_col(
                        fill = theme$fill[2],
                        color = theme$color[1],
                        width = 0.7
                    ) +
                    ggplot2::labs(x = var, y = "Liczebnosc")
            } else {
                counts <- as.data.frame(table(data[[var]], data[[splitBy]], useNA = "no"))
                names(counts) <- c("level", "group", "count")

                plot <- ggplot2::ggplot(
                    data = counts,
                    ggplot2::aes(x = level, y = count, fill = group)
                ) +
                    ggplot2::geom_col(
                        color = theme$color[1],
                        position = ggplot2::position_dodge(width = 0.8),
                        width = 0.7
                    ) +
                    ggplot2::labs(x = var, y = "Liczebnosc", fill = splitBy)
            }

            plot <- plot + ggtheme
            plot <- plot + ggplot2::theme(
                legend.position = "bottom",
                legend.box = "vertical"
            )

            if (private$.needsAngledLevels(data[[var]]))
                plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))

            return(plot)
        },
        .mosaicPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            var <- image$state$var
            splitBy <- image$state$splitBy
            data <- private$.cleanData(var, splitBy)

            if (is.null(data) || nrow(data) == 0)
                return(FALSE)

            if (is.null(splitBy))
                rects <- private$.mosaicDataSimple(data, var)
            else
                rects <- private$.mosaicDataGrouped(data, var, splitBy)

            if (nrow(rects) == 0)
                return(FALSE)

            plot <- ggplot2::ggplot(rects) +
                ggplot2::geom_rect(
                    ggplot2::aes(
                        xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        fill = fill
                    ),
                    color = "white"
                ) +
                ggplot2::scale_x_continuous(
                    breaks = rects$xmid[!duplicated(rects$level)],
                    labels = rects$level[!duplicated(rects$level)],
                    expand = c(0, 0)
                ) +
                ggplot2::scale_y_continuous(expand = c(0, 0)) +
                ggplot2::labs(
                    x = var,
                    y = ifelse(is.null(splitBy), "Proporcja", splitBy),
                    fill = ifelse(is.null(splitBy), var, splitBy)
                ) +
                ggtheme
            plot <- plot + ggplot2::theme(
                legend.position = "bottom",
                legend.box = "vertical"
            )

            if (is.null(splitBy))
                plot <- plot + ggplot2::guides(fill = "none")

            if (private$.needsAngledLevels(data[[var]]))
                plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))

            return(plot)
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
        },
        .plotLevels = function(data, var, splitBy) {
            levels <- list(data[[var]])

            if (! is.null(splitBy))
                levels <- c(levels, list(data[[splitBy]]))

            return(lapply(levels, base::levels))
        },
        .plotSize = function(levels, plot) {
            nLevels <- as.numeric(sapply(levels, length))
            nLevels <- ifelse(is.na(nLevels[1:2]), 1, nLevels[1:2])
            nCharLevels <- as.numeric(sapply(lapply(levels, nchar), max))
            nCharLevels <- ifelse(is.na(nCharLevels[1:2]), 0, nCharLevels[1:2])

            width <- max(520, 95 * nLevels[1])
            height <- 380

            if (length(levels) > 1) {
                width <- max(620, 115 * nLevels[1])
                height <- max(420, 26 * nLevels[2] + 390)
            }

            if (plot == "mosaic") {
                width <- max(width, 650)
                height <- max(height, 460)
            }

            if (nLevels[1] > 3 || nCharLevels[1] > 12)
                height <- height + 70

            return(c(width, height))
        },
        .needsAngledLevels = function(column) {
            levels <- base::levels(column)
            return(length(levels) > 3 || (length(levels) > 0 && max(nchar(levels)) > 12))
        },
        .mosaicDataSimple = function(data, var) {
            counts <- as.data.frame(table(data[[var]], useNA = "no"))
            names(counts) <- c("level", "count")
            counts <- counts[counts$count > 0, ]

            total <- sum(counts$count)
            if (total == 0)
                return(data.frame())

            widths <- counts$count / total
            xmax <- cumsum(widths)
            xmin <- c(0, head(xmax, -1))

            data.frame(
                level = counts$level,
                fill = counts$level,
                xmin = xmin,
                xmax = xmax,
                ymin = 0,
                ymax = 1,
                xmid = (xmin + xmax) / 2
            )
        },
        .mosaicDataGrouped = function(data, var, splitBy) {
            mat <- table(data[[var]], data[[splitBy]], useNA = "no")
            total <- sum(mat)

            if (total == 0)
                return(data.frame())

            levelTotals <- rowSums(mat)
            widths <- levelTotals / total
            xmax <- cumsum(widths)
            xmin <- c(0, head(xmax, -1))

            pieces <- list()
            iter <- 1
            for (i in seq_len(nrow(mat))) {
                if (levelTotals[i] == 0)
                    next

                heights <- mat[i, ] / levelTotals[i]
                ymax <- cumsum(heights)
                ymin <- c(0, head(ymax, -1))

                for (j in seq_len(ncol(mat))) {
                    if (heights[j] == 0)
                        next

                    pieces[[iter]] <- data.frame(
                        level = rownames(mat)[i],
                        fill = colnames(mat)[j],
                        xmin = xmin[i],
                        xmax = xmax[i],
                        ymin = ymin[j],
                        ymax = ymax[j],
                        xmid = (xmin[i] + xmax[i]) / 2
                    )
                    iter <- iter + 1
                }
            }

            do.call(rbind, pieces)
        }
    )
)

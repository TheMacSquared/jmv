#' @importFrom ggplot2 ggplot aes
#' @importFrom jmvcore .
simpleCorrClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "simpleCorrClass",
    inherit = simpleCorrBase,
    private = list(
        .run = function() {
            if (is.null(self$options$var1) || is.null(self$options$var2))
                return()

            var1Name <- self$options$var1
            var2Name <- self$options$var2
            method <- self$options$method

            x <- jmvcore::toNumeric(self$data[[var1Name]])
            y <- jmvcore::toNumeric(self$data[[var2Name]])

            complete <- !is.na(x) & !is.na(y)
            x <- x[complete]
            y <- y[complete]
            n <- length(x)

            if (n < 4) {
                self$results$table$setNote("err",
                    "Potrzeba co najmniej 4 kompletne obserwacje.")
                return()
            }

            # cor.test for Pearson/Spearman
            result <- try(
                suppressWarnings(cor.test(x, y, method = method)),
                silent = TRUE
            )

            if (inherits(result, "try-error")) {
                self$results$table$setNote("err",
                    "Nie udalo sie obliczyc korelacji.")
                return()
            }

            r <- as.numeric(result$estimate)
            pVal <- result$p.value
            # df is only defined for Pearson; Spearman doesn't have it
            df <- if (!is.null(result$parameter)) as.integer(result$parameter) else n - 2

            self$results$table$setRow(rowNo = 1, values = list(
                var1 = var1Name,
                var2 = var2Name,
                r = r,
                df = df,
                p = pVal,
                n = n
            ))

            methodLabel <- if (method == "pearson") "Pearson" else "Spearman"
            self$results$table$setNote("info",
                paste0("Metoda: ", methodLabel, "; N = ", n))

            self$results$plot$setState(list(
                var1Name = var1Name,
                var2Name = var2Name,
                x = x,
                y = y,
                r = r,
                method = method
            ))
        },
        .scatterPlot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            s <- image$state
            df <- data.frame(x = s$x, y = s$y)
            methodLabel <- if (s$method == "pearson") "Pearson" else "Spearman"

            p <- ggplot(df, aes(x = x, y = y)) +
                ggplot2::geom_point(
                    alpha = 0.5,
                    size = 2.5,
                    color = theme$color[1]
                ) +
                ggplot2::geom_smooth(
                    method = "lm",
                    formula = y ~ x,
                    se = FALSE,
                    color = "firebrick",
                    linewidth = 1
                ) +
                ggplot2::labs(
                    x = s$var1Name,
                    y = s$var2Name,
                    subtitle = sprintf("r (%s) = %.3f", methodLabel, s$r)
                ) +
                ggtheme

            return(p)
        }
    )
)

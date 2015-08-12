library(shiny)
library(compute.es)
library(car)


shinyServer(function(input, output) {
    
    options(warn=-1)
    
    gendat <- reactive ({
        nx <- input$nx
        mx <- input$mx
        sdx <- input$sdx
        ny <- input$ny
        my <- input$my
        sdy <- input$sdy
        
        gendat1 <- function(n, mean, sd) return(scale(rnorm(n))*sd+mean)
        
        x <- gendat1(nx, mx, sdx)
        
        y <- gendat1(ny, my, sdy)
        
        list(x = x, y = y)
    })



    sliderValues <- reactive ({
        n1 <- as.integer(input$nx)
        n2 <- as.integer(input$ny)
        
        data.frame(
            Group = c("A", "B"),
            n = c(n1, n2),
            Mean = c(input$mx, input$my),
            SD = c(input$sdx, input$sdy),
            stringsAsFactors=FALSE)
    })
    
    
    
    output$distPlot <- renderPlot({
        x <- gendat()$x
        x <- x[!is.na(x)]
        
        simple.bincount <- function(x, breaks) {
            nx <- length(x)
            nbreaks <- length(breaks)
            counts <- integer(nbreaks - 1)
            for (i in 1:nx) {
                lo <- 1
                hi <- nbreaks
                if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
                    while (hi - lo >= 2) {
                        new <- (hi + lo) %/% 2
                        if(x[i] > breaks[new])
                        lo <- new
                        else
                        hi <- new
                    }
                    counts[lo] <- counts[lo] + 1
                }
            }
            return(counts)
        }
        
        nclass <- nclass.FD(x)
        breaks <- pretty(x, nclass)
        counts <- simple.bincount(x, breaks)
        counts.max <- max(counts)
        
        h <- hist(x, las=1, breaks="FD", xlab= "Vertical line shows the mean.",
        ylim=c(0, counts.max*1.2), main="", col = rgb(0,0,1,1/4))
        rug(x)
        abline(v = mean(x, na.rm=T), col = "blue", lwd = 2)
        xfit <- seq(min(x, na.rm=T), max(x, na.rm=T))
        yfit <- dnorm(xfit, mean = mean(x, na.rm=T), sd = sd(x, na.rm=T))
        yfit <- yfit * diff(h$mids[1:2]) * length(x)
        lines(xfit, yfit, col = "blue", lwd = 2)
    })
    
    
    
    output$overPlot <- renderPlot({
        x <- gendat()$x
        x <- x[!is.na(x)]
        
        y <- gendat()$y
        y <- y[!is.na(y)]
        
        simple.bincount <- function(x, breaks) {
            nx <- length(x)
            nbreaks <- length(breaks)
            counts <- integer(nbreaks - 1)
            for (i in 1:nx) {
                lo <- 1
                hi <- nbreaks
                if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
                    while (hi - lo >= 2) {
                        new <- (hi + lo) %/% 2
                        if(x[i] > breaks[new])
                        lo <- new
                        else
                        hi <- new
                    }
                    counts[lo] <- counts[lo] + 1
                }
            }
            return(counts)
        }
        
        nclass.x <- nclass.FD(x)
        breaks.x <- pretty(x, nclass.x)
        counts.x <- simple.bincount(x, breaks.x)
        counts.max.x <- max(counts.x)
        
        nclass.y <- nclass.FD(y)
        breaks.y <- pretty(y, nclass.y)
        counts.y <- simple.bincount(y, breaks.y)
        counts.max.y <- max(counts.y)
        
        counts.max <- max(c(counts.max.x, counts.max.y))
        
        
        xy.min <- min(c(x,y))
        xy.min <- xy.min - xy.min*0.1
        xy.max <- max(c(x,y))
        xy.max <- xy.max + xy.max*0.1
        
        p1 <- hist(x, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
        p2 <- hist(y, xlim = c(xy.min, xy.max), ylim = c(0, counts.max*1.3))
        
        plot(p1, las=1, xlab = "Group 1 is expressed in blue; Group 2 in red. Vertical lines show the means.",
        main = "", col = rgb(0,0,1,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3))
        plot(p2, las=1, xlab = "", main = "", col = rgb(1,0,0,1/4), xlim = c(xy.min,xy.max), ylim = c(0, counts.max*1.3), add = T)
        
        abline(v = mean(x), col = "blue", lwd = 2)
        abline(v = mean(y), col = "red", lwd = 2)
        
    })
    
    
    
    output$t.distPlot <- renderPlot({
        nx <- input$nx
        mx <- input$mx
        sdx <- input$sdx
        ny <- input$ny
        my <- input$my
        sdy <- input$sdy
        
        if (input$varequal) {
            df <- nx+ny-2
            v1 <- ((nx-1)*sdx^2+(ny-1)*sdy^2)/df
            tstat <- round((mx-my)/sqrt(v1*(1/nx+1/ny)),3)
            diff <- round((mx - my), 3)
            options(scipen=100)
            P <- 2 * pt(-abs(tstat), df)
            
        } else {
            
            stderrx <- sqrt(sdx^2/nx)
            stderry <- sqrt(sdy^2/ny)
            stderr <- sqrt(stderrx^2 + stderry^2)
            df <- round(stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1)),3)
            tstat <- round((mx - my)/stderr,3)
            options(scipen=100)
            P <- 2 * pt(-abs(tstat), df)

        }
        
        
        xlim = c(-5, 5)
        curve(dt(x,df),-xlim,xlim,ylab="density",xlab="t-value", main=paste("p = ", P, sep=""))
        
        abline(v = qt(0.025, df), lty=2)
        abline(v = qt(0.975, df), lty=2)
        abline(v = tstat, lty=1, lwd=2, col="red")
        text(0, 0.03, paste("df = ", df, sep=""))
        text(tstat, 0.1, paste("t = ", tstat, sep=""))

    })



    difference <- reactive({
            nx <- input$nx
            mx <- input$mx
            sdx <- input$sdx
            ny <- input$ny
            my <- input$my
            sdy <- input$sdy
            
            if (input$varequal) {
                df <- nx+ny-2
                v <- ((nx-1)*sdx^2+(ny-1)*sdy^2)/df
                diff <- round((mx - my), 3)
                diff.std <- sqrt(v * (1/nx + 1/ny))
                diff.lower <- round(diff + diff.std * qt(0.05/2, df),3)
                diff.upper <- round(diff + diff.std * qt(0.05/2, df, lower.tail = FALSE),3)
            } else {
                stderrx <- sqrt(sdx^2/nx)
                stderry <- sqrt(sdy^2/ny)
                stderr <- sqrt(stderrx^2 + stderry^2)
                df <- round(stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1)),3)
                tstat <- round(abs(mx - my)/stderr,3)
                diff <- round((mx - my), 3)
                cint <- qt(1 - 0.05/2, df)
                diff.lower <- round(((tstat - cint) * stderr),3)
                diff.upper <- round(((tstat + cint) * stderr),3)
            }
            
            cat("Mean of the differences [95% CI] =", diff, "[", diff.lower,",", diff.upper,"]", "\n")
    })



    es <- reactive({
        nx <- input$nx
        mx <- input$mx
        sdx <- input$sdx
        ny <- input$ny
        my <- input$my
        sdy <- input$sdy
    
        mes(mx, my, sdx, sdy, nx, ny)
    })
    
    
    
    ttest <- reactive({
        nx <- input$nx
        mx <- input$mx
        sdx <- input$sdx
        ny <- input$ny
        my <- input$my
        sdy <- input$sdy
        
     if (input$varequal) {
        df1 <- nx+ny-2
        v1 <- ((nx-1)*sdx^2+(ny-1)*sdy^2)/df1
        tstat1 <- round((mx-my)/sqrt(v1*(1/nx+1/ny)),3)
        diff <- round((mx - my), 3)
        P1 <- 2 * pt(-abs(tstat1), df1)
        
        cat("Independent t-test (equal variances assumed)", "\n",
        " t =", tstat1, ",", "df =", df1, ",", "p-value =", P1, "\n")
        
     } else {

        stderrx <- sqrt(sdx^2/nx)
        stderry <- sqrt(sdy^2/ny)
        stderr <- sqrt(stderrx^2 + stderry^2)
        df2 <- round(stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1)),3)
        tstat2 <- round((mx - my)/stderr,3)
        P2 <- 2 * pt(-abs(tstat2), df2)
        
        cat("Welch's t-test (equal variances not assumed)", "\n",
            " t =", tstat2, ",", "df =", df2, ",", "p-value =", P2, "\n")
     }
     })
    
    

    vartest <- reactive({
        x <- gendat()$x
        x <- x[!is.na(x)]
        
        y <- gendat()$y
        y <- y[!is.na(y)]
        
        score <- c(x, y)
        group <- factor(c(rep("Data 1", length(x)), rep("Data 2", length(y))))
        
        leveneTest(score, group, center=mean)
    })
    
    
    
    output$ciPlot <- renderPlot({
        
        plotMeans <- function (response, factor1, factor2, error.bars = c("se", "sd",
            "conf.int", "none"), level = 0.95, xlab = deparse(substitute(factor1)),
            ylab = paste("mean of", deparse(substitute(response))), legend.lab = deparse(substitute(factor2)),
            main = "Plot of Means", pch = 1:n.levs.2, lty = 1:n.levs.2,
            col = palette(), ...)
            {
            if (!is.numeric(response))
            stop(gettextRcmdr("Argument response must be numeric."))
            xlab
            ylab
            legend.lab
            error.bars <- match.arg(error.bars)
            if (missing(factor2)) {
                if (!is.factor(factor1))
                stop(gettextRcmdr("Argument factor1 must be a factor."))
                valid <- complete.cases(factor1, response)
                factor1 <- factor1[valid]
                response <- response[valid]
                means <- tapply(response, factor1, mean)
                sds <- tapply(response, factor1, sd)
                ns <- tapply(response, factor1, length)
                if (error.bars == "se")
                sds <- sds/sqrt(ns)
                if (error.bars == "conf.int")
                sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
                sds/sqrt(ns)
                sds[is.na(sds)] <- 0
                yrange <- if (error.bars != "none")
                c(min(means - sds, na.rm = TRUE), max(means + sds,
                na.rm = TRUE))
                else range(means, na.rm = TRUE)
                levs <- levels(factor1)
                n.levs <- length(levs)
                plot(c(1, n.levs), yrange, type = "n", xlab = xlab, ylab = ylab,
                axes = FALSE, main = main, ...)
                points(1:n.levs, means, type = "b", pch = 16, cex = 2)
                box()
                axis(2)
                axis(1, at = 1:n.levs, labels = levs)
                if (error.bars != "none")
                arrows(1:n.levs, means - sds, 1:n.levs, means + sds,
                angle = 90, lty = 1, code = 3, length = 0.125)
            }
            else {
                if (!(is.factor(factor1) | is.factor(factor2)))
                stop(gettextRcmdr("Arguments factor1 and factor2 must be factors."))
                valid <- complete.cases(factor1, factor2, response)
                factor1 <- factor1[valid]
                factor2 <- factor2[valid]
                response <- response[valid]
                means <- tapply(response, list(factor1, factor2), mean)
                sds <- tapply(response, list(factor1, factor2), sd)
                ns <- tapply(response, list(factor1, factor2), length)
                if (error.bars == "se")
                sds <- sds/sqrt(ns)
                if (error.bars == "conf.int")
                sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
                sds/sqrt(ns)
                sds[is.na(sds)] <- 0
                yrange <- if (error.bars != "none")
                c(min(means - sds, na.rm = TRUE), max(means + sds,
                na.rm = TRUE))
                else range(means, na.rm = TRUE)
                levs.1 <- levels(factor1)
                levs.2 <- levels(factor2)
                n.levs.1 <- length(levs.1)
                n.levs.2 <- length(levs.2)
                if (length(pch) == 1)
                pch <- rep(pch, n.levs.2)
                if (length(col) == 1)
                col <- rep(col, n.levs.2)
                if (length(lty) == 1)
                lty <- rep(lty, n.levs.2)
                if (n.levs.2 > length(col))
                stop(sprintf(gettextRcmdr("Number of groups for factor2, %d, exceeds number of distinct colours, %d."),
                n.levs.2, length(col)))
                plot(c(1, n.levs.1 * 1.4), yrange, type = "n", xlab = xlab,
                ylab = ylab, axes = FALSE, main = main, ...)
                box()
                axis(2)
                axis(1, at = 1:n.levs.1, labels = levs.1)
                for (i in 1:n.levs.2) {
                    points(1:n.levs.1, means[, i], type = "b", pch = pch[i],
                    cex = 2, col = col[i], lty = lty[i])
                    if (error.bars != "none") 
                    arrows(1:n.levs.1, means[, i] - sds[, i], 1:n.levs.1, 
                    means[, i] + sds[, i], angle = 90, code = 3, 
                    col = col[i], lty = lty[i], length = 0.125)
                }
                x.posn <- n.levs.1 * 1.1
                y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
                text(x.posn, y.posn, legend.lab, adj = c(0, -0.5))
                legend(x.posn, y.posn, levs.2, pch = pch, col = col, 
                lty = lty)
            }
            invisible(NULL)
        }
        
        

        x <- gendat()$x
        y <- gendat()$y
        
        nx <- length(x)
        mx <- mean(x)
        sdx <- sd(x)
        ny <- length(y)
        my <- mean(y)
        sdy <- sd(y)
        
        if (input$varequal) {
            df <- nx+ny-2
            v <- ((nx-1)*sdx^2+(ny-1)*sdy^2)/df
            tstat <- round(abs(mx-my)/sqrt(v*(1/nx+1/ny)),3)
            diff <- round((mx - my), 3)
            diff.std <- sqrt(v * (1/nx + 1/ny))
            diff.lower <- round(diff + diff.std * qt(0.05/2, df),3)
            diff.upper <- round(diff + diff.std * qt(0.05/2, df, lower.tail = FALSE),3)
            options(scipen=100)
            P <- 2 * pt(-abs(tstat), df)
        } else {
            stderrx <- sqrt(sdx^2/nx)
            stderry <- sqrt(sdy^2/ny)
            stderr <- sqrt(stderrx^2 + stderry^2)
            df <- round(stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1)),3)
            tstat <- round(abs(mx - my)/stderr,3)
            diff <- round((mx - my), 3)
            cint <- qt(1 - 0.05/2, df)
            diff.lower <- round(((tstat - cint) * stderr),3)
            diff.upper <- round(((tstat + cint) * stderr),3)
            options(scipen=100)
            P <- 2 * pt(-abs(tstat), df)
        }
        
        s.within <- sqrt(((nx - 1) * sdx^2 + (ny - 1) * sdy^2)/(nx + ny - 2))
        d <- round((mx - my)/s.within,3)
        
        score <- rbind(x, y)
        group <- factor(c(rep("Group A", length(x)), rep("Group B", length(y))))
        
        xy.min <- min(c(x, y))
        xy.min <- xy.min - xy.min*0.1
        xy.max <- max(c(x, y))
        xy.max <- xy.max + xy.max*0.1
        
        plotMeans(score, group, error.bars="conf.int", xlim=c(0,4), ylim=c(xy.min, xy.max), main="Error bars show 95% CI.", xlab="", ylab="Mean")
        
        abline(h=mean(x), col = "blue", lty = 1, lwd=1)
        abline(h=mean(y), col = "red", lty = 1, lwd=1)
        
        text(1, xy.min, paste("n = ", length(x), sep=""), cex=0.8)
        text(2, xy.min, paste("n = ", length(y), sep=""), cex=0.8)
        
        text(x = 1.5, y = xy.max-xy.max*0.1, paste("p = ", P, sep=""))
        text(x = 1.5, y = xy.max-xy.max*0.15, paste("d = ", d, sep=""))
        
        text(x = 3, y = xy.max-xy.max*0.1, paste("Group A - Group B"))
        text(x = 3, y = xy.max-xy.max*0.15, paste(diff, "[", diff.lower,",", diff.upper,"]"))
        text(x = 3, y = mean(y), paste("0"))
        text(x = 3, y = xy.min, paste("Mean of difference"))
        
        # 95%CI
        segments(x0 = 2.9, y0 = mean(x) - (diff - diff.lower), x1 = 3.1, y1 = mean(x) - (diff - diff.lower), col = "red", lwd=5)
        segments(x0 = 3.0, y0 = mean(x) - (diff - diff.lower), x1 = 3.0, y1 = mean(x) + (diff.upper - diff), col = "red", lwd=5)
        segments(x0 = 2.9, y0 = mean(x) + (diff.upper - diff), x1 = 3.1, y1 = mean(x) + (diff.upper - diff), col = "red", lwd=5)
        points(3, mean(x), pch=20, col= "red", cex=2.5)

    })
    





    # Show the values using an HTML table
    output$values <- renderTable({
    sliderValues()
    })
    
    output$gendat.out <- renderPrint({
        gendat()
    })
    
    output$difference.out <- renderPrint({
        difference()
    })
    
    output$es.out <- renderPrint({
        es()
    })
    
    output$ttest.out <- renderPrint({
        ttest()
    })

    output$vartest.out <- renderPrint({
        vartest()
    })

})
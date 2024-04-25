#------------------------------------------------------------------------------
# File: analyze.R
# Author: Ray Griner
# Date: 30-Mar-2024
# Purpose: analyze.R was used to generate the statistical results put in the
#   README.md for v0.8.2. This cleans up the file a bit and reruns pointing
#   to the new C++ directory location for the files used as input. It
#   generates the same results.
# [RG20240331] - correct Wolter results to 45/1000000 instead of 14/1000000.
#   This doesn't change any conclusions.
# [RG20240415] - No longer need to process three separate files for
#   Up-Color-AnyRun rule.
# [RG20240417] - Add fill.from.nosplit function and use it to fill in some
#   results for Up-Color-HighRun that were unsolved.
#------------------------------------------------------------------------------
# Need Hmisc for binconf
library(Hmisc)

#-----------------------------------------------------------------------------
# Purpose: Report the win rate, 95% CI, and calculate statistical tests
#   for comparison with Masten and Wolter
# Parameters:
#     x: numerator of proportion
#     n: denominator of proportion
#     emptyrule: rule for how empty piles are handled so we know which
#         external proportion to compare to
#-----------------------------------------------------------------------------
check.prob <- function(x, n, emptyrule) {
    cat(paste0("Wins: ", x, "\n"))
    cat(paste0("Win rate and 95% CI:\n"))
    binconf.rc = binconf(x=x, n=n)
    print(binconf.rc)

    fisher_exact <- NULL
    if (is.null(emptyrule)) emptyrule="do not test"

    if (emptyrule == "high run") {
        masten_test = prop.test(x=c(x, 14498), n=c(n, 100000))
        cat("\nTest this and Masten proportions equal:\n")
        print(masten_test)
    }
    if (emptyrule == "any run") {
        masten_test = prop.test(x=c(x, 63635), n=c(n, 100000))
        cat("\nTest this and Masten proportions equal:\n")
        print(masten_test)
    }
    if (emptyrule == "none") {
        masten_test = prop.test(x=c(x, 900), n=c(n, 100000))
        cat("\nTest this and Masten proportions equal:\n")
        print(masten_test)
        wolter_test = prop.test(x=c(x, 45), n=c(n, 1000000))
        cat("\n\nTest this and Wolter proportions equal:\n")
        print(wolter_test)

        # yes, Barnard's is more powerful, etc..., but for now I
        # just want to use something in the stats library vs having
        # to validate something by an unknown author
        cat("\n\nFisher exact test in case above doesn't converge:\n")
        cont.table = matrix(data=c(x, 45, n-x, 1000000-45), nrow=2, ncol=2)
        fisher_exact = prop.test(x=cont.table)
        print(cont.table)
        print(fisher_exact)
        cat("\n")
    }
    return (list(binconf = binconf.rc, masten_test = masten_test,
        fisher_exact = fisher_exact))
}

#-----------------------------------------------------------------------------
# Purpose: Rarely, the NoSplit rules give a win when the Split rules do not
#   complete until memory is exhausted. But a win under NoSplit must be a win
#   under Split, so we update the results here.
#-----------------------------------------------------------------------------
fill.from.nosplit <- function(split.df, nosplit.df) {
    unsolved <- (split.df$rc == 3)
    solved <- (unsolved & nosplit.df$rc == 1)
    split.df[unsolved & solved, "rc"] = 1
    split.df[unsolved & solved, "max_score"] = 52
    #split.df[solved, c("max_score")] = 52
    if (any(solved)) {
        cat(paste0("\nNoSplit win used when Split was not solved for ",
            sum(solved, na.rm=TRUE), " game(s) (row_indices=",
            toString(which(solved)), ")\n"))
    }
    return (split.df)
}

fmt.pval <- function(p.value) {
    return (ifelse(p.value < .0001, "<.0001",
        ifelse(p.value<.001, sprintf("%6.4f", p.value),
            ifelse(p.value<.01, sprintf("%5.3f", p.value),
                sprintf("%4.2f", p.value)))))
}

analyze <- function(file = NULL, data = NULL, emptyrule = NULL, rule= NULL,
                    rule_footnote = NULL, filemax = NULL) {
    if (!is.null(file) && !file.exists(file)) {
        stop(paste("analyze: file does not exist: ", file))
    }

    cat("-----------------------------------------------")
    cat("------------------------------\n")
    if (!is.null(file)) cat(paste0("File: ", file, "\n"))
    if (!is.null(data)) cat(paste0("Data: ", file, "\n"))
    cat("-----------------------------------------------")
    cat("------------------------------\n\n")

    if ( (is.null(file) & is.null(data)) || (!is.null(file) & !is.null(data))) {
        stop("analyze: either file or data parameter must be specified")
    }

    if (is.null(data)) {
        df <- read.csv(file)
    } else {
        df <- data
    }

    n.total <- nrow(df)
    is.completed <- ( (df$rc == 1) | (df$rc == 2))
    n.completed <- sum( is.completed, na.rm=TRUE)

    # Print number of completed simulations
    cat(paste0("Observations (n): ", n.total, "\n"))
    comp.df <- df[(df$rc == 1) | (df$rc == 2),]
    cat(paste0("Simulation completed (n): ", n.completed, "\n\n"))

    #
    if (is.null(rule_footnote)) {
       marker=""
    } else {
       marker=paste0(" [", rule_footnote, "]")
    }

    to_output(paste0("| ", formatC(paste0("*",rule,"*", marker), width=-26), " "))
    to_output(paste0("| ", formatC(format(n.completed, big.mark=",",
                                   trim=TRUE), width=7),
                     "     "))


    # Print win rates, 95% CIs, and p-values
    to_output("| ")
    if (n.total == n.completed) {
        ret = check.prob(x=sum( (df$rc == 1), na.rm=TRUE),
                   n=n.completed, emptyrule=emptyrule)
        out_str = sprintf("%d (%2.1f%% [%2.1f%%, %2.1f%%])",
            sum( (df$rc == 1), na.rm=TRUE),
            100*ret$binconf[1, "PointEst"],
            100*ret$binconf[1, "Lower"],
            100*ret$binconf[1, "Upper"])

        to_output(paste0(formatC(out_str, width=-28), " "))
        to_output(paste0("| ",
            formatC(fmt.pval(ret$masten_test$p.value), width=-6), " "))
    }
    else {
        cat(paste0("\nWARNING: Some simulations stopped before completion.\n"))
        cat(paste0("\nAssuming all incomplete simlations are losses:\n"))
        ret.low <- check.prob(x=sum( (df$rc == 1), na.rm=TRUE),
                   n=n.completed, emptyrule=emptyrule)
        print(ret.low)
        cat(paste0("\nAssuming all incomplete simlations are wins:\n"))
        ret.high <- check.prob(x=sum( (df$rc == 1) | (df$rc == 3), na.rm=TRUE),
                   n=n.completed, emptyrule=emptyrule)
        print(ret.high)

        out_str = sprintf("%d (%2.1f%% [%2.1f%%, %2.1f%%], %2.1f%% [%2.1f%%, %2.1f%%])",
            sum( (df$rc == 1), na.rm=TRUE),
            100*ret.low$binconf[1, "PointEst"],
            100*ret.low$binconf[1, "Lower"],
            100*ret.low$binconf[1, "Upper"],
            100*ret.high$binconf[1, "PointEst"],
            100*ret.high$binconf[1, "Lower"],
            100*ret.high$binconf[1, "Upper"])
        to_output(paste0(formatC(out_str, width=-28), " "))
        to_output(paste0("| ",
            fmt.pval(ret.low$masten_test$p.value), ", ",
            fmt.pval(ret.high$masten_test$p.value), " "))
    }

    stchk = df$n_states_checked
    cat("\nStates checked:\n")
    cat(paste0("Mean (SD): ", mean(stchk), " (", sd(stchk), ")\n"))
    cat(paste0("Max: ", max(stchk), "\n"))
    # Print mean (sd) states examined
    out_str = sprintf("| %.1f (%.1f) ", mean(stchk)/1000, sd(stchk)/1000)
    to_output(formatC(out_str, width=-20))
    out_str = sprintf("| %.1f", max(stchk)/1000000)
    to_output(formatC(out_str, width=-10))

    if (!is.null(filemax)) {
        dfmax = read.csv(filemax)
        score = dfmax$max_score
        stmax = dfmax$n_states_checked
        diff.rc = (dfmax$rc != df$rc)
        if (any(diff.rc)) {
           cat(sprintf("WARNING: %s: %d record(s) with different return codes in file and filemax\n",
               rule, sum(diff.rc, na.rm=TRUE)), sep='')
        }
    }
    else {
        score = df$max_score
    }

    cat("\nScore:\n")
    hist(score, main=rule)
    cat(paste0("Mean (SD): ", mean(score), " (", sd(score), ")\n"))
    cat(paste0("Max: ", max(score), "\n"))
    out_str = sprintf("| %.1f (%.1f) ", mean(score), sd(score))
    to_output(formatC(out_str, width=-14))

    if (!is.null(filemax)) {
        cat("\nStates checked (when maximize_score=true):\n")
        cat(paste0("Mean (SD): ", mean(stmax), " (", sd(stmax), ")\n"))
        cat(paste0("Max: ", max(stmax), "\n"))
        out_str = sprintf("| %.1f (%.1f) ", mean(stmax)/1000, sd(stmax)/1000)
        to_output(formatC(out_str, width=-20))
        out_str = sprintf("| %.1f", max(stmax)/1000000)
        to_output(formatC(out_str, width=-10))
    }
    else {
        out_str = "| NA                | NA      "
        to_output(out_str)
    }

    to_output("|\n")
    #print(df)
}

output.file = "analysis_output.txt"

to_output <- function(str_to_print) {
    cat(str_to_print, file=output.file, append=TRUE)
}

print_table_header <- function() {
    to_output("Output Summary for Pasting in README.md:\n")
    to_output("(but if win rates, 95% CI, and P-value are the same when we\n")
    to_output("present lower/upper bounds, delete those manually, if not\n")
    to_output("the same, add footnote back to win rate header and update\n")
    to_output("other footnotes as needed):\n\n")
    to_output("| Rule Variant               | Completed Simulations, (n) |")
    to_output(" Wins, n (% [95% CI]) | P-value [a] |")
    to_output(" Mean (SD) States Examined (10^3) [b] |")
    to_output(" Maximum States Examined (10^6) [b] |")
    to_output(" Mean (SD) Best Possible Score |")
    to_output(" Mean (SD) States Examined (10^3) [c] |")
    to_output(" Maximum States Examined (10^6) [c] |\n")

    to_output("| :------------------------- | :---------: |")
    to_output(" :-------------------------: | :-----: | :---------------: |")
    to_output(" :-----: | :----: | :----: | :----: |\n")
}

cat("", file=output.file)
print_table_header()

# Directory for regular results
rdir = "../tests/results/"
# Directory for results using `maximize_score=true`
mdir = "../tests/results/"

#-------------------------------------------------------------------------------
# First do some manual work for the Up-Color-HighRun rules, which has a few
# cases that are unsolved.
#-------------------------------------------------------------------------------
df.up_color_highrun_nosplit = read.csv(paste0(rdir, "up_color_highrun_nosplit.out"))
df_most = read.csv(paste0(rdir, "up_color_highrun.out"))

df_fill = fill.from.nosplit(df_most, df.up_color_highrun_nosplit)

df.up_color_highrun_5912 = read.csv(paste0(rdir, "up_color_highrun_5912.out"))
df.up_color_highrun_9901 = read.csv(paste0(rdir, "up_color_highrun_9901.out"))

df.up_color_highrun = rbind(
    df_fill[df_fill$rep_id != 5912 & df_fill$rep_id != 9901, ],
    df.up_color_highrun_5912,
    df.up_color_highrun_9901)

analyze(file=paste0(rdir, "down_color_none.out"),          emptyrule="none",     rule="Down-Color-None",
        filemax=paste0(mdir, "down_color_none_max.out"))
analyze(file=paste0(rdir, "up_color_none.out"),            emptyrule="none",     rule="Up-Color-None",
        filemax=paste0(mdir, "up_color_none_max.out"))
analyze(file=paste0(rdir, "up_color_none_nosplit.out"),    emptyrule="none",     rule="Up-Color-None-NoSplit",
        filemax=paste0(mdir, "up_color_none_nosplit_max.out"))
analyze(data=df.up_color_highrun,                          emptyrule="high run", rule="Up-Color-HighRun", rule_footnote="b")
analyze(file=paste0(rdir, "up_color_highrun_nosplit.out"), emptyrule="high run", rule="Up-Color-HighRun-NoSplit")
analyze(file=paste0(rdir, "up_color_anyrun.out"),          emptyrule="any run",  rule="Up-Color-AnyRun")
analyze(file=paste0(rdir, "up_color_anyrun_nosplit.out"),  emptyrule="any run",  rule="Up-Color-AnyRun-NoSplit")
analyze(file=paste0(rdir, "dalton_none.out"),              emptyrule="none",     rule="Up-Suit-None",
        filemax=paste0(mdir, "dalton_none_max.out"))
analyze(file=paste0(rdir, "parlett.out"),                  emptyrule="none",     rule="Up-Suit-None-NoSplit",
        filemax=paste0(mdir, "parlett_max.out"))
analyze(file=paste0(rdir, "dalton_highrun.out"),           emptyrule="high run", rule="Up-Suit-HighRun")
analyze(file=paste0(rdir, "dalton_highrun_nosplit.out"),   emptyrule="high run", rule="Up-Suit-HighRun-NoSplit")
analyze(file=paste0(rdir, "dalton_anyrun.out"),            emptyrule="any run",  rule="Up-Suit-AnyRun")
analyze(file=paste0(rdir, "dalton_anyrun_nosplit.out"),    emptyrule="any run",  rule="Up-Suit-AnyRun-NoSplit")


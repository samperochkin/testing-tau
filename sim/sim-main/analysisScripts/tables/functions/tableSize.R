tableSize <- function(R, M, distribution){
  
  if(distribution == "normal") distribution_text <- "Normal distribution"
  if(distribution == "t4") distribution_text <- "$t_4$ distribution"
  if(distribution == "clayton") distribution_text <- "Clayton copula"
  if(distribution == "gumbel") distribution_text <- "Gumbel copula"
  
  env <- environment()
  i <- 0
  
  rowPart <- function(){
    env$i <- env$i + 1
    paste0(paste0(R[i,1:3], collapse = " & "),
           " && ",
           paste0(R[i,4:6], collapse = " & "),
           " && ", paste0(R[i,7:9], collapse = " & "))
  }
  
  
  cc <- cat("\\begin{table}[htbp]
 \\captionsetup{width=1\\linewidth,font=small,skip=0pt}
      \\caption{Estimated sizes (in \\%) for the tests of $H_0$ with $\\B = \\bs{1}_p$ performed at the nominal level 5\\%. Each entry is based on $",M,"$ samples of size $n$ in dimension $d$ drawn from a ", distribution_text, " with Kendall's tau matrix $\\bs{T}$ is as in Eq.~\\eqref{eq:T-equi-null}.}
      \\label{tab:sim-level-", distribution,"}
      \\begin{center}
      \\fontsize{8.75}{8.75}\\selectfont
      \\vskip-12pt
      \\begin{tabular}{*{2}{l}*{12}{r}}
      \\toprule
      \\multicolumn{14}{c}{$E_{np}$ with $\\SA = \\Sh_{np}$}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14} 
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      \\multirow{ 2}{*}{$\\Sh_{np}^{\\rm P}$} &  5 && ", rowPart(), " \\\\
      & 15 && ", rowPart(), " \\\\
      \\cmidrule(lr){2-14}
      \\multirow{ 2}{*}{$\\Sh_{np}^{\\rm J}$} &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), " \\\\
      \\midrule
      \\multicolumn{14}{c}{$M_{np}$ with $\\SA = \\Sh_{np}$}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14} 
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      \\multirow{2}{*}{$\\Sh_{np}^{\\rm P}$} 
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      \\multirow{2}{*}{$\\Sh_{np}^{\\rm J}$}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      \\midrule
      \\multicolumn{14}{c}{$E_{np}$ with $\\SA = (1/n)\\I_p$}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14}
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      &  5 && ", rowPart(), "\\\\
      $\\Sh_{np}^{\\rm P}$ & 15 && ", rowPart(), "\\\\
      & 25 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      $\\Sh_{np}^{\\rm J}$ & 25 && ", rowPart(), "\\\\
      & 50 && ", rowPart(), "\\\\
      & 100 && ", rowPart(), "\\\\
      \\midrule
      \\multicolumn{14}{c}{$M_{np}$ with $\\SA = (1/n)\\I_p$}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14}
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      &  5 && ", rowPart(), "\\\\
      $\\Sh_{np}^{\\rm P}$ & 15 && ", rowPart(), "\\\\
      & 25 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      $\\Sh_{np}^{\\rm J}$ & 25 && ", rowPart(), "\\\\
      & 50 && ", rowPart(), "\\\\
      & 100 && ", rowPart(), "\\\\
      \\bottomrule
      \\end{tabular}
      \\end{center}
      \\vskip-9pt
      \\small
      Statistics: $E_{np}$ Euclidean norm-based statistic defined in Eq. \\eqref{eq:euclidean}; $M_{np}$ supremum norm-based statistic defined in Eq. \\eqref{eq:supremum}. Estimators: $\\Sh_{np}^{\\rm P}$ plug-in estimator; $\\Sh_{np}^{\\rm J}$ jackknife estimator. *The results marked by an asterisk were computed on at least 2000 simulations; the simulations for which $\\Sh_{np}$ was not positive definite were discarded. Blank entries correspond to cases where $\\Sh_{np}$ was positive definite less than 5\\% of the times. In all other cases, $\\Sh_{np}$ was always positive definite.
      \\end{table}"
      )
}
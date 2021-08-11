tablePowerStar <- function(R, M, distribution, dtau, dtau_type){
  
  if(distribution == "normal") distribution_text <- "Normal distribution"
  if(distribution == "t4") distribution_text <- "$t_4$ distribution"
  if(distribution == "clayton") distribution_text <- "Clayton copula"
  if(distribution == "gumbel") distribution_text <- "Gumbel copula"
  
  dtau_id <- dtau*10
  
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
\\caption{Estimated rejection rates of tests of $H_0^*$ with $\\mathcal{G}=\\{\\{1,\\ldots,d\\}\\}$ performed at nominal level $5$\\%. Each entry is based on $",M,"$ $n \\times d$ datasets drawn from a ",distribution_text," with Kendall's tau matrix $\\bs{T}_{\\Delta}$ in Eq.~\\eqref{eq:departure} (i) with $\\Delta = ",dtau,"$; $\\bs{T}$ is as in Eq.~\\eqref{eq:T-equi-null}.}
      ",paste0("\\label{tab:sim-power-star-", dtau_type,"-", dtau_id,"-", distribution,"}"),"
      \\begin{center}
      \\fontsize{8.75}{8.75}\\selectfont
      \\vskip-12pt
      \\begin{tabular}{*{2}{l}*{12}{r}}
      \\toprule
      \\multicolumn{14}{c}{$E_{np}$ with $\\SA = \\Sh_{np}$ for ",paste0(dtau_type)," departures ($\\Delta = ",dtau,"$)}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14} 
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      &  5 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm P}$ & 15 && ", rowPart(), "\\\\
      & 25 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm J}$ & 25 && ", rowPart(), "\\\\
      & 50 && ", rowPart(), "\\\\
      & 100 && ", rowPart(), "\\\\
      \\midrule
      \\multicolumn{14}{c}{$M_{np}$ with $\\SA = \\Sh_{np}$ for ",dtau_type," departures ($\\Delta = ",dtau,"$)}\\\\
      \\midrule
      &  5 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm P}$ & 15 && ", rowPart(), "\\\\
      & 25 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm J}$ & 25 && ", rowPart(), "\\\\
      & 50 && ", rowPart(), "\\\\
      & 100 && ", rowPart(), "\\\\
      \\midrule
      \\multicolumn{14}{c}{$E_{np}$ with $\\SA = (1/n)\\I_p$ for ",paste0(dtau_type)," departures ($\\Delta = ",dtau,"$)}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14}
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      &  5 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm P}$ & 15 && ", rowPart(), "\\\\
      & 25 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm J}$ & 25 && ", rowPart(), "\\\\
      & 50 && ", rowPart(), "\\\\
      & 100 && ", rowPart(), "\\\\
      \\midrule
      \\multicolumn{14}{c}{$M_{np}$ with $\\SA = (1/n)\\I_p$ for ",paste0(dtau_type)," departures ($\\Delta = ",dtau,"$)}\\\\
      \\midrule
      \\multicolumn{2}{c}{} & & \\multicolumn{3}{c}{$\\tau = 0$} & & \\multicolumn{3}{c}{$\\tau = 0.3$} & & \\multicolumn{3}{c}{$\\tau = 0.6$}\\\\
      \\cmidrule(lr){4-6}  \\cmidrule(lr){8-10}  \\cmidrule(lr){12-14}
      $\\Sh_{np}$ & $d$\\big|$n$ & & 50 & 100 & 150 & & 50 & 100 & 150 & & 50 & 100 & 150 \\\\
      \\midrule
      &  5 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm P}$ & 15 && ", rowPart(), "\\\\
      & 25 && ", rowPart(), "\\\\
      \\cmidrule(lr){2-14}
      &  5 && ", rowPart(), "\\\\
      & 15 && ", rowPart(), "\\\\
      $\\Sb_{np}^{\\rm J}$ & 25 && ", rowPart(), "\\\\
      & 50 && ", rowPart(), "\\\\
      & 100 && ", rowPart(), "\\\\
      \\bottomrule
      \\end{tabular}
      \\end{center}
      \\vskip-9pt
      \\small
      Statistics: $E_{np}$ Euclidean norm-based statistic defined in Eq.~\\eqref{eq:euclidean}; $M_{np}$ supremum norm-based statistic defined in Eq.~\\eqref{eq:supremum}. Estimators: $\\Sb_{np}^{\\rm P}$ structured plug-in estimator; $\\Sb_{np}^{\\rm J}$ structured jackknife estimator.
      \\end{table}
            
            
            "
      )
}
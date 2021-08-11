tablePowerStarBlock <- function(R, M, distribution, dtau){
  
  if(distribution == "normal") distribution_text <- "Normal distribution"
  if(distribution == "t4") distribution_text <- "$t_4$ distribution"

  env <- environment()
  i <- 0
  
  rowPart <- function(){
    env$i <- env$i + 1
    paste0(paste0(R[i,1:6], collapse = " & "),
           " && ",
           paste0(R[i,7:12], collapse = " & "))
  }
  
  
  cat("\\begin{table}
\\caption{Estimated rejection rates of tests of $H_0^*$ with $\\mathcal{G}$ of \\eqref{eq:G-blocks}, performed at nominal level $5$\\%.
Each entry is based on $",M,"$ $n\\times d$ datasets from a ",distribution_text," with Kendall's tau matrix  $\\T_\\Delta$ as in Eq.~$(\\ref{eq:departure})$ (i) with $\\Delta = ",dtau,"$ and $\\T$ as in Eq.~$(\\ref{eq:T-block})$ with $c_{k\\ell} = 0.4 - (0.15)|k-\\ell|$ for all $k,\\ell \\in \\{1,\\dots,3\\}$.}
      ",paste0("\\label{tab:sim-power-star-blocks-",distribution,"}"),"
      \\begin{center}
      \\scriptsize
      \\vskip-9pt
      \\begin{tabular}{*{2}{l}*{3}{r}@{\\hspace{0.7cm}}*{3}{r}*{1}{c}*{3}{r}@{\\hspace{0.7cm}}*{3}{r}}
      \\toprule
      & &  \\multicolumn{6}{c}{$E_{np}$ with $\\SA = \\Sh_{np}$ for single dep.} & {\\hspace{0.2cm}} & \\multicolumn{6}{c}{$M_{np}$ with $\\SA = \\Sh_{np}$ for single dep.} \\\\
      \\cmidrule(lr){3-8}\\cmidrule(lr){10-15}
      &&  \\multicolumn{3}{c}{balanced} & \\multicolumn{3}{c}{unbalanced} & &  \\multicolumn{3}{c}{balanced} & \\multicolumn{3}{c}{unbalanced}\\\\
      \\cmidrule(lr){3-8}\\cmidrule(lr){10-15}
      $\\Sh_{np}$ & $d$\\big|$n$ & 50 & 150 & 250 & 50 & 150 & 250 && 50 & 150 & 250 & 50 & 150 & 250 \\\\
      \\midrule
      & 6 &", rowPart(), "\\\\ 
      $\\Sb_{np}^{\\rm P}$
      & 12 &", rowPart(), "\\\\ 
      &18 &", rowPart(), "\\\\
      \\cmidrule(lr){2-15}
      &6&", rowPart(), "\\\\ 
      $\\Sb_{np}^{\\rm J}$
      &12 &", rowPart(), "\\\\
      &18&", rowPart(), "\\\\ 
      \\midrule
      & &  \\multicolumn{6}{c}{$E_{np}$ with $\\SA = (1/n)\\I_{p}$ for single dep.} & {\\hspace{0.2cm}} & \\multicolumn{6}{c}{$M_{np}$ with $\\SA = (1/n)\\I_{p}$ for single dep.} \\\\
      \\cmidrule(lr){3-8}\\cmidrule(lr){10-15}
      &&  \\multicolumn{3}{c}{balanced} & \\multicolumn{3}{c}{unbalanced} & &  \\multicolumn{3}{c}{balanced} & \\multicolumn{3}{c}{unbalanced}\\\\
      \\cmidrule(lr){3-8}\\cmidrule(lr){10-15}
      $\\Sh_{np}$ & $d$\\big|$n$ & 50 & 150 & 250 & 50 & 150 & 250 && 50 & 150 & 250 & 50 & 150 & 250 \\\\
      \\midrule
      & 6 &", rowPart(), "\\\\ 
      $\\Sb_{np}^{\\rm P}$
      & 12 &", rowPart(), "\\\\ 
      &18 &", rowPart(), "\\\\
      \\cmidrule(lr){2-15}
      &6&", rowPart(), "\\\\ 
      $\\Sb_{np}^{\\rm J}$
      &12 &", rowPart(), "\\\\
      &18&", rowPart(), "\\\\ 
      \\bottomrule
      \\end{tabular}
      \\end{center}
      \\vskip-9pt
      \\small
      Statistics: $E_{np}$ Euclidean norm-based statistic defined in Eq. \\eqref{eq:euclidean}; $M_{np}$ supremum norm-based statistic defined in Eq. \\eqref{eq:supremum}. Estimators: $\\Sb_{np}^{\\rm P}$ structured plug-in estimator; $\\Sb_{np}^{\\rm J}$ structured jackknife estimator.
      \\end{table}
      
      
      ")
}
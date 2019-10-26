breakProblems <- function(dend, vec.address, alpha){
  
  for(v in rev(vec.address)){
    
    # ss <- 1
    # v <- vec.address[[ss]]
    
    #### GET NODE
    if(length(v) == 1){
      node <- dend
    }else{
      node <- dend[[v[-1]]]
    }

        
  #### IF LEAF NOTHING TO DO
  if(attr(node,"members") <= 2) next
  
    #### OTHERWISE, INVESTIGATE THEIR OUTER PVALUES
    pvals <- get_root_branches_attr(node,"Opval")
    if(is.list(pvals)){
      pvals[sapply(pvals,is.null)] <- list(NA)
      pvals <- unlist(pvals)
    } 
    homo.outer <- pvals > alpha
    
    #### IF ANY HETEROGENOUR OUTER STRUCTURE
    #### BREAK IT AND LABEL THE CURRENT STRUCTURE type = 0
    #### ELSE LABEL THE CURRENT STRUCTURE type = 1
    if(any(!homo.outer, na.rm=T)){
      
      for(j in rev(which(!homo.outer))){
        node <- unbranch(node,j)
      }
      attr(node,"type") <- 0
    } 
      

    #### PLACE OBTAINED NODE INTO DEND
    if(length(v) == 1){
      dend <- node
    }else{
      dend[[v[-1]]] <- node
    }
    
    # ss <- ss + 1
  }
  dend
}

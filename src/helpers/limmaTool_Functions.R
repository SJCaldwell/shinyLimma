setClass("Gene",
         slots = list(symbol = "character", logFC = "numeric", pVal = "numeric", Adj_pVal = "numeric", Homologs = "character"))

setClass("Experiment",
         slots = list(Gene = "list", numGenes = "numeric", name = "character"))

library(ggplot2)
#library(gtools)
#' Append Gene object to experiment List
#'
#' Mostly works under the hood to append Gene objects to Experiment objects. 
#' @param Experiment and Gene2Add (a Gene object) No defaults.
#' @keywords limma, logFC, comparison, Gene
#' @export Returns changed experiment Object. 
#' @examples
#' Experiment.appendGene()
Experiment.appendGene = function(Experiment, Gene2Add){
  if(class(Experiment)[1] != "Experiment") stop("Experiment must be of experiment type")
  if(class(Gene2Add)[1] != "Gene") stop("Gene argument must be of type Gene")
  
  Experiment@Gene[length(Experiment@Gene) + 1] = Gene2Add
  
  Experiment@numGenes = Experiment@numGenes +1
  
  return(Experiment)
  
}


#' A Check for experiment LogFC
#'
#' Checks if gene is differentially expressed more in Experiment object A than B. Returns false if it's equal to or less than.
#' @param exp_a and exp_b stands for two different Limma Experiment objects. No defaults.
#' @keywords limma, logFC, comparison
#' @export
#' @examples
#' GeneisGreaterThanLogFC()
Gene.isGreaterLogFC = function(symbol,exp_A,exp_B){
  #Type checking
  if(class(symbol) != "character") stop("Symbol must be a character!")
  
  if(!(class(exp_A)[1] == "Experiment" && class(exp_B)[1] == "Experiment")) stop ("exp_a and exp_b must be of type Experiment!")
  #See if you can find the symbol, regardless of case
  
  for(i in 1:length(exp_A@Gene)){
    if (exp_A@Gene[[i]]@symbol == symbol){
      exp_A_Gene = exp_A@Gene[[i]]
      break
    }
  }
  
  if(is.null(exp_A_Gene)){
    stop("Symbol is not contained in Experiment A!")
  }
  
  for(i in 1:length(exp_B@Gene)){
    if (exp_B@Gene[[i]]@symbol == symbol){
      exp_B_Gene = exp_B@Gene[[i]]
      break
    }
  }
  
  if(is.null(exp_B_Gene)){
    stop("Symbol is not contained in Experiment B!")
  }
  
  if(exp_A_Gene@logFC > exp_B_Gene@logFC){
    return (TRUE)
  }else{
    return (FALSE)
  }
  
}



#' Reads Limma results into an Experiment list of Gene Objects
#'
#' This function reads in a table of a specified format and turns it into an Experiment object.
#' @param filepath gives direction to Limma output table, name is the name of your Limma Experiment.
#' @keywords limma, read_table, comparison
#' @export
#' @examples
#' turnTableIntoExperiment()
turnTableIntoExperiment = function(filepath, name){
  raw_table = read.table(filepath, sep = "\t", header = TRUE)
  
  Experiment = new("Experiment", numGenes = 0, name = name)
  
  for (i in 1:(nrow(raw_table))){
    newGene = new("Gene", symbol = as.character(raw_table$Symbol[i]), logFC = as.numeric(raw_table$logFC[i]), pVal = as.numeric(raw_table$p_Val[i]))
    if(hasHomolog(newGene@symbol)){
      newGene@Homologs = getHomolog(newGene@symbol)
    }
    Experiment = Experiment.appendGene(Experiment, newGene)
  }
  return (Experiment)
}



#' Finds gene with a given symbol in an Experiment Package
#'
#' This function reads through the Experiment and returns the Gene object of interest 
#' @param symbol, Exp 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.getGene(symbol, Exp)
Experiment.getGene = function(symbol, Exp){
  
  for(i in 1:length(Exp@Gene)){
    if (tolower(Exp@Gene[[i]]@symbol) == tolower(symbol)){
      GeneOfInterest = Exp@Gene[[i]]
      return(GeneOfInterest)
    }
  }
  stop("Symbol is not contained in experiment. Returns nothing.")
}

#' Checks if a gene was significant in an experiment at a desired level.
#'
#' This function reads through the Experiment and returns True if the Gene was at a p-value equal or lower to the p-value argument.
#' @param symbol, Exp, p = 0.05 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.isSignificant(symbol, Exp, p = .05)
Experiment.IsSignificant = function(symbol, Exp, p = .05){
  
  for(i in 1:length(Exp@Gene)){
    if ((tolower(Exp@Gene[[i]]@symbol)) == tolower(symbol)){
      GeneOfInterest = Exp@Gene[[i]]
      if (GeneOfInterest@pVal <= p){
        return (TRUE)
      }else{
        return (FALSE)
      }
    }
  }
  stop("Symbol is not contained in experiment. Returns nothing.")
  
}


#' Checks if a gene was significant in an experiment at a desired level.
#'
#' This function reads through the Experiment and returns True if the Gene was at a p-value equal or lower to the p-value argument.
#' @param symbol, Exp, p = 0.05 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.getSigGenes(Exp, p = .05)
Experiment.getSigGenes = function(Exp, p = .05){
  significantGenes = list()
  for(i in 1:length(Exp@Gene)){
    if (Exp@Gene[[i]]@pVal <= p){
      significantGenes[length(significantGenes) + 1] = Exp@Gene[[i]]
    }
  }
  return (significantGenes)
  
}

#' Checks if a gene was significant in an experiment at a desired level.
#'
#' This function reads through the Experiment and returns a list of significant genes at enter p-value.
#' @param symbol, Exp, p = 0.05 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.getSigGenes(Exp, p = .05)
Experiment.getSigGenes = function(Exp, p = .05){
  significantGenes = list()
  for(i in 1:length(Exp@Gene)){
    if (Exp@Gene[[i]]@pVal <= p){
      significantGenes[length(significantGenes) + 1] = Exp@Gene[[i]]
    }
  }
  return (significantGenes)
  
}


#' Checks if a gene was significant in an experiment at a desired level of FDR.
#'
#' This function reads through the Experiment and returns a list of significant genes at enter p-value.
#' @param symbol, Exp, p = 0.05 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.getSigGenes(Exp, p = .05)
Experiment.getFdrSigGenes = function(Exp, p = .05){
  significantGenes = list()
  for(i in 1:length(Exp@Gene)){
    if (Exp@Gene[[i]]@Adj_pVal <= p){
      significantGenes[length(significantGenes) + 1] = Exp@Gene[[i]]
    }
  }
  return (significantGenes)
}


#' Checks if a gene was significantly downregulated in two experiments.
#'
#' This function reads through the Experiment and returns a list of significant genes at enter p-value.
#' @param symbol, Exp, p = 0.05 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.getSigAndSameDirection(Exp_A, Exp_B, p = .05)
Experiment.getSigAndSameDirectionPositive = function(Exp_A, Exp_B, p = .05){
  if(!(class(Exp_A)[1] == "Experiment" && class(Exp_B)[1] == "Experiment")) stop ("Exp_A and Exp_B must be of type Experiment!")
  positive = list()
  genes_in_A = Experiment.getSigGenes(Exp_A)
  genes_in_B = Experiment.getSigGenes(Exp_B)
  for(i in 1:length(genes_in_A)){
    for(j in 1:length(genes_in_B)){
      
      if(genes_in_A[[i]]@symbol == genes_in_B[[j]]@symbol){
          if(IsSameSign(genes_in_A[[i]]@logFC, genes_in_B[[j]]@logFC) && genes_in_A[[i]]@logFC >= 0){
            positive[length(positive) + 1] = genes_in_A[[i]]
          }
      }
    }
  }
  return (positive)
}

#' Checks if a gene was significantly upregulated in an experiment at a desired level of FDR.
#'
#' This function reads through the Experiment and returns a list of significant genes at enter p-value.
#' @param symbol, Exp, p = 0.05 
#' @keywords limma, fetch, comparison, gene
#' @export
#' @examples
#' Experiment.getSigAndSameDirection(Exp_A, Exp_B, p = .05)
Experiment.getSigAndSameDirectionNegative = function(Exp_A, Exp_B, p = .05){
  if(!(class(Exp_A)[1] == "Experiment" && class(Exp_B)[1] == "Experiment")) stop ("Exp_A and Exp_B must be of type Experiment!")
  negative = list()
  genes_in_A = Experiment.getSigGenes(Exp_A)
  genes_in_B = Experiment.getSigGenes(Exp_B)
  for(i in 1:length(genes_in_A)){
    for(j in 1:length(genes_in_B)){
      if(genes_in_A[[i]]@symbol == genes_in_B[[j]]@symbol){
        if(IsSameSign(genes_in_A[[i]]@logFC, genes_in_B[[j]]@logFC) && genes_in_A[[i]]@logFC < 0){
          negative[length(negative) + 1] = genes_in_A[[i]]
        }
      }else{
        if ((length(genes_in_A[[i]]@Homologs) > 0) & (genes_in_A[[i]]@symbol %in% genes_in_B[[j]]@Homologs)){
          if(IsSameSign(genes_in_A[[i]]@logFC, genes_in_B[[j]]@logFC & genes_in_A[[i]]@logFC < 0)){
            negative[length(negative) +1] = genes_in_A[[i]]
            cat("\nFOUND A HOMOLOG W DIFFERENT NAME\n")
          }
        
        }
      }
    }
  }
  return (negative)
}


IsSameSign = function(a, b){
  return((a * b) >= 0)
  
}

Gene.ListToTable = function(geneList){
  if (!class(geneList) == "list") stop ("Argument must be a list")
  
  if(!class(geneList[[1]])[1] == "Gene") stop ("Argument must be a list of Genes!")
  
  geneMatrix <- matrix(nrow = length(geneList), ncol = 2)
  colnames(geneMatrix) <- c("Symbol", "Fold_Change")
  
  for(i in 1:length(geneList)){
    gene <- geneList[[i]]
    geneMatrix[i,1] <- gene@symbol
    geneMatrix[i,2] <- as.numeric(gene@logFC) 
    
  }
  return (geneMatrix)
  
}


#' Plots logFC, either from two gene objects or two gene objects sharing the same Experiment.
#'
#' If gene1 and gene2 are gene objects, their logFcs will be plotted against each other.
#' If not, gene1 and gene2 must be characters, and the Experiment.getGene will be called internally to get the gene objects and plot them.
#' @param gene1, gene2, Experiment 
#' @keywords limma, fetch, comparison, gene, plot, logfc
#' @export
#' @examples
#' plotLogFC(gene1, gene2, Experiment) 
plotLogFC <- function(gene1, gene2, Experiment){
  if((class(gene1)[1] == "Gene") && class(gene2)[1] == "Gene"){

twovals <- c(gene1@logFC, gene2@logFC)
names(twovals) <- c(gene1@symbol, gene2@symbol)
twovals <- as.data.frame(twovals)
colnames(twovals) <- "logFC"
twovals <- cbind(twovals, rownames(twovals))
colnames(twovals) <- c("logFC", "symbol")

d <- ggplot(data= twovals, aes(x = symbol, y = logFC, colour = symbol))
d <- d + geom_bar(stat = "identity", width = .5)
d
return (d)
  }else{
  
    if(class(Experiment)[1] == "Experiment"){
      cat("AT ELSE")
      
      char1ToGene = Experiment.getGene(gene1, Experiment)
      char2ToGene = Experiment.getGene(gene2, Experiment)
      plotLogFC(char1ToGene, char2ToGene)
    }
}
}

plotFC <- function(gene1, gene2, Experiment){
  if((class(gene1)[1] == "Gene") && class(gene2)[1] == "Gene"){
    
    twovals <- c(logratio2foldchange(gene1@logFC), logratio2foldchange(gene2@logFC))
    names(twovals) <- c(gene1@symbol, gene2@symbol)
    twovals <- as.data.frame(twovals)
    colnames(twovals) <- "Fold Change"
    twovals <- cbind(twovals, rownames(twovals))
    colnames(twovals) <- c("Fold Change", "symbol")
    
    d <- ggplot(data= twovals, aes(x = symbol, y = logFC, colour = symbol))
    d <- d + geom_bar(stat = "identity", width = .5)
    d
    return (d)
  }else{
    
    if(class(Experiment)[1] == "Experiment"){
      cat("AT ELSE")
      
      char1ToGene = Experiment.getGene(gene1, Experiment)
      char2ToGene = Experiment.getGene(gene2, Experiment)
      plotLogFC(char1ToGene, char2ToGene)
    }
  }
}

isHomolog <- function(gene1, gene2){
  gene1 = tolower(gene1)
  gene2 = tolower(gene2)
  human_mouse_ortho$Symbol = tolower(human_mouse_ortho$Symbol)
  if(!(gene1 %in% human_mouse_ortho$Symbol & gene2 %in% human_mouse_ortho$Symbol)){
    return (FALSE)
  }else{
    #Both gene names are included
    gene1_info = human_mouse_ortho[human_mouse_ortho$Symbol == gene1,]
    gene2_info = human_mouse_ortho[human_mouse_ortho$Symbol == gene2,]
    if (gene1_info$HomoloGene.ID == gene2_info$HomoloGene.ID){
      return (TRUE)
    }else{
      return (FALSE)
    }
  }
  
}

hasHomolog = function(gene){
  gene <- tolower(gene)
  human_mouse_ortho$Symbol = tolower(human_mouse_ortho$Symbol)
  if(gene %in% human_mouse_ortho$Symbol){
    HomoID <- human_mouse_ortho[human_mouse_ortho$Symbol == gene,]$HomoloGene.ID
    if(length(human_mouse_ortho[human_mouse_ortho$HomoloGene.ID == HomoID,])>1){
      return (TRUE)
    }
  }
  return(FALSE)
}

getHomolog = function(gene){
  gene = tolower(gene)
  human_mouse_ortho$Symbol = tolower(human_mouse_ortho$Symbol)
  HomologList = list()
  ID = human_mouse_ortho[human_mouse_ortho$Symbol == gene,]$HomoloGene.ID
  Homos = human_mouse_ortho[human_mouse_ortho$HomoloGene.ID == ID,]
  return (Homos$Symbol) 
}


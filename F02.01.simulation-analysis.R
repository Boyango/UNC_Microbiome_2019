source("Script/C01.02.simulation.setup.R")
# rounding numbers
if (FALSE) {result.stat %<>% mutate(LB.nonz = round(LB.nonz, 3), LB.zero = round(LB.zero, 3),
                        LB.glob = round(LB.glob, 3),
                        LN = round(LN, 3), KW = round(KW, 3),
                        MAST.nonz = round(MAST.nonz, 3), MAST.zero = round(MAST.zero, 3),
                        MAST.glob = round(MAST.glob, 3),
                        Wg.nonz = round(Wg.nonz, 3), Wg.zero = round(Wg.zero, 3),
                        Wg.glob = round(Wg.glob, 3))}

test.name = data.frame(abbr = c("LB.nonz", "LB.zero", "LB.glob", "LN", "KW",
                                "MAST.nonz", "MAST.zero", "MAST.glob",
                                "Wg.nonz", "Wg.zero", "Wg.glob", "(Reserved)"),
                       full = c("Logistic Beta - nonzero", "Logistic Beta - zero", "Logistic Beta - global", 
                                "Log-Normal", "Kruskal-Wallis",
                                "MAST - nonzero", "MAST - zero", "MAST - global",
                                "Wagner - nonzero", "Wagner - zero", "Wagner - global", "(Reserved)"))

pval.plot <- function(result.stat, parameter_in_use = parameter1,
                      i = 1, test = "LB.glob", k.index=1:dim(parameter_in_use)[1], 
                      j.index = 1:dim(kappa)[1], ylim=0:1, title = NULL
                      ) {
  
  test.full = test.name[test == test.name[,1],2]  # full name
  delta.description = delta[i,]
  param.k = apply(parameter_in_use[k.index,-1], 1, function(x) paste0("(", paste(x, collapse=", "), ")"))
  
  if (i == (dim(delta)[1]+1)) { # legend page
    #table <- parameter1
    #p <- tableGrob(table, theme = ttheme_default(base_size=4))
    
    grid:::textGrob(paste0("Test = ", test.full, "\n",
                      ##   "\nColor = batch effect (red: large, blue: small, green: no)",
                           "\nColor = batch effect (red: no, brown: small_1, green: large_1, 
                                                    blue:small_2 , purple:large_2)",
                            "\n\nParam = 1~8 (pi=30%), 9~16 (pi=50%), 17~24 (pi=60%)",
                            "\n          25~32 & 41~43 (pi=90%),  33~40 & 44~46 (pi=95%)",
                      ##      "\n        9n + 1~3 (theta=1), 4~6 (theta=3), 7~9 (theta=10)",
                      ##      "\n        3n + 1 (mu=0.5), 2 (mu=1), 3(mu=2)",
                      ##      "\n  e.g. setting 22 = pi 90%, theta 3, mu 0.5.",
                            "\n\nEffect = mu(exp(+/-2)), theta (exp(+/-0.5), pi (expit +/- 0.5)"
                            ), just = "left", x = unit(0.1, "npc"), y = unit(0.5, "npc"),
                            gp=grid:::gpar(fontsize=10, col="black")) -> p
    
  } else {
    result.stat %>% 
      dplyr::filter_(.dots=paste0("i==",i)) %>%
      dplyr::filter(k %in% k.index) %>%
      dplyr::filter(j %in% j.index) %>%
      mutate_(.dots=setNames(list(as.name(test)),"p.value")) %>%
      ggplot(aes(factor(k), p.value, col=batch, group=batch, fill = batch)) +
      # geom_point() + geom_line() +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_hline(yintercept=0.05, col="black", linetype = 2) + ylim(ylim) + 
      # theme(legend.position="bottom") + 
      theme(legend.position = "none", axis.text.x = element_text(angle=90)) +
      scale_x_discrete(labels=param.k) +
      xlab(expression("Parameter settings (" * mu ~ ", " * theta * ", " * pi * ")")) +
    
      # ggtitle(paste0("Effect in: ",gsub("Effect\\_","",delta.description$detail),
      #                "\nTest: ", test.full)) -> p
      ggtitle(
        if (is.null(title)) {
          paste0("Effect in: ",gsub("Effect\\_","",delta.description$detail))
        } else {
          title
        } ) -> p
      
  }
  p
}


if (FALSE) {
  pval.plot(result.stat, i=1, test="Wg.glob", k.index= c(2,3,5,6,11,12,14,15))
  pval.plot(result.stat, i=1, test="Wg.glob", k.index= c(2,3,5,6,11,12,14,15), j.index=c(1,3), title="Wagner test")
  pval.plot(result.stat, i=11, test="LN")
  ggsave()
  
}

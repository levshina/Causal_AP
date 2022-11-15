library(pcalg)
library(glmmTMB)
library(lme4)

pilot <- read.table("Data/pilot.txt", header = T, sep = "\t", quote = "", stringsAsFactors = T)
summary(pilot)


data_caus <- pilot[, c("Genus", "Macroarea_origin", "Total_users", "L2Prop", "SO_Form", "SO_entropy", "Verb")]

random_df <- data_caus[, 1:2]
fixed_df <- data_caus[3:7]
fixed_df$Total_users <- log(fixed_df$Total_users)

#in order to use the beta distribution:
fixed_df$L2Prop[fixed_df$L2Prop == 0] <- 0.001
fixed_df$L2Prop[fixed_df$L2Prop == 1] <- 0.999

fixed_df$SO_entropy[fixed_df$SO_entropy == 0] <- 0.001
fixed_df$SO_entropy[fixed_df$SO_entropy == 1] <- 0.999

Genus <- data_caus$Genus
Area <- data_caus$Macroarea_origin

#to create suffStat$C as a correlation matrix (it doesn't really play a role):
fixed_df_numeric <- data.frame(Total_users = fixed_df$Total_users, 
                                 L2Prop = fixed_df$L2Prop,
                                 SO_Form = as.numeric(fixed_df$SO_Form) - 1,
                                 SO_entropy = fixed_df$SO_entropy,
                                Verb = as.numeric(fixed_df$Verb))
suffStat <- list(C = cor(fixed_df_numeric), n = nrow(fixed_df)) 

regressionTest <- function(x_ind, y_ind, S_inds, suffStat, cor_method = "spearman"){
x_name <- colnames(fixed_df)[x_ind]
y_name <- colnames(fixed_df)[y_ind]
print (x_name)
print(y_name)
print(S_inds)

if (length(S_inds) > 0){ 
S_names <- colnames(fixed_df)[S_inds]
formula_XS <- as.formula(paste(x_name, "~ 1 + ", paste(paste(S_names, "+ ",collapse=""),  "(1|Genus) + (1|Area)")))
print(formula_XS)
formula_YS <- as.formula(paste(y_name, "~ 1 +",  paste(paste(S_names, "+ ",collapse=""),  "(1|Genus) + (1|Area)")))
print(formula_YS)
}

else {
  formula_XS <- as.formula(paste(x_name, "~ 1 + (1|Genus) + (1|Area)"))
  formula_YS <- as.formula(paste(y_name, "~ 1 + (1|Genus) + (1|Area)"))
}
if (is.numeric(fixed_df[, x_ind])){

 if (all(fixed_df[, x_ind]) > 0 & all(fixed_df[, x_ind]) < 1)
{fitXS <- glmmTMB(formula_XS, data = fixed_df, family = beta_family)}  

  else {fitXS <- lmer(formula_XS, data = fixed_df, "REML" = TRUE)}
}

else{
fitXS <- glmer(formula_XS, data = fixed_df, family = binomial)
}

sink(file = "regression_logfile.txt",
     append = TRUE, type = "output")
print(summary(fitXS))
sink()

if (is.numeric(fixed_df[, y_ind])){
  if (all(fixed_df[, y_ind]) > 0 & all(fixed_df[, y_ind]) < 1)
  {fitYS <- glmmTMB(formula_YS, data = fixed_df, family = beta_family)}   

  fitYS <- lmer(formula_YS, data = fixed_df, "REML" = TRUE)
}

else{
fitYS <- glmer(formula_YS, data = fixed_df, family = binomial)
}

sink(file = "regression_logfile.txt",
     append = TRUE, type = "output")
print(summary(fitYS))
sink()

residualsXS <- residuals(fitXS)
residualsYS <- residuals(fitYS)
pval <- cor.test(residualsXS, residualsYS, method = cor_method)$p.value  
return (pval)  
}

fci_test <- fci(suffStat, indepTest = regressionTest,
                skel.method = "stable", labels = colnames(fixed_df), 
                type = "normal", alpha = 0.1, verbose = TRUE)
plot(fci_test)

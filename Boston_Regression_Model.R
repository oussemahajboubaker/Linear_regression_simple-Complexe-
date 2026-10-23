# =====================================================================
# CORRECTION COMPLETE - Exercice de regression lineaire (jeu Boston)
# Lien avec vos notes : Part 1 (theorie) et Part 2 (code R)
# =====================================================================

library(MASS)   # jeu de donnees Boston + stepAIC
library(car)    # vif, ncvTest

data(Boston)
Boston$black <- NULL   # variable retiree (probleme ethique)

# ---------------------------------------------------------------------
# PARTIE 1. EXPLORATION (B.2 - B.5)
# ---------------------------------------------------------------------

# Q1. Structure des donnees
str(Boston)
summary(Boston)
dim(Boston)            # 506 observations, 13 variables (apres retrait de black)

# Q2. Normalite de medv
hist(Boston$medv, main = "Valeur mediane des logements (medv)", xlab = "medv (k$)")
shapiro.test(Boston$medv)
# p-value tres petite -> on rejette H0 : medv n'est pas normale
# (distribution asymetrique, plafonnee a 50).
# Rappel : l'hypothese du modele concerne les ERREURS, pas directement y.

# Q3. Variable binaire chas -> facteur
table(Boston$chas)
Boston$chas <- factor(Boston$chas, labels = c("non", "oui"))
table(Boston$chas)

# Q4. Test t pour deux groupes (version de Welch par defaut)
t.test(medv ~ chas, data = Boston)
# Les quartiers au bord de la riviere ont un medv moyen plus eleve.

# Q5. Correlations entre variables numeriques
vars <- c("medv", "rm", "lstat", "nox", "tax", "dis")
round(cor(Boston[, vars]), 3)
pairs(Boston[, vars], gap = 1/2, main = "Boston")
# medv est fortement liee a lstat (negativement, ~ -0.74) et a rm (positivement, ~ 0.70).
# Les predicteurs sont aussi correles entre eux (nox/dis, nox/tax...) :
# risque de multicolinearite (voir partie 6).

# ---------------------------------------------------------------------
# PARTIE 2. REGRESSION SIMPLE A LA MAIN (sections 2 a 6 de la Part 1)
# Modele : medv ~ lstat
# ---------------------------------------------------------------------
x <- Boston$lstat
y <- Boston$medv
n <- length(y)

# Q6. Etapes de la MCO
xbar <- mean(x)
ybar <- mean(y)
beta1 <- sum((x - xbar) * (y - ybar)) / sum((x - xbar)^2)   # pente
beta0 <- ybar - beta1 * xbar                                # intercept
c(beta0 = beta0, beta1 = beta1)

# Q7. Verification avec lm()
fit1 <- lm(medv ~ lstat, data = Boston)
coef(fit1)
all.equal(unname(coef(fit1)), c(beta0, beta1))   # TRUE

# Q8. Residus et metriques d'erreur
yhat <- beta0 + beta1 * x
e    <- y - yhat
RSS  <- sum(e^2)
MSE  <- RSS / n
RMSE <- sqrt(MSE)
MAE  <- mean(abs(e))
c(RSS = RSS, MSE = MSE, RMSE = RMSE, MAE = MAE)
# Le RMSE est exprime dans l'unite de y (k$) : erreur typique de prediction
# d'environ 6 k$ ( = 6 000 $ ).

# Q9. Graphique + prediction pour lstat = 10
plot(x, y, pch = 19, col = "grey40", xlab = "lstat (%)", ylab = "medv (k$)",
     main = "medv ~ lstat")
abline(beta0, beta1, col = "red", lwd = 2)
segments(x, y, x, yhat, col = "blue", lty = 3)   # residus
beta0 + beta1 * 10
predict(fit1, newdata = data.frame(lstat = 10))  # meme resultat

# Q10. Linearite ?
plot(fitted(fit1), residuals(fit1), pch = 19, col = "grey40",
     xlab = "Valeurs ajustees", ylab = "Residus", main = "Residus vs ajustees")
abline(h = 0, col = "red", lty = 2)
lines(lowess(fitted(fit1), residuals(fit1)), col = "blue", lwd = 2)
# La courbe en U montre que la relation n'est PAS lineaire :
# l'hypothese de linearite / esperance nulle des erreurs est violee.
# On peut ameliorer avec lstat^2 ou log(lstat) (voir Q31).

# ---------------------------------------------------------------------
# PARTIE 3. CALCUL MATRICIEL (partie A de la Part 2)
# Modele : medv ~ rm + lstat + ptratio
# ---------------------------------------------------------------------

# Q11. Matrice de conception avec colonne de 1
X3     <- as.matrix(Boston[, c("rm", "lstat", "ptratio")])
Xtilde <- cbind(1, X3)
colnames(Xtilde)[1] <- "(Intercept)"
y      <- Boston$medv
n      <- nrow(Xtilde)
p      <- ncol(X3)

# Q12. Estimateur MCO : beta = (X'X)^(-1) X'y
betaest <- drop(solve(crossprod(Xtilde), crossprod(Xtilde, y)))
betaest

# Q13. Variance de l'erreur et erreurs standard
sigma2est  <- drop(crossprod(y - Xtilde %*% betaest) / (n - (p + 1)))
varbetaest <- diag(sigma2est * solve(crossprod(Xtilde)))
sebetaest  <- sqrt(varbetaest)
cbind(Estimate = betaest, Std.Error = sebetaest)

# Q14. Comparaison avec lm()
fit3 <- lm(medv ~ rm + lstat + ptratio, data = Boston)
summary(fit3)$coefficients
all.equal(unname(coef(fit3)), unname(betaest))                                  # TRUE
all.equal(unname(summary(fit3)$coefficients[, 2]), unname(sebetaest))           # TRUE
all.equal(summary(fit3)$sigma^2, sigma2est)                                     # TRUE

# Q15. Matrice chapeau H = X (X'X)^(-1) X'
H <- Xtilde %*% solve(crossprod(Xtilde)) %*% t(Xtilde)
sum(diag(H))                       # = p + 1 = 4
all.equal(unname(drop(H %*% y)), unname(fitted(fit3)))   # H y = valeurs ajustees
lev <- diag(H)                     # leviers h_ii
which.max(lev)                     # observation de plus grand levier (la 366)
max(lev)                           # environ 0.063
all.equal(unname(lev), unname(hatvalues(fit3)))  # TRUE

# ---------------------------------------------------------------------
# PARTIE 4. REGRESSION MULTIPLE COMPLETE (B.7)
# ---------------------------------------------------------------------

# Q16. Modele avec toutes les variables
full <- lm(medv ~ ., data = Boston)
summary(full)
# Toutes les variables sont significatives a 5 % sauf indus et age.
# R2 = 0.734, sigma = 4.798 sur 493 ddl.
# Interpretation de rm : a autres variables fixees, une piece de plus en moyenne
# augmente medv predit d'environ 3.7 k$.

# Q17. TSS, RSS, R2 a la main
y   <- Boston$medv
TSS <- sum((y - mean(y))^2)
RSS <- sum(residuals(full)^2)
R2  <- 1 - RSS / TSS
c(TSS = TSS, RSS = RSS, R2 = R2, R2_lm = summary(full)$r.squared)
anova(glm(medv ~ 1, data = Boston), test = "Chisq")   # ligne NULL : deviance = TSS

# Q18. sigma = sqrt(RSS / (n - p - 1))
n <- nrow(Boston)
p <- length(coef(full)) - 1          # nombre de colonnes de X sans l'intercept
c(n = n, p = p, ddl = n - p - 1)
sqrt(RSS / (n - p - 1))
summary(full)$sigma                   # meme valeur

# ---------------------------------------------------------------------
# PARTIE 5. SELECTION DE VARIABLES (B.8)
# ---------------------------------------------------------------------

# Q19. stepAIC dans les deux sens
step_model <- MASS::stepAIC(full, direction = "both")
summary(step_model)
formula(step_model)
# Les variables retirees sont (dans l'ordre) indus puis age.

# Q20. AIC a la main : n*log(RSS/n) + 2k   (k = p + 1)
RSS_step <- sum(residuals(step_model)^2)
k_step   <- length(coef(step_model))
n * log(RSS_step / n) + 2 * k_step
# Valeur identique a celle affichee par stepAIC (extractAIC)
extractAIC(step_model)

RSS_full <- sum(residuals(full)^2)
k_full   <- length(coef(full))
n * log(RSS_full / n) + 2 * k_full
extractAIC(full)

# Q21. Tableau comparatif (comme le Tableau 14 de vos notes)
resume <- function(m) {
  s   <- summary(m)
  p_m <- length(coef(m)) - 1
  c(p = p_m,
    ddl = df.residual(m),
    RSS = sum(residuals(m)^2),
    sigma = s$sigma,
    R2 = s$r.squared,
    R2_adj = s$adj.r.squared,
    AIC = extractAIC(m)[2])
}
comparaison <- rbind(Complet = resume(full), Final = resume(step_model))
round(comparaison, 3)
# Le RSS du modele final est un peu plus grand (normal en retirant des variables),
# mais sigma, R2 ajuste et AIC sont meilleurs ou equivalents : modele plus simple.

# ---------------------------------------------------------------------
# PARTIE 6. MULTICOLINEARITE (B.9)
# ---------------------------------------------------------------------

# Q22. VIF
vif(full)
vif(step_model)
# Regles (VIF classique) : > 5 modere, > 10 severe.
# tax (VIF = 9.0) et rad (VIF = 7.4) sont les plus eleves : elles sont
# fortement correlees entre elles. Aucune ne depasse 10 ici.
# Apres selection, tax et rad restent autour de 7 : a surveiller.
# Si un facteur a plusieurs colonnes, vif() renvoie le GVIF et on lit GVIF^(1/(2*Df)),
# a comparer aux seuils 2.24 (VIF 5) et 3.16 (VIF 10) de vos notes.

# Q23. VIF de tax a la main : 1 / (1 - R2_j)
aux  <- lm(tax ~ . - medv, data = Boston)   # tax regressee sur les autres predicteurs
R2_j <- summary(aux)$r.squared
1 / (1 - R2_j)
vif(full)["tax"]                            # meme valeur

# ---------------------------------------------------------------------
# PARTIE 7. DIAGNOSTICS ET HYPOTHESES (section 8 de la Part 1)
# ---------------------------------------------------------------------

# Q24. Les 4 graphiques
par(mfrow = c(2, 2))
plot(step_model)
par(mfrow = c(1, 1))
# Residuals vs Fitted  -> linearite et esperance nulle des erreurs
#                         (une courbure apparait : modele mal specifie)
# Normal Q-Q           -> normalite des erreurs (les points s'ecartent de la droite aux extremites)
# Scale-Location       -> variance constante (homoscedasticite)
# Residuals vs Leverage-> points influents (distance de Cook)

# Q25. Tests formels
shapiro.test(residuals(step_model))   # normalite des residus
car::ncvTest(step_model)              # H0 : variance constante
# Les deux p-values sont tres petites : normalite et homoscedasticite sont
# rejetees. Le modele lineaire simple est donc imparfait ici (non-linearite
# de lstat, plafonnement de medv a 50). Les erreurs standard classiques sont
# a interpreter avec prudence.

# Q26. Observations influentes (Cook)
cook <- cooks.distance(step_model)
seuil <- 4 / nrow(Boston)
infl <- which(cook > seuil)
length(infl)
head(sort(cook[infl], decreasing = TRUE))

# ---------------------------------------------------------------------
# PARTIE 8. EVALUATION DE LA PREDICTION
# ---------------------------------------------------------------------

# Q27. Decoupage entrainement / test (80 % / 20 %)
set.seed(123)
idx   <- sample(seq_len(nrow(Boston)), size = round(0.8 * nrow(Boston)))
train <- Boston[idx, ]
test  <- Boston[-idx, ]
c(train = nrow(train), test = nrow(test))

# Q28. Ajustement sur l'entrainement, erreurs sur le test
fit_full <- lm(medv ~ ., data = train)
fit_step <- lm(formula(step_model), data = train)

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))

pred_full <- predict(fit_full, newdata = test)
pred_step <- predict(fit_step, newdata = test)

resultats <- rbind(
  Complet = c(RMSE_test = rmse(test$medv, pred_full), MAE_test = mae(test$medv, pred_full),
              R2_train = summary(fit_full)$r.squared),
  Final   = c(RMSE_test = rmse(test$medv, pred_step), MAE_test = mae(test$medv, pred_step),
              R2_train = summary(fit_step)$r.squared)
)
round(resultats, 3)

# Q29. Conclusion
# On compare les RMSE / MAE sur le TEST. Le R2 d'entrainement augmente toujours
# quand on ajoute des variables : il ne permet pas de choisir le meilleur modele.
# Le modele simplifie fait en general aussi bien (ou presque) que le complet
# avec moins de parametres.

# ---------------------------------------------------------------------
# PARTIE 9. BONUS
# ---------------------------------------------------------------------

# Q30. Simulation : l'estimateur est-il sans biais ?
# On prend les coefficients et sigma du modele final comme "vrais" parametres.
Xs      <- model.matrix(step_model)          # matrice de conception (avec intercept)
beta_v  <- coef(step_model)
sigma_v <- summary(step_model)$sigma
n_s     <- nrow(Xs)
p1      <- ncol(Xs)

nsim    <- 1000
betasim <- matrix(NA, nrow = nsim, ncol = p1)   # on pre-alloue (plus rapide que rbind)
set.seed(2)
for (i in 1:nsim) {
  ysim <- drop(Xs %*% beta_v) + rnorm(n_s, 0, sigma_v)
  betasim[i, ] <- drop(solve(crossprod(Xs), crossprod(Xs, ysim)))
}
colnames(betasim) <- names(beta_v)

res_sim <- cbind(
  Vrai          = beta_v,
  Moyenne_est   = colMeans(betasim),
  Var_simulee   = apply(betasim, 2, var),
  Var_theorique = diag(sigma_v^2 * solve(crossprod(Xs)))
)
round(res_sim, 5)
# La moyenne des estimations est quasi egale au vrai beta (sans biais) et la
# variance simulee est quasi egale a sigma^2 (X'X)^(-1) (formule correcte).

par(mfrow = c(2, 3))
for (j in 1:min(6, p1)) {
  hist(betasim[, j], main = colnames(betasim)[j], xlab = "", col = "grey85")
  abline(v = beta_v[j], col = "red", lty = 2, lwd = 2)
  abline(v = mean(betasim[, j]), col = "darkgreen", lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))

# Q31. Ameliorer le modele (non-linearite, asymetrie de medv)
f_log  <- medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + lstat
m_base <- lm(f_log, data = train)
m_quad <- lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + lstat + I(lstat^2),
             data = train)
m_log  <- lm(log(medv) ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + log(lstat),
             data = train)

p_base <- predict(m_base, test)
p_quad <- predict(m_quad, test)
p_log  <- exp(predict(m_log, test))     # retour a l'echelle d'origine (k$)

round(rbind(
  Lineaire  = c(RMSE = rmse(test$medv, p_base), MAE = mae(test$medv, p_base)),
  Quadratique = c(RMSE = rmse(test$medv, p_quad), MAE = mae(test$medv, p_quad)),
  Log       = c(RMSE = rmse(test$medv, p_log),  MAE = mae(test$medv, p_log))
), 3)
# Le terme en lstat^2 ou le passage au log reduisent generalement l'erreur de test :
# la relation avec lstat est courbe, pas droite.

# Verifier les diagnostics du meilleur modele
par(mfrow = c(2, 2)); plot(m_log); par(mfrow = c(1, 1))
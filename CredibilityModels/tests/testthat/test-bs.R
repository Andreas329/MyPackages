##-------------------------------------------------------------------------
## Teste BuehlmannStraub
##
## Autor: Andreas Hausmann
##-------------------------------------------------------------------------

context("bs")
test_that("bs-fire",{
  #vgl. Buehlmann-Gisler Uebung 4.1
  dat.fire <- data.frame(Rk = c(rep(paste0("R", (1:5)), each = 5)),
                         Jahr = rep(1:5, 5),
                         Anzahl = c(729, 786, 872, 951, 1019,
                                    1631, 1802, 2090, 2300, 2368,
                                    796, 827, 874, 917, 944,
                                    3152, 3454, 3715, 3859, 4198,
                                    400, 420, 422, 424, 440),
                         NL = c(0.8, 1.4, 0.3, 0.88, 1.6,
                                0.06, 0.72, 0.16, 0.20, 0.38,
                                1.80, 0.6, 0.8, 1.9, 1.1,
                                0.56, 1.2, 0.84, 1.07, 0.8,
                                0.1, 0, 0.4, 2.4, 0.1))

  bs.fire <- bs(formula = NL ~ Rk, weights = Anzahl, data = dat.fire)
  expect_equal(trunc(bs.fire$s.sq), 261)
  expect_equal(round(bs.fire$tau, 3), 0.102)
  expect_equal(bs.fire$s.sq/bs.fire$tau, bs.fire$kappa)
  expect_equal(round(bs.fire$est$alpha, 2), c(R1 = 0.63,R2 = 0.80, R3 = 0.63, R4 = 0.88, R5 = 0.45))
  expect_equal(round(bs.fire$cred.est, 2), c(R1 = 0.94,R2 = 0.40, R3 = 1.08, R4 = 0.89, R5 = 0.71))

})


test_that("bs-fire-two-groups",{
  #vgl. Buehlmann-Gisler Uebung 4.1
  dat.fire <- data.frame(Rk1 = c(rep("a", 15), rep(2, 10)),
                         Rk2 = c(rep(1:3, each = 5), rep(1:2, each = 5)),
                         Jahr = rep(1:5, 5),
                         Anzahl = c(729, 786, 872, 951, 1019,
                                    1631, 1802, 2090, 2300, 2368,
                                    796, 827, 874, 917, 944,
                                    3152, 3454, 3715, 3859, 4198,
                                    400, 420, 422, 424, 440),
                         NL = c(0.8, 1.4, 0.3, 0.88, 1.6,
                                0.06, 0.72, 0.16, 0.20, 0.38,
                                1.80, 0.6, 0.8, 1.9, 1.1,
                                0.56, 1.2, 0.84, 1.07, 0.8,
                                0.1, 0, 0.4, 2.4, 0.1))

  bs.fire <- bs(formula = NL ~ Rk1 + Rk2, weights = Anzahl, data = dat.fire)
  expect_equal(trunc(bs.fire$s.sq), 261)
  expect_equal(round(bs.fire$tau, 3), 0.102)
  expect_equal(bs.fire$s.sq/bs.fire$tau, bs.fire$kappa)
  expect_equal(round(bs.fire$alpha, 2), c(R1 = 0.63,R2 = 0.80, R3 = 0.63, R4 = 0.88, R5 = 0.45))
  expect_equal(round(bs.fire$cred.est, 2), c(R1 = 0.94,R2 = 0.40, R3 = 1.08, R4 = 0.89, R5 = 0.71))

})



#Teste Ein Eintrag pro Jahr und Key-Wert
#----------------
#
# test_that("Test ein Eintrag pro Jahr und Key", {
#   dat.test <- data.frame(Kanton = c(rep(c("AG", "BE"), each = 3), "AG"),
#                          Jahr = c(2012:2014, 2012:2014, 2014),
#                          Anzahl = c(20, 18, 23, 100, 104, 99, 6),
#                          NL = c(1561, 1864, 1254, 56, 74, 66, 15))
#   expect_error(BuehlmannStraub(dat.test, "NL", "Anzahl", "Jahr", "Kanton" ,"homogen"))
# })

# Teste Berechnung
#-----------------------
#
# test_that("Berechnung Fire", {
#   #vgl. Buehlmann-Gisler ?bung 4.1
#   dat.fire <- data.frame(Rk = c(rep(paste0("R", (1:5)), each = 5)),
#                          Jahr = rep(1:5, 5),
#                          Anzahl = c(729, 786, 872, 951, 1019,
#                                     1631, 1802, 2090, 2300, 2368,
#                                     796, 827, 874, 917, 944,
#                                     3152, 3454, 3715, 3859, 4198,
#                                     400, 420, 422, 424, 440),
#                          NL = c(0.8, 1.4, 0.3, 0.88, 1.6,
#                                 0.06, 0.72, 0.16, 0.20, 0.38,
#                                 1.80, 0.6, 0.8, 1.9, 1.1,
#                                 0.56, 1.2, 0.84, 1.07, 0.8,
#                                 0.1, 0, 0.4, 2.4, 0.1))
#
#   res.bs <-BuehlmannStraub(dat.fire, "NL", "Anzahl", "Jahr", "Rk")
#
#
#   #Vergleiche mit acutar::cm
#   #-------------------------
#
#   dat.cm <- ReshapeDataCred(dat.fire, "NL", "Anzahl", "Jahr", "Rk")
#   res.cm <- cm(~Rk, dat.cm, ratio.1:ratio.5, weight.1:weight.5,
#                method = "Buhlmann-Gisler")
#
#   #sigma
#   expect_equal(as.vector(res.cm[["unbiased"]]["Rk"]),
#                as.vector(res.bs[["sigma_sq"]]))
#   #tau
#   expect_equal(as.vector(res.cm[["unbiased"]]["portfolio"]),
#                as.vector(res.bs[["tau_sq"]]))
#   #mu.null
#   expect_equal(res.cm[["means"]][["portfolio"]], as.vector(res.bs[["mu_null"]]))
#   #pred
#   my.pred <-  res.bs[["est"]][,"ratio_pred"]
#   names(my.pred) <- res.bs[["est"]][, "Rk"]
#   expect_equal(predict(res.cm), my.pred)
#
# })
#
#
#
#
# test_that("Berechung GroupHealth",{
#   #vgl. Ohlsson S. 79
#   dat.gh <- data.frame(Company = rep(paste0("C", (1:5)), each = 10),
#                        Jahr = rep(1:10, 5),
#                        Anzahl = c(44, 50, 56, 58, 58, 56, 54, 52, 52, 46,
#                                   20, 20, 24, rep(0, 7),
#                                   8, 6, 10, 6, 8, 4, 6, rep(0, 3),
#                                   22, 22, 18, 20, 12, 10, 12, 10, 6, 6,
#                                   26, 24, 22, 18, 20, 16, 12, 14, 14, 0),
#                        NL = c(540, 514, 576, 483, 481, 493, 438, 588, 541, 441,
#                               99, 103, 163, rep(0, 7),
#                               0, 400, 1042, 313, 0, 833, 0, rep(0, 3),
#                               275, 278, 430 ,196, 667, 185, 517, 204, 323, 968,
#                               543, 984, 727, 562, 722, 610, 794, 299, 580 ,0))
#
#   expect_warning(res.bs <- BuehlmannStraub(dat.gh, "NL", "Anzahl", "Jahr", "Company"))
#
#   #Vergleiche mit actuar::cm
#   #-------------------------
#
#   #ergibt gleiches Resultate falls 0er Gewichte = NA, sonst falsches Resultat
#
#   dat.gh[["Anzahl_NA"]] <- ifelse(dat.gh[["Anzahl"]]== 0,NA, dat.gh[["Anzahl"]])
#   dat.gh[["NL_NA"]] <- ifelse(dat.gh[["Anzahl"]]== 0,NA, dat.gh[["NL"]])
#   dat.cm.na <- ReshapeDataCred(dat.gh, "NL_NA", "Anzahl_NA", "Jahr", "Company")
#   res.cm.na <- cm(~Company, dat.cm.na, ratio.1:ratio.10, weight.1:weight.10,
#                   method = "Buhlmann-Gisler")
#
#   #sigma
#   expect_equal(as.vector(res.cm.na[["unbiased"]]["Company"]),
#                as.vector(res.bs[["sigma_sq"]]))
#   #tau
#   expect_equal(as.vector(res.cm.na[["unbiased"]]["portfolio"]),
#                as.vector(res.bs[["tau_sq"]]))
#   #mu.null
#   expect_equal(res.cm.na[["means"]][["portfolio"]], as.vector(res.bs[["mu_null"]]))
#   #pred
#   my.pred <-  res.bs[["est"]][,"ratio_pred"]
#   names(my.pred) <- res.bs[["est"]][, "Company"]
#   expect_equal(predict(res.cm.na), my.pred)
#
#   # Teste einbindung mu.null
#   #--------------------------
#
#   expect_warning(res.bs <- BuehlmannStraub(dat.gh, "NL", "Anzahl", "Jahr", "Company",
#                                            mu.null = 1000))
#
#   res.mu.null.cm <- res.cm.na[["cred"]]*res.cm.na[["means"]][["Company"]]+
#     (1-res.cm.na[["cred"]])*1000
#   names(res.mu.null.cm) <- names(res.cm.na[["means"]][["Company"]])
#
#   #pred
#   my.pred <-  res.bs[["est"]][,"ratio_pred"]
#   names(my.pred) <- res.bs[["est"]][, "Company"]
#   expect_equal(res.mu.null.cm, my.pred)
# })

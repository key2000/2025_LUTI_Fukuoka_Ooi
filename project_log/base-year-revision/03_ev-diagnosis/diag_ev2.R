options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
source("scripts/scenario_employment_2009_2021.R")
r <- all_res
# 中間の分布（2009と2021の平均）
mid <- emp_years$emp2009$L_j_hat; mid$L_j_hat <- round((emp_years$emp2009$L_j_hat$L_j_hat + emp_years$emp2021$L_j_hat$L_j_hat)/2)
k_w <- payroll0 / sum(omega_j$omega_j * mid$L_j_hat, na.rm = TRUE)
r$mid <- run_scenario("mid", mid, scn0_omega_j_matrix * k_w, v_init = results$scn0$v, max_iter = 50, msa = 0.5, tol = 100)
ev <- c(
  s0_09 = ev_benefit(r$scn0, r$emp2009), `09_s0` = ev_benefit(r$emp2009, r$scn0),
  s0_21 = ev_benefit(r$scn0, r$emp2021), `21_s0` = ev_benefit(r$emp2021, r$scn0),
  `09_21` = ev_benefit(r$emp2009, r$emp2021), `21_09` = ev_benefit(r$emp2021, r$emp2009),
  s0_mid = ev_benefit(r$scn0, r$mid))
print(round(ev))
# 従業ゾーン別の寄与（scn0→2009）
zone_ev <- function(base, scn) {
  dv <- matrix(exp(scn$v) - exp(base$v), nrow = nz_res, ncol = nz_work, byrow = TRUE)
  e <- (1 / alpha_0) * base$state$r_ij_H^alpha_a * dv
  colSums((base$state$l_i_j + scn$state$l_i_j) / 2 * e, na.rm = TRUE) / 10
}
zt <- data.frame(zone = rownames(scn0_L_j_hat), L0 = scn0_L_j_hat$L_j_hat,
                 L09 = emp_years$emp2009$L_j_hat$L_j_hat, L21 = emp_years$emp2021$L_j_hat$L_j_hat,
                 v0 = exp(r$scn0$v), v09 = exp(r$emp2009$v), v21 = exp(r$emp2021$v),
                 ev09 = zone_ev(r$scn0, r$emp2009), ev21 = zone_ev(r$scn0, r$emp2021))
print(zt, digits = 4)
saveRDS(list(r = r, emp_years = emp_years, zt = zt, ev = ev),
        "/private/tmp/claude-501/-Users-natsumioi-2025-LUTI-Fukuoka-Ooi/df327c61-809d-47fc-94e5-df6a73435d00/scratchpad/diag_ev2.rds")
cat("[done]\n")

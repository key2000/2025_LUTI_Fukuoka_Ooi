# EVの違和感の診断：scn0を厳しい収束判定で回し直し、収束の甘さだけでEVがどれだけ出るか
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(diag_t0 = Sys.time())
source("scripts/scenario_employment_2009_2021.R")
cat("\n[main+scenario done]", format(Sys.time()-getOption("diag_t0")), "\n")
cat("scale:", sapply(emp_years, `[[`, "scale"), " wage_k:", sapply(emp_years, `[[`, "wage_k"), "\n")
for (nm in names(all_res)) cat(nm, "iterations:", all_res[[nm]]$iterations, " final_diff:", all_res[[nm]]$final_diff, "\n")
tight <- list()
tight$scn0 <- run_scenario("scn0_tight", scn0_L_j_hat, scn0_omega_j_matrix, v_init = results$scn0$v, max_iter = 50, msa = 0.5, tol = 1)
for (y in names(emp_years)) tight[[y]] <- run_scenario(paste0(y,"_tight"), emp_years[[y]]$L_j_hat, emp_years[[y]]$omega_j_matrix, v_init = results$scn0$v, max_iter = 50, msa = 0.5, tol = 1)
ev <- c(
  scn0_vs_scn0tight   = ev_benefit(all_res$scn0, tight$scn0),
  orig_09             = ev_benefit(all_res$scn0, all_res$emp2009),
  orig_21             = ev_benefit(all_res$scn0, all_res$emp2021),
  orig_21_09          = ev_benefit(all_res$emp2009, all_res$emp2021),
  tight_09            = ev_benefit(tight$scn0, tight$emp2009),
  tight_21            = ev_benefit(tight$scn0, tight$emp2021),
  tight_21_09         = ev_benefit(tight$emp2009, tight$emp2021),
  emp09_orig_vs_tight = ev_benefit(all_res$emp2009, tight$emp2009))
print(round(ev))
ind_t <- sapply(tight, indicators); print(round(ind_t, 2)); print(round(ind, 2))
saveRDS(list(ev = ev, ind_t = ind_t, ind = ind, iters = sapply(tight, `[[`, "iterations")),
        "/private/tmp/claude-501/-Users-natsumioi-2025-LUTI-Fukuoka-Ooi/df327c61-809d-47fc-94e5-df6a73435d00/scratchpad/diag_ev.rds")
cat("[all done]", format(Sys.time()-getOption("diag_t0")), "\n")

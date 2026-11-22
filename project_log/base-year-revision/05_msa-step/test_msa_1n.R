# 案C の試験：LUTIループの混合比を 0.5 固定 → 1/(n+1) にして、揺れが消えるか確かめる
# メインスクリプトは変更しない（run_scenario を写した関数をここで定義）
options(device = function(...) grDevices::png(tempfile(fileext = ".png"), width = 800, height = 600, type = "quartz"))
options(msa_t0 = Sys.time())
source("scripts/scenario_employment_2009_2021.R")
cat("\n[main+scenario done]", format(Sys.time() - getOption("msa_t0")), "\n")

run_scenario_msa1n <- function(scenario_name, L_j_hat_scn, omega_j_matrix_scn, v_init,
                               n_iter = 50, checkpoints = c(10, 20, 30, 40, 50)) {
  dists_road <- dists0.road * 1
  v_l <- v_init
  snaps <- list(); log <- data.frame()
  for (it in 1:n_iter) {
    V.car_l <- para[1] * dists_road + para[2] + para[4] * parkPrice / 2
    den_l   <- exp(V.bike) + exp(V.car_l) + exp(V.rail)
    P.car_l <- exp(V.car_l) / den_l; P.rail_l <- exp(V.rail) / den_l; P.bike_l <- exp(V.bike) / den_l
    agcc_l <- (P.bike_l * V.bike + P.car_l * V.car_l + P.rail_l * V.rail) / para[1]
    c_ij_local <- agcc_l * 1.600
    disposable_income_ij_local <- pmax(-c_ij_local + omega_j_matrix_scn, 0)
    assign("L_j_hat", L_j_hat_scn, envir = .GlobalEnv)
    assign("c_ij", c_ij_local, envir = .GlobalEnv)
    assign("disposable_income_ij", disposable_income_ij_local, envir = .GlobalEnv)
    sol <- nleqslv(x = v_l, fn = gapf_vj, global = "dbldog",
                   control = list(ftol = 1e-8, xtol = 1e-8, maxit = 200, allowSingular = TRUE))
    v_l <- sol$x
    state <- caluculate_model_state(exp(v_l))
    if (it %in% checkpoints)
      snaps[[as.character(it)]] <- list(v = v_l, state = state, P.car = P.car_l, P.rail = P.rail_l, P.bike = P.bike_l)
    ODD.car <- state$l_i_j * P.car_l
    trips <- as.data.frame.table(ODD.car, responseName = "demand"); names(trips) <- c("from", "to", "demand")
    ta <- assign_traffic(Graph = sgr, from = trips$from, to = trips$to, demand = trips$demand,
                         max_gap = ta_max_gap, algorithm = "bfw", verbose = FALSE)
    sgr2 <- makegraph(df = ta$data[, c("from", "to", "cost")], directed = TRUE, capacity = 1e4,
                      alpha = alpha, beta = beta, coords = nodes)
    dists_road_new <- get_distance_matrix(sgr2, from = zones, to = centers, algorithm = "mch")
    step <- 1 / (it + 1)                                   # 変更点：0.5 固定 → 1/(n+1)
    diff_raw <- sum((dists_road_new - dists_road)^2)       # 従来の diff（新しい配分結果と平均の差）
    dists_road_upd <- step * dists_road_new + (1 - step) * dists_road
    diff_avg <- sum((dists_road_upd - dists_road)^2)       # 平均そのものの動き
    dists_road <- dists_road_upd
    log <- rbind(log, data.frame(it = it, step = step, diff_raw = diff_raw, diff_avg = diff_avg))
    cat("MSA1n:", scenario_name, "| it", it, "| diff_raw", formatC(diff_raw, format = "e", digits = 2),
        "| diff_avg", formatC(diff_avg, format = "e", digits = 2), "\n")
  }
  list(snaps = snaps, log = log)
}

m <- list()
m$scn0 <- run_scenario_msa1n("scn0", scn0_L_j_hat, scn0_omega_j_matrix, v_init = results$scn0$v)
for (y in names(emp_years))
  m[[y]] <- run_scenario_msa1n(y, emp_years[[y]]$L_j_hat, emp_years[[y]]$omega_j_matrix, v_init = results$scn0$v)

cps <- names(m$scn0$snaps)
ev_tab <- t(sapply(cps, function(k) {
  s0 <- m$scn0$snaps[[k]]; s9 <- m$emp2009$snaps[[k]]; s21 <- m$emp2021$snaps[[k]]
  c(s0_09 = ev_benefit(s0, s9), s09_0 = ev_benefit(s9, s0),
    s0_21 = ev_benefit(s0, s21), s21_0 = ev_benefit(s21, s0),
    s09_21 = ev_benefit(s9, s21), s21_09 = ev_benefit(s21, s9))
}))
drift <- t(sapply(cps[-1], function(k) {
  prev <- cps[which(cps == k) - 1]
  sapply(names(m), function(nm) ev_benefit(m[[nm]]$snaps[[prev]], m[[nm]]$snaps[[k]]))
}))
co2 <- sapply(cps, function(k) sapply(names(m), function(nm) indicators(m[[nm]]$snaps[[k]])["co2_total"]))
cat("\n== EV by checkpoint (万円/月) ==\n"); print(round(ev_tab))
cat("\n== drift: EV between successive checkpoints, same scenario ==\n"); print(round(drift))
cat("\n== CO2 total by checkpoint ==\n"); print(round(co2, 2))
saveRDS(list(log = lapply(m, `[[`, "log"), ev = ev_tab, drift = drift, co2 = co2),
        "project_log/base-year-revision/05_msa-step/test_msa_1n_result.rds")
cat("[all done]", format(Sys.time() - getOption("msa_t0")), "\n")

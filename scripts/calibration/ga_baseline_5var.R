# scripts/calibration/ga_baseline_5var.R
# project_log/calibration/01_baseline-5param-ga/ に対応する、5構造パラメータのみのGA較正（k_i=0.1固定）。
# 元は scripts/Fukuoka_LUTI_model_260617.R の if(FALSE) ブロック内（旧L.1790-1967付近）にあった
# calibration_fitness() を、k_i を 01 の仕様通り固定値0.1に補正した上で抽出したもの
# （project_log/calibration/01_baseline-5param-ga/meta.md: 「k_i はモデル本来の定数0.1に
# 固定したまま動かさない」）。元コードは k_i_c <- k_i_estimated_final
# （scripts/calibration/ki_estimation.R 系統の逆算値、01の較正とは無関係な変数）を参照しており
# 01の実際の仕様とは食い違っていたため、この抽出時に k_i_c <- rep(0.1, nz_res) に修正した。
#
# 事前提提: このファイルは本体パイプラインの途中（道路網・土地利用データ・モデル関数
# caluculate_model_state/gapf_vj 等の定義済み・household/athome_df/athome_ar 読込済み）
# で source() される想定。以下のオブジェクトが呼び出し元の環境に存在している必要がある:
# dists0.road, sgr, nodes, zones, centers, alpha, beta, V.bike, V.rail, para, parkPrice,
# omega_j_matrix, L_j_hat, G0_i, phi, phi_pub, rr_a, nz_res, nz_work, v_start,
# household, athome_df, athome_ar, assign_traffic(), makegraph(), get_distance_matrix()

calibration_fitness_baseline <- function(x, nz_res, dists0.road, sgr, nodes, zones, centers,
                                          alpha, beta, V.bike, V.rail, para, parkPrice,
                                          omega_j_matrix, L_j_hat, G0_i, phi, phi_pub, rr_a,
                                          nz_work, v_start, household, athome_df, athome_ar) {

  # ---- パラメータを展開 ----
  alpha_a_c <- x[1]
  alpha_z_c <- 1 - alpha_a_c
  alpha_0_c <- (alpha_z_c ^ alpha_z_c) * (alpha_a_c ^ alpha_a_c)
  gamma_0_c <- x[2]
  gamma_1_c <- x[3]
  theta_H_c <- x[4]
  theta_L_c <- x[5]
  # 01_baseline-5param-ga の仕様どおり、k_i はモデル本来の定数0.1に固定（GAの探索対象にしない）。
  # 元コードは別系統（ki_estimation.R）のk_i_estimated_finalをグローバル参照していたが、
  # 01の較正とは無関係のため、抽出時に定数0.1へ補正した。
  k_i_c <- rep(0.1, nz_res)


  tryCatch({

    # ---- LUTIループをfitness内部で完走させる ----
    dists_road_local <- dists0.road * 1
    flag_local <- 0
    ii_local   <- 1

    while (flag_local == 0) {

      # ① モード選択
      V.car_l  <- para[1] * dists_road_local + para[2] + para[4] * parkPrice / 2
      den_l    <- exp(V.bike) + exp(V.car_l) + exp(V.rail)
      P.bike_l <- exp(V.bike)  / den_l
      P.car_l  <- exp(V.car_l) / den_l
      P.rail_l <- exp(V.rail)  / den_l
      agcc_l   <- (P.bike_l * V.bike + P.car_l * V.car_l + P.rail_l * V.rail) / para[1]
      c_ij_l   <- agcc_l * 1.600
      disp_inc_l <- pmax(-c_ij_l + omega_j_matrix, 0)

      # ② 土地利用均衡（ローカルパラメータ版）
      calc_state_local <- function(v_j_vec) {
        v_j_mat  <- matrix(v_j_vec, nrow = nz_res, ncol = nz_work, byrow = TRUE)
        r_ij_H   <- (alpha_0_c * disp_inc_l / v_j_mat) ^ (1 / alpha_a_c)
        r_ij_H[is.nan(r_ij_H)] <- 0
        exp_r    <- exp(theta_H_c * r_ij_H)
        exp_r[r_ij_H == 0] <- 0
        P_j_i    <- exp_r / pmax(rowSums(exp_r, na.rm = TRUE), 1e-9)
        r_bar_i  <- rowSums(r_ij_H * P_j_i, na.rm = TRUE)
        pg       <- gamma_1_c / (1 - gamma_1_c)
        rg_i     <- phi * (1 - gamma_1_c) *
          (r_bar_i * gamma_0_c) ^ (1 / (1 - gamma_1_c)) *
          (gamma_1_c / k_i_c) ^ pg
        G_i_l    <- G0_i * exp(theta_L_c * (rg_i - rr_a)) /
          (1 + exp(theta_L_c * (rg_i - rr_a))) * phi_pub * phi
        A_Fi_S   <- gamma_0_c * ((r_bar_i * gamma_0_c * gamma_1_c) / k_i_c) ^ pg * G_i_l
        A_Fi_S[is.nan(A_Fi_S)] <- 0
        A_fij_S  <- A_Fi_S * P_j_i
        a_fij_H  <- alpha_a_c * disp_inc_l / pmax(r_ij_H, 1e-9)
        l_i_j    <- A_fij_S / pmax(a_fij_H, 1e-9)
        l_i_j[is.nan(l_i_j)] <- 0
        l_i_j[a_fij_H == 0]  <- 0
        list(l_i_j     = l_i_j,
             r_bar_i   = r_bar_i,
             a_fij_H   = a_fij_H,
             L_j_tilde = colSums(l_i_j, na.rm = TRUE))
      }

      gapf_local <- function(v_j_vec2) {
        s <- calc_state_local(exp(v_j_vec2))
        s$L_j_tilde - L_j_hat$L_j_hat
      }

      res_lu <- nleqslv(
        x       = v_start,
        fn      = gapf_local,
        global  = "dbldog",
        control = list(ftol = 1e-6, xtol = 1e-6, maxit = 100)
      )
      if (!(res_lu$termcd %in% c(1, 2))) return(-200)

      state_l <- calc_state_local(exp(res_lu$x))

      # ③ 交通配分
      ODD.car_l <- state_l$l_i_j * P.car_l
      trips_l   <- as.data.frame.table(ODD.car_l, responseName = "demand")
      names(trips_l) <- c("from", "to", "demand")

      traffic_l <- assign_traffic(
        Graph     = sgr,
        from      = trips_l$from,
        to        = trips_l$to,
        demand    = trips_l$demand,
        max_gap   = 1e-2,
        algorithm = "bfw",
        verbose   = FALSE
      )
      sgr2_l <- makegraph(
        df       = traffic_l$data[, c("from", "to", "cost")],
        directed = TRUE,
        capacity = 1e4,
        alpha    = alpha,
        beta     = beta,
        coords   = nodes
      )
      dists_road_prev  <- dists_road_local
      dists_road_new   <- get_distance_matrix(sgr2_l,
                                              from      = zones,
                                              to        = centers,
                                              algorithm = "mch")
      dists_road_local <- 0.5 * dists_road_new + 0.5 * dists_road_prev

      diff_local <- sum((dists_road_prev - dists_road_local) ^ 2)
      ii_local   <- ii_local + 1

      # GA評価中は反復回数を制限（本番LUTIは ii>1000 まで）
      if (diff_local < 1000 | ii_local > 20) {
        cat("ii=", ii_local, "diff=", diff_local, "\n")  # 何回目で打ち切られたか確認
        flag_local <- 1
      }
    }
    # ---- LUTIループここまで ----


    # ---- 観測値との誤差計算（全指標を対数変換）----

    # 世帯数
    est_hh_df <- tibble(
      KEY_CODE = rownames(state_l$l_i_j) %>% gsub("^mc_", "", .),
      est_hh   = rowSums(state_l$l_i_j, na.rm = TRUE)
    )
    comp_hh <- household %>%
      left_join(est_hh_df, by = "KEY_CODE") %>%
      filter(!is.na(obs_hh), !is.na(est_hh),
             obs_hh > 0, est_hh > 0)
    w_hh_i <- comp_hh$obs_hh / sum(comp_hh$obs_hh)
    f_hh <- sum(w_hh_i*(log(comp_hh$est_hh) - log(comp_hh$obs_hh)) ^ 2)

    # 家賃（r_bar_i は千円/m2 → ×1000 で円/m2 に換算）
    est_rent_df <- tibble(
      KEY_CODE = names(state_l$r_bar_i) %>% gsub("^mc_", "", .),
      est_rent = as.numeric(state_l$r_bar_i) * 1000
    )
    comp_rent <- athome_df %>%
      left_join(est_rent_df, by = "KEY_CODE") %>%
      left_join(household, by = "KEY_CODE") %>%
      filter(!is.na(avg_rent), !is.na(est_rent),
             avg_rent > 0, est_rent > 0,
            !is.na(obs_hh), obs_hh > 0)
    w_rent_i <- comp_rent$obs_hh / sum(comp_rent$obs_hh)
    f_rent <- sum(w_rent_i*(log(comp_rent$est_rent) - log(comp_rent$avg_rent)) ^ 2)

    # 床面積
    est_ar_vec <- rowSums(state_l$a_fij_H * state_l$l_i_j, na.rm = TRUE) /
      rowSums(state_l$l_i_j, na.rm = TRUE)
    est_ar_df <- tibble(
      KEY_CODE = rownames(state_l$a_fij_H) %>% gsub("^mc_", "", .),
      est_ar   = est_ar_vec
    )
    comp_ar <- athome_ar %>%
      left_join(est_ar_df, by = "KEY_CODE") %>%
      left_join(household, by = "KEY_CODE") %>%
      filter(!is.na(avg_room_ar), !is.na(est_ar),
             avg_room_ar > 0, est_ar > 0,
            !is.na(obs_hh), obs_hh > 0)
    w_ar_i <- comp_ar$obs_hh / sum(comp_ar$obs_hh)
    f_ar <- sum(w_ar_i*(log(comp_ar$est_ar) - log(comp_ar$avg_room_ar)) ^ 2)

    # ---- ペナルティ項 ----
    x0      <- c(0.3, 0.2, 0.6, 0.1, 2.0)
    lambda  <- 0.05
    penalty <- lambda * sum(((x - x0) / x0) ^ 2)

    # ---- 重みつき合計Loss ----
    w_hh   <- 1.0
    w_rent <- 0.5
    w_ar   <- 0.3

    total_loss <- w_hh * f_hh + w_rent * f_rent + w_ar * f_ar + penalty

    return(-total_loss)  # GAは最大化 → 符号反転

  }, error = function(e) {
    return(-200)
  })
}


#' 01_baseline-5param-ga の5変数GA較正を実行する（k_i=0.1固定）
#'
#' @return list(params, fitness, ga_result)
calibrate_ga_baseline_5var <- function(dists0.road, sgr, nodes, zones, centers, alpha, beta,
                                        V.bike, V.rail, para, parkPrice, omega_j_matrix, L_j_hat,
                                        G0_i, phi, phi_pub, rr_a, nz_res, nz_work, v_start,
                                        household, athome_df, athome_ar,
                                        popSize = 50, maxiter = 100) {
  library(GA)
  library(parallel)
  library(doParallel)
  library(nleqslv)

  # k_iの探索範囲（元の定数0.1を基準に、現実的な範囲を設定）— この較正では使わないが
  # 元コードとの対応のため名前だけ残す
  # k_lower <- 0.01
  # k_upper <- 0.4

  # x = c(alpha_a, gamma_0, gamma_1, theta_H, theta_L) の5次元のみ（k_iはfitness内部で0.1固定）
  lower_vec <- c(0.1,  0.05, 0.3,  0.02, 0.5)
  upper_vec <- c(0.5,  0.8,  0.9,  0.5,  5.0)

  # GAの並列ワーカーは1引数(x)のfitness関数しか受け付けないため、この関数の引数を
  # レキシカルスコープで捕捉する薄いラッパーを作り、それをclusterExportする。
  fitness_for_ga <- function(x) {
    calibration_fitness_baseline(x, nz_res, dists0.road, sgr, nodes, zones, centers,
                                  alpha, beta, V.bike, V.rail, para, parkPrice,
                                  omega_j_matrix, L_j_hat, G0_i, phi, phi_pub, rr_a,
                                  nz_work, v_start, household, athome_df, athome_ar)
  }

  # ---- クラスター準備 ----
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, varlist = c("calibration_fitness_baseline", "fitness_for_ga"),
                envir = environment())

  clusterEvalQ(cl, {
    library(nleqslv)
    library(cppRouting)
    library(dplyr)
    library(tibble)
  })

  # ---- 単発テスト（GA本番の前に正常な値が返るか確認）----
  x0_test <- c(0.3, 0.2, 0.6, 0.1, 2.0)
  test_result <- fitness_for_ga(x0_test)
  cat("単発テスト fitness値:", test_result, "\n")
  # -200が返る場合はLUTIループの非収束やデータ不整合が原因。正常な値が出ることを
  # 確認してから下のGA本番実行に進むこと。

  # ---- GA実行 ----
  cat("GA開始:", format(Sys.time(), "%H:%M:%S"), "\n")
  elapsed <- system.time({
    ga_result_calib <- ga(
      type    = "real-valued",
      fitness = fitness_for_ga,

      #         alpha_a  gamma_0  gamma_1  theta_H  theta_L
      lower   = lower_vec,
      upper   = upper_vec,

      popSize = popSize,
      maxiter = maxiter,

      parallel = cl,
      monitor  = TRUE
    )
  })
  cat("GA終了:", format(Sys.time(), "%H:%M:%S"), "\n")
  print(elapsed)

  closeAllConnections()
  stopCluster(cl)
  gc()
  gc()

  # ---- 本体スクリプトが読み込む形式に正規化して保存 ----
  # どの較正パターンでも calib_result$params（5変数named vector）と
  # calib_result$k_i（長さnz_resのベクトル）を必ず持つ、という共通フォーマットにしておくことで、
  # 本体スクリプト側のローダーがパターンごとの分岐を持たずに済む。
  params <- ga_result_calib@solution[1, ]
  names(params) <- c("alpha_a", "gamma_0", "gamma_1", "theta_H", "theta_L")

  calib_result <- list(
    params    = params,
    k_i       = rep(0.1, nz_res),  # 01はk_i固定（GAの探索対象にしない）
    fitness   = ga_result_calib@fitnessValue,
    ga_result = ga_result_calib
  )
  save(calib_result, file = "project_log/calibration/01_baseline-5param-ga/ga_result.RData")

  plot(ga_result_calib)
  png("project_log/calibration/01_baseline-5param-ga/plots/ga_convergence.png",
      width = 900, height = 750, res = 150)
  plot(ga_result_calib)
  dev.off()

  calib_result
}

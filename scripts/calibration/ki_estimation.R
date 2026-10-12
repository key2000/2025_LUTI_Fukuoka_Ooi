# scripts/calibration/ki_estimation.R
# project_log/calibration/03_ki-backward-calc/ → 04_ki-outlier-crop-kriging/ → 05_ki-relaxation/
# の系譜（1つながりの試行、3者は代替案ではなく積み上げ）に対応する、k_iの解析的推定モジュール。
# 元は scripts/Fukuoka_LUTI_model_260617.R の if(FALSE) ブロック内（旧L.807-1532付近、
# 旧L.738-780/785-792のセットアップ込み）にあったトップレベルのコードを、1つの関数に
# まとめて抽出したもの。
#
# 現在ライブなパイプラインには組み込まれていない（現在のデフォルトはk_i=0.1固定。
# scripts/calibration/ga_baseline_5var.R および本体スクリプトのPhase 1参照）。
# この解析的k_i推定を再検討する際に使えるモジュールとして保持している。
# 各段階のプロット・結果は project_log/calibration/05_ki-relaxation/ 等を参照。
#
# ---- グローバル変数への副作用について（重要）----
# gapf_vj() / caluculate_model_state()（本体スクリプトのL.473-550付近で定義）は、
# 引数として渡されない自由変数（k_i, disposable_income_ij 等）をグローバル環境から
# 参照する作りになっている。元のトップレベルコードはこれを前提に、LUTI均衡を解く直前に
# グローバルの k_i / disposable_income_ij を書き換えていた。関数化にあたり、ただの
# ローカル代入 (<-) にすると gapf_vj/caluculate_model_state から見えなくなり、計算結果が
# 壊れてしまう。そのため、元コードでグローバルを書き換えていた箇所は <<-（スーパー代入）を
# 使っている。一方、この関数内だけで完結する計算上の中間変数（k_i_estimated, k_i_df,
# k_i_final, k_i_relax_final 等）は通常の <- のままにしている。
#
# 事前提提: このファイルは本体パイプラインの途中（道路網・土地利用データ読込済み、
# caluculate_model_state/gapf_vj 等のモデル関数定義済み、dists0/G0_i等のベースライン
# データ準備済み）で source() される想定。

#' k_iの解析的逆算 → 外れ値クリギング補間 → 逐次緩和推定、を通しで実行する
#'
#' @param key_code_sf メッシュ(KEY_CODE)ポリゴンのsf
#' @param dists0 ゾーン×センターの基準距離行列（居住地iの行順がk_iベクトルの順序になる）
#' @param G0_i, phi, phi_pub, rr_a 土地利用モデルの供給側パラメータ（ベースライン固定値）
#' @param gamma_0, gamma_1, theta_L 土地利用モデルの構造パラメータ（ベースライン固定値。
#'   alpha_a/alpha_0/theta_Hはこの関数の計算式では使わないため引数に含めていない）
#' @param v_start nleqslvの初期値ベクトル
#' @param nz_res 居住地ゾーン数
#' @param sgr, nodes, zones, centers, alpha, beta 道路網・交通配分まわりの設定
#' @param V.bike, V.rail モード選択の固定効用
#' @param para モード選択パラメータ
#' @param parkPrice 駐車料金行列
#' @param caluculate_model_state, gapf_vj 本体スクリプトで定義済みのLUTI均衡関数
#'   （グローバルのk_i/disposable_income_ijを参照する点に注意。上記コメント参照）
#' @param assign_traffic, makegraph, get_distance_matrix cppRouting系の道路配分関数
#'
#' @return invisible(k_i_relax_kriged_final)。加えてグローバルのk_iをこの最終値に更新する
#'   （<<-。呼び出し元がこの関数の結果をそのままモデル実行に使えるようにするため）。
estimate_ki_analytical <- function(key_code_sf, dists0, G0_i, phi, phi_pub, rr_a,
                                    gamma_0, gamma_1, theta_L, v_start, nz_res,
                                    sgr, nodes, zones, centers, alpha, beta,
                                    V.bike, V.rail, para, parkPrice,
                                    caluculate_model_state, gapf_vj,
                                    assign_traffic, makegraph, get_distance_matrix) {

  # ============================================================
  # ==== セットアップ（旧L.738-780）：世帯数・家賃・床面積データ ====
  # ============================================================

  # --- 世帯数（国勢調査）---
  csv.household <- "data/raw/国勢調査_人口及び世帯数_1kmメッシュ/tblT001100S5030.csv"
  household <- read.csv(
    csv.household,
    fileEncoding = "CP932",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  ) %>%
    dplyr::select(KEY_CODE, T001100035) %>%
    dplyr::rename(obs_hh = "T001100035") %>%
    dplyr::mutate(KEY_CODE = as.character(KEY_CODE),
                  obs_hh = as.numeric(obs_hh))
  household <- household[-1, ]

  # --- 家賃・床面積（@homeデータ）---
  load("data/athome_crop.xdr")

  athome_df <- athome_crop %>%
    dplyr::select(rent, room_ar, KEY_CODE) %>%
    mutate(rent_par_ar = rent / room_ar) %>%
    st_drop_geometry() %>%
    group_by(KEY_CODE) %>%
    summarise(avg_rent = mean(rent_par_ar, na.rm = TRUE))
    # 単位：円/m2（モデルのr_bar_iは千円/m2なので比較時に×1000）

  athome_ar <- athome_crop %>%
    dplyr::select(KEY_CODE, room_ar) %>%
    st_drop_geometry() %>%
    group_by(KEY_CODE) %>%
    summarise(avg_room_ar = mean(room_ar, na.rm = TRUE))
    # 単位：m2

  # アットホームサンプル数確認
  athome_count <- athome_crop %>%
    st_drop_geometry() %>%
    group_by(KEY_CODE) %>%
    summarise(n_sample = n())  # ゾーンごとのサンプル数

  athome_unreliable <- key_code_sf %>%
    left_join(athome_count, by = "KEY_CODE") %>%
    mutate(sample_status = if_else(n_sample < 5 , "less5", "over5"))

  # ==== キャリブレーションログ用：出力フォルダの準備 ====
  # ここから先で作る図は project_log/calibration/<試行フォルダ>/plots/ に保存する。
  # project_log/calibration/README.md 参照。
  calib_stage_dirs <- c(
    "03_ki-backward-calc",
    "04_ki-outlier-crop-kriging",
    "05_ki-relaxation"
  )
  for (calib_d in calib_stage_dirs) {
    dir.create(file.path("project_log", "calibration", calib_d, "plots"), recursive = TRUE, showWarnings = FALSE)
  }


  # ============================================================
  # ==== Step 1: k_iを逆算（旧L.807-897, 03_ki-backward-calc）====
  # ============================================================

  # 観測総床面積の作成
  # athome_ar$avg_room_ar：ゾーンごとの平均床面積（m²/戸）
  # household$obs_hh     ：ゾーンごとの世帯数（戸）
  obs_floor_total <- athome_ar %>%
    left_join(household, by = "KEY_CODE") %>%
    mutate(obs_A_Fi = avg_room_ar * obs_hh) %>%   # 平均床面積 × 世帯数
    dplyr::select(KEY_CODE, obs_A_Fi, avg_room_ar)


  obs_floor_total_sf <- key_code_sf %>%
    left_join(obs_floor_total, by = "KEY_CODE")

  ggplot2::ggplot() +
    geom_sf(data = obs_floor_total_sf, aes(fill = obs_A_Fi), color = NA) +
    scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
    labs(title = "Observed Total Floor Area (by Zone)", fill = "Total Floor Area (m²)") +
    theme_minimal()
  ggsave("project_log/calibration/03_ki-backward-calc/plots/observed_floor_area.png", width = 6, height = 5.5, dpi = 150)

  # r_bar_i として観測家賃を使う（単位を合わせる：円/m2 → 千円/m2）
  obs_rent_ki <- athome_df %>%
    mutate(r_bar_i_obs = avg_rent / 1000) %>%      # 円/m2 → 千円/m2
    dplyr::select(KEY_CODE, r_bar_i_obs)

  # ゾーン順序（dists0の行順）でデータを整理
  zone_ids <- rownames(dists0) %>% gsub("^mc_", "", .)

  ki_input <- tibble(KEY_CODE = zone_ids) %>%
    left_join(obs_floor_total, by = "KEY_CODE") %>%
    left_join(obs_rent_ki,     by = "KEY_CODE")

  cat("obs_A_Fi が存在するゾーン数:", sum(!is.na(ki_input$obs_A_Fi)), "\n")
  cat("r_bar_i_obs が存在するゾーン数:", sum(!is.na(ki_input$r_bar_i_obs)), "\n")

  # ---- uniroot で各ゾーンの k_i を逆算 ----
  k_i_estimated <- numeric(nz_res)

  for (i in seq_len(nz_res)) {

    r_bar_i_i  <- ki_input$r_bar_i_obs[i]
    G0_i_i     <- G0_i[i]
    obs_A_Fi_i <- ki_input$obs_A_Fi[i]

    if (is.na(obs_A_Fi_i) | is.na(r_bar_i_i) |
        obs_A_Fi_i <= 0   | r_bar_i_i <= 0   | G0_i_i <= 0) {
      k_i_estimated[i] <- 0.1
      next
    }

    # f(k_i) = A_Fi(k_i) - A_Fi_obs = 0 を解く
    # local() で環境を固定し、クロージャの問題を回避
    result <- local({
      r   <- r_bar_i_i
      G   <- G0_i_i
      obs <- obs_A_Fi_i

      f <- function(ki) {
        pg   <- gamma_1 / (1 - gamma_1)
        rg_i <- phi * (1 - gamma_1) *
                (r * gamma_0) ^ (1 / (1 - gamma_1)) *
                (gamma_1 / ki) ^ pg
        G_i  <- G * exp(theta_L * (rg_i - rr_a)) /
                 (1 + exp(theta_L * (rg_i - rr_a))) * phi_pub * phi
        A_Fi <- gamma_0 * (r * gamma_0 * gamma_1 / ki) ^ pg * G_i
        return(A_Fi - obs)
      }

      tryCatch(
        uniroot(f, interval = c(0.01, 0.5), tol = 1e-8),
        error = function(e) NULL
      )
    })

    k_i_estimated[i] <- if (!is.null(result)) result$root else 0.1
  }

  cat("解が求まったゾーン数:", sum(k_i_estimated != 0.1), "\n")
  cat("k_i_estimatedの範囲:", range(k_i_estimated), "\n")

  cat("k_i_estimated の範囲:", range(k_i_estimated), "\n")
  cat("uniroot 失敗（初期値0.1のまま）のゾーン数:",
      sum(k_i_estimated == 0.1), "\n")

  hist(k_i_estimated, main = "Distribution of Back-calculated k_i", xlab = "k_i")
  png("project_log/calibration/03_ki-backward-calc/plots/k_i_estimated_hist.png", width = 900, height = 750, res = 150)
  hist(k_i_estimated, main = "Distribution of Back-calculated k_i", xlab = "k_i")
  dev.off()


  # ==================================================================
  # ==== Step 2: 外れ値クリギング（旧L.900-1070, 04_ki-outlier-crop-kriging）====
  # ==================================================================
  library(gstat)
  library(sp)
  library(sf)

  # ---- クリギングの準備 ----

  # k_i_estimated とゾーンのKEY_CODEを対応づける
  zone_ids <- rownames(dists0) %>% gsub("^mc_", "", .)

  k_i_df <- tibble(
    KEY_CODE = zone_ids,
    k_i      = k_i_estimated,
    # uniroot成功したゾーン（=観測データあり）かどうかのフラグ
    has_data = (k_i_estimated != 0.1) & !is.na(k_i_estimated)
  )
  cat("has_data = TRUE のゾーン数:", sum(k_i_df$has_data), "\n") # OK, データあり341ゾーン

  # ---- 外れ値（上下5%）の判定 ----
  # 逆算に成功したゾーン（has_data==TRUE）のk_iの上下5%を外れ値とし、
  # クリギングの学習データからは除外して、代わりに補間対象に含める
  q_low  <- quantile(k_i_df$k_i[k_i_df$has_data], probs = 0.05, na.rm = TRUE)
  q_high <- quantile(k_i_df$k_i[k_i_df$has_data], probs = 0.95, na.rm = TRUE)

  k_i_df <- k_i_df %>%
    mutate(
      is_outlier    = has_data & (k_i < q_low | k_i > q_high),
      # クリギングの学習データとして使うか（外れ値は除く）
      use_for_krige = has_data & !is_outlier
    )

  cat("外れ値の範囲（上下5%）:", q_low, "-", q_high, "\n")
  cat("外れ値として除外したゾーン数:", sum(k_i_df$is_outlier), "\n")

  # key_code_sfにk_iを結合（空間情報を持たせる）
  k_i_sf <- key_code_sf %>%
    left_join(k_i_df, by = "KEY_CODE") %>%
    mutate(centroid = st_centroid(geometry))  # メッシュの重心座標

  # 重心座標を取り出す
  coords <- st_coordinates(st_centroid(k_i_sf))
  k_i_sf <- k_i_sf %>%
    mutate(x = coords[, 1], y = coords[, 2])

  # ---- 外れ値カット前のk_iの空間分布を確認（データなしゾーンは除く）----
  ggplot(k_i_sf %>% filter(has_data == TRUE)) +
    geom_sf(aes(fill = k_i), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", direction = -1, name = "k_i") +
    labs(title = "Spatial Distribution of k_i (Before Outlier Removal)") +
    theme_void()
  ggsave("project_log/calibration/04_ki-outlier-crop-kriging/plots/k_i_spatial_before_outlier.png", width = 6, height = 5.5, dpi = 150)

  # ---- 外れ値カット後（クリギング補完前）のk_iの空間分布を確認（データなしゾーンは除く）----
  ggplot(k_i_sf %>% filter(use_for_krige == TRUE)) +
    geom_sf(aes(fill = k_i), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", direction = -1, name = "k_i") +
    labs(title = "Spatial Distribution of k_i (After Outlier Removal, Before Kriging)") +
    theme_void()
  ggsave("project_log/calibration/04_ki-outlier-crop-kriging/plots/k_i_spatial_after_outlier_before_kriging.png", width = 6, height = 5.5, dpi = 150)

  # ---- 観測あり・なしに分ける ----
  # クリギングの学習データ：観測あり かつ 外れ値（上下5%）でないゾーン
  k_i_obs <- k_i_sf %>%
    filter(use_for_krige == TRUE, !is.na(k_i)) %>%
    st_drop_geometry()

  # 補間対象：観測なしゾーン + 外れ値として除外したゾーン
  k_i_pred <- k_i_sf %>%
    filter(use_for_krige == FALSE | is.na(k_i)) %>%
    st_drop_geometry()

  cat("観測あり（クリギングの入力、外れ値除く）:", nrow(k_i_obs), "ゾーン\n")
  cat("観測なし＋外れ値（補間対象）:", nrow(k_i_pred), "ゾーン\n")

  # ---- sp形式に変換（gstatが必要とする形式）----
  coordinates(k_i_obs)  <- ~x + y
  coordinates(k_i_pred) <- ~x + y

  # ---- バリオグラムの推定 ----
  # k_iの空間的ばらつきのパターンを学習する
  vgm_emp <- variogram(k_i ~ 1, data = k_i_obs)
  plot(vgm_emp, main = "Empirical Variogram")
  png("project_log/calibration/04_ki-outlier-crop-kriging/plots/variogram_empirical.png", width = 900, height = 750, res = 150)
  plot(vgm_emp, main = "Empirical Variogram")
  dev.off()

  # バリオグラムモデルをフィット（球面モデルが一般的だが、他のモデルも試せるようにしておく）
  # selected_vgm_model を変えるだけでモデルを切り替えられる
  # "Sph"=球面, "Exp"=指数, "Gau"=ガウシアン, "Mat"=マテルン, "Lin"=線形, "auto"=SSE最小のモデルを自動選択
  selected_vgm_model <- "Sph"

  vgm_candidates <- list(
    Sph = vgm(psill = var(k_i_obs$k_i), model = "Sph", range = 10000, nugget = 0),
    Exp = vgm(psill = var(k_i_obs$k_i), model = "Exp", range = 10000, nugget = 0),
    Gau = vgm(psill = var(k_i_obs$k_i), model = "Gau", range = 10000, nugget = 0),
    Mat = vgm(psill = var(k_i_obs$k_i), model = "Mat", range = 10000, nugget = 0, kappa = 0.5),
    Lin = vgm(psill = var(k_i_obs$k_i), model = "Lin", range = 10000, nugget = 0)
  )

  if (selected_vgm_model == "auto") {
    # 各モデルをフィットし、SSE（当てはまり誤差）が最小のものを自動選択
    vgm_comparison <- do.call(rbind, lapply(names(vgm_candidates), function(m) {
      fit <- fit.variogram(vgm_emp, model = vgm_candidates[[m]])
      data.frame(model = m, SSErr = attr(fit, "SSErr"))
    }))
    vgm_comparison <- vgm_comparison[order(vgm_comparison$SSErr), ]
    print(vgm_comparison)
    best_model <- vgm_comparison$model[1]
    cat("自動選択されたバリオグラムモデル:", best_model, "\n")
    vgm_fit <- fit.variogram(vgm_emp, model = vgm_candidates[[best_model]])
  } else {
    vgm_fit <- fit.variogram(vgm_emp, model = vgm_candidates[[selected_vgm_model]])
  }

  print(vgm_fit)
  plot(vgm_emp, vgm_fit, main = paste("Variogram Model Fit -", selected_vgm_model))
  png("project_log/calibration/04_ki-outlier-crop-kriging/plots/variogram_fit.png", width = 900, height = 750, res = 150)
  plot(vgm_emp, vgm_fit, main = paste("Variogram Model Fit -", selected_vgm_model))
  dev.off()

  # ---- クリギング補間 ----
  k_i_kriged <- krige(
    formula   = k_i ~ 1,      # 通常クリギング（トレンドなし）
    locations = k_i_obs,       # 観測データ（入力）
    newdata   = k_i_pred,      # 補間対象ゾーン
    model     = vgm_fit
  )

  cat("クリギング補間完了\n")
  cat("補間値の範囲:", range(k_i_kriged$var1.pred), "\n")

  # ---- 補間値をk_i_estimatedに反映 ----
  # 観測あり（外れ値除く）ゾーン：逆算値をそのまま使う
  # 観測なし・外れ値ゾーン：クリギング補間値を使う

  k_i_pred_df <- tibble(
    KEY_CODE = k_i_pred$KEY_CODE,
    k_i_kriged = pmax(k_i_kriged$var1.pred, 0.01)  # 負の値を防ぐ下限
  )

  k_i_final <- k_i_df %>%
    left_join(k_i_pred_df, by = "KEY_CODE") %>%
    mutate(
      k_i_final = case_when(
        use_for_krige == TRUE  ~ k_i,            # 観測あり（外れ値除く）：逆算値
        use_for_krige == FALSE ~ k_i_kriged,     # 観測なし・外れ値：クリギング補間値
        TRUE                    ~ 0.1             # それ以外：初期値
      )
    )

  # k_iのベクトルとして取り出す（dists0の行順）
  k_i_estimated_final <- k_i_final$k_i_final
  cat("最終的なk_iの範囲:", range(k_i_estimated_final, na.rm=TRUE), "\n")

  # ---- 外れ値カット後（クリギング補完後）の全ゾーンのk_iの空間分布を確認 ----
  k_i_map <- key_code_sf %>%
    left_join(k_i_final %>% dplyr::select(KEY_CODE, k_i_final, has_data),
              by = "KEY_CODE")

  ggplot(k_i_map) +
    geom_sf(aes(fill = k_i_final), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", direction = -1,
                          name = "k_i") +
    labs(title = "Spatial Distribution of k_i (After Outlier Removal and Kriging, All Zones)") +
    theme_void()
  ggsave("project_log/calibration/04_ki-outlier-crop-kriging/plots/k_i_spatial_after_kriging_all_zones.png", width = 6, height = 5.5, dpi = 150)

  # k_iをグローバル変数として固定
  # gapf_vj/caluculate_model_stateはグローバルのk_iを参照するため、この後すぐには
  # 使わないが、以降の緩和法ループの初期状態として一貫性を保つためにここでも反映しておく。
  k_i <<- k_i_estimated_final


  # ================================================================
  # ==== Step 3: k_iの逐次推定（緩和法, 旧L.1073-1195, 05_ki-relaxation）====
  # ================================================================
  # 発想：
  #   観測値 (Afi*, r_i*) と、現在のKで解いたモデル整合解 (Afi°, r_i°) を
  #   交互に使ってゾーンごとにKを再推定し、緩和（successive averaging）しながら
  #   Kが収束するまで繰り返す。
  #
  #   ・対象ゾーンは @home データが実際にあるゾーン（has_data==TRUE）のみ。
  #     クリギング補完値（k_i_estimated_final）は本手法では一切使わない。
  #   ・K0 は観測値 (Afi*, r_i*) から直接逆算した k_i_estimated（クリギング前）。
  #
  #   ① K_(t-1) でLUTI均衡を解き、モデル整合的な (Afi°, r_i°) を得る
  #   ② has_data==TRUEの各ゾーンiで、以下を同時に最小化するKを求める
  #        (Afi(K | r_i°) - Afi*_i)^2 + (r_i(K | Afi°) - r_i*_i)^2
  #      → 候補 K~_t
  #   ③ 緩和更新： K_t = alpha_relax * K_(t-1) + (1 - alpha_relax) * K~_t
  #   ④ Σ|K_t - K_(t-1)| < eps_relax となるまで①〜③を繰り返す（has_dataゾーンのみで集計）

  # ---- Kの探索範囲・緩和パラメータ ----
  k_lower_relax  <- 0.01
  k_upper_relax  <- 0.4
  alpha_relax    <- 0.5     # 緩和係数：K_(t-1)側の重み（大きいほど更新が保守的）
  eps_relax      <- 1e-3    # 収束判定：Σ|K_t - K_(t-1)|（has_dataゾーンのみ）
  max_iter_relax <- 80

  # ---- 観測ターゲット（ki_inputはdists0の行順=ゾーン順に整列済み）----
  Afi_star   <- ki_input$obs_A_Fi        # 観測：総床面積（=平均床面積×世帯数）
  r_star     <- ki_input$r_bar_i_obs     # 観測：家賃（千円/m2）

  # 対象ゾーン：@homeデータが実際にあるゾーンのみ（クリギング補完対象は除外）
  valid_zone <- k_i_df$has_data

  # ---- モデルの床面積関数 A_Fi(K, r)（eq.14, eq.15と同一）----
  model_A_Fi <- function(K, r, G0) {
    pg   <- gamma_1 / (1 - gamma_1)
    rg_i <- phi * (1 - gamma_1) * (r * gamma_0) ^ (1 / (1 - gamma_1)) * (gamma_1 / K) ^ pg
    G_i  <- G0 * exp(theta_L * (rg_i - rr_a)) / (1 + exp(theta_L * (rg_i - rr_a))) * phi_pub * phi
    gamma_0 * ((r * gamma_0 * gamma_1) / K) ^ pg * G_i
  }

  # ---- A_Fi(K, r) = A_target を r について解く（Kを固定した時の整合家賃）----
  model_r_from_AFi <- function(K, A_target, G0, r_interval = c(1e-3, 100)) {
    f <- function(r) model_A_Fi(K, r, G0) - A_target
    tryCatch(
      uniroot(f, interval = r_interval, tol = 1e-8)$root,
      error = function(e) NA_real_
    )
  }

  # ---- ゾーンiの目的関数：床面積式の残差^2 + 家賃式の残差^2 ----
  zone_objective <- function(K, r0, A0, G0, A_target, r_target) {
    term_A <- (model_A_Fi(K, r0, G0) - A_target) ^ 2
    r_hat  <- model_r_from_AFi(K, A0, G0)
    term_r <- if (is.na(r_hat)) 1e6 else (r_hat - r_target) ^ 2
    term_A + term_r
  }

  # ---- 反復（緩和法）本体 ----
  # K0：観測値から直接逆算した値（クリギング前）。has_data==FALSEのゾーンは
  #     uniroot失敗時のフォールバック値0.1のままだが、これらは③で更新対象外。
  K_current <- k_i_estimated

  for (t in seq_len(max_iter_relax)) {

    cat("=== k_i 逐次推定（緩和法） iteration", t, "===\n")

    # ① 現在のKでLUTI均衡を解き、モデル整合的な (Afi°, r_i°) を得る
    # gapf_vj/caluculate_model_stateはグローバルのk_iを参照するため<<-が必要
    # （通常のローカル代入だとgapf_vjから見えず、常に前回のKのままで解かれてしまう）
    k_i <<- K_current
    res_relax <- nleqslv(
      x       = v_start,
      fn      = gapf_vj,
      global  = "dbldog",
      control = list(ftol = 1e-8, xtol = 1e-8, maxit = 200)
    )
    state_relax <- caluculate_model_state(exp(res_relax$x))

    r0_vec <- state_relax$r_bar_i
    A0_vec <- rowSums(state_relax$a_fij_H * state_relax$l_i_j, na.rm = TRUE)

    # ② has_data==TRUEのゾーンのみ、Kの候補 K~_t を再推定
    K_tilde <- K_current
    for (i in which(valid_zone)) {
      opt <- optimize(
        zone_objective,
        interval = c(k_lower_relax, k_upper_relax),
        r0       = r0_vec[i],
        A0       = A0_vec[i],
        G0       = G0_i[i],
        A_target = Afi_star[i],
        r_target = r_star[i]
      )
      K_tilde[i] <- opt$minimum
    }

    # ③ 緩和更新（successive averaging）： K_t = alpha*K_(t-1) + (1-alpha)*K~_t
    #    has_data==FALSEのゾーンはK_tilde==K_currentのまま＝更新されない
    K_new <- alpha_relax * K_current + (1 - alpha_relax) * K_tilde

    # ④ 収束判定（has_dataゾーンのみで集計）
    diff_relax <- sum(abs(K_new[valid_zone] - K_current[valid_zone]), na.rm = TRUE)
    cat("  Sigma|K_t - K_(t-1)| (has_dataゾーンのみ) =", diff_relax, "\n")

    K_current <- K_new

    if (diff_relax < eps_relax) {
      cat("収束しました（iteration", t, "）\n")
      break
    }
  }

  k_i_relax_final <- K_current   # has_data==TRUEのゾーンのみ更新済み、他は0.1のまま
  # グローバルのk_iを更新（以降のモデル実行・gapf_vj/caluculate_model_stateから見えるように）
  k_i <<- k_i_relax_final

  cat("\n最終的なk_i（緩和法・has_dataゾーンのみ）の範囲:\n")
  print(summary(k_i_relax_final[valid_zone]))
  hist(k_i_relax_final[valid_zone],
       main = "Distribution of k_i After Iterative Estimation (Relaxation Method, has_data Zones Only)",
       xlab = "k_i")
  png("project_log/calibration/05_ki-relaxation/plots/k_i_relax_hist.png", width = 900, height = 750, res = 150)
  hist(k_i_relax_final[valid_zone],
       main = "Distribution of k_i After Iterative Estimation (Relaxation Method, has_data Zones Only)",
       xlab = "k_i")
  dev.off()


  # ======================================================================
  # ==== Step 4: 緩和法k_iをクリギングで全ゾーンへ拡張（旧L.1197-1311）====
  # ======================================================================
  # 反復法（緩和法）ではhas_data==TRUEのゾーンのみKを更新しており、
  # それ以外のゾーンはk_i_estimatedのフォールバック値(0.1)のままになっている。
  # ここでは有効ゾーン（has_data==TRUE）の緩和法推定値をそのまま学習データとして
  # 空間クリギングを行い、それ以外のゾーン（データなし）のkappaを補完する。
  # gstat/sp/sfはStep 2で読み込み済み。

  # ---- 緩和法k_iをデータフレーム化（zone_idsはStep 1と同じdists0行順）----
  k_i_relax_df <- tibble(
    KEY_CODE = zone_ids,
    k_i      = k_i_relax_final,
    has_data = valid_zone   # 反復法で実際に更新したゾーンか
  )
  cat("[緩和法k_i] has_data==TRUEのゾーン数:", sum(k_i_relax_df$has_data), "\n")

  # ---- 空間情報の付与（重心座標）----
  k_i_relax_sf <- key_code_sf %>%
    left_join(k_i_relax_df, by = "KEY_CODE") %>%
    mutate(centroid = st_centroid(geometry))

  coords_relax <- st_coordinates(st_centroid(k_i_relax_sf))
  k_i_relax_sf <- k_i_relax_sf %>%
    mutate(x = coords_relax[, 1], y = coords_relax[, 2])

  # ---- 観測あり（has_data==TRUE）／なし（has_data==FALSE）に分割 ----
  k_i_relax_obs <- k_i_relax_sf %>%
    filter(has_data == TRUE, !is.na(k_i)) %>%
    st_drop_geometry()

  k_i_relax_pred <- k_i_relax_sf %>%
    filter(has_data == FALSE | is.na(k_i)) %>%
    st_drop_geometry()

  cat("[緩和法k_i] クリギング学習データ:", nrow(k_i_relax_obs), "ゾーン\n")
  cat("[緩和法k_i] 補完対象（データなし）:", nrow(k_i_relax_pred), "ゾーン\n")

  coordinates(k_i_relax_obs)  <- ~x + y
  coordinates(k_i_relax_pred) <- ~x + y

  # ---- バリオグラム推定・フィット（球面モデルが一般的だが、他のモデルも試せるようにしておく）----
  # selected_vgm_model_relax を変えるだけでモデルを切り替えられる
  # "Sph"=球面, "Exp"=指数, "Gau"=ガウシアン, "Mat"=マテルン, "Lin"=線形, "auto"=SSE最小のモデルを自動選択
  selected_vgm_model_relax <- "auto"

  vgm_emp_relax <- variogram(k_i ~ 1, data = k_i_relax_obs)

  vgm_candidates_relax <- list(
    Sph = vgm(psill = var(k_i_relax_obs$k_i), model = "Sph", range = 10000, nugget = 0),
    Exp = vgm(psill = var(k_i_relax_obs$k_i), model = "Exp", range = 10000, nugget = 0),
    Gau = vgm(psill = var(k_i_relax_obs$k_i), model = "Gau", range = 10000, nugget = 0),
    Mat = vgm(psill = var(k_i_relax_obs$k_i), model = "Mat", range = 10000, nugget = 0, kappa = 0.5),
    Lin = vgm(psill = var(k_i_relax_obs$k_i), model = "Lin", range = 10000, nugget = 0)
  )

  if (selected_vgm_model_relax == "auto") {
    # 各モデルをフィットし、SSE（当てはまり誤差）が最小のものを自動選択
    vgm_comparison_relax <- do.call(rbind, lapply(names(vgm_candidates_relax), function(m) {
      fit <- fit.variogram(vgm_emp_relax, model = vgm_candidates_relax[[m]])
      data.frame(model = m, SSErr = attr(fit, "SSErr"))
    }))
    vgm_comparison_relax <- vgm_comparison_relax[order(vgm_comparison_relax$SSErr), ]
    print(vgm_comparison_relax)
    best_model_relax <- vgm_comparison_relax$model[1]
    cat("自動選択されたバリオグラムモデル（緩和法）:", best_model_relax, "\n")
    vgm_fit_relax <- fit.variogram(vgm_emp_relax, model = vgm_candidates_relax[[best_model_relax]])
  } else {
    vgm_fit_relax <- fit.variogram(vgm_emp_relax, model = vgm_candidates_relax[[selected_vgm_model_relax]])
  }

  print(vgm_fit_relax)
  plot(vgm_emp_relax, vgm_fit_relax, main = paste("Variogram Model Fit (Relaxation Method k_i) -", selected_vgm_model_relax))
  png("project_log/calibration/05_ki-relaxation/plots/variogram_fit_relax.png", width = 900, height = 750, res = 150)
  plot(vgm_emp_relax, vgm_fit_relax, main = paste("Variogram Model Fit (Relaxation Method k_i) -", selected_vgm_model_relax))
  dev.off()

  # ---- クリギング補間 ----
  k_i_relax_kriged <- krige(
    formula   = k_i ~ 1,
    locations = k_i_relax_obs,
    newdata   = k_i_relax_pred,
    model     = vgm_fit_relax
  )
  cat("[緩和法k_i] クリギング補間完了\n")
  cat("[緩和法k_i] 補間値の範囲:", range(k_i_relax_kriged$var1.pred), "\n")

  # ---- 補間値を反映して全ゾーン分のk_iベクトルを作成 ----
  k_i_relax_pred_df <- tibble(
    KEY_CODE   = k_i_relax_pred$KEY_CODE,
    k_i_kriged = pmax(k_i_relax_kriged$var1.pred, 0.01)   # 負値を防ぐ下限
  )

  k_i_relax_final_all <- k_i_relax_df %>%
    left_join(k_i_relax_pred_df, by = "KEY_CODE") %>%
    mutate(
      k_i_final = case_when(
        has_data == TRUE  ~ k_i,          # 有効ゾーン：緩和法の推定値
        has_data == FALSE ~ k_i_kriged,   # データなし：クリギング補間値
        TRUE               ~ 0.1
      )
    )

  # k_iのベクトルとして取り出す（dists0の行順）
  k_i_relax_kriged_final <- k_i_relax_final_all$k_i_final
  cat("[緩和法k_i] クリギング後の最終k_i範囲:", range(k_i_relax_kriged_final, na.rm = TRUE), "\n")

  k_i_relax_map <- key_code_sf %>%
    left_join(k_i_relax_final_all %>% dplyr::select(KEY_CODE, k_i_final, has_data),
              by = "KEY_CODE")

  ggplot(k_i_relax_map) +
    geom_sf(aes(fill = k_i_final), color = "gray50", linewidth = 0.1) +
    scale_fill_viridis_c(option = "plasma", direction = -1, name = "k_i") +
    labs(title = "Spatial Distribution of Relaxation-Method k_i (After Kriging, All Zones)") +
    theme_void()
  ggsave("project_log/calibration/05_ki-relaxation/plots/k_i_relax_spatial_after_kriging.png", width = 6, height = 5.5, dpi = 150)


  # ==============================================================================
  # ==== Step 5: 緩和法k_i（クリギング後）の検証：実データとの比較（旧L.1314-1532）====
  # ==============================================================================
  # alpha_a, gamma_0, gamma_1, theta_H, theta_L はベースライン値のまま、
  # k_iのみ「逐次推定（緩和法）＋クリギング補完」で求めた値（k_i_relax_kriged_final）を
  # 使ってLUTIループを収束まで実行し、実データ（世帯数・家賃・床面積）と比較する。
  #
  # 元コードはここで交通配分の作業用距離行列としてグローバルのdists0.roadを直接
  # 書き換えていたが（旧L.1334, 1380-1385）、これは単発の検証用再計算であり、この関数の
  # 呼び出しが本体スクリプトの他の場所のdists0.roadの状態に影響を与えるべきではないため、
  # ここではローカル変数 dists_road_local に置き換えている（<<-は使わない）。

  library(patchwork)
  library(wCorr)

  make_map <- function(data, fill_var, title, option, limits) {
    ggplot(data) +
      geom_sf(aes(fill = .data[[fill_var]]), color = "gray50", linewidth = 0.1) +
      scale_fill_viridis_c(option = option, direction = -1,
                           limits = limits, oob = scales::squish,
                           labels = scales::label_comma()) +
      labs(title = title) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = NA))
  }

  # グローバルのk_iを最終値に更新（この後の検証ループのgapf_vj/caluculate_model_stateから
  # 見えるようにするため<<-が必要）。この関数の最終的な返り値・副作用でもある。
  k_i <<- k_i_relax_kriged_final
  dists_road_local <- dists0   # 交通配分を毎回同じ条件から独立に収束させる（ローカルのみ）

  flag_kirelax  <- 0
  alpha_kirelax <- 0.5
  ii_kirelax    <- 1
  tt0_kirelax   <- proc.time()

  while (flag_kirelax == 0) {
    V.car  <- para[1] * dists_road_local + para[2] + para[4] * parkPrice / 2
    den    <- exp(V.bike) + exp(V.car) + exp(V.rail)
    P.bike <- exp(V.bike) / den
    P.car  <- exp(V.car)  / den
    P.rail <- exp(V.rail) / den
    agcc   <- (P.bike * V.bike + P.car * V.car + P.rail * V.rail) / para[1]
    c_ij   <- agcc * 1.600
    # disposable_income_ijもgapf_vj/caluculate_model_stateがグローバルから参照するため、
    # k_iと同じ理由で<<-が必要（このループはdists_road_localを毎回更新しながら
    # 独立に収束させる検証用の再計算であり、更新後の値をモデル関数から見せる必要がある）。
    disposable_income_ij <<- pmax(-c_ij + omega_j_matrix, 0)

    result_nleqslv_kirelax <- nleqslv(
      x = v_start, fn = gapf_vj,
      global = "dbldog",
      control = list(ftol = 1e-8, xtol = 1e-8, maxit = 200)
    )
    v_equilibrium_kirelax <- result_nleqslv_kirelax$x %>% exp()
    final_state_kirelax   <- caluculate_model_state(v_equilibrium_kirelax)

    ODD.car <- final_state_kirelax$l_i_j * P.car
    trips   <- as.data.frame.table(ODD.car, responseName = "demand")
    names(trips) <- c("from", "to", "demand")

    traffic_kirelax <- assign_traffic(
      Graph     = sgr,
      from      = trips$from,
      to        = trips$to,
      demand    = trips$demand,
      max_gap   = 1e-2,
      algorithm = "bfw",
      verbose   = FALSE
    )
    sgr2_kirelax <- makegraph(
      df       = traffic_kirelax$data[, c("from", "to", "cost")],
      directed = TRUE,
      capacity = 1e4,
      alpha    = alpha,
      beta     = beta,
      coords   = nodes
    )
    dists_road_local_b <- dists_road_local
    dists1.road   <- get_distance_matrix(sgr2_kirelax,
                                         from      = zones,
                                         to        = centers,
                                         algorithm = "mch")
    dists_road_local <- alpha_kirelax * dists1.road + (1 - alpha_kirelax) * dists_road_local_b
    diff_kirelax <- sum((dists_road_local_b - dists_road_local) ^ 2)
    cat("[緩和法k_i・クリギング後 検証] ii=", ii_kirelax, ", diff=", diff_kirelax, "\n")
    ii_kirelax <- ii_kirelax + 1
    if (diff_kirelax < 100 | ii_kirelax > 1e3) flag_kirelax <- 1
  }
  proc.time() - tt0_kirelax


  ## 世帯数の比較
  est_hh_df_kirelax <- tibble(
    KEY_CODE = rownames(final_state_kirelax$l_i_j) %>% gsub("^mc_", "", .),
    est_hh   = rowSums(final_state_kirelax$l_i_j, na.rm = TRUE)
  )

  hhC_kirelax <- key_code_sf %>%
    left_join(est_hh_df_kirelax, by = "KEY_CODE") %>%
    left_join(household, by = "KEY_CODE") %>%
    mutate(obs_hh = tidyr::replace_na(obs_hh, 0))

  r_hh_weighted_kirelax <- weightedCorr(
    x = hhC_kirelax$obs_hh,
    y = hhC_kirelax$est_hh,
    weights = hhC_kirelax$obs_hh,
    method = "Pearson"
  )
  r2_hh_weighted_kirelax <- r_hh_weighted_kirelax^2
  r2_hh_kirelax <- (cor(hhC_kirelax$obs_hh, hhC_kirelax$est_hh, use = "complete.obs"))^2
  cat("[緩和法k_i・クリギング後] 世帯数 重みつきR²:", r2_hh_weighted_kirelax, "\n")

  plot(hhC_kirelax$obs_hh, hhC_kirelax$est_hh,
       xlab = "Observed: Number of Households", ylab = "Model: Number of Households (Relaxation k_i, After Kriging)")
  abline(0, 1, col = "red")
  legend("topleft", legend = bquote(R^2 == .(sprintf("%.3f", r2_hh_kirelax))), bty = "n", cex = 1.1)
  png("project_log/calibration/05_ki-relaxation/plots/kirelax_hh_scatter.png", width = 900, height = 750, res = 150)
  plot(hhC_kirelax$obs_hh, hhC_kirelax$est_hh,
       xlab = "Observed: Number of Households", ylab = "Model: Number of Households (Relaxation k_i, After Kriging)")
  abline(0, 1, col = "red")
  legend("topleft", legend = bquote(R^2 == .(sprintf("%.3f", r2_hh_kirelax))), bty = "n", cex = 1.1)

  dev.off()

  p_hh_obs_kirelax   <- make_map(hhC_kirelax, "obs_hh", "Observed: Number of Households",           "magma", c(0, 25000))
  p_hh_model_kirelax <- make_map(hhC_kirelax, "est_hh", "Model: Number of Households (Relaxation k_i, After Kriging)", "magma", c(0, 25000))
  print(p_hh_obs_kirelax + p_hh_model_kirelax)
  ggsave("project_log/calibration/05_ki-relaxation/plots/kirelax_hh_maps.png",
         plot = p_hh_obs_kirelax + p_hh_model_kirelax, width = 9, height = 5, dpi = 150)


  ## 家賃の比較
  est_rent_df_kirelax <- tibble(
    KEY_CODE = names(final_state_kirelax$r_bar_i) %>% gsub("^mc_", "", .),
    est_rent = as.numeric(final_state_kirelax$r_bar_i) * 1000
  )

  rentC_kirelax <- key_code_sf %>%
    left_join(athome_df, by = "KEY_CODE") %>%
    left_join(est_rent_df_kirelax, by = "KEY_CODE") %>%
    left_join(household, by = "KEY_CODE")

  rentC_kirelax_flt <- rentC_kirelax %>%
    st_drop_geometry() %>%
    filter(!is.na(avg_rent), !is.na(est_rent), !is.na(obs_hh),
           avg_rent > 0, est_rent > 0, obs_hh > 0)

  r_rent_weighted_kirelax <- weightedCorr(
    x = rentC_kirelax_flt$avg_rent,
    y = rentC_kirelax_flt$est_rent,
    weights = rentC_kirelax_flt$obs_hh,
    method = "Pearson"
  )
  r2_rent_weighted_kirelax <- r_rent_weighted_kirelax^2
  r2_rent_kirelax <- (cor(rentC_kirelax_flt$avg_rent, rentC_kirelax_flt$est_rent))^2
  cat("[緩和法k_i・クリギング後] 家賃 重みつきR²:", r2_rent_weighted_kirelax, "\n")

  plot(rentC_kirelax_flt$avg_rent, rentC_kirelax_flt$est_rent,
       xlab = "Observed: Rent", ylab = "Model: Rent (Relaxation k_i, After Kriging)")
  abline(0, 1, col = "red")
  legend("topleft", legend = bquote(R^2 == .(sprintf("%.3f", r2_rent_kirelax))), bty = "n", cex = 1.1)
  png("project_log/calibration/05_ki-relaxation/plots/kirelax_rent_scatter.png", width = 900, height = 750, res = 150)

  plot(rentC_kirelax_flt$avg_rent, rentC_kirelax_flt$est_rent,
       xlab = "Observed: Rent", ylab = "Model: Rent (Relaxation k_i, After Kriging)")
  abline(0, 1, col = "red")
  legend("topleft", legend = bquote(R^2 == .(sprintf("%.3f", r2_rent_kirelax))), bty = "n", cex = 1.1)
  dev.off()

  p_rent_obs_kirelax   <- make_map(rentC_kirelax, "avg_rent", "Observed: Rent",           "plasma", c(0, 3200))
  p_rent_model_kirelax <- make_map(rentC_kirelax, "est_rent", "Model: Rent (Relaxation k_i, After Kriging)", "plasma", c(0, 3200))
  print(p_rent_obs_kirelax + p_rent_model_kirelax)
  ggsave("project_log/calibration/05_ki-relaxation/plots/kirelax_rent_maps.png",
         plot = p_rent_obs_kirelax + p_rent_model_kirelax, width = 9, height = 5, dpi = 150)


  ## 床面積の比較
  est_ar_vec_kirelax <- rowSums(final_state_kirelax$a_fij_H * final_state_kirelax$l_i_j, na.rm = TRUE) /
    rowSums(final_state_kirelax$l_i_j, na.rm = TRUE)
  est_ar_df_kirelax <- tibble(
    KEY_CODE = rownames(final_state_kirelax$a_fij_H) %>% gsub("^mc_", "", .),
    est_ar   = est_ar_vec_kirelax
  )

  arC_kirelax <- key_code_sf %>%
    left_join(athome_ar, by = "KEY_CODE") %>%
    left_join(est_ar_df_kirelax, by = "KEY_CODE") %>%
    left_join(household, by = "KEY_CODE")

  arC_kirelax_flt <- arC_kirelax %>%
    st_drop_geometry() %>%
    filter(!is.na(avg_room_ar), !is.na(est_ar), !is.na(obs_hh),
           avg_room_ar > 0, est_ar > 0, obs_hh > 0)

  r_ar_weighted_kirelax <- weightedCorr(
    x = arC_kirelax_flt$avg_room_ar,
    y = arC_kirelax_flt$est_ar,
    weights = arC_kirelax_flt$obs_hh,
    method = "Pearson"
  )
  r2_ar_weighted_kirelax <- r_ar_weighted_kirelax^2
  r2_ar_kirelax <- (cor(arC_kirelax_flt$avg_room_ar, arC_kirelax_flt$est_ar))^2
  cat("[緩和法k_i・クリギング後] 床面積 重みつきR²:", r2_ar_weighted_kirelax, "\n")

  plot(arC_kirelax_flt$avg_room_ar, arC_kirelax_flt$est_ar,
       xlab = "Observed: Floor Area", ylab = "Model: Floor Area (Relaxation k_i, After Kriging)")
  abline(0, 1, col = "red")
  legend("topleft", legend = bquote(R^2 == .(sprintf("%.3f", r2_ar_kirelax))), bty = "n", cex = 1.1)
  png("project_log/calibration/05_ki-relaxation/plots/kirelax_ar_scatter.png", width = 900, height = 750, res = 150)
  plot(arC_kirelax_flt$avg_room_ar, arC_kirelax_flt$est_ar,
       xlab = "Observed: Floor Area", ylab = "Model: Floor Area (Relaxation k_i, After Kriging)")
  abline(0, 1, col = "red")
  legend("topleft", legend = bquote(R^2 == .(sprintf("%.3f", r2_ar_kirelax))), bty = "n", cex = 1.1)
  dev.off()

  p_ar_obs_kirelax   <- make_map(arC_kirelax, "avg_room_ar", "Observed: Floor Area",           "viridis", c(0, 200))
  p_ar_model_kirelax <- make_map(arC_kirelax, "est_ar",      "Model: Floor Area (Relaxation k_i, After Kriging)", "viridis", c(0, 200))
  print(p_ar_obs_kirelax + p_ar_model_kirelax)
  ggsave("project_log/calibration/05_ki-relaxation/plots/kirelax_ar_maps.png",
         plot = p_ar_obs_kirelax + p_ar_model_kirelax, width = 9, height = 5, dpi = 150)


  # ---- 決定係数（R²）の算出（緩和法k_i：世帯数・家賃・床面積）----
  # r2_hh_kirelax / r2_rent_kirelax / r2_ar_kirelax は各散布図の直前で計算済み

  cat("[緩和法k_i・クリギング後] 決定係数（R²）\n")
  cat("  世帯数：", r2_hh_kirelax,   "\n")
  cat("  家賃　：", r2_rent_kirelax, "\n")
  cat("  床面積：", r2_ar_kirelax,   "\n")

  # ---- 関数の返り値 ----
  # グローバルのk_iは上のStep 5冒頭で既にk_i_relax_kriged_finalに更新済み（<<-）。
  # 呼び出し元にも同じ値を明示的に返しておく。
  return(invisible(k_i_relax_kriged_final))
}


#' 03→05のk_i解析的推定を、本体スクリプトのactive_calibration切り替えで
#' 選べる形式（calib_result）として保存する
#'
#' このモジュールはk_iしか推定しない。5変数（alpha_a, gamma_0, gamma_1, theta_H, theta_L）は
#' ここでは較正せず、とりあえず project_log/calibration/01_baseline-5param-ga/ga_result.RData
#' の値を借用し、k_iだけをこの関数で推定した値に差し替えた組み合わせとして保存する。
#' そのため「5変数とk_iを同時に較正した結果」ではない点に注意（fitnessは比較不能なのでNAにする）。
#' 将来、5変数側もこのk_iを前提に再較正したくなったら、その時に本来のGA較正を実装して
#' このparams借用を置き換える。
#'
#' @param ... estimate_ki_analytical()と同じ引数一式
#' @param baseline_rdata 5変数を借用する先（既定: 01_baseline-5param-gaのキャッシュ）
#' @return calib_result（本体スクリプトのload_calibration_result()と同じ形式）
calibrate_ki_relaxation <- function(key_code_sf, dists0, G0_i, phi, phi_pub, rr_a,
                                     gamma_0, gamma_1, theta_L, v_start, nz_res,
                                     sgr, nodes, zones, centers, alpha, beta,
                                     V.bike, V.rail, para, parkPrice,
                                     caluculate_model_state, gapf_vj,
                                     assign_traffic, makegraph, get_distance_matrix,
                                     baseline_rdata = "project_log/calibration/01_baseline-5param-ga/ga_result.RData") {

  k_i_vec <- estimate_ki_analytical(
    key_code_sf = key_code_sf, dists0 = dists0, G0_i = G0_i, phi = phi,
    phi_pub = phi_pub, rr_a = rr_a, gamma_0 = gamma_0, gamma_1 = gamma_1,
    theta_L = theta_L, v_start = v_start, nz_res = nz_res,
    sgr = sgr, nodes = nodes, zones = zones, centers = centers,
    alpha = alpha, beta = beta, V.bike = V.bike, V.rail = V.rail,
    para = para, parkPrice = parkPrice,
    caluculate_model_state = caluculate_model_state, gapf_vj = gapf_vj,
    assign_traffic = assign_traffic, makegraph = makegraph,
    get_distance_matrix = get_distance_matrix
  )

  if (!file.exists(baseline_rdata)) {
    stop(
      "05_ki-relaxationの5変数は01_baseline-5param-gaを借用する仕様のため、\n",
      baseline_rdata, " が必要です。先に01を実行してキャッシュを作ってください。"
    )
  }
  e <- new.env()
  load(baseline_rdata, envir = e)
  params <- e$calib_result$params  # 借用（このモジュールでは5変数を推定していない）

  calib_result <- list(
    params    = params,
    k_i       = k_i_vec,
    fitness   = NA,   # 5変数とk_iを同時に較正した結果ではないため比較不能
    ga_result = NULL,
    note      = "5変数は01_baseline-5param-gaを借用（暫定）。k_iのみ03-05の解析的推定に置き換え。"
  )
  save(calib_result, file = "project_log/calibration/05_ki-relaxation/ga_result.RData")

  calib_result
}

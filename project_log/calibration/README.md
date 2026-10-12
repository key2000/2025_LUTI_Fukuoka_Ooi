# project_log/calibration/

キャリブレーションの各試行を1フォルダ=1試行で記録する場所。

## 新しい試行を追加する手順

1. `project_log/calibration/` の下に `NN_短い名前/` というフォルダを作る（NNは表示順の連番、なくてもよい）。
   例: `03_ki-backward-calc/`
2. その中に `meta.md` を作る（下のテンプレート参照）。
3. その試行で見せたいプロットを `plots/` に保存する（`ggsave()` や `png()`/`dev.off()` でそのまま保存すればよい）。

## 計算結果のキャッシュ（`ga_result.RData`）

GAなど計算に時間がかかる較正の結果は、各試行フォルダ内に `ga_result.RData` として保存する
（例: `02_ga-800dim/ga_result.RData`）。`meta.md` 本文に「## 計算結果のキャッシュ」の見出しを立て、
読み込み方法と再現に必要な設定を記載する。

- `.RData` は `.gitignore`（`*.RData`）対象で、ローカルキャッシュ専用。Gitでは追跡しない。
- 再計算を避けたいだけなら、ファイル名を増やしてバックアップを作るのではなく、この場所に上書き/追記する。
- メインスクリプトから読み込む場合の形式（`calib_result`）は `scripts/calibration/README.md` を参照。

## meta.md テンプレート

機械判定に使う項目（`order`/`date`/`title`/`verdict`/`paper`/`paper_ref`/`plot`）だけを
YAMLフロントマターに残し、手法・パラメータ・指標・メモは本文のMarkdownに書く。
本文は見出し構成が自由なので、数式（LaTeX風表記でもコードブロックでもよい）やコード断片を
そのまま書ける（旧 `meta.yml` は `method:`/`params:`/`metrics:`/`note:` が1行の文字列で
読みにくく、数式を書く場所もなかったため `meta.md` に移行した）。

```markdown
---
order: 3                          # 表示順。省略した場合は date 順になる
date: "2026-07-07"
title: "k_i の解析的逆算に転換"
verdict: adopted                  # adopted / rejected / in_progress のいずれか（試行の中での判定）
paper: none                       # 論文採否。none（既定）/ adopted。採用はユーザーが判定する
# paper_ref: paper/ch3_method.md#3.3  # paper: adopted のときだけ、paper/ 内の書き込み先を記す
plot: plots/ki_backward_hist.png  # この試行を代表する画像1枚（相対パス）
---

## 手法

GAでk_iを探す代わりに、@homeデータ（観測家賃・平均床面積×世帯数）を
目標値として uniroot でゾーンごとに直接解く。

（数式・コードブロックなど自由に書いてよい）

## パラメータ

- 探索/推定方法: uniroot
- 対象ゾーン数: 847

## 指標

- 解が求まったゾーン数: 341
- r2_hh: 0.81

## メモ

データがないゾーンは初期値0.1のまま残し、後続のクリギングで補間する前提。
```

- `verdict` は `adopted`（採用）/ `rejected`（不採用）/ `in_progress`（進行中）の3種類。
  これは**較正の試行の中での判定**であり、論文に載せるかどうかとは別。
- `paper` は論文採否で、`none`（既定）/ `adopted` の2種類。`adopted` にできるのは、ユーザーが
  「論文に採用」と判定したときだけ。記入は `paper/` への昇格と同時に、スキル `promote-to-paper` が行う
  （`paper/README.md`）。`verdict: adopted` でも `paper: none` のことは普通にある。
  論文採用済みの試行を一覧するには `grep -l "^paper: adopted" project_log/calibration/*/meta.md`。
- `plot` は1枚だけ。複数見せたい場合は先に `magick` などで1枚に合成してから指定する。
- 本文の見出し（`## 手法` / `## パラメータ` / `## 指標` / `## メモ`）は目安。試行によっては
  `## 計算結果のキャッシュ`（`ga_result.RData` の再現方法）や `## 注意点` を追加してよい。

## scripts/Fukuoka_LUTI_model_260617.R が自動保存するプロット

`03_ki-backward-calc` 〜 `05_ki-relaxation` に対応する箇所には、実行するとその場で
`project_log/calibration/0N_.../plots/` に画像を書き出すコードを埋め込んである
（`ggsave()` または `png()`/`dev.off()`。元のプロット呼び出しの直後にそのまま追加してあるだけなので、
画面表示は今まで通り）。スクリプトをPositronで実行すれば、以下のファイルが自動的に増える。

**03_ki-backward-calc/plots/**
- `athome_sample_reliability.png` — @homeサンプル数によるゾーン判定
- `observed_floor_area.png` — 観測総床面積（ゾーンごと）
- `k_i_estimated_hist.png` — 逆算したk_iの分布

**04_ki-outlier-crop-kriging/plots/**
- `k_i_spatial_before_outlier.png` / `k_i_spatial_after_outlier_before_kriging.png`
- `variogram_empirical.png` / `variogram_fit.png`
- `k_i_spatial_after_kriging_all_zones.png`
- `preGA_hh_scatter.png` / `preGA_hh_maps.png`（世帯数の実データ×モデル比較）
- `preGA_rent_scatter.png` / `preGA_rent_maps.png`（家賃）
- `preGA_ar_scatter.png` / `preGA_ar_maps.png`（床面積）
- `preGA_floor_area_error_map.png`（床面積誤差の空間分布）

**05_ki-relaxation/plots/**
- `k_i_relax_hist.png` — 緩和法後のk_i分布（has_dataゾーンのみ）
- `variogram_fit_relax.png`
- `k_i_relax_spatial_after_kriging.png`
- `kirelax_hh_scatter.png` / `kirelax_hh_maps.png`
- `kirelax_rent_scatter.png` / `kirelax_rent_maps.png`
- `kirelax_ar_scatter.png` / `kirelax_ar_maps.png`

上記のうち見せたい1枚（今は `plots/formula.png` を指している）を、各 `meta.md` の `plot:` に
指定しておく。

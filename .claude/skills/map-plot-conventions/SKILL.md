---
name: map-plot-conventions
description: このLUTIプロジェクトで地図プロット（ggplot2のマップ、シナリオ間差分マップ、make_map()・make_diff_map2()などの地図関数、ggsave()でのPNG保存）を新規作成・修正するときに使う。差分マップのスケール上下限の決め方と、PNGの背景色の指定ルールを扱う。
---

# 地図プロットの規約

`scripts/Fukuoka_LUTI_model_260617.R` で地図を描くときの、2つの決まり事。

## 1. シナリオ間差分マップのスケールは `sym_limits()` で決める

`シナリオ比較####` セクションの差分マップでは、`c(-500, 300)` のようなキリのいい固定値を
スケールの上下限に使わない。固定値は実データの分布とずれやすく、実際の差分がスケール中央に
偏って発色が薄くなり、視認性が落ちるため。

代わりに、同セクション内で定義されている `sym_limits()` を使う。比較対象シナリオ（B・C）の
差分値の絶対値の最大値を求め、その値を `c(-lim, lim)` として両シナリオ共通のスケールに使う。

```r
hh_lim <- sym_limits(diff_household, c("diff_household_01", "diff_household_02"))
```

新しい指標の差分マップを追加するときも、このパターンに従う。

## 2. `theme_void()` の地図は背景色を明示する

`make_map()`、`make_diff_map2()` など `theme_void()` を使う地図プロット関数は、背景関連の要素も
`element_blank()` になる。明示的に次を追加しないと、`ggsave()` したPNGの背景が透明になる。

```r
theme(plot.background = element_rect(fill = "white", color = NA))
```

透明のままだと、資料に貼ったときに背景色を拾って見た目が崩れる。
新しい地図プロット関数を追加するときも、このパターンに従う。

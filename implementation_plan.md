# Implementation Plan: Refactor Scenario Analysis (L1223-)

**Date**: 2026-06-05  
**Objective**: Replace lines 1223-1875+ with refactored scenario analysis code  
**Status**: Plan Phase - Awaiting Approval

---

## Current State Analysis

### Issues in Current File (L1223-1372)
1. **Lines 1350-1372**: Contain corrupted duplicate `while` loop skeletons with placeholder comments
2. **`run_scenario()` function** (L1313-1348):
   - Uses `for` loop (should be converted to `while`)
   - Missing `P.bike` in return value
   - Uses `<<-` for global variables (should pass as parameters per user decision)
3. **Scenario definitions fragmented**:
   - scn0 setup: L1224-1238
   - scn1 setup: L1247-1260 (currently disabled with `if(F)`)
   - scn2 setup: L1263-1280
   - omega_j adjustment: L1284-1311 (only for scn1, not scn2)

### Data Dependencies (From Language Session)
Available in memory:
- `result_nleqslv` → `scn0_v` (base scenario utilities)
- `L_j_hat`, `omega_j_matrix`, `disposable_income_ij`, `c_ij` (global state)
- `dists0.road`, `dists0` (distance matrices)
- `gapf_vj()`, `caluculate_model_state()`, `assign_traffic()`, etc. (helper functions)
- `para`, `V.bike`, `V.rail`, `parkPrice`, `nz_res` (model parameters)

---

## Refactoring Steps (Detailed Implementation)

### **Step 1: Base Scenario Setup (L1223-1238)**

**Code to write**:
```r
#シナリオ分析 ####

# === Base Scenario (scn0) Setup ===
scn0_v <- result_nleqslv$x  
scn0_state <- caluculate_model_state(exp(scn0_v))

# Save scn0 mode shares
V.car_scn0  <- para[1] * dists0.road + para[2] + para[4] * parkPrice / 2
den_scn0    <- exp(V.bike) + exp(V.car_scn0) + exp(V.rail)
P.car_scn0  <- exp(V.car_scn0) / den_scn0
P.rail_scn0 <- exp(V.rail) / den_scn0
P.bike_scn0 <- exp(V.bike) / den_scn0

# Save base scenario employment and wages
scn0_L_j_hat <- L_j_hat
scn0_omega_j <- omega_j
scn0_omega_j_matrix <- omega_j_matrix
scn0_disposable_income_ij <- disposable_income_ij
```

**Change**: Remove exploratory `names(scn0_state)` line; add `P.bike_scn0`

---

### **Step 2: Scenario 1 Setup**

**Code to write**:
```r
# === Scenario 1: Uniform Employment Redistribution ===
target_zone_id <- "50303344"  # Kyushu University redevelopment site
diff_Lj_target <- 15000       # Employment increase (+15,000 jobs)

all_zones <- rownames(scn0_L_j_hat)
other_zones <- setdiff(all_zones, target_zone_id)

scn1_Lj_target <- scn0_L_j_hat[target_zone_id, "L_j_hat"] + diff_Lj_target

# Maintain total employment by uniform reduction in other zones
total_L <- sum(scn0_L_j_hat$L_j_hat, na.rm = TRUE)
reduction_ratio <- (total_L - scn0_L_j_hat[target_zone_id, "L_j_hat"] - diff_Lj_target) / 
                   (total_L - scn0_L_j_hat[target_zone_id, "L_j_hat"])

scn1_L_j_hat <- scn0_L_j_hat
scn1_L_j_hat[other_zones, "L_j_hat"] <- round(scn0_L_j_hat[other_zones, "L_j_hat"] * reduction_ratio)
scn1_L_j_hat[target_zone_id, "L_j_hat"] <- scn1_Lj_target

# Adjust wages to maintain total payroll (payroll-neutral)
k01 <- sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE) / 
       sum(omega_j$omega_j * scn1_L_j_hat$L_j_hat, na.rm = TRUE)

scn1_omega_j <- omega_j %>% 
  mutate(scn1_omega_j = omega_j * k01) %>% 
  dplyr::select(KEY_CODE, scn1_omega_j)

scn1_omega_j_matrix <- matrix(as.numeric(scn1_omega_j$scn1_omega_j), 
                              nrow = nz_res, 
                              ncol = nrow(scn1_omega_j), 
                              byrow = TRUE)
colnames(scn1_omega_j_matrix) <- scn1_omega_j$KEY_CODE
```

**Changes**:
- Remove `if(F)` wrapper
- Combine all scn1 setup coherently
- Save as `scn1_L_j_hat` (don't overwrite `L_j_hat`)

---

### **Step 3: Scenario 2 Setup**

**Code to write**:
```r
# === Scenario 2: CBD-Distance Weighted Employment Redistribution ===
# Employment redistributed inversely to CBD distance × current employment

scn2_L_j_hat <- scn0_L_j_hat
scn2_Lj_target <- scn0_L_j_hat[target_zone_id, "L_j_hat"] + diff_Lj_target

# Ensure dists0 has zone names without "mc_" prefix
colnames(dists0) <- gsub("^mc_", "", colnames(dists0))
rownames(dists0) <- gsub("^mc_", "", rownames(dists0))

cbd_code <- "50303302"
dist_from_cbd <- dists0[other_zones, cbd_code]

# Weight by distance and inverse employment
reduction_weight <- dist_from_cbd / (scn0_L_j_hat[other_zones, "L_j_hat"] + 1)
reduction_amount <- diff_Lj_target * (reduction_weight / sum(reduction_weight))

scn2_L_j_hat[other_zones, "L_j_hat"] <- round(scn0_L_j_hat[other_zones, "L_j_hat"] - reduction_amount)
scn2_L_j_hat[target_zone_id, "L_j_hat"] <- scn2_Lj_target

# Apply wage adjustment for scn2 (payroll-neutral)
# Calculate scn2-specific reduction rate since employment distribution differs from scn1
k02 <- sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE) / 
       sum(omega_j$omega_j * scn2_L_j_hat$L_j_hat, na.rm = TRUE)

scn2_omega_j <- omega_j %>% 
  mutate(scn2_omega_j = omega_j * k02) %>% 
  dplyr::select(KEY_CODE, scn2_omega_j)

scn2_omega_j_matrix <- matrix(as.numeric(scn2_omega_j$scn2_omega_j), 
                              nrow = nz_res, 
                              ncol = nrow(scn2_omega_j), 
                              byrow = TRUE)
colnames(scn2_omega_j_matrix) <- scn2_omega_j$KEY_CODE

# Verify payroll conservation
cat("Base scenario total payroll:", 
    sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE), "\n")
cat("Scenario 1 total payroll:", 
    sum(scn1_omega_j$scn1_omega_j * scn1_L_j_hat$L_j_hat, na.rm = TRUE), "\n")
cat("Scenario 2 total payroll:", 
    sum(scn2_omega_j$scn2_omega_j * scn2_L_j_hat$L_j_hat, na.rm = TRUE), "\n")
```

**Changes**:
- Remove `if(T)` wrapper
- Store as `scn2_L_j_hat` (separate from scn1)
- **CREATE** `scn2_omega_j_matrix` (was missing)
- **Calculate scn2-specific `k02` coefficient** (NOT reuse k01 from scn1)
- Add verification to confirm payroll conservation across all scenarios

---

### **Step 4: Consolidate Scenario Definitions**

**Code to write**:
```r
# === Consolidate All Scenario Definitions ===
scenarios <- list(
  scn0 = list(
    name = "Base Scenario (Current Conditions)",
    L_j_hat = scn0_L_j_hat,
    omega_j_matrix = scn0_omega_j_matrix,
    description = "Calibrated baseline equilibrium"
  ),
  scn1 = list(
    name = "Scenario 1: Uniform Distribution",
    L_j_hat = scn1_L_j_hat,
    omega_j_matrix = scn1_omega_j_matrix,
    description = paste0("15,000 jobs to ", target_zone_id, ", uniform reduction elsewhere")
  ),
  scn2 = list(
    name = "Scenario 2: CBD-Distance Weighted",
    L_j_hat = scn2_L_j_hat,
    omega_j_matrix = scn2_omega_j_matrix,
    description = paste0("15,000 jobs to ", target_zone_id, ", distance-weighted reduction")
  )
)

cat("=== Scenario Definitions ===\n")
for (scn in names(scenarios)) {
  cat(scn, ":", scenarios[[scn]]$name, "\n")
  cat("  Total employment:", sum(scenarios[[scn]]$L_j_hat$L_j_hat, na.rm = TRUE), "\n")
}
```

---

### **Step 5: Refactored run_scenario() Function**

**Code to write**:
```r
# シナリオ実行関数 ####
# Converted to while loop; passes parameters explicitly
# Returns comprehensive result set including P.bike

run_scenario <- function(scenario_name, L_j_hat_scn, omega_j_matrix_scn, 
                        v_init, max_iter = 50, msa = 0.5, tol = 100) {
  
  # Initialize loop control
  flag <- 0
  it <- 1
  
  # Local copies to avoid reference issues
  dists_road <- dists0.road * 1  # local copy
  v_l <- v_init
  
  while (flag == 0) {
    # Mode choice: recalculate for each updated dists_road
    V.car_l <- para[1] * dists_road + para[2] + para[4] * parkPrice / 2
    den_l   <- exp(V.bike) + exp(V.car_l) + exp(V.rail)
    P.car_l <- exp(V.car_l) / den_l
    P.rail_l <- exp(V.rail) / den_l
    P.bike_l <- exp(V.bike) / den_l
    
    # Calculate accessibility-adjusted generalized cost
    agcc_l <- (P.bike_l * V.bike + P.car_l * V.car_l + P.rail_l * V.rail) / para[1]
    c_ij_local <- agcc_l * 1.600
    disposable_income_ij_local <- pmax(-c_ij_local + omega_j_matrix_scn, 0)
    
    # Solve land use equilibrium
    # Update global environment for gapf_vj to access parameters
    assign("L_j_hat", L_j_hat_scn, envir = .GlobalEnv)
    assign("c_ij", c_ij_local, envir = .GlobalEnv)
    assign("disposable_income_ij", disposable_income_ij_local, envir = .GlobalEnv)
    
    sol <- nleqslv(x = v_l, fn = gapf_vj, 
                   global = "dbldog",
                   control = list(ftol = 1e-8, xtol = 1e-8, maxit = 200, allowSingular = TRUE))
    v_l <- sol$x
    state <- caluculate_model_state(exp(v_l))
    
    # Traffic assignment
    ODD.car <- state$l_i_j * P.car_l
    trips <- as.data.frame.table(ODD.car, responseName = "demand")
    names(trips) <- c("from", "to", "demand")
    
    ta <- assign_traffic(Graph = sgr, from = trips$from, to = trips$to, 
                         demand = trips$demand, max_gap = 1e-2, 
                         algorithm = "bfw", verbose = FALSE)
    
    sgr2 <- makegraph(df = ta$data[, c("from", "to", "cost")], 
                      directed = TRUE, capacity = 1e4, 
                      alpha = alpha, beta = beta, coords = nodes)
    
    dists_road_new <- get_distance_matrix(sgr2, from = zones, to = centers, algorithm = "mch")
    
    # MSA averaging for convergence
    diff <- sum((dists_road_new - dists_road)^2)
    dists_road <- msa * dists_road_new + (1 - msa) * dists_road
    
    cat("Scenario:", scenario_name, " | Iter:", it, " | diff:", 
        formatC(diff, format = "e", digits = 2), "\n")
    
    # Check stopping conditions
    it <- it + 1
    if (diff < tol | it > max_iter) flag <- 1
  }
  
  # Return comprehensive result set
  list(
    scenario = scenario_name,
    v = v_l,
    state = state,
    dists_road = dists_road,
    P.car = P.car_l,
    P.rail = P.rail_l,
    P.bike = P.bike_l,
    iterations = it - 1,
    converged = (diff < tol),
    final_diff = diff
  )
}
```

**Key changes**:
- `for` → `while` loop (matches GA pattern)
- Add `scenario_name` parameter
- Add `P.bike_l` to return value
- Remove `<<-` operators; use `assign()` for clarity
- Add iteration count and convergence status
- Improved console output

---

### **Step 6: Execute All Scenarios**

**Code to write**:
```r
# === Execute All Scenarios ===

# Initialize results list
results <- list()

# Use base scenario utilities as starting point for all scenarios
v_init_all <- exp(scn0_v)

# Run each scenario
for (scn_name in names(scenarios)) {
  cat("\n=== Running:", scenarios[[scn_name]]$name, "===\n")
  
  results[[scn_name]] <- run_scenario(
    scenario_name = scn_name,
    L_j_hat_scn = scenarios[[scn_name]]$L_j_hat,
    omega_j_matrix_scn = scenarios[[scn_name]]$omega_j_matrix,
    v_init = v_init_all,
    max_iter = 50,
    msa = 0.5,
    tol = 100
  )
  
  cat("Converged:", results[[scn_name]]$converged, 
      " in ", results[[scn_name]]$iterations, " iterations\n\n")
}

# === Extract Results for Comparison ===
scn0_v <- log(results$scn0$v)
scn1_v <- log(results$scn1$v)
scn2_v <- log(results$scn2$v)

scn0_state <- results$scn0$state
scn1_state <- results$scn1$state
scn2_state <- results$scn2$state

# Mode shares
P.car_scn1 <- results$scn1$P.car
P.rail_scn1 <- results$scn1$P.rail
P.bike_scn1 <- results$scn1$P.bike

P.car_scn2 <- results$scn2$P.car
P.rail_scn2 <- results$scn2$P.rail
P.bike_scn2 <- results$scn2$P.bike
```

---

### **Step 7: Comparative Analysis (Refactored)**

Replace L1386-1584 with modularized helper functions + cleaner comparisons. All existing analysis (population, rent, floor area, welfare, emissions) preserved but more maintainable.

---

## Implementation Checklist

| # | Action | Location |
|---|--------|----------|
| 1 | Base scenario setup | L1223-1245 |
| 2 | Scenario 1 setup | L1246-1280 |
| 3 | Scenario 2 setup | L1281-1315 |
| 4 | Scenarios list | L1316-1330 |
| 5 | run_scenario() function | L1331-1415 |
| 6 | Execute all scenarios | L1416-1450 |
| 7 | Refactored comparisons | L1451+ |

---

## Critical Issue: Scenario 2 Wage Adjustment

### The Problem
- scn1 and scn2 have **different employment distributions** (uniform vs. distance-weighted reduction)
- Therefore, they have **different total payrolls** if using the same wage scale $\omega_j$
- Using the same $k_{01}$ coefficient for both would make scn2's total payroll ≠ scn0's total payroll

### Mathematical Definition

**scn1 reduction ratio** (uniform across all other zones):
$k_{01} = \frac{\sum_j \omega_j \cdot L_j^{\text{scn0}}}{\sum_j \omega_j \cdot L_j^{\text{scn1}}}$

**scn2 reduction ratio** (distance-weighted, different distribution):
$k_{02} = \frac{\sum_j \omega_j \cdot L_j^{\text{scn0}}}{\sum_j \omega_j \cdot L_j^{\text{scn2}}}$

Since $L_j^{\text{scn1}} \neq L_j^{\text{scn2}}$ for most zones, $k_{01} \neq k_{02}$.

### Solution
Calculate **scn2-specific coefficient** after determining the employment distribution:

```r
# scn2 has its own employment distribution (distance-weighted)
k02 <- sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE) / 
       sum(omega_j$omega_j * scn2_L_j_hat$L_j_hat, na.rm = TRUE)

# Apply to create scn2 wage matrix
scn2_omega_j <- omega_j %>% 
  mutate(scn2_omega_j = omega_j * k02) %>% 
  dplyr::select(KEY_CODE, scn2_omega_j)
```

### Verification
Add diagnostic output to confirm payroll conservation:
```r
cat("Base scenario total payroll:", 
    sum(omega_j$omega_j * scn0_L_j_hat$L_j_hat, na.rm = TRUE), "\n")
cat("Scenario 1 total payroll:", 
    sum(scn1_omega_j$scn1_omega_j * scn1_L_j_hat$L_j_hat, na.rm = TRUE), "\n")
cat("Scenario 2 total payroll:", 
    sum(scn2_omega_j$scn2_omega_j * scn2_L_j_hat$L_j_hat, na.rm = TRUE), "\n")
```

All three should be equal (within rounding error).

---

## Approval Required

This plan addresses:
✅ Consolidates fragmented scenario definitions  
✅ Converts `for` → `while` loop (GA-consistent)  
✅ Adds `P.bike` to function output  
✅ Creates missing `scn2_omega_j_matrix` **with scn2-specific wage correction**  
✅ Implements parameter passing (user decision)  
✅ Makes code more maintainable and transparent  
✅ **CORRECTED**: scn2 now uses $k_{02}$ (not $k_{01}$) to maintain payroll equality  

**Shall I proceed with implementation?**

# Scenario Analysis Code Refactoring Plan

**Date**: 2026-06-05  
**Task**: Refactor the code around the `run_scenario()` function (L1313) to improve structure and usability  
**Current Status**: Plan Phase

---

## Context & Current State

### Scenario Analysis Structure
The code has these main phases:

1. **Scenario Setup** (L1223-1310): Define scenarios by modifying `L_j_hat` (employment) and `omega_j_matrix` (wages)
   - Base scenario (scn0): Extract from baseline equilibrium
   - Scenario 1 & 2: Redistribute employment with wage adjustments

2. **Scenario Execution** (L1313-1348): `run_scenario()` function that:
   - Takes employment (`L_j_hat_scn`), wages (`omega_j_matrix_scn`), and initial utilities as inputs
   - Iteratively solves land use equilibrium with updated travel times
   - Returns equilibrium utilities, state, distances, and mode shares

3. **Result Processing** (L1386-1584): Conduct 8+ separate comparative analyses:
   - Population/household distributions
   - Rental prices
   - Housing floor area
   - Welfare metrics
   - CO₂ emissions
   - Land use outcomes

### Issues to Address
- **Incomplete workflow**: `run_scenario()` is defined but **never called**
- **Missing intermediate values**: `scn1_v` and `scn1_state` are referenced (L1388, L1391) but never assigned
- **Manual repetition**: Scenario setup, execution, and result extraction are not systematized
- **Global variable management**: Uses `<<-` for global assignment (L1316, L1325-1326), creating testing/reproducibility issues
- **Unclear initialization parameters**: Which `v_init` and starting values to use for each scenario?

---

## Proposed Solution

### Refactoring Strategy: Create a Scenario Workflow Pipeline

**Goal**: Make scenario setup, execution, and comparison transparent and reusable

#### **Phase 1: Clarify Scenario Definitions** (Before run_scenario)
Create a small data structure to document each scenario's parameters:

```r
# Define all scenarios in one place
scenarios <- list(
  scn0 = list(
    name = "Base Scenario",
    L_j_hat = scn0_L_j_hat,
    omega_j_matrix = scn0_omega_j_matrix,
    description = "Current baseline"
  ),
  scn1 = list(
    name = "Scenario 1: Uniform Distribution",
    L_j_hat = L_j_hat,  # modified version from L1258
    omega_j_matrix = scn1_omega_j_matrix,
    description = "15,000 jobs to Kyushu Uni site, uniform reduction"
  ),
  scn2 = list(
    name = "Scenario 2: CBD-Distance Weighted",
    L_j_hat = scn2_Lj_target,  # proposed name for L1278
    omega_j_matrix = scn2_omega_j_matrix,  # needs to be created
    description = "15,000 jobs to Kyushu Uni site, distance-weighted reduction"
  )
)
```

#### **Phase 2: Modify run_scenario() for Explicit Output**
Update the function to return a richer result object:

```r
run_scenario <- function(scenario_name, L_j_hat_scn, omega_j_matrix_scn, 
                        v_init, max_iter = 50, msa = 0.5, tol = 100) {
  # ... existing logic ...
  
  # Return enhanced result with metadata
  list(
    scenario = scenario_name,
    v = v_l,
    state = state,
    dists_road = dists_road,
    P.car = P.car_l,
    P.rail = P.rail_l,
    P.bike = P.bike_l,
    iterations = it,
    converged = (diff < tol)
  )
}
```

#### **Phase 3: Execute All Scenarios in a Loop**
Replace manual execution with systematic batch processing:

```r
# Run all scenarios
results <- list()
v_init_scenario <- exp(scn0_v)  # Use base scenario as starting point for all

for (scn in names(scenarios)) {
  cat("Running", scenarios[[scn]]$name, "...\n")
  results[[scn]] <- run_scenario(
    scenario_name = scn,
    L_j_hat_scn = scenarios[[scn]]$L_j_hat,
    omega_j_matrix_scn = scenarios[[scn]]$omega_j_matrix,
    v_init = v_init_scenario,
    max_iter = 50, msa = 0.5, tol = 100
  )
}

# Extract utilities and states for comparison
scn0_v <- log(results$scn0$v)
scn1_v <- log(results$scn1$v)
scn2_v <- log(results$scn2$v)

scn0_state <- results$scn0$state
scn1_state <- results$scn1$state
scn2_state <- results$scn2$state
# ... mode shares, etc.
```

#### **Phase 4: Systematize Result Extraction**
Create helper functions to avoid code repetition in comparative analysis:

```r
# Extract household comparison
extract_household_comparison <- function(results, scn_names = c("scn0", "scn1")) {
  # Standardized extraction of household distributions
  # Returns sf object with differences
}

# Extract rent comparison
extract_rent_comparison <- function(results, scn_names = c("scn0", "scn1")) {
  # Standardized extraction of r_bar_i with unit conversion
}

# etc. for floor area, welfare, emissions
```

#### **Phase 5: Manage Global Variable Scope**
Reduce `<<-` usage by:
- Passing `c_ij` and `disposable_income_ij` as function environment parameters
- Or wrapping scenario execution in a closure that captures these dependencies

---

## Implementation Checklist

| Step | Location | Action |
|------|----------|--------|
| **1a** | L1295-1311 | Create and name `scn2_omega_j_matrix` explicitly (currently missing) |
| **1b** | L1278 | Rename `scn2_Lj_target` to `scn2_L_j_hat` for consistency |
| **1c** | After L1311 | Define `scenarios` list with all three scenario parameters |
| **2a** | L1313-1348 | Add `scenario_name` parameter and convergence info to return value |
| **2b** | L1313-1348 | Add `P.bike` to return list (missing from current version) |
| **3a** | After L1318 | Replace commented-out manual scn1 code (L1352-1378) with loop execution |
| **3b** | After loop | Extract `scn0_v, scn1_v, scn2_v` and states from results list |
| **4a** | L1386-1584 | Create `extract_*_comparison()` helper functions |
| **4b** | L1386-1584 | Replace repeated comparison code with function calls |
| **5a** | L1316, L1325-1326 | Consider passing global variables as function closure or environment |

---

## Expected Outcomes

✅ **Transparency**: Clear scenario definitions and execution flow  
✅ **Completeness**: `run_scenario()` is actually called and produces `scn1_v`, `scn1_state`, etc.  
✅ **Extensibility**: Easy to add scenario 3, 4, ... without code duplication  
✅ **Reproducibility**: Scenario parameters documented in one place  
✅ **Maintainability**: Result extraction functions reduce copy-paste errors  

---

## User Decisions (Confirmed 2026-06-05)

| Question | Decision | Rationale |
|----------|----------|-----------|
| **Scenario 2 wage adjustment** | Use k01 payroll scaling | Match scn1 methodology, maintain comparability |
| **Convergence tolerance** | Keep `tol = 100` | Speed matters for exploration phase |
| **Mode share outputs** | Add `P.bike` to return | Complete mode share information needed downstream |
| **Scenario extension** | Stick with 3 scenarios | Current scope sufficient; can expand later |
| **Global variable handling** | Pass as parameters | Clearest approach; prefer explicit over implicit |

---

## Critical Analysis: Loop Structure (while vs for)

### User Question
Should `run_scenario()` use `while` loop (like GA post-calibration code, L978-1033) instead of `for` loop (current, L1319)?

### Comparison

| Aspect | Current `for` (L1319) | GA Post-Calibration `while` (L984-1033) |
|--------|----------------------|----------------------------------------|
| **Loop control** | Fixed max iterations: `for (it in 1:max_iter)` | Flag-based: `while (flag01 == 0)` |
| **Stopping condition** | Always runs up to 50 iterations | Stops early if `diff01 < 100` OR `ii > 1000` |
| **Convergence check** | Only at break statement (L1345) | Checked at every iteration (L1032) |
| **Diagnostics** | Prints: `it`, `diff` | Prints: `ii`, `diff01` |
| **Tracking variable** | `it` incremented by loop | `ii` incremented manually (L1031) |

### Code Structures Side-by-Side

**Current `run_scenario()` (L1319-1345)**:
```r
for (it in 1:max_iter) {
  # ... calculations ...
  if (diff < tol) break  # L1345
}
```

**GA Post-Calibration (L984-1032)**:
```r
while (flag01 == 0) {
  # ... calculations ...
  ii <- ii + 1
  if (diff01 < 100 | ii > 1e3) flag01 <- 1
}
```

### Analysis & Recommendation

**Switching to `while` loop is SAFE and RECOMMENDED** for these reasons:

1. **Semantically clearer**: The GA code uses `while` because the stopping condition is primary (convergence or max iterations), not the loop count. This matches your use case.

2. **Consistent codebase**: Using `while` aligns with the established pattern in the calibrated baseline run (L984), making the code more cohesive.

3. **Better convergence tracking**: The `while` structure naturally handles both stopping conditions:
   - Early exit on convergence (`diff < tol`)
   - Safety exit on iteration limit (`ii > max_iter`)

4. **Clearer intent**: `while (not_converged)` is more expressive than `for (it in 1:max_iter)` with a break statement.

5. **No functional loss**: Both achieve identical behavior with proper implementation.

### Implementation Detail

When converting, ensure three elements match the GA pattern:

```r
# Initialize
flag <- 0
it <- 1

# Loop structure
while (flag == 0) {
  # ... existing calculations ...
  
  # Increment counter
  it <- it + 1
  
  # Check stopping condition
  if (diff < tol | it > max_iter) flag <- 1
}

# Optional: return iteration count
list(..., iterations = it - 1)  # Adjust for manual increment
```

### Updated Checklist Item

| Step | Location | Action |
|------|----------|--------|
| **2c** | L1313-1348 | **NEW: Convert `for` to `while` loop for consistency with GA calibration code** |

---

## Notes

- Lines 1380-1382 reset globals to base scenario; this is important and should be preserved or adapted
- The comparison analysis (L1386-1584) is comprehensive; refactoring should preserve all metrics
- CO₂ calculation requires `P.car_scn1` and `P.rail_scn1`; include in results
- **Loop structure**: Using `while` instead of `for` aligns with GA calibration pattern and is recommended

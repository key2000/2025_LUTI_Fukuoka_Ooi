# Concern: Distance Matrix (dists0.road) Updates Across Multiple LUTI Runs

**Date**: 2026-06-05  
**Issue**: `dists0.road` is updated in multiple LUTI executions, potentially causing state inconsistency  
**Status**: Analysis & Plan

---

## Problem Statement

The code executes LUTI three times:
1. **GA calibration loop** (L659-976): Finds optimal parameters
2. **Post-calibration LUTI** (L984-1033): Runs LUTI with best parameters
3. **Scenario analysis** (L1351-1420): Runs LUTI for scn0, scn1, scn2

Each time, `dists0.road` is **updated** via MSA averaging in the while loop:

**GA Post-Calibration (L1028)**:
```r
dists0.road <- alpha01 * dists1.road + (1 - alpha01) * dists0.road.b
```

**Scenario Function (L1343 in current code)** simulates this within `run_scenario()`:
```r
dists_road <- msa * dists_road_new + (1 - msa) * dists_road
```

---

## Code Trace: dists0.road Updates

### **Initial State**
- `dists0.road` loaded from network (L769-771): Initial distance matrix
- `dists0.bike`, `dists0.rail` also loaded

### **Phase 1: GA Iteration Loop (L659-976)**
- LUTI runs with candidate parameters
- **Within GA loop**: `dists0.road` NOT modified (each GA iteration uses fresh starting value)
- After GA loop: `dists0.road` unchanged

### **Phase 2: Post-Calibration LUTI (L984-1033)**
- LUTI runs with **BEST parameters** from GA
- **Within while loop**: `dists0.road` UPDATED repeatedly
- **After loop completes**: `dists0.road` contains FINAL converged distances from post-calibration run
- **State**: `dists0.road` = equilibrium distances with final calibrated parameters

### **Phase 3: Scenario LUTI (L1351-1420)**
- Function receives `v_init` = utilities from phase 2
- Function creates **LOCAL copy**: `dists_road <- dists0.road * 1` (L1317)
- **Within scenario while loop**: LOCAL `dists_road` updated, NOT global `dists0.road`
- **After function returns**: Global `dists0.road` unchanged from phase 2 result

---

## Analysis: Is This a Problem?

### **Current Implementation**
```r
# In run_scenario() - Line 1317
dists_road <- dists0.road * 1  # LOCAL COPY made
while (flag == 0) {
  # ... calculations ...
  dists_road <- msa * dists_road_new + (1 - msa) * dists_road  # Updates LOCAL copy
}
return(...)  # dists0.road not modified
```

### **Assessment**

| Aspect | Status | Explanation |
|--------|--------|-------------|
| **Phase 2 → Phase 3 transition** | ✅ **OK** | Phase 2's final `dists0.road` used as starting point for scenarios (correct) |
| **Within scenario execution** | ✅ **OK** | Each scenario uses local copy; doesn't pollute global state |
| **Multiple scenarios** | ✅ **OK** | All three scenarios (scn0, scn1, scn2) start from same `dists0.road` baseline |
| **Reproducibility** | ✅ **OK** | After all scenarios run, `dists0.road` = post-calibration equilibrium (unchanged) |

### **Why This Is Actually Good Design**

1. **Isolation**: Scenarios don't interfere with baseline
2. **Comparability**: All scenarios use identical starting distances
3. **Baseline fidelity**: Post-calibration equilibrium preserved for reference

---

## Potential Misunderstanding

The user may be concerned about:

**Scenario A: "Each scenario modifies the global dists0.road"**
```
GA calibration: dists0.road → d_GA
Post-calib:     dists0.road → d_POST (based on d_GA)
Scenario 1:     dists0.road → d_SCN1 (based on d_POST)
Scenario 2:     dists0.road → d_SCN2 (based on d_SCN1) ← PROBLEM!
```

**Scenario B: "Each scenario starts fresh from same baseline"**
```
GA calibration: dists0.road → d_GA
Post-calib:     dists0.road → d_POST (based on d_GA)
Scenario 1:     dists_road_local (copy of d_POST)
Scenario 2:     dists_road_local (fresh copy of d_POST) ← CORRECT
```

The code implements **Scenario B** (correct approach).

---

## Code Review: Confirm Scenario A vs B

In `run_scenario()` function (current implementation):

```r
run_scenario <- function(..., v_init, ...) {
  dists_road <- dists0.road * 1       # ← Creates LOCAL copy
  
  while (flag == 0) {
    # ...
    dists_road <- msa * dists_road_new + (1 - msa) * dists_road  # ← Updates LOCAL only
  }
  
  return(list(..., dists_road = dists_road))  # Returns local copy
  # Global dists0.road NOT modified
}
```

**Verification from R session**:
- `dists0.road` exists as global variable
- Each call to `run_scenario()` creates local `dists_road` variable
- Assignment within function doesn't use `<<-` (would be needed to modify global)
- Pattern correct ✅

---

## Recommendation: Keep Current Design

The current approach is sound. However, to make this **explicit and clear** in the refactored code:

### **Improvement 1: Add Comments**
```r
run_scenario <- function(...) {
  # Create LOCAL copy of global baseline distances
  # This prevents scenario iterations from affecting the baseline
  dists_road <- dists0.road * 1
  
  while (flag == 0) {
    # ...
    # Updates LOCAL dists_road only (not global dists0.road)
    dists_road <- msa * dists_road_new + (1 - msa) * dists_road
  }
}
```

### **Improvement 2: Document in Code**
Add diagnostic output to verify isolation:
```r
# Before scenarios
dists0_check_before <- sum(dists0.road)

# After all scenarios
results <- list()
for (scn in names(scenarios)) {
  results[[scn]] <- run_scenario(...)
}

# After scenarios
dists0_check_after <- sum(dists0.road)

# Verify dists0.road unchanged
cat("dists0.road checksum before:", dists0_check_before, "\n")
cat("dists0.road checksum after:", dists0_check_after, "\n")
cat("Isolation OK:", dists0_check_before == dists0_check_after, "\n")
```

### **Improvement 3: Clarify Documentation**

Add this comment block before scenario execution:

```r
# === Scenario Execution ===
# IMPORTANT: Distance matrix (dists0.road) handling
# • dists0.road = baseline equilibrium from post-calibration LUTI (L984-1033)
# • Each run_scenario() call creates LOCAL copy: dists_road <- dists0.road * 1
# • Scenario iterations update LOCAL dists_road only, not global dists0.road
# • Result: All scenarios use SAME starting distances → comparable results
# • Baseline dists0.road preserved after all scenarios complete
# • No state contamination between scenarios ✓
```

---

## Final Assessment

**Is there a problem?** ❌ **No**

The code correctly:
- ✅ Isolates each scenario with a local distance copy
- ✅ Ensures all scenarios start from same baseline
- ✅ Preserves global `dists0.road` after scenario runs complete
- ✅ Maintains reproducibility and comparability

**Action**: Keep the design; add clarifying comments and optional diagnostics.

---

## Checklist for Implementation Plan

| Item | Status | Notes |
|------|--------|-------|
| Maintain local copy pattern in `run_scenario()` | ✅ Keep | `dists_road <- dists0.road * 1` |
| Add isolation comments | ⚠️ Add | Explain why local copy, what it achieves |
| Add optional diagnostics | ⚠️ Optional | Checksum before/after if desired |
| Update implementation plan | ⚠️ Update | Mark distance handling as verified |

---

## Updated Implementation Plan Notes

Add to `implementation_plan.md` **Step 5 (run_scenario function)** section:

**Distance Matrix Handling**:
```
• Each run_scenario() creates LOCAL copy: dists_road <- dists0.road * 1
• Prevents global dists0.road modification during scenario iterations
• All scenarios start from same baseline (post-calibration equilibrium)
• Global dists0.road preserved throughout
• Result: Clean isolation between scenarios ✓
```


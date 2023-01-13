// Bonding curve contract with debugging entrypoints and features enabled and
// piecewise cost_mutez formula
// Similar example here: ../swaps/fa2_allowlisted_swap_with_burn.mligo
#if !DEBUG_BONDING_CURVE
#define DEBUG_BONDING_CURVE
#if !PIECEWISE_BONDING_CURVE
#define PIECEWISE_BONDING_CURVE
#include "bonding_curve.mligo"
#endif
#endif

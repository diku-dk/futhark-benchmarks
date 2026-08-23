# FinanceBench

Futhark implementations of the
[FinanceBench](https://github.com/cavazos-lab/FinanceBench) benchmark suite.

The suite contains four financial computing benchmarks:

- **black-scholes.fut** — Black-Scholes-Merton analytical option pricing
  (5,000,000 European options priced in parallel).

- **monte-carlo.fut** — Monte Carlo path-based put option pricing using
  geometric Brownian motion (400,000 paths × 250 time steps).

- **bonds.fut** — Fixed-rate coupon bond pricing using discounted cash flows
  (1,000,000 bonds priced in parallel).

- **repo.fut** — Repurchase agreement (repo) forward value computation
  (1,000,000 repos priced in parallel).

## References

- [FinanceBench source (cavazos-lab)](https://github.com/cavazos-lab/FinanceBench)
- [Improving FinanceBench (StreamHPC)](https://streamhpc.com/blog/2020-08-28/improving-financebench-part-ii-low-hanging-fruit/)

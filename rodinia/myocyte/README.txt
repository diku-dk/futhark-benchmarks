The OpenCL version needs isinf(x) and isnan(x) in order to run correctly.
The dataset has been extended: workload=64*1024 from 1 in Rodinia.
Observed a speedup of 10x w.r.t. Rodinia cuda version (after inserting
isinf() and isnan() in the generated OpenCL code).

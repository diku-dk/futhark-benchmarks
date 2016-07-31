This program is divided into two parts - `lavaMD-input.fut`, which
takes a box count and produces corresponding input, and `lavaMD.fut`,
which implements the actual computational kernel.

The reason behind this split is the input generation stage being
fairly time-consuming, but not worth measuring, as it is done on the
host (and using `rand()`) in the Rodinia implementation.

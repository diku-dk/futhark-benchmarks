#!/bin/sh

accelerate_benchmark() {
    n_bodies="$1"
    time_step="$2"
    epsilon="$3"
    
    echo "Accelerate with n_bodies=$n_bodies, time_step=$time_step, and epsilon=$epsilon."
    accelerate-nbody --test=False --benchmark=True \
                     --bodies="$n_bodies" \
                     --timestep="$time_step" \
                     --epsilon="$epsilon"
}

accelerate_benchmark 100 1.0 50.0
accelerate_benchmark 1000 1.0 50.0
accelerate_benchmark 10000 1.0 50.0
accelerate_benchmark 100000 1.0 50.0

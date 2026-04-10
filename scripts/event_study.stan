data {
    int<lower=0> N;
    array[N] int<lower=0> E; // Employed or not
    array[N] int<lower=0> A;
    array[N] int<lower=0> trial;
    array[N] int<lower=-5, upper=10> event_time;
    array[N] int<lower=1, upper=2> sex;
} 

parameters {
    vector[N] logit_p;
}

model {
    E ~ binomial_logit(trial, logit_p);
}

generated quantities {
   vector[N] p;
   p = inv_logit(logit_p);
}

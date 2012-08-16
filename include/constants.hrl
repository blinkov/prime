-define(PRIMES, primes).
-define(WORKERS, workers).
-define(ETS_OPTIONS, [public, named_table, ordered_set, {write_concurrency, true}]).

-define(BATCH_SIZE, 100).
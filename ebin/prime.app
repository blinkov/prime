{application,prime,
             [{description,"Distributed prime numbers finder"},
              {vsn,"0.1.0"},
              {modules,[prime,prime_sup,worker_server]},
              {registered,[prime_sup]},
              {applications,[kernel,stdlib]},
              {mod,{prime,[]}},
              {env,[]}]}.

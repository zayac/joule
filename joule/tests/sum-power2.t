(:"k": nil, "result": {"p": double}:) <= (:"power_int"((or a b)): {"v": int } | $^sum:);
(:"sum_double"(b): {"a": double, "b": double | $_sum_double}, "sum_int"(b): {"a": int, "b": int | $_sum_int} | $^sum:) <= (:"result"((or (not f_power__1_power_double) (not f_power__1_power_int))): {"p": double | $__1_power_double_1_power_int} | $^power:);

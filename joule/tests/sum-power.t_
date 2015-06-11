$_sum_double <= $_sum_int_sum_double;
$_sum_int <= $_sum_int_sum_double;

(:
  "k": nil,
  "sum_int": {"a": int, "b": int, "base": int}
:)
<=

(:
  "sum_double"(f_sum_sum_double): {"a": double, "b": double | $_sum_double},
  "sum_int"(f_sum_sum_int): {"a": int, "b": int | $_sum_int} | $^sum
:);

$_sum_double <= $_sum_int_sum_double;
$_sum_int <= $_sum_int_sum_double;
$_power_double <= $_power_double_power_int;
$_power_int <= $_power_double_power_int;

(:
  "power_int"((or f_sum_sum_double f_sum_sum_int)): {"v": int | $_sum_int_sum_double} | $^sum
:)
<=

(:
  "power_double"(f_power_power_double): {"base": double, "v": double | $_power_double},
  "power_int"(f_power_power_int): {"base": int, "v": int | $_power_int} | $^power
:);


(:
  "result"((or f_power_power_double f_power_power_int)): {"p": double | $_power_double_power_int} | $^power
:)
<=

(:
  "k": nil,
  "result": {"p": double}
:);



(:
  k: nil,
  "sum_int": 
    {
      a: int,
      b: int,
      base: int
    }
:)
<=

(:
  "sum_double"(f_sum_sum_double): 
    {
      a: double,
      b: double | $sum_double
    },
  "sum_int"(f_sum_sum_int): 
    {
      a: int,
      b: int | $sum_int
    } | $f_sum
:);

$union_power_double_power_int <= (union $power_double $power_int);
$union_sum_int_sum_double <= (union $sum_int $sum_double);

(:
  "power_int"((or f_sum_sum_double f_sum_sum_int)): 
    {
      v: int | $union_sum_int_sum_double
    } | $f_sum
:)
<=

(:
  "power_double"(f_power_power_double): 
    {
      base: double,
      v: double | $power_double
    },
  "power_int"(f_power_power_int): 
    {
      base: int,
      v: int | $power_int
    } | $f_power
:);


(:
  result((or f_power_power_double f_power_power_int)): 
    {
      p: double | $union_power_double_power_int
    } | $f_power
:)
<=

(:
  k: nil,
  result: 
    {
      p: double
    }
:);


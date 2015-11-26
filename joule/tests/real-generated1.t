
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
  "sum_double"((not f_sum_sum_double)): 
    {
      a: double,
      b: double | $_sum_double
    },
  "sum_int"((not f_sum_sum_int)): 
    {
      a: int,
      b: int | $_sum_int
    } | $^f_sum
:);

$_union_power_double_power_int <= (union $_power_double $_power_int);
$_union_sum_int_sum_double <= (union $_sum_int $_sum_double);

(:
  "power_int"((or (not f_sum_sum_double) (not f_sum_sum_int))): 
    {
      v: int | $_union_sum_int_sum_double
    } | $^f_sum
:)
<=

(:
  "power_double"((not f_power_power_double)): 
    {
      base: double,
      v: double | $_power_double
    },
  "power_int"((not f_power_power_int)): 
    {
      base: int,
      v: int | $_power_int
    } | $^f_power
:);


(:
  result((or (not f_power_power_double) (not f_power_power_int))): 
    {
      p: double | $_union_power_double_power_int
    } | $^f_power
:)
<=

(:
  k: nil,
  result: 
    {
      p: double
    }
:);


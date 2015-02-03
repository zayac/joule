$_sum_double <= $_sum_int_sum_double;
$_sum_int <= $_sum_int_sum_double;
$_DOWN_class_sum_AClass <= {"%self%(%self% &)": "hash_4096886239439440404", "%self%()": "hash_1326381182976453604", "a_fkXtwLcuiG": int, "b": int, "bar(int, int, int)": (int "hash_13986934892522949596"), "c_8kNisFTT28": int, "foo(global::GlobalObject &)": (void "hash_15788462380324938053")};

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
$_DOWN_class_sum_AClass <= {"%self%(%self% &)": "hash_4096886239439440404", "%self%()": "hash_1326381182976453604", "a_fkXtwLcuiG": int, "b": int, "bar(int, int, int)": (int "hash_13986934892522949596"), "c_8kNisFTT28": int, "foo(global::GlobalObject &)": (void "hash_15788462380324938053")};
$_power_double <= $_power_double_power_int;
$_power_int <= $_power_double_power_int;
$_DOWN_class_power_BClass <= {"%self%(%self% &)": "hash_4096886239439440404", "a_62YYn7BOTz": int, "b": int, "foo(global::GlobalObject &)": (void (override "hash_15506319419576443349"))};
$_DOWN_class_power_BClass <= {"a_fkXtwLcuiG": int, "b": int, "c_8kNisFTT28": int};

(:
  "power_int"((or f_sum_sum_double f_sum_sum_int)): {"v": $_DOWN_class_sum_AClass | $_sum_int_sum_double} | $^sum
:)
<=

(:
  "power_double"(f_power_power_double): {"v": $_DOWN_class_power_BClass | $_power_double},
  "power_int"(f_power_power_int): {"v": $_DOWN_class_power_BClass | $_power_int} | $^power
:);


(:
  "result"((or f_power_power_double f_power_power_int)): {"p": $_DOWN_class_power_BClass | $_power_double_power_int} | $^power
:)
<=

(:
  "k": nil,
  "result": {"p": {"b": int}}
:);


$_sum_double <= $_sum_int_sum_double;
$_sum_int <= $_sum_int_sum_double;
$_sum_DOWN_class_AClass <= {"%self%()": "hash_8744321538534338666", "%self%(const %self% &)": "hash_4096886239439440404", "a_TBihd2P83W": int, "b": int, "bar(int, int, int)": (int "hash_13986934892522949596"), "c_WJ5pUjggb4": int, "foo(global::GlobalObject &)": (void "hash_2072339399142873774")};

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
$_sum_DOWN_class_AClass <= {"%self%()": "hash_8744321538534338666", "%self%(const %self% &)": "hash_4096886239439440404", "a_TBihd2P83W": int, "b": int, "bar(int, int, int)": (int "hash_13986934892522949596"), "c_WJ5pUjggb4": int, "foo(global::GlobalObject &)": (void "hash_2072339399142873774")};
$_power_double <= $_power_double_power_int;
$_power_int <= $_power_double_power_int;
$_power_DOWN_class_BClass <= {"%self%(const %self% &)": "hash_4096886239439440404", "a_jRW1KN17b6": int, "b": int, "foo(global::GlobalObject &)": (void (override "hash_15506319419576443349"))};
$_power_DOWN_class_BClass <= {"a_TBihd2P83W": int, "b": int, "c_WJ5pUjggb4": int};

(:
  "power_int"((or f_sum_sum_double f_sum_sum_int)): {"v": $_sum_DOWN_class_AClass | $_sum_int_sum_double} | $^sum
:)
<=

(:
  "power_double"(f_power_power_double): {"v": $_power_DOWN_class_BClass | $_power_double},
  "power_int"(f_power_power_int): {"v": $_power_DOWN_class_BClass | $_power_int} | $^power
:);


(:
  "result"((or f_power_power_double f_power_power_int)): {"p": $_power_DOWN_class_BClass | $_power_double_power_int} | $^power
:)
<=

(:
  "k": nil,
  "result": {"p": {"b": int}}
:);


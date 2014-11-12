(: sumi: { a: int, b: int, base: int } :) <=
(: sumd(ssd): { a: double, b: double | $sum_double },
   sumi(ssi): { a: int, b: int | $sum_int } | $sum :);

(: powerd(ssd): { v: double | $sum_double }, poweri(ssi): { v: int | $sum_int } | $sum :) <=
(: powerd(ppd): { base: double, v: double | $power_double },
   poweri(ppi): { base: int, v: int | $power_int } | $power :);

(: result(or ppd ppi): { p: double | $power_double } | $power :) <=
(: result: { p: double } :);


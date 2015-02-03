(: sumi: { a: int, b: int, base: int } :) <=
(: sumd(ssd): { a: double, b: double | $_sum_double },
   sumi(ssi): { a: int, b: int | $_sum_int } | $^sum :);

(: powerd(ssd): { v: double | $_sum_double }, poweri(ssi): { v: int | $_sum_int } | $^sum :) <=
(: powerd(ppd): { base: double, v: double | $_power_double },
   poweri(ppi): { base: int, v: int | $_power_int } | $^power :);

(: result(or ppd ppi): { p: double | $_power_double } | $^power :) <=
(: result: { p: double } :);


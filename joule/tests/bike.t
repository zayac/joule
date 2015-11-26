(: bike(x): {price: int | $_p}, acc(y): {price: int | $_q} :) <= (: bike: {price: int, frame: int} :);
(: comp: {price: int, frame: int} :) <= (: comp(x): {price: int | $_p } :);
none <= (: acc(y): {price: int | $_q} :);

{a: int, b: int, c: int} <= {a: nil | $_x};
[$_x | $_y ] <= $_z;
$_z <= [{b(q): $_w} | $_u];
[$_w] <= $_y;
$_u <= <(not q): [int], q: [char]>;

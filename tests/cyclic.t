{a: int, b: int, c: int} <= {a: nil | $x};
[$x | $y ] <= $z;
$z <= [{b($q): $w} | $u];
[$w] <= $y;
$u <= <$q: [int], (not $q): [char]>;

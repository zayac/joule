$__1_denoise <= $__1_denoise_send_img;
$__1_denoise <= $__1_denoise_error;
$__1_initKMeans <= $__1_initKMeans_kMeans;
$__1_initKMeans <= $__1_initKMeans_error;
$__1_kMeans <= $__1_kMeans_loop;
$__1_kMeans <= $__1_kMeans_result;
$__1_kMeans <= $__1_kMeans_error;
$__1_read_color <= $__1_read_color_1_read_grayscale_1_read_unchanged_send_img;
$__1_read_grayscale <= $__1_read_color_1_read_grayscale_1_read_unchanged_send_img;
$__1_read_unchanged <= $__1_read_color_1_read_grayscale_1_read_unchanged_send_img;
$__1_read_color <= $__1_read_color_1_read_grayscale_1_read_unchanged_error;
$__1_read_grayscale <= $__1_read_color_1_read_grayscale_1_read_unchanged_error;
$__1_read_unchanged <= $__1_read_color_1_read_grayscale_1_read_unchanged_error;
$__1_read_color <= $__1_read_color_1_read_grayscale_1_read_unchanged_send_img;
$__1_read_grayscale <= $__1_read_color_1_read_grayscale_1_read_unchanged_send_img;
$__1_read_unchanged <= $__1_read_color_1_read_grayscale_1_read_unchanged_send_img;
$__1_read_color <= $__1_read_color_1_read_grayscale_1_read_unchanged_error;
$__1_read_grayscale <= $__1_read_color_1_read_grayscale_1_read_unchanged_error;
$__1_read_unchanged <= $__1_read_color_1_read_grayscale_1_read_unchanged_error;
(:"error"(f_denoise__1_denoise): {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_denoise_error}:) <= (:"error": {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>"}:);
(:"initKMeans"(f_denoise__1_denoise): {"img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_denoise_send_img} | $^denoise:) <= (:"initKMeans"(f_initKMeans__1_initKMeans): {"K": int, "img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_initKMeans} | $^initKMeans:);
(:"initKMeans": {"K": int, "img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>"}, "read_color": {"K": int, "depth": int, "fname": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>"}:) <= (:"read_color"(f_read__1_read_color): {"fname": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_read_color}, "read_grayscale"(f_read__1_read_grayscale): {"fname": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_read_grayscale}, "read_unchanged"(f_read__1_read_unchanged): {"fname": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_read_unchanged} | $^read:);
(:"error"(f_initKMeans__1_initKMeans): {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_initKMeans_error}:) <= (:"error": {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>"}:);
(:"kMeans"(f_initKMeans__1_initKMeans): {"K": int, "epsilon": double, "img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>", "old_centers_v": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_initKMeans_kMeans} | $^initKMeans:) <= (:"kMeans"(f_kMeans__1_kMeans): {"K": int, "epsilon": double, "img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>", "old_centers_v": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_kMeans} | $^kMeans:);
(:"result"(f_kMeans__1_kMeans): {"centers": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_kMeans_result}:) <= (:"result"(f_kMeans__1_kMeans): {"centers": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>"}:);
(:"error"(f_kMeans__1_kMeans): {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_kMeans_error}:) <= (:"error": {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>"}:);
(:"kMeans"(f_kMeans__1_kMeans): {"K": int, "epsilon": double, "img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>", "old_centers_v": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_kMeans_loop} | $^kMeans:) <= (:"kMeans"(f_kMeans__1_kMeans): {"K": int, "epsilon": double, "img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>", "old_centers_v": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>" | $__1_kMeans} | $^kMeans:);
(:"denoise"((or f_read__1_read_color f_read__1_read_grayscale f_read__1_read_unchanged)): {"img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>", "kind": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_read_color_1_read_grayscale_1_read_unchanged_send_img} | $^read:) <= (:"denoise"(f_denoise__1_denoise): {"img": "std::vector<class std::vector<double, class std::allocator<double> >, class std::allocator<class std::vector<double, class std::allocator<double> > >>", "kind": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_denoise} | $^denoise:);
(:"error"((or f_read__1_read_color f_read__1_read_grayscale f_read__1_read_unchanged)): {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>" | $__1_read_color_1_read_grayscale_1_read_unchanged_error}:) <= (:"error": {"msg": "std::basic_string<char, struct std::char_traits<char>, class std::allocator<char>>"}:);
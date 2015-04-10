#define COMMA ,
#define CAL_FI_HIDE_CLASSES 1
#define read_DOWN__2_error_ochannels {2 COMMA "error"}
#define read_DOWN_send_img_ochannels {1 COMMA "denoise"}
#define read_DOWN__1_read_color_decl  COMMA int K
#define read_DOWN__1_read_color_use  COMMA K
#define read_DOWN__1_read_color_tuple_get  COMMA std::get<1>(_data)
#define read_DOWN__1_read_color_types  COMMA int
#define read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_decl 
#define read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_use 
#define read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_decl  COMMA int K
#define read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_use  COMMA K
#define read_DOWN__1_read_grayscale_decl  COMMA int K
#define read_DOWN__1_read_grayscale_use  COMMA K
#define read_DOWN__1_read_grayscale_tuple_get  COMMA std::get<1>(_data)
#define read_DOWN__1_read_grayscale_types  COMMA int
#define read_DOWN__1_read_unchanged_decl  COMMA int K
#define read_DOWN__1_read_unchanged_use  COMMA K
#define read_DOWN__1_read_unchanged_tuple_get  COMMA std::get<1>(_data)
#define read_DOWN__1_read_unchanged_types  COMMA int
#define read_UP_read do {\
	if (_msg.getType() == "init") {\
		output(1, std::move(_msg));\
		return;\
	}\
} while (0);
#define f_read__1_read_grayscale
#define f_read__1_read_unchanged

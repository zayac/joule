#define COMMA ,
#define CAL_FI_HIDE_CLASSES 1
#define denoise_DOWN__2_error_ochannels {2 COMMA "error"}
#define denoise_DOWN_send_img_ochannels {1 COMMA "init"}
#define denoise_DOWN__1_denoise_color_decl  COMMA int K
#define denoise_DOWN__1_denoise_color_use  COMMA K
#define denoise_DOWN__1_denoise_color_tuple_get  COMMA std::get<1>(_data)
#define denoise_DOWN__1_denoise_color_types  COMMA int
#define denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_decl 
#define denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_use 
#define denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_decl  COMMA int K
#define denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_use  COMMA K
#define denoise_DOWN__1_denoise_grayscale_decl  COMMA int K
#define denoise_DOWN__1_denoise_grayscale_use  COMMA K
#define denoise_DOWN__1_denoise_grayscale_tuple_get  COMMA std::get<1>(_data)
#define denoise_DOWN__1_denoise_grayscale_types  COMMA int
#define denoise_UP_denoise do {\
	if (_msg.getType() == "init") {\
		output(1, std::move(_msg));\
		return;\
	}\
} while (0);
#define f_denoise__1_denoise_grayscale

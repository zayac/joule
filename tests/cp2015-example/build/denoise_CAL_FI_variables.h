#define COMMA ,
#define CAL_FI_HIDE_CLASSES 1
#define denoise_DOWN__2_error_ochannels {2 COMMA "error"}
#define denoise_DOWN_send_img_ochannels {1 COMMA "init"}
#define denoise_DOWN__1_denoise_decl  COMMA int K
#define denoise_DOWN__1_denoise_use  COMMA K
#define denoise_DOWN__1_denoise_tuple_get  COMMA std::get<2>(_data)
#define denoise_DOWN__1_denoise_types  COMMA int
#define denoise_DOWN__1_denoise_error_decl 
#define denoise_DOWN__1_denoise_error_use 
#define denoise_DOWN__1_denoise_send_img_decl  COMMA int K
#define denoise_DOWN__1_denoise_send_img_use  COMMA K
#define denoise_UP_denoise do {\
	if (_msg.getType() == "init") {\
		output(1, std::move(_msg));\
		return;\
	}\
} while (0);

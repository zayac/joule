#include "cal.h"
#include "denoise_CAL_FI_variables.h"
#include "cal.h"


#if !defined(f__1_denoise_color) || !defined(f__1_denoise_grayscale)
message send_img(std::vector<std::vector<double>> img denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_decl);
#endif


#if !defined(f__1_denoise_color) || !defined(f__1_denoise_grayscale)
message _2_error(std::string msg denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_decl);
#endif


#ifndef f_denoise__1_denoise_color
variant _1_denoise_color(std::vector<std::vector<double>> img denoise_DOWN__1_denoise_color_decl) {
    cv::Mat result;
    cv::fastNlMeansDenoisingColored(img, result);
    if(result.empty())
        _2_error("error" denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_use);
    else
        send_img(result denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_use);
}
#endif


#ifndef f_denoise__1_denoise_grayscale
variant _1_denoise_grayscale(std::vector<std::vector<double>> img denoise_DOWN__1_denoise_grayscale_decl) {
    cv::Mat result;
    cv::fastNlMeansDenoising(img, result);
    if(result.empty())
        _2_error("error" denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_use);
    else
        send_img(result denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_use);
}
#endif

void input_1(Message&& _msg) {
#ifndef f_denoise__1_denoise_color
	if (_msg.getType() == "denoise_color") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::vector<std::vector<double> > denoise_DOWN__1_denoise_color_types > _data;
		iarchive(std::get<0>(_data) denoise_DOWN__1_denoise_color_tuple_get );
		_1_denoise_color(std::get<0>(_data) denoise_DOWN__1_denoise_color_tuple_get );
		return;
	}
#endif
#ifndef f_denoise__1_denoise_grayscale
	if (_msg.getType() == "denoise_grayscale") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::vector<std::vector<double> > denoise_DOWN__1_denoise_grayscale_types > _data;
		iarchive(std::get<0>(_data) denoise_DOWN__1_denoise_grayscale_tuple_get );
		_1_denoise_grayscale(std::get<0>(_data) denoise_DOWN__1_denoise_grayscale_tuple_get );
		return;
	}
#endif
	denoise_UP_denoise
}

#if !defined(f_denoise__1_denoise_color) || !defined(f_denoise__1_denoise_grayscale)
message _2_error(std::string msg denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ denoise_DOWN__2_error_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(msg denoise_DOWN__1_denoise_color_1_denoise_grayscale_error_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif

#if !defined(f_denoise__1_denoise_color) || !defined(f_denoise__1_denoise_grayscale)
message send_img(std::vector<std::vector<double> > img denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ denoise_DOWN_send_img_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(img denoise_DOWN__1_denoise_color_1_denoise_grayscale_send_img_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif



#include "cal.h"
#include "denoise_CAL_FI_variables.h"
#include "cal.h"


#if !defined(f__1_denoise)
message send_img(std::vector<std::vector<double>> img denoise_DOWN__1_denoise_send_img_decl);
#endif


#if !defined(f__1_denoise)
message _2_error(std::string msg denoise_DOWN__1_denoise_error_decl);
#endif


#ifndef f_denoise__1_denoise
variant _1_denoise(std::vector<std::vector<double>> img, std::string kind denoise_DOWN__1_denoise_decl) {
    cv::Mat result;
    if (kind == "grayscale")
        cv::fastNlMeansDenoising(img, result);
    else if (kind == "color")
        cv::fastNlMeansDenoisingColored(img, result);
    else {
        _2_error("Unexpected image format" denoise_DOWN__1_denoise_error_use);
    }
    send_img(result denoise_DOWN__1_denoise_send_img_use);
}
#endif

void input_1(Message&& _msg) {
#ifndef f_denoise__1_denoise
	if (_msg.getType() == "denoise") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::vector<std::vector<double> >, std::string denoise_DOWN__1_denoise_types > _data;
		iarchive(std::get<0>(_data), std::get<1>(_data) denoise_DOWN__1_denoise_tuple_get );
		_1_denoise(std::get<0>(_data), std::get<1>(_data) denoise_DOWN__1_denoise_tuple_get );
		return;
	}
#endif
	denoise_UP_denoise
}

#if !defined(f_denoise__1_denoise)
message _2_error(std::string msg denoise_DOWN__1_denoise_error_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ denoise_DOWN__2_error_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(msg denoise_DOWN__1_denoise_error_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif

#if !defined(f_denoise__1_denoise)
message send_img(std::vector<std::vector<double> > img denoise_DOWN__1_denoise_send_img_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ denoise_DOWN_send_img_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(img denoise_DOWN__1_denoise_send_img_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif



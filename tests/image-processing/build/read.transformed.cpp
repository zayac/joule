#include "cal.h"
#include "read_CAL_FI_variables.h"
#include "cal.h"


#if !defined(f__1_read_color) || !defined(f__1_read_grayscale) || !defined(f__1_read_unchanged)
message send_img(std::vector<std::vector<double>> img, std::string kind read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_decl);
#endif


#if !defined(f__1_read_color) || !defined(f__1_read_grayscale) || !defined(f__1_read_unchanged)
message _2_error(std::string msg read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_decl);
#endif


#ifndef f_read__1_read_color
variant _1_read_color(std::string fname read_DOWN__1_read_color_decl) {
    cv::Mat image = cv::imread(fname, CV_LOAD_IMAGE_COLOR);
    if(!image.data)
        _2_error("Could not open or find the image" read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_use);
    else {
        send_img(image, "color" read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_use);
    }
}
#endif


#ifndef f_read__1_read_grayscale
variant _1_read_grayscale(std::string fname read_DOWN__1_read_grayscale_decl) {
    cv::Mat image = cv::imread(fname, CV_LOAD_IMAGE_GRAYSCALE);
    if(!image.data)
        _2_error("Could not open or find the image" read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_use);
    else {
        send_img(image, "grayscale" read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_use);
    }
}
#endif


#ifndef f_read__1_read_unchanged
variant _1_read_unchanged(std::string fname read_DOWN__1_read_unchanged_decl) {
    cv::Mat image = cv::imread(fname, CV_LOAD_IMAGE_UNCHANGED);
    if(!image.data)
        _2_error("Could not open or find the image" read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_use);
    else {
        send_img(image, "color" read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_use);
    }
}
#endif

void input_1(Message&& _msg) {
#ifndef f_read__1_read_color
	if (_msg.getType() == "read_color") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::string read_DOWN__1_read_color_types > _data;
		iarchive(std::get<0>(_data) read_DOWN__1_read_color_tuple_get );
		_1_read_color(std::get<0>(_data) read_DOWN__1_read_color_tuple_get );
		return;
	}
#endif
#ifndef f_read__1_read_grayscale
	if (_msg.getType() == "read_grayscale") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::string read_DOWN__1_read_grayscale_types > _data;
		iarchive(std::get<0>(_data) read_DOWN__1_read_grayscale_tuple_get );
		_1_read_grayscale(std::get<0>(_data) read_DOWN__1_read_grayscale_tuple_get );
		return;
	}
#endif
#ifndef f_read__1_read_unchanged
	if (_msg.getType() == "read_unchanged") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::string read_DOWN__1_read_unchanged_types > _data;
		iarchive(std::get<0>(_data) read_DOWN__1_read_unchanged_tuple_get );
		_1_read_unchanged(std::get<0>(_data) read_DOWN__1_read_unchanged_tuple_get );
		return;
	}
#endif
	read_UP_read
}

#if !defined(f_read__1_read_color) || !defined(f_read__1_read_grayscale) || !defined(f_read__1_read_unchanged)
message _2_error(std::string msg read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ read_DOWN__2_error_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(msg read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_error_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif

#if !defined(f_read__1_read_color) || !defined(f_read__1_read_grayscale) || !defined(f_read__1_read_unchanged)
message send_img(std::vector<std::vector<double> > img, std::string kind read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ read_DOWN_send_img_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(img, kind read_DOWN__1_read_color_1_read_grayscale_1_read_unchanged_send_img_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif


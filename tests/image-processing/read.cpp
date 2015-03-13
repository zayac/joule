#include "cal.h"

message send_img(cv::Mat img, std::string kind);
message _2_error(std::string msg);

variant _1_read_color(std::string fname) {
    cv::Mat image = cv::imread(fname, CV_LOAD_IMAGE_COLOR);
    if(!image.data)
        _2_error("Could not open or find the image");
    else
        send_img(image, "color");
}

variant _1_read_grayscale(std::string fname) {
    cv::Mat image = cv::imread(fname, CV_LOAD_IMAGE_GRAYSCALE);
    if(!image.data)
        _2_error("Could not open or find the image");
    else
        send_img(image, "grayscale");
}

variant _1_read_unchanged(std::string fname) {
    cv::Mat image = cv::imread(fname, CV_LOAD_IMAGE_UNCHANGED);
    if(!image.data)
        _2_error("Could not open or find the image");
    else
        send_img(image, "color");
}

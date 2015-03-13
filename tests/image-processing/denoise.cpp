#include "cal.h"

message send_img(cv::Mat img);
message _2_error(std::string msg);

variant _1_denoise(cv::Mat img, std::string kind) {
    cv::Mat result;
    if (kind == "grayscale")
        cv::fastNlMeansDenoising(img, result);
    else if (kind == "color")
        cv::fastNlMeansDenoisingColored(img, result);
    else {
        _2_error("Unexpected image format");
    }
    send_img(result);
}


#include "cal.h"

message send_img(std::vector<std::vector<double>> img);
message _2_error(std::string msg);

variant _1_denoise_color(std::vector<std::vector<double>> img) {
    cv::Mat result;
    cv::fastNlMeansDenoisingColored(img, result);
    if(result.empty())
        _2_error("error");
    else
        send_img(result);
}

variant _1_denoise_grayscale(std::vector<std::vector<double>> img) {
    cv::Mat result;
    cv::fastNlMeansDenoising(img, result);
    if(result.empty())
        _2_error("error");
    else
        send_img(result);
}


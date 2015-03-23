#include "cal.h"

class Dist {
public:
    double norm(const float* const a, const float * const b, int dims) const {
        return 0;
    }
};

message send_img(std::vector<std::vector<double>> img, Dist D);
message _2_error(std::string msg);

variant _1_denoise(std::vector<std::vector<double>> img, std::string kind) {
    cv::Mat result;
    if (kind == "grayscale")
        cv::fastNlMeansDenoising(img, result);
    else if (kind == "color")
        cv::fastNlMeansDenoisingColored(img, result);
    else {
        _2_error("Unexpected image format");
    }
    send_img(result, Dist());
}


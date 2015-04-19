#include "cal.h"
#include "init_CAL_FI_variables.h"
#include "cal.h"
#include <time.h>


#if !defined(f__1_init)
message _1_kMeans(std::vector<std::vector<double>> img, std::vector<std::vector<double>> old_centers_v, int K, double epsilon init_DOWN__1_init_kMeans_decl);
#endif


#if !defined(f__1_init)
message _2_error(std::string msg init_DOWN__1_init_error_decl);
#endif


static void generateRandomCenter(const std::vector<cv::Vec2f>& box, float* center) {
    size_t j, dims = box.size();
    float margin = 1.f / dims;
    for (j = 0; j < dims; ++j)
        center[j] = ((float)rand()*(1.f+margin*2.f)-margin)*(box[j][1] - box[j][0]) + box[j][0];
}

#ifndef f_init__1_init
variant _1_init(std::vector<std::vector<double>> img, int K init_DOWN__1_init_decl) {
    cv::Mat data0(img);
    bool isrow = data0.rows == 1 && data0.channels() > 1;
    int N = !isrow ? data0.rows : data0.cols;
    int dims = (!isrow ? data0.cols : 1) * data0.channels();
    int type = data0.depth();

    if (!(data0.dims <= 2 && type == CV_32F && K > 0 && N >= K)) {
        _2_error("Cannot perform K-means algorithm for this configuration" init_DOWN__1_init_error_use);
        return;
    }

    cv::Mat data(N, dims, CV_32F, data0.ptr(), isrow ? dims * sizeof(float) : static_cast<size_t>(data0.step));

    cv::Mat centers(K, dims, type), old_centers(K, dims, type), temp(1, dims, type);
    std::vector<int> counters(K);
    std::vector<cv::Vec2f> _box(dims);
    cv::Vec2f* box = &_box[0];
    double best_compactness = DBL_MAX, compactness = 0;
    int a, iter, i, j, k;
    double epsilon = 0.;

    const float* sample = data.ptr<float>(0);
    for (j = 0; j < dims; ++j)
        box[j] = cv::Vec2f(sample[j], sample[j]);

    for (i = 1; i < N; ++i) {
        sample = data.ptr<float>(i);
        for (j = 0; j < dims; ++j) {
            float v = sample[j];
            box[j][0] = std::min(box[j][0], v);
            box[j][1] = std::max(box[j][1], v);
        }
    }

    generateRandomCenter(_box, centers.ptr<float>(k));

    std::vector<std::vector<double>> _centers;
    centers.copyTo(_centers);
    _1_kMeans(img, _centers, K, epsilon init_DOWN__1_init_kMeans_use);
}
#endif

void input_1(Message&& _msg) {
#ifndef f_init__1_init
	if (_msg.getType() == "init") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::vector<std::vector<double> >, int init_DOWN__1_init_types > _data;
		iarchive(std::get<0>(_data), std::get<1>(_data) init_DOWN__1_init_tuple_get );
		_1_init(std::get<0>(_data), std::get<1>(_data) init_DOWN__1_init_tuple_get );
		return;
	}
#endif
	init_UP_init
}

#if !defined(f_init__1_init)
message _1_kMeans(std::vector<std::vector<double> > img, std::vector<std::vector<double> > old_centers_v, int K, double epsilon init_DOWN__1_init_kMeans_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ init_DOWN__1_kMeans_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(img, old_centers_v, K, epsilon init_DOWN__1_init_kMeans_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif

#if !defined(f_init__1_init)
message _2_error(std::string msg init_DOWN__1_init_error_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ init_DOWN__2_error_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(msg init_DOWN__1_init_error_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif



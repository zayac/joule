#include "cal.h"
#include "kMeans_CAL_FI_variables.h"
#include "cal.h"


#if !defined(f__1_kMeans)
message _1_loop(std::vector<std::vector<double>> img, std::vector<std::vector<double>> old_centers_v, int K, double epsilon kMeans_DOWN__1_kMeans_loop_decl);
#endif


#if !defined(f__1_kMeans)
message result(std::vector<std::vector<double>> centers kMeans_DOWN__1_kMeans_result_decl);
#endif


#if !defined(f__1_kMeans)
message error(std::string msg kMeans_DOWN__1_kMeans_error_decl);
#endif


#ifndef f_kMeans__1_kMeans
variant _1_kMeans(std::vector<std::vector<double>> img, std::vector<std::vector<double>> old_centers_v, int K, double epsilon kMeans_DOWN__1_kMeans_decl) {
    cv::Mat centers(cv::Scalar(0)), old_centers(old_centers_v);
    cv::Mat data0(img);
    bool isrow = data0.rows == 1 && data0.channels() > 1;
    int N = !isrow ? data0.rows : data0.cols;
    int dims = (!isrow ? data0.cols : 1) * data0.channels();
    int type = data0.depth();

    if (!(data0.dims <= 2 && type == CV_32F && K > 0 && N >= K)) {
        error("Cannot perform K-means algorithm for this configuration" kMeans_DOWN__1_kMeans_error_use);
        return;
    }

    cv::Mat data(N, dims, CV_32F, data0.ptr(), isrow ? dims * sizeof(float) : static_cast<size_t>(data0.step));
    cv::Mat temp(1, dims, type);

    std::vector<int> counters(K, 0);
    const float* sample = data.ptr<float>(0);

    double max_center_shift = 0;

    for (int k = 0; k < K; ++k) {
        if (counters[k] != 0)
            continue;

        int max_k = 0;
        for (int k1 = 1; k1 < K; ++k1) {
            if (counters[max_k] < counters[k1])
                max_k = k1;
        }

        double max_dist = 0;
        int farthest_i = -1;
        float* new_center = centers.ptr<float>(k);
        float* old_center = centers.ptr<float>(max_k);
        float* _old_center = temp.ptr<float>();
        float scale = 1.f/counters[max_k];
        for (int j = 0; j < dims; ++j)
            _old_center[j] = old_center[j]*scale;

        for (int i = 0; i < N; ++i) {
            sample = data.ptr<float>(i);
            double dist = cv::normL2Sqr_(sample, _old_center, dims);

            if (max_dist <= dist) {
                max_dist = dist;
                farthest_i = i;
            }
        }

        counters[max_k]--;
        counters[k]++;
        sample = data.ptr<float>(farthest_i);

        for (int j = 0; j < dims; ++j) {
            old_center[j] -= sample[j];
            new_center[j] += sample[j];
        }
    }

    for (int k = 0; k < K; ++k) {
        float* center = centers.ptr<float>(k);
        if (counters[k] == 0) {
            error("For some reason one of the clusters is empty" kMeans_DOWN__1_kMeans_error_use);
            return;
        }
        float scale = 1.f/counters[k];
        for (int j = 0; j < dims; ++j)
            center[j] *= scale;

        double dist = 0;
        const float* old_center = old_centers.ptr<float>(k);
        for (int j = 0; j < dims; ++j) {
            double t = center[j] - old_center[j];
            dist += t * t;
        }
        max_center_shift = std::max(max_center_shift, dist);
    }

    std::vector<std::vector<double>> _centers;
    centers.copyTo(_centers);
    if (max_center_shift <= epsilon) {
        result(_centers kMeans_DOWN__1_kMeans_result_use);
    } else {
        _1_loop(img, _centers, K, epsilon kMeans_DOWN__1_kMeans_loop_use);
    }
}
#endif

void input_1(Message&& _msg) {
#ifndef f_kMeans__1_kMeans
	if (_msg.getType() == "kMeans") {
		cereal::BinaryInputArchive iarchive(_msg.ss);
		std::tuple<std::vector<std::vector<double> >, std::vector<std::vector<double> >, int, double kMeans_DOWN__1_kMeans_types > _data;
		iarchive(std::get<0>(_data), std::get<1>(_data), std::get<2>(_data), std::get<3>(_data) kMeans_DOWN__1_kMeans_tuple_get );
		_1_kMeans(std::get<0>(_data), std::get<1>(_data), std::get<2>(_data), std::get<3>(_data) kMeans_DOWN__1_kMeans_tuple_get );
		return;
	}
#endif
	kMeans_UP_kMeans
}

#if !defined(f_kMeans__1_kMeans)
message _1_loop(std::vector<std::vector<double> > img, std::vector<std::vector<double> > old_centers_v, int K, double epsilon kMeans_DOWN__1_kMeans_loop_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ kMeans_DOWN__1_loop_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(img, old_centers_v, K, epsilon kMeans_DOWN__1_kMeans_loop_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif

#if !defined(f_kMeans__1_kMeans)
message error(std::string msg kMeans_DOWN__1_kMeans_error_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ kMeans_DOWN_error_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(msg kMeans_DOWN__1_kMeans_error_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif

#if !defined(f_kMeans__1_kMeans)
message result(std::vector<std::vector<double> > centers kMeans_DOWN__1_kMeans_result_decl) {
	for (const std::pair<int, std::string>& _p : std::vector<std::pair<int, std::string>>({ kMeans_DOWN_result_ochannels })) {
		Message _msg;
		cereal::BinaryOutputArchive oarchive(_msg.ss);
		oarchive(centers kMeans_DOWN__1_kMeans_result_use);
		_msg.setType(_p.second);
		output(_p.first, std::move(_msg));
	}
}
#endif



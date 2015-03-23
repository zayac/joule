#include "cal.h"

class Distance {
public:
    double norm(const float * const a, const float * const b, int dims) const;
};

message _1_loop(std::vector<std::vector<double>> img, std::vector<std::vector<double>> old_centers_v, int K, double epsilon, Distance D);
message result(std::vector<std::vector<double>> centers);
message error(std::string msg);

variant _1_kMeans(std::vector<std::vector<double>> img, std::vector<std::vector<double>> old_centers_v, int K, double epsilon, Distance D) {
    cv::Mat centers(cv::Scalar(0)), old_centers(old_centers_v);
    cv::Mat data0(img);
    bool isrow = data0.rows == 1 && data0.channels() > 1;
    int N = !isrow ? data0.rows : data0.cols;
    int dims = (!isrow ? data0.cols : 1) * data0.channels();
    int type = data0.depth();

    if (!(data0.dims <= 2 && type == CV_32F && K > 0 && N >= K)) {
        error("Cannot perform K-means algorithm for this configuration");
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
            double dist = D.norm(sample, _old_center, dims);

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
            error("For some reason one of the clusters is empty");
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
        result(_centers);
    } else {
        _1_loop(img, _centers, K, epsilon, D);
    }
}


typedef void variant;
typedef void message;
typedef void variant_message;

namespace global {

class GlobalObject {
private:
    int a;
public:
    GlobalObject() : a(5) {}
};

}

#ifdef CAL_FI_VARIABLES_ENABLED
#include "CAL_FI_variables.h"
#endif

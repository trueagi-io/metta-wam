#include <chicken.h>

void CHICKEN_INIT(void)
{
    CHICKEN_run(C_toplevel);
}

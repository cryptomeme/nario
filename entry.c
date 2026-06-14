#include "HsFFI.h"
#ifdef __GLASGOW_HASKELL__
#include "Main_stub.h"
#endif

#include <SDL/SDL.h>

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);

  start_hs();

  hs_exit();
  return 0;
}

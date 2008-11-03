#include "HsFFI.h"
#ifdef __GLASGOW_HASKELL__
#include "Main_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_Main ( void );
#endif

#include "SDL.h"

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_Main);
#endif

  start_hs();

  hs_exit();
  return 0;
}

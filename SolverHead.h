#include <math.h>
#include <stdio.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <kinsol/kinsol.h>
#include <kinsol/kinsol_dense.h>
#include <ida/ida.h>
#include <ida/ida_spgmr.h>
#include <ida/ida_dense.h>
#include <stdlib.h>

static int modelResiduals(double t, N_Vector y, N_Vector derivy, N_Vector resids, void* user_data);
static int boundaryResiduals(double t, N_Vector y, N_Vector derivy, N_Vector resids, void* user_data);
static int modelRoots(double t, N_Vector y, N_Vector derivy, realtype *gout, void *user_data);
static int gNumVars, gNumInterventions, gNumEquations, gNumBoundaryEquations;
static void setupIdVector(N_Vector id);

static double max(double x, double y) { if (x > y) return x; else return y; }
static double min(double x, double y) { if (x < y) return x; else return y; }

static int imax(int x, int y) { if (x > y) return x; else return y; }

static char* quote(const char* msg)
{
  const char* p;
  int l;
  for (p = msg; *p; p++)
    if (*p == '\\' || *p == '\n' || *p == '"')
      l += 2;
    else
      l++;

  char* news = malloc(l + 1);
  char* newp = news;
  for (p = msg; *p; p++)
    if (*p == '\\' || *p == '\n' || *p == '"')
    {
      *newp++ = '\\';
      if (*p == '\n')
        *newp++ = 'n';
      else
        *newp++ = *p;
    }
    else
      *newp++ = *p;
  *newp++ = 0;
  return news;
}
static void checkedConditionFail(const char* msg)
{
  printf("CheckedConditionFail \"%s\"\n]\n", quote(msg));
  exit(3);
}

static void
handle_error(int err_code, const char* module, const char* function, char* msg, void* user_data)
{
  if (err_code >= 0)
  {
    printf("Warning (%u, \"%s\", \"%s\", \"%s\"),\n", err_code, module, function, msg);
    return;
  }

  printf("FatalError (%u, \"%s\", \"%s\", \"%s\")\n]\n", err_code, module, function, msg);
  exit(1);
}

static void
show_results(double t, N_Vector y, N_Vector derivy)
{
  double* v = N_VGetArrayPointer(y),
        * dv = N_VGetArrayPointer(derivy);
  int i;
  printf("Result (%g, [", t);
  for (i = 0; i < gNumVars; i++)
  {
    if (i > 0)
      printf(",");
    printf("%g", v[i]);
  }
  printf("], [");
  for (i = 0; i < gNumVars; i++)
  {
    if (i > 0)
      printf(",");
    printf("%g", dv[i]);
  }
  printf("]),\n");
}

double gtStart;

static int
iv_sys_fn(N_Vector u, N_Vector f, void* dat)
{
  double* yp = N_VGetArrayPointer(u),
        * dyp = yp + gNumVars;
  double *fp = N_VGetArrayPointer(f);

  N_Vector y = N_VMake_Serial(gNumVars, yp), dy = N_VMake_Serial(gNumVars, dyp);

  int i;
  
  N_VConst(0, f);

  if (modelResiduals(gtStart, y, dy, f, NULL) != 0)
    return -1;

  if (boundaryResiduals(gtStart, y, dy, f, NULL) != 0)
    return -1;

  N_VDestroy(y);
  N_VDestroy(dy);

  return 0;
}

static void
do_ida_solve(double tStart, double tMaxSolverStep, double tMaxReportStep, double tEnd, int everyStep,
             double reltol, double abstol)
{
  void *ida_mem = IDACreate();
  N_Vector y = N_VNew_Serial(gNumVars);
  N_Vector yp = N_VNew_Serial(gNumVars);

  gtStart = tStart;
  N_VConst(0, y);
  N_VConst(0, yp);

  printf("[\n");

  {
    void * kin_mem = KINCreate();
    int kinsize = imax(gNumVars * 2, gNumEquations + gNumBoundaryEquations);
    N_Vector ksy = N_VNew_Serial(kinsize),
             scale = N_VNew_Serial(kinsize);

    N_VConst(1, ksy);
    N_VConst(100.0, scale);

    KINSetErrHandlerFn(kin_mem, handle_error, NULL);
    KINInit(kin_mem, iv_sys_fn, ksy);
    KINSetMaxNewtonStep(kin_mem, 1E100);
    KINSpgmr(kin_mem, 0);
    KINSol(kin_mem, ksy, KIN_LINESEARCH, scale, scale);

    {
      double * ksyptr = N_VGetArrayPointer(ksy), *yptr = N_VGetArrayPointer(y), 
        *ypptr = N_VGetArrayPointer(yp);
      int i;
      for (i = 0; i < gNumVars; i++)
        *yptr++ = *ksyptr++;
      for (i = 0; i < gNumVars; i++)
        *ypptr++ = *ksyptr++;
    }

    KINFree(&kin_mem);
    N_VDestroy(ksy);
    N_VDestroy(scale);
  }
  
  IDAInit(ida_mem, modelResiduals, tStart, y, yp);
  IDASStolerances(ida_mem, reltol, abstol);
  IDASpgmr(ida_mem, 0);
  // IDADense(ida_mem, imax(gNumVars, gNumEquations));
  IDASetErrHandlerFn(ida_mem, handle_error, NULL);
  IDASetNoInactiveRootWarn(ida_mem);
  IDASetMaxStep(ida_mem, tMaxSolverStep);
  IDASetStopTime(ida_mem, tEnd);

  N_Vector idv = N_VNew_Serial(imax(gNumVars, gNumEquations));
  setupIdVector(idv);
  IDASetId(ida_mem, idv);
  N_VDestroy(idv);

  // IDACalcIC(ida_mem, IDA_YA_YDP_INIT, tStart + tMaxReportStep);
  IDARootInit(ida_mem, gNumInterventions, modelRoots);

  show_results(tStart, y, yp);

  double tnext = tStart;
  while (1)
  {
    tnext += tMaxReportStep;
    int ret = IDASolve(ida_mem, tnext, &tnext, y, yp, everyStep ? IDA_ONE_STEP : IDA_NORMAL);
    show_results(tnext, y, yp);
    if (ret == IDA_TSTOP_RETURN)
      break;
  }

  IDAFree(&ida_mem);

  printf("Success\n]\n");
}

static double smax(double x, double y)
{
  // return (x /* + y * y */);
  return ((x >= 0.0) ? 1.0 : -1.0) * ((y >= 0.0) ? 1.0 : -1.0) * max(fabs(x), fabs(y));
}

/*
 * Now the main solver...
 */
int
main(int argc, char** argv)
{
  if (argc != 8)
  {
    printf("Usage: solver tStart maxSolverStep maxReportStep tEnd showEveryStep reltol abstol\n");
    return 2;
  }
  do_ida_solve(strtod(argv[1], NULL), strtod(argv[2], NULL), strtod(argv[3], NULL),
               strtod(argv[4], NULL), strtoul(argv[5], NULL, 10),
               strtod(argv[6], NULL), strtod(argv[7], NULL));
  return 0;
}

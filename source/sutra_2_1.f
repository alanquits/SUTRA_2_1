C     MAIN PROGRAM       S U T R A _ M A I N       SUTRA VERSION 2.1     SUTRA_MAIN.....100
C_______________________________________________________________________ SUTRA_MAIN.....200
C|                                                                     | SUTRA_MAIN.....300
C|                                                                     | SUTRA_MAIN.....400
C|                   UNITED STATES GEOLOGICAL SURVEY                   | SUTRA_MAIN.....500
C|          MODEL FOR SATURATED-UNSATURATED, VARIABLE-DENSITY          | SUTRA_MAIN.....600
C|          GROUND-WATER FLOW WITH SOLUTE OR ENERGY TRANSPORT          | SUTRA_MAIN.....700
C|                                                                     | SUTRA_MAIN.....800
C|                                                                     | SUTRA_MAIN.....900
C|                                                                     | SUTRA_MAIN....1000
C|                                                                     | SUTRA_MAIN....1100
C|                       _______________________                       | SUTRA_MAIN....1200
C|                      |                       |                      | SUTRA_MAIN....1300
C|                      |   S   U   T   R   A   |                      | SUTRA_MAIN....1400
C|                      |_______________________|                      | SUTRA_MAIN....1500
C|                                                                     | SUTRA_MAIN....1600
C|                                                                     | SUTRA_MAIN....1700
C|                Saturated    Unsaturated    TRAnsport                | SUTRA_MAIN....1800
C|                =            =              ===                      | SUTRA_MAIN....1900
C|                                                                     | SUTRA_MAIN....2000
C|                                                                     | SUTRA_MAIN....2100
C|                                                                     | SUTRA_MAIN....2200
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....2300
C|    *                                                           *    | SUTRA_MAIN....2400
C|    *  PHYSICS OPTIONS:                                         *    | SUTRA_MAIN....2500
C|    *  -> Saturated and/or unsaturated ground-water flow        *    | SUTRA_MAIN....2600
C|    *  -> Either single species reactive solute transport       *    | SUTRA_MAIN....2700
C|    *     or thermal energy transport                           *    | SUTRA_MAIN....2800
C|    *  GEOMETRY OPTIONS:                                        *    | SUTRA_MAIN....2900
C|    *  -> Two-dimensional areal or cross-sectional simulation   *    | SUTRA_MAIN....3000
C|    *  -> Fully three-dimensional simulation                    *    | SUTRA_MAIN....3100
C|    *  -> Either two- or three-dimensional Cartesian or         *    | SUTRA_MAIN....3200
C|    *     two-dimensional radial coordinates                    *    | SUTRA_MAIN....3300
C|    *  NUMERICAL METHODS:                                       *    | SUTRA_MAIN....3400
C|    *  -> Hybrid Galerkin-finite-element method and             *    | SUTRA_MAIN....3500
C|    *     integrated-finite-difference method                   *    | SUTRA_MAIN....3600
C|    *     with two-dimensional quadrilateral or                 *    | SUTRA_MAIN....3700
C|    *     three-dimensional generalized hexahedral              *    | SUTRA_MAIN....3800
C|    *     finite elements                                       *    | SUTRA_MAIN....3900
C|    *  -> Finite-difference time discretization                 *    | SUTRA_MAIN....4000
C|    *  -> Nonlinear iterative, sequential or steady-state       *    | SUTRA_MAIN....4100
C|    *     solution modes                                        *    | SUTRA_MAIN....4200
C|    *  -> Direct and iterative solvers                          *    | SUTRA_MAIN....4300
C|    *  OUTPUT OPTIONS:                                          *    | SUTRA_MAIN....4400
C|    *  -> Optional fluid velocity calculation                   *    | SUTRA_MAIN....4500
C|    *  -> Optional observation well output                      *    | SUTRA_MAIN....4600
C|    *  -> Optional fluid mass and solute mass or energy budget  *    | SUTRA_MAIN....4700
C|    *  -> Flexible, columnwise output of solution               *    | SUTRA_MAIN....4800
C|    *                                                           *    | SUTRA_MAIN....4900
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....5000
C|                                                                     | SUTRA_MAIN....5100
C|                                                                     | SUTRA_MAIN....5200
C|                                                                     | SUTRA_MAIN....5300
C|       Complete explanation of the function and use of this code     | SUTRA_MAIN....5400
C|       is given in :                                                 | SUTRA_MAIN....5500
C|                                                                     | SUTRA_MAIN....5600
C|       Voss, Clifford I., and Provost, Alden M., 2002,               | SUTRA_MAIN....5700
C|            SUTRA - A model for saturated-unsaturated                | SUTRA_MAIN....5800
C|            variable-density ground-water flow with                  | SUTRA_MAIN....5900
C|            solute or energy transport: U.S. Geological              | SUTRA_MAIN....6000
C|            Survey Water-Resources Investigations Report             | SUTRA_MAIN....6100
C|            02-4231, 250p. (Version of June 2, 2008)                 | SUTRA_MAIN....6200
C|                                                                     | SUTRA_MAIN....6300
C|                                                                     | SUTRA_MAIN....6400
C|                                                                     | SUTRA_MAIN....6500
C|       Users who wish to be notified of updates of the SUTRA         | SUTRA_MAIN....6600
C|       code and documentation may be added to the mailing list       | SUTRA_MAIN....6700
C|       by sending a request to :                                     | SUTRA_MAIN....6800
C|                                                                     | SUTRA_MAIN....6900
C|                           SUTRA Support                             | SUTRA_MAIN....7000
C|                       U.S. Geological Survey                        | SUTRA_MAIN....7100
C|                        431 National Center                          | SUTRA_MAIN....7200
C|                       Reston, Virginia 20192                        | SUTRA_MAIN....7300
C|                                USA                                  | SUTRA_MAIN....7400
C|                                                                     | SUTRA_MAIN....7500
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....7600
C|    *                                                           *    | SUTRA_MAIN....7700
C|    *  The SUTRA code and documentation were originally         *    | SUTRA_MAIN....7800
C|    *  prepared under a joint research project of the U.S.      *    | SUTRA_MAIN....7900
C|    *  Geological Survey, Department of the Interior, Reston,   *    | SUTRA_MAIN....8000
C|    *  Virginia, and the Engineering and Services Laboratory,   *    | SUTRA_MAIN....8100
C|    *  U.S. Air Force Engineering and Services Center, Tyndall  *    | SUTRA_MAIN....8200
C|    *  A.F.B., Florida.  The SUTRA code and documentation are   *    | SUTRA_MAIN....8300
C|    *  available for unlimited distribution.                    *    | SUTRA_MAIN....8400
C|    *                                                           *    | SUTRA_MAIN....8500
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....8600
C|                                                                     | SUTRA_MAIN....8700
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....8800
C|    *                                                           *    | SUTRA_MAIN....8900
C|    *  Original Release: 1984                                   *    | SUTRA_MAIN....9000
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9100
C|    *                                                           *    | SUTRA_MAIN....9200
C|    *  First Revision: June 1990, Version V06902D               *    | SUTRA_MAIN....9300
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9400
C|    *                                                           *    | SUTRA_MAIN....9500
C|    *  Second Revision: September 1997, Version V09972D         *    | SUTRA_MAIN....9600
C|    *  by: C.I. Voss and David Boldt, U.S. Geological Survey    *    | SUTRA_MAIN....9700
C|    *                                                           *    | SUTRA_MAIN....9800
C|    *  Third Revision: September 2003, Version 2D3D.1           *    | SUTRA_MAIN....9900
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10000
C|    *                                                           *    | SUTRA_MAIN...10100
C|    *  Fourth Revision: June 2008, Version 2.1                  *    | SUTRA_MAIN...10200
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10300
C|    *                                                           *    | SUTRA_MAIN...10400
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN...10500
C|                                                                     | SUTRA_MAIN...10600
C|                                                                     | SUTRA_MAIN...10700
C|_____________________________________________________________________| SUTRA_MAIN...10800
C                                                                        SUTRA_MAIN...10900
C                                                                        SUTRA_MAIN...11000
C                                                                        SUTRA_MAIN...11100
      PROGRAM SUTRA_MAIN                                                 SUTRA_MAIN...11200
      USE ALLARR                                                         SUTRA_MAIN...11300
      USE PTRDEF                                                         SUTRA_MAIN...11400
      USE EXPINT                                                         SUTRA_MAIN...11500
      USE SCHDEF                                                         SUTRA_MAIN...11600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA_MAIN...11700
      PARAMETER (NCOLMX=9)                                               SUTRA_MAIN...11800
C                                                                        SUTRA_MAIN...11900
C.....PROGRAMMERS SET SUTRA VERSION NUMBER HERE (8 CHARACTERS MAXIMUM)   SUTRA_MAIN...12000
      CHARACTER*8, PARAMETER :: VERN='2.1'                               SUTRA_MAIN...12100
C                                                                        SUTRA_MAIN...12200
      CHARACTER*8 VERNUM, VERNIN                                         SUTRA_MAIN...12300
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA_MAIN...12400
      CHARACTER*80 SIMULA(5),MSHTYP(2),LAYNOR(2),SIMSTR,MSHSTR,LAYSTR    SUTRA_MAIN...12500
      CHARACTER*80 CUNSAT, CSSFLO ,CSSTRA, CREAD                         SUTRA_MAIN...12600
      CHARACTER*80 UNSSTR, SSFSTR ,SSTSTR, RDSTR                         SUTRA_MAIN...12700
      CHARACTER*80 UNAME,FNAME,FNINP,FNICS                               SUTRA_MAIN...12800
      CHARACTER*80 ERRCOD,CHERR(10)                                      SUTRA_MAIN...12900
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA_MAIN...13000
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA_MAIN...13100
      CHARACTER*10 ADSMOD                                                SUTRA_MAIN...13200
      CHARACTER INTFIL*1000                                              SUTRA_MAIN...13300
      INTEGER RMVDIM,IMVDIM,CMVDIM,PMVDIM                                SUTRA_MAIN...13400
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                SUTRA_MAIN...13500
      LOGICAL ONCEFO                                                     SUTRA_MAIN...13600
      DIMENSION FNAME(0:8),IUNIT(0:8)                                    SUTRA_MAIN...13700
      DIMENSION FNAIN(2,20)                                              SUTRA_MAIN...13800
      DIMENSION INERR(10), RLERR(10)                                     SUTRA_MAIN...13900
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             SUTRA_MAIN...14000
      DIMENSION NKS(2), KLIST(2,20)                                      SUTRA_MAIN...14100
      DIMENSION KTYPE(2)                                                 SUTRA_MAIN...14200
      COMMON /CLAY/ LAYSTR                                               SUTRA_MAIN...14300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA_MAIN...14400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SUTRA_MAIN...14500
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  SUTRA_MAIN...14600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA_MAIN...14700
     1   NSOP,NSOU,NBCN                                                  SUTRA_MAIN...14800
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SUTRA_MAIN...14900
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SUTRA_MAIN...15000
      COMMON /FNAINS/ FNAIN                                              SUTRA_MAIN...15100
      COMMON /FNAMES/ UNAME,FNAME                                        SUTRA_MAIN...15200
      COMMON /FO/ONCEFO                                                  SUTRA_MAIN...15300
      COMMON /FUNINS/ NKS,KLIST                                          SUTRA_MAIN...15400
      COMMON /FUNITA/ IUNIT                                              SUTRA_MAIN...15500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SUTRA_MAIN...15600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA_MAIN...15700
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            SUTRA_MAIN...15800
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA_MAIN...15900
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        SUTRA_MAIN...16000
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTRA_MAIN...16100
     1   KSCRN,KPAUSE                                                    SUTRA_MAIN...16200
      COMMON /MODSOR/ ADSMOD                                             SUTRA_MAIN...16300
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      SUTRA_MAIN...16400
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA_MAIN...16500
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA_MAIN...16600
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7, ONCEK8                       SUTRA_MAIN...16700
      COMMON /SCH/ NSCH,ISCHTS                                           SUTRA_MAIN...16800
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SUTRA_MAIN...16900
      COMMON /SOLVN/ NSLVRS                                              SUTRA_MAIN...17000
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SUTRA_MAIN...17100
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA_MAIN...17200
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  SUTRA_MAIN...17300
      COMMON /VER/ VERNUM, VERNIN                                        SUTRA_MAIN...17400
C....."NSLVRS" AND THE ARRAYS "SOLWRD" AND "SOLNAM" ARE INITIALIZED      SUTRA_MAIN...17500
C        IN THE BLOCK-DATA SUBPROGRAM "BDINIT"                           SUTRA_MAIN...17600
C                                                                        SUTRA_MAIN...17700
C                                                                        SUTRA_MAIN...17800
C.....COPY PARAMETER VERN (SUTRA VERSION NUMBER) TO VARIABLE VERNUM,     SUTRA_MAIN...17900
C        WHICH IS PASSED THROUGH COMMON BLOCK VER.                       SUTRA_MAIN...18000
      VERNUM = VERN                                                      SUTRA_MAIN...18100
C                                                                        SUTRA_MAIN...18200
C.....SET THE ALLOCATION FLAGS TO FALSE                                  SUTRA_MAIN...18300
      ALLO1 = .FALSE.                                                    SUTRA_MAIN...18400
      ALLO2 = .FALSE.                                                    SUTRA_MAIN...18500
      ALLO3 = .FALSE.                                                    SUTRA_MAIN...18600
C                                                                        SUTRA_MAIN...18700
C_______________________________________________________________________ SUTRA_MAIN...18800
C|                                                                     | SUTRA_MAIN...18900
C|  *****************************************************************  | SUTRA_MAIN...19000
C|  *                                                               *  | SUTRA_MAIN...19100
C|  *   **********  M E M O R Y   A L L O C A T I O N  **********   *  | SUTRA_MAIN...19200
C|  *                                                               *  | SUTRA_MAIN...19300
C|  *   The main arrays used by SUTRA are dimensioned dynamically   *  | SUTRA_MAIN...19400
C|  *   in the main program, SUTRA_MAIN.  The amount of storage     *  | SUTRA_MAIN...19500
C|  *   required by these arrays depends on the dimensionality of   *  | SUTRA_MAIN...19600
C|  *   the problem (2D or 3D) and the particular solver(s) used.   *  | SUTRA_MAIN...19700
C|  *                                                               *  | SUTRA_MAIN...19800
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...19900
C|  *               |     sum of real     |    sum of integer   |   *  | SUTRA_MAIN...20000
C|  *               |   array dimensions  |   array dimensions  |   *  | SUTRA_MAIN...20100
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...20200
C|  *   | 2D,       | (2*NBI+27)*NN+19*NE |  NN+5*NE+NSOP+NSOU  |   *  | SUTRA_MAIN...20300
C|  *   | direct    |    +3*NBCN+6*NOBS   |    +2*NBCN+NOBS     |   *  | SUTRA_MAIN...20400
C|  *   | solver    |      +2*NSCH+22     |      +3*NSCH+4      |   *  | SUTRA_MAIN...20500
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...20600
C|  *   | 2D,       | 2*NELT+28*NN+19*NE  | NELT+2*NN+5*NE+NSOP |   *  | SUTRA_MAIN...20700
C|  *   | iterative |   +3*NBCN+6*NOBS    |  +NSOU+2*NBCN+NOBS  |   *  | SUTRA_MAIN...20800
C|  *   | solver(s) |   +2*NSCH+NWF+220   |    +3*NSCH+NWI+2    |   *  | SUTRA_MAIN...20900
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21000
C|  *   | 3D,       | (2*NBI+27)*NN+45*NE |  NN+9*NE+NSOP+NSOU  |   *  | SUTRA_MAIN...21100
C|  *   | direct    |    +3*NBCN+6*NOBS   |    +2*NBCN+NOBS     |   *  | SUTRA_MAIN...21200
C|  *   | solver    |      +2*NSCH+8      |      +3*NSCH+4      |   *  | SUTRA_MAIN...21300
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21400
C|  *   | 3D,       | 2*NELT+28*NN+45*NE  | NELT+2*NN+9*NE+NSOP |   *  | SUTRA_MAIN...21500
C|  *   | iterative |   +3*NBCN+6*NOBS    |  +NSOU+2*NBCN+NOBS  |   *  | SUTRA_MAIN...21600
C|  *   | solver(s) |   +2*NSCH+NWF+6     |    +3*NSCH+NWI+2    |   *  | SUTRA_MAIN...21700
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21800
C|  *                                                               *  | SUTRA_MAIN...21900
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...22000
C|  *               |  sum of character   |  sum of dimensions  |   *  | SUTRA_MAIN...22100
C|  *               |   array effective   |     of arrays of    |   *  | SUTRA_MAIN...22200
C|  *               |     dimensions      |       pointers      |   *  | SUTRA_MAIN...22300
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...22400
C|  *   | all cases |  73*NOBS + 89*NSCH  |        2*NSCH       |   *  | SUTRA_MAIN...22500
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...22600
C|  *                                                               *  | SUTRA_MAIN...22700
C|  *   Quantities in the tables above are defined in Section 7.3   *  | SUTRA_MAIN...22800
C|  *   of the published documentation (Voss & Provost, 2002,       *  | SUTRA_MAIN...22900
C|  *   USGS Water-Resources Investigations Report 02-4231,         *  | SUTRA_MAIN...23000
C|  *   Version of June 2, 2008).                                   *  | SUTRA_MAIN...23100
C|  *                                                               *  | SUTRA_MAIN...23200
C|  *   During each run, SUTRA writes memory usage information to   *  | SUTRA_MAIN...23300
C|  *   the LST output file.                                        *  | SUTRA_MAIN...23400
C|  *                                                               *  | SUTRA_MAIN...23500
C|  *****************************************************************  | SUTRA_MAIN...23600
C|_____________________________________________________________________| SUTRA_MAIN...23700
C                                                                        SUTRA_MAIN...23800
C                                                                        SUTRA_MAIN...23900
C_______________________________________________________________________ SUTRA_MAIN...24000
C|                                                                     | SUTRA_MAIN...24100
C|  *****************************************************************  | SUTRA_MAIN...24200
C|  *                                                               *  | SUTRA_MAIN...24300
C|  *   ***********  F I L E   A S S I G N M E N T S  ***********   *  | SUTRA_MAIN...24400
C|  *                                                               *  | SUTRA_MAIN...24500
C|  *   Unit K0 contains the FORTRAN unit number and filename       *  | SUTRA_MAIN...24600
C|  *   assignments for the various SUTRA input and output files.   *  | SUTRA_MAIN...24700
C|  *   Each line of Unit K0 begins with a file type, followed by   *  | SUTRA_MAIN...24800
C|  *   a unit number and a filename for that type, all in free     *  | SUTRA_MAIN...24900
C|  *   format. Permitted file types are INP, ICS, LST, RST, NOD,   *  | SUTRA_MAIN...25000
C|  *   ELE, OBS, OBC, and SMY. Assignments may be listed in any    *  | SUTRA_MAIN...25100
C|  *   order.  Example ("#" indicates a comment):                  *  | SUTRA_MAIN...25200
C|  *   'INP'  50  'project.inp'   # required                       *  | SUTRA_MAIN...25300
C|  *   'ICS'  55  'project.ics'   # required                       *  | SUTRA_MAIN...25400
C|  *   'LST'  60  'project.lst'   # required                       *  | SUTRA_MAIN...25500
C|  *   'RST'  66  'project.rst'   # required if ISTORE>0           *  | SUTRA_MAIN...25600
C|  *   'NOD'  70  'project.nod'   # optional                       *  | SUTRA_MAIN...25700
C|  *   'ELE'  80  'project.ele'   # optional                       *  | SUTRA_MAIN...25800
C|  *   'OBS'  90  'project.obs'   # optional                       *  | SUTRA_MAIN...25900
C|  *   'OBC'  90  'project.obc'   # optional                       *  | SUTRA_MAIN...26000
C|  *   'SMY'  40  'project.smy'   # optional; defaults to          *  | SUTRA_MAIN...26100
C|  *                              #           filename="SUTRA.SMY" *  | SUTRA_MAIN...26200
C|  *                                                               *  | SUTRA_MAIN...26300
C|  *   Note that the filenames for types OBS and OBC are actually  *  | SUTRA_MAIN...26400
C|  *   root names from which SUTRA will automatically generate     *  | SUTRA_MAIN...26500
C|  *   observation output filenames based on the combinations of   *  | SUTRA_MAIN...26600
C|  *   schedules and output formats that appear in the observation *  | SUTRA_MAIN...26700
C|  *   specifications.  If a unit number of zero is specified for  *  | SUTRA_MAIN...26800
C|  *   a file, SUTRA will automatically assign a valid unit number *  | SUTRA_MAIN...26900
C|  *   to that file.                                               *  | SUTRA_MAIN...27000
C|  *                                                               *  | SUTRA_MAIN...27100
C|  *****************************************************************  | SUTRA_MAIN...27200
C|_____________________________________________________________________| SUTRA_MAIN...27300
C                                                                        SUTRA_MAIN...27400
C.....SET FILENAME AND FORTRAN UNIT NUMBER FOR UNIT K0                   SUTRA_MAIN...27500
      UNAME = 'SUTRA.FIL'                                                SUTRA_MAIN...27600
      K0 = 10                                                            SUTRA_MAIN...27700
C.....INITIALIZE "INSERT" FILE COUNTERS                                  SUTRA_MAIN...27800
      NKS(1) = 0                                                         SUTRA_MAIN...27900
      NKS(2) = 0                                                         SUTRA_MAIN...28000
C.....INITIALIZE NFLOMX TO ZERO NOW IN CASE TERMINATION SEQUENCE IS      SUTRA_MAIN...28100
C        CALLED BEFORE NFLOMX GETS SET.                                  SUTRA_MAIN...28200
      NFLOMX = 0                                                         SUTRA_MAIN...28300
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR THIS SIMULATION,       SUTRA_MAIN...28400
C        EXCEPT OBSERVATION OUTPUT FILES.                                SUTRA_MAIN...28500
      ONCEFO = .FALSE.                                                   SUTRA_MAIN...28600
      CALL FOPEN()                                                       SUTRA_MAIN...28700
C.....STORE INP AND ICS FILENAMES FOR LATER REFERENCE, SINCE THE         SUTRA_MAIN...28800
C        CORRESPONDING ENTRIES IN FNAME MAY BE OVERWRITTEN BY FILE       SUTRA_MAIN...28900
C        INSERTION.                                                      SUTRA_MAIN...29000
      FNINP = FNAME(1)                                                   SUTRA_MAIN...29100
      FNICS = FNAME(2)                                                   SUTRA_MAIN...29200
C                                                                        SUTRA_MAIN...29300
C                                                                        SUTRA_MAIN...29400
C.....OUTPUT BANNER                                                      SUTRA_MAIN...29500
      WRITE(K3,110) TRIM(VERNUM)                                         SUTRA_MAIN...29600
  110 FORMAT('1',131('*')////3(132('*')////)////                         SUTRA_MAIN...29700
     1   47X,' SSSS   UU  UU  TTTTTT  RRRRR     AA  '/                   SUTRA_MAIN...29800
     2   47X,'SS   S  UU  UU  T TT T  RR  RR   AAAA '/                   SUTRA_MAIN...29900
     3   47X,'SSSS    UU  UU    TT    RRRRR   AA  AA'/                   SUTRA_MAIN...30000
     4   47X,'    SS  UU  UU    TT    RR R    AAAAAA'/                   SUTRA_MAIN...30100
     5   47X,'SS  SS  UU  UU    TT    RR RR   AA  AA'/                   SUTRA_MAIN...30200
     6   47X,' SSSS    UUUU     TT    RR  RR  AA  AA'/                   SUTRA_MAIN...30300
     7   7(/),37X,'U N I T E D    S T A T E S   ',                       SUTRA_MAIN...30400
     8   'G E O L O G I C A L   S U R V E Y'////                         SUTRA_MAIN...30500
     9   45X,'SUBSURFACE FLOW AND TRANSPORT SIMULATION MODEL'/           SUTRA_MAIN...30600
     *   //58X,'-SUTRA VERSION ',A,'-'///                                SUTRA_MAIN...30700
     A   36X,'*  SATURATED-UNSATURATED FLOW AND SOLUTE OR ENERGY',       SUTRA_MAIN...30800
     B   ' TRANSPORT  *'////4(////132('*')))                             SUTRA_MAIN...30900
C                                                                        SUTRA_MAIN...31000
C_______________________________________________________________________ SUTRA_MAIN...31100
C|                                                                     | SUTRA_MAIN...31200
C|  *****************************************************************  | SUTRA_MAIN...31300
C|  *                                                               *  | SUTRA_MAIN...31400
C|  *   *********  R E A D I N G   I N P U T   D A T A  *********   *  | SUTRA_MAIN...31500
C|  *   *********  A N D   E R R O R   H A N D L I N G  *********   *  | SUTRA_MAIN...31600
C|  *                                                               *  | SUTRA_MAIN...31700
C|  *   SUTRA typically reads input data line by line as follows.   *  | SUTRA_MAIN...31800
C|  *   Subroutine READIF is called to skip over any comment        *  | SUTRA_MAIN...31900
C|  *   lines and read a single line of input data (up to 1000      *  | SUTRA_MAIN...32000
C|  *   characters) into internal file INTFIL. The input data       *  | SUTRA_MAIN...32100
C|  *   are then read from INTFIL. In case of an error, subroutine  *  | SUTRA_MAIN...32200
C|  *   SUTERR is called to report it, and control passes to the    *  | SUTRA_MAIN...32300
C|  *   termination sequence in subroutine TERSEQ.  The variable    *  | SUTRA_MAIN...32400
C|  *   ERRCOD is used to identify the nature of the error and is   *  | SUTRA_MAIN...32500
C|  *   set prior to calling READIF. The variables CHERR, INERR,    *  | SUTRA_MAIN...32600
C|  *   and RLERR can be used to send character, integer, or real   *  | SUTRA_MAIN...32700
C|  *   error information to subroutine SUTERR.                     *  | SUTRA_MAIN...32800
C|  *   Example from the main program:                              *  | SUTRA_MAIN...32900
C|  *                                                               *  | SUTRA_MAIN...33000
C|  *   ERRCOD = 'REA-INP-3'                                        *  | SUTRA_MAIN...33100
C|  *   CALL READIF(K1, INTFIL, ERRCOD)                             *  | SUTRA_MAIN...33200
C|  *   READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,             *  | SUTRA_MAIN...33300
C|  *  1   NSOP,NSOU,NOBS                                           *  | SUTRA_MAIN...33400
C|  *   IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR) *  | SUTRA_MAIN...33500
C|  *                                                               *  | SUTRA_MAIN...33600
C|  *****************************************************************  | SUTRA_MAIN...33700
C|_____________________________________________________________________| SUTRA_MAIN...33800
C                                                                        SUTRA_MAIN...33900
C.....INPUT DATASET 1:  OUTPUT HEADING                                   SUTRA_MAIN...34000
      ERRCOD = 'REA-INP-1'                                               SUTRA_MAIN...34100
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...34200
      READ(INTFIL,117,IOSTAT=INERR(1)) TITLE1                            SUTRA_MAIN...34300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SUTRA_MAIN...34400
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...34500
      READ(INTFIL,117,IOSTAT=INERR(1)) TITLE2                            SUTRA_MAIN...34600
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SUTRA_MAIN...34700
  117 FORMAT(80A1)                                                       SUTRA_MAIN...34800
C                                                                        SUTRA_MAIN...34900
C.....INPUT DATASET 2A:  SIMULATION TYPE (TYPE OF TRANSPORT)             SUTRA_MAIN...35000
C        (SET ME=-1 FOR SOLUTE TRANSPORT, ME=+1 FOR ENERGY TRANSPORT)    SUTRA_MAIN...35100
      ERRCOD = 'REA-INP-2A'                                              SUTRA_MAIN...35200
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...35300
      READ(INTFIL,*,IOSTAT=INERR(1)) SIMSTR                              SUTRA_MAIN...35400
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SUTRA_MAIN...35500
      CALL PRSWDS(SIMSTR, ' ', 5, SIMULA, NWORDS)                        SUTRA_MAIN...35600
      IF(SIMULA(1).NE.'SUTRA     ') THEN                                 SUTRA_MAIN...35700
         ERRCOD = 'INP-2A-1'                                             SUTRA_MAIN...35800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...35900
      END IF                                                             SUTRA_MAIN...36000
      IF (SIMULA(2).EQ.'VERSION   ') THEN                                SUTRA_MAIN...36100
         VERNIN = SIMULA(3)                                              SUTRA_MAIN...36200
         IF (VERNIN.EQ.'2D3D.1 ') THEN                                   SUTRA_MAIN...36300
            VERNIN = '2.0'                                               SUTRA_MAIN...36400
         ELSE IF ((VERNIN.NE.'2.0 ').AND.(VERNIN.NE.'2.1 ')) THEN        SUTRA_MAIN...36500
            ERRCOD = 'INP-2A-4'                                          SUTRA_MAIN...36600
            CHERR(1) = VERNIN                                            SUTRA_MAIN...36700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...36800
         END IF                                                          SUTRA_MAIN...36900
         IOFF = 2                                                        SUTRA_MAIN...37000
      ELSE                                                               SUTRA_MAIN...37100
         VERNIN = '2.0'                                                  SUTRA_MAIN...37200
         IOFF = 0                                                        SUTRA_MAIN...37300
      END IF                                                             SUTRA_MAIN...37400
      IF(SIMULA(2+IOFF).EQ.'SOLUTE    ') GOTO 120                        SUTRA_MAIN...37500
      IF(SIMULA(2+IOFF).EQ.'ENERGY    ') GOTO 140                        SUTRA_MAIN...37600
      IF (IOFF.EQ.0) THEN                                                SUTRA_MAIN...37700
         ERRCOD = 'INP-2A-2'                                             SUTRA_MAIN...37800
      ELSE                                                               SUTRA_MAIN...37900
         ERRCOD = 'INP-2A-3'                                             SUTRA_MAIN...38000
      END IF                                                             SUTRA_MAIN...38100
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           SUTRA_MAIN...38200
  120 ME=-1                                                              SUTRA_MAIN...38300
      WRITE(K3,130)                                                      SUTRA_MAIN...38400
  130 FORMAT('1'//132('*')///20X,'* * * * *   S U T R A   S O L U ',     SUTRA_MAIN...38500
     1   'T E   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...38600
     2   /132('*')/)                                                     SUTRA_MAIN...38700
      GOTO 160                                                           SUTRA_MAIN...38800
  140 ME=+1                                                              SUTRA_MAIN...38900
      WRITE(K3,150)                                                      SUTRA_MAIN...39000
  150 FORMAT('1'//132('*')///20X,'* * * * *   S U T R A   E N E R ',     SUTRA_MAIN...39100
     1   'G Y   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...39200
     2   /132('*')/)                                                     SUTRA_MAIN...39300
  160 CONTINUE                                                           SUTRA_MAIN...39400
C                                                                        SUTRA_MAIN...39500
C.....INPUT DATASET 2B:  MESH STRUCTURE                                  SUTRA_MAIN...39600
      ERRCOD = 'REA-INP-2B'                                              SUTRA_MAIN...39700
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...39800
      READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR                              SUTRA_MAIN...39900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SUTRA_MAIN...40000
      CALL PRSWDS(MSHSTR, ' ', 2, MSHTYP, NWORDS)                        SUTRA_MAIN...40100
C.....KTYPE SET ACCORDING TO THE TYPE OF FINITE-ELEMENT MESH:            SUTRA_MAIN...40200
C        2D MESH          ==>   KTYPE(1) = 2                             SUTRA_MAIN...40300
C        3D MESH          ==>   KTYPE(1) = 3                             SUTRA_MAIN...40400
C        IRREGULAR MESH   ==>   KTYPE(2) = 0                             SUTRA_MAIN...40500
C        LAYERED MESH     ==>   KTYPE(2) = 1                             SUTRA_MAIN...40600
C        REGULAR MESH     ==>   KTYPE(2) = 2                             SUTRA_MAIN...40700
C        BLOCKWISE MESH   ==>   KTYPE(2) = 3                             SUTRA_MAIN...40800
      IF (MSHTYP(1).EQ.'2D        ') THEN                                SUTRA_MAIN...40900
         KTYPE(1) = 2                                                    SUTRA_MAIN...41000
      ELSE IF (MSHTYP(1).EQ.'3D        ') THEN                           SUTRA_MAIN...41100
         KTYPE(1) = 3                                                    SUTRA_MAIN...41200
      ELSE                                                               SUTRA_MAIN...41300
         ERRCOD = 'INP-2B-1'                                             SUTRA_MAIN...41400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...41500
      END IF                                                             SUTRA_MAIN...41600
      IF ((MSHTYP(2).EQ.'REGULAR   ').OR.                                SUTRA_MAIN...41700
     1    (MSHTYP(2).EQ.'BLOCKWISE ')) THEN                              SUTRA_MAIN...41800
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...41900
         IF (KTYPE(1).EQ.2) THEN                                         SUTRA_MAIN...42000
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2              SUTRA_MAIN...42100
            NN3 = 1                                                      SUTRA_MAIN...42200
         ELSE                                                            SUTRA_MAIN...42300
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2, NN3         SUTRA_MAIN...42400
         END IF                                                          SUTRA_MAIN...42500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     SUTRA_MAIN...42600
         IF ((NN1.LT.2).OR.(NN2.LT.2).OR.                                SUTRA_MAIN...42700
     1      ((KTYPE(1).EQ.3).AND.(NN3.LT.2))) THEN                       SUTRA_MAIN...42800
            ERRCOD = 'INP-2B-3'                                          SUTRA_MAIN...42900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...43000
         END IF                                                          SUTRA_MAIN...43100
         IF (MSHTYP(2).EQ.'BLOCKWISE ') THEN                             SUTRA_MAIN...43200
            KTYPE(2) = 3                                                 SUTRA_MAIN...43300
            ERRCOD = 'REA-INP-2B'                                        SUTRA_MAIN...43400
            DO 177 I1=1,KTYPE(1)                                         SUTRA_MAIN...43500
               CALL READIF(K1, INTFIL, ERRCOD)                           SUTRA_MAIN...43600
               READ(INTFIL,*,IOSTAT=INERR(1)) IDUM1, (IDUM2, I2=1,IDUM1) SUTRA_MAIN...43700
               IF (INERR(1).NE.0) CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)  SUTRA_MAIN...43800
  177       CONTINUE                                                     SUTRA_MAIN...43900
         ELSE                                                            SUTRA_MAIN...44000
            KTYPE(2) = 2                                                 SUTRA_MAIN...44100
         END IF                                                          SUTRA_MAIN...44200
      ELSE IF (MSHTYP(2).EQ.'LAYERED   ') THEN                           SUTRA_MAIN...44300
         IF (KTYPE(1).EQ.2) THEN                                         SUTRA_MAIN...44400
            ERRCOD = 'INP-2B-5'                                          SUTRA_MAIN...44500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...44600
         END IF                                                          SUTRA_MAIN...44700
         KTYPE(2) = 1                                                    SUTRA_MAIN...44800
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...44900
         READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR,NLAYS,NNLAY,NELAY,LAYSTR  SUTRA_MAIN...45000
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)        SUTRA_MAIN...45100
         CALL PRSWDS(LAYSTR, ' ', 1, LAYNOR, NWORDS)                     SUTRA_MAIN...45200
         IF ((LAYNOR(1).NE.'ACROSS').AND.(LAYNOR(1).NE.'WITHIN')) THEN   SUTRA_MAIN...45300
            ERRCOD = 'INP-2B-6'                                          SUTRA_MAIN...45400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...45500
         END IF                                                          SUTRA_MAIN...45600
         IF ((NLAYS.LT.2).OR.(NNLAY.LT.4).OR.(NELAY.LT.1)) THEN          SUTRA_MAIN...45700
            ERRCOD = 'INP-2B-7'                                          SUTRA_MAIN...45800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...45900
         END IF                                                          SUTRA_MAIN...46000
      ELSE IF (MSHTYP(2).EQ.'IRREGULAR ') THEN                           SUTRA_MAIN...46100
         KTYPE(2) = 0                                                    SUTRA_MAIN...46200
      ELSE                                                               SUTRA_MAIN...46300
         ERRCOD = 'INP-2B-4'                                             SUTRA_MAIN...46400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...46500
      END IF                                                             SUTRA_MAIN...46600
C                                                                        SUTRA_MAIN...46700
C.....OUTPUT DATASET 1                                                   SUTRA_MAIN...46800
      WRITE(K3,180) TITLE1,TITLE2                                        SUTRA_MAIN...46900
  180 FORMAT(////1X,131('-')//26X,80A1//26X,80A1//1X,131('-'))           SUTRA_MAIN...47000
C                                                                        SUTRA_MAIN...47100
C.....OUTPUT FILE UNIT ASSIGNMENTS                                       SUTRA_MAIN...47200
      WRITE(K3,202) IUNIT(1),FNINP,IUNIT(2),FNICS,IUNIT(0),FNAME(0),     SUTRA_MAIN...47300
     1   IUNIT(3),FNAME(3)                                               SUTRA_MAIN...47400
  202 FORMAT(/////11X,'F I L E   U N I T   A S S I G N M E N T S'//      SUTRA_MAIN...47500
     1   13X,'INPUT UNITS:'/                                             SUTRA_MAIN...47600
     2   13X,' INP FILE (MAIN INPUT)          ',I7,4X,                   SUTRA_MAIN...47700
     3      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...47800
     4   13X,' ICS FILE (INITIAL CONDITIONS)  ',I7,4X,                   SUTRA_MAIN...47900
     5      'ASSIGNED TO ',A80//                                         SUTRA_MAIN...48000
     6   13X,'OUTPUT UNITS:'/                                            SUTRA_MAIN...48100
     7   13X,' SMY FILE (RUN SUMMARY)         ',I7,4X,                   SUTRA_MAIN...48200
     8      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...48300
     9   13X,' LST FILE (GENERAL OUTPUT)      ',I7,4X,                   SUTRA_MAIN...48400
     T      'ASSIGNED TO ',A80)                                          SUTRA_MAIN...48500
      IF(IUNIT(4).NE.-1) WRITE(K3,203) IUNIT(4),FNAME(4)                 SUTRA_MAIN...48600
  203 FORMAT(13X,' RST FILE (RESTART DATA)        ',I7,4X,               SUTRA_MAIN...48700
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...48800
      IF(IUNIT(5).NE.-1) WRITE(K3,204) IUNIT(5),FNAME(5)                 SUTRA_MAIN...48900
  204 FORMAT(13X,' NOD FILE (NODEWISE OUTPUT)     ',I7,4X,               SUTRA_MAIN...49000
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...49100
      IF(IUNIT(6).NE.-1) WRITE(K3,206) IUNIT(6),FNAME(6)                 SUTRA_MAIN...49200
  206 FORMAT(13X,' ELE FILE (VELOCITY OUTPUT)     ',I7,4X,               SUTRA_MAIN...49300
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...49400
      IF(IUNIT(7).NE.-1) WRITE(K3,207) IUNIT(7),                         SUTRA_MAIN...49500
     1   TRIM(FNAME(7)) // " (BASE FILENAME)"                            SUTRA_MAIN...49600
  207 FORMAT(13X,' OBS FILE (OBSERVATION OUTPUT) (',I7,')',3X,           SUTRA_MAIN...49700
     1   'ASSIGNED TO ',A)                                               SUTRA_MAIN...49800
      IF(IUNIT(8).NE.-1) WRITE(K3,208) IUNIT(8),                         SUTRA_MAIN...49900
     1   TRIM(FNAME(8)) // " (BASE FILENAME)"                            SUTRA_MAIN...50000
  208 FORMAT(13X,' OBC FILE (OBSERVATION OUTPUT) (',I7,')',3X,           SUTRA_MAIN...50100
     1   'ASSIGNED TO ',A)                                               SUTRA_MAIN...50200
      WRITE(K3,209)                                                      SUTRA_MAIN...50300
  209 FORMAT(/14X,'NAMES FOR OBS AND OBC FILES WILL BE GENERATED',       SUTRA_MAIN...50400
     1   ' AUTOMATICALLY FROM THE BASE NAMES LISTED ABOVE AND SCHEDULE', SUTRA_MAIN...50500
     2   ' NAMES'/14X,'LISTED LATER IN THIS FILE.  UNIT NUMBERS',        SUTRA_MAIN...50600
     3   ' ASSIGNED TO THESE FILES WILL BE THE FIRST AVAILABLE',         SUTRA_MAIN...50700
     4   ' NUMBERS GREATER THAN'/14X,'OR EQUAL TO THE VALUES LISTED',    SUTRA_MAIN...50800
     5   ' ABOVE IN PARENTHESES.')                                       SUTRA_MAIN...50900
C                                                                        SUTRA_MAIN...51000
C.....INPUT DATASET 3:  SIMULATION CONTROL NUMBERS                       SUTRA_MAIN...51100
      ERRCOD = 'REA-INP-3'                                               SUTRA_MAIN...51200
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...51300
      READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS      SUTRA_MAIN...51400
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SUTRA_MAIN...51500
      IF (KTYPE(2).GT.1) THEN                                            SUTRA_MAIN...51600
         NN123 = NN1*NN2*NN3                                             SUTRA_MAIN...51700
         IF(NN123.NE.NN) THEN                                            SUTRA_MAIN...51800
           ERRCOD = 'INP-2B,3-1'                                         SUTRA_MAIN...51900
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      SUTRA_MAIN...52000
         END IF                                                          SUTRA_MAIN...52100
         IF (KTYPE(1).EQ.3) THEN                                         SUTRA_MAIN...52200
            NE123 = (NN1 - 1)*(NN2 - 1)*(NN3 - 1)                        SUTRA_MAIN...52300
         ELSE                                                            SUTRA_MAIN...52400
            NE123 = (NN1 - 1)*(NN2 - 1)                                  SUTRA_MAIN...52500
         END IF                                                          SUTRA_MAIN...52600
         IF(NE123.NE.NE) THEN                                            SUTRA_MAIN...52700
           ERRCOD = 'INP-2B,3-2'                                         SUTRA_MAIN...52800
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      SUTRA_MAIN...52900
         END IF                                                          SUTRA_MAIN...53000
      ELSE IF (MSHTYP(2).EQ.'LAYERED   ') THEN                           SUTRA_MAIN...53100
         NNTOT = NLAYS*NNLAY                                             SUTRA_MAIN...53200
         IF(NNTOT.NE.NN) THEN                                            SUTRA_MAIN...53300
           ERRCOD = 'INP-2B,3-3'                                         SUTRA_MAIN...53400
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      SUTRA_MAIN...53500
         END IF                                                          SUTRA_MAIN...53600
         NETOT = (NLAYS - 1)*NELAY                                       SUTRA_MAIN...53700
         IF(NETOT.NE.NE) THEN                                            SUTRA_MAIN...53800
           ERRCOD = 'INP-2B,3-4'                                         SUTRA_MAIN...53900
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      SUTRA_MAIN...54000
         END IF                                                          SUTRA_MAIN...54100
      ENDIF                                                              SUTRA_MAIN...54200
C                                                                        SUTRA_MAIN...54300
C.....INPUT AND OUTPUT DATASET 4:  SIMULATION MODE OPTIONS               SUTRA_MAIN...54400
      ERRCOD = 'REA-INP-4'                                               SUTRA_MAIN...54500
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...54600
      READ(INTFIL,*,IOSTAT=INERR(1)) UNSSTR,SSFSTR,SSTSTR,RDSTR,ISTORE   SUTRA_MAIN...54700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SUTRA_MAIN...54800
      CALL PRSWDS(UNSSTR, ' ', 1, CUNSAT, NWORDS)                        SUTRA_MAIN...54900
      CALL PRSWDS(SSFSTR, ' ', 1, CSSFLO, NWORDS)                        SUTRA_MAIN...55000
      CALL PRSWDS(SSTSTR, ' ', 1, CSSTRA, NWORDS)                        SUTRA_MAIN...55100
      CALL PRSWDS(RDSTR,  ' ', 1, CREAD, NWORDS)                         SUTRA_MAIN...55200
      ISMERR = 0                                                         SUTRA_MAIN...55300
      IF (CUNSAT.EQ.'UNSATURATED') THEN                                  SUTRA_MAIN...55400
         IUNSAT = +1                                                     SUTRA_MAIN...55500
      ELSE IF (CUNSAT.EQ.'SATURATED') THEN                               SUTRA_MAIN...55600
         IUNSAT = 0                                                      SUTRA_MAIN...55700
      ELSE                                                               SUTRA_MAIN...55800
         ERRCOD = 'INP-4-1'                                              SUTRA_MAIN...55900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...56000
      END IF                                                             SUTRA_MAIN...56100
      IF (CSSFLO.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...56200
         ISSFLO = 0                                                      SUTRA_MAIN...56300
      ELSE IF (CSSFLO.EQ.'STEADY') THEN                                  SUTRA_MAIN...56400
         ISSFLO = +1                                                     SUTRA_MAIN...56500
      ELSE                                                               SUTRA_MAIN...56600
         ERRCOD = 'INP-4-2'                                              SUTRA_MAIN...56700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...56800
      END IF                                                             SUTRA_MAIN...56900
      IF (CSSTRA.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...57000
         ISSTRA = 0                                                      SUTRA_MAIN...57100
      ELSE IF (CSSTRA.EQ.'STEADY') THEN                                  SUTRA_MAIN...57200
         ISSTRA = +1                                                     SUTRA_MAIN...57300
      ELSE                                                               SUTRA_MAIN...57400
         ERRCOD = 'INP-4-3'                                              SUTRA_MAIN...57500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...57600
      END IF                                                             SUTRA_MAIN...57700
      IF (CREAD.EQ.'COLD') THEN                                          SUTRA_MAIN...57800
         IREAD = +1                                                      SUTRA_MAIN...57900
      ELSE IF (CREAD.EQ.'WARM') THEN                                     SUTRA_MAIN...58000
         IREAD = -1                                                      SUTRA_MAIN...58100
      ELSE                                                               SUTRA_MAIN...58200
         ERRCOD = 'INP-4-4'                                              SUTRA_MAIN...58300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...58400
      END IF                                                             SUTRA_MAIN...58500
      WRITE(K3,210)                                                      SUTRA_MAIN...58600
  210 FORMAT(////11X,'S I M U L A T I O N   M O D E   ',                 SUTRA_MAIN...58700
     1   'O P T I O N S'/)                                               SUTRA_MAIN...58800
      IF(ISSTRA.EQ.1.AND.ISSFLO.NE.1) THEN                               SUTRA_MAIN...58900
         ERRCOD = 'INP-4-5'                                              SUTRA_MAIN...59000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...59100
      ENDIF                                                              SUTRA_MAIN...59200
      IF(IUNSAT.EQ.+1) WRITE(K3,215)                                     SUTRA_MAIN...59300
      IF(IUNSAT.EQ.0) WRITE(K3,216)                                      SUTRA_MAIN...59400
  215 FORMAT(11X,'- ALLOW UNSATURATED AND SATURATED FLOW:  UNSATURATED', SUTRA_MAIN...59500
     1   ' PROPERTIES ARE USER-PROGRAMMED IN SUBROUTINE   U N S A T')    SUTRA_MAIN...59600
  216 FORMAT(11X,'- ASSUME SATURATED FLOW ONLY')                         SUTRA_MAIN...59700
      IF(ISSFLO.EQ.+1.AND.ME.EQ.-1) WRITE(K3,219)                        SUTRA_MAIN...59800
      IF(ISSFLO.EQ.+1.AND.ME.EQ.+1) WRITE(K3,220)                        SUTRA_MAIN...59900
      IF(ISSFLO.EQ.0) WRITE(K3,221)                                      SUTRA_MAIN...60000
  219 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...60100
     1   'INITIAL CONCENTRATION CONDITIONS')                             SUTRA_MAIN...60200
  220 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...60300
     1   'INITIAL TEMPERATURE CONDITIONS')                               SUTRA_MAIN...60400
  221 FORMAT(11X,'- ALLOW TIME-DEPENDENT FLOW FIELD')                    SUTRA_MAIN...60500
      IF(ISSTRA.EQ.+1) WRITE(K3,225)                                     SUTRA_MAIN...60600
      IF(ISSTRA.EQ.0) WRITE(K3,226)                                      SUTRA_MAIN...60700
  225 FORMAT(11X,'- ASSUME STEADY-STATE TRANSPORT')                      SUTRA_MAIN...60800
  226 FORMAT(11X,'- ALLOW TIME-DEPENDENT TRANSPORT')                     SUTRA_MAIN...60900
      IF(IREAD.EQ.-1) WRITE(K3,230)                                      SUTRA_MAIN...61000
      IF(IREAD.EQ.+1) WRITE(K3,231)                                      SUTRA_MAIN...61100
  230 FORMAT(11X,'- WARM START - SIMULATION IS TO BE ',                  SUTRA_MAIN...61200
     1   'CONTINUED FROM PREVIOUSLY-STORED DATA')                        SUTRA_MAIN...61300
  231 FORMAT(11X,'- COLD START - BEGIN NEW SIMULATION')                  SUTRA_MAIN...61400
      IF(ISTORE.GT.0) WRITE(K3,240) ISTORE                               SUTRA_MAIN...61500
      IF(ISTORE.EQ.0) WRITE(K3,241)                                      SUTRA_MAIN...61600
  240 FORMAT(11X,'- STORE RESULTS AFTER EVERY',I9,' TIME STEPS IN',      SUTRA_MAIN...61700
     1   ' RESTART FILE AS BACKUP AND FOR USE IN A SIMULATION RESTART')  SUTRA_MAIN...61800
  241 FORMAT(11X,'- DO NOT STORE RESULTS FOR USE IN A',                  SUTRA_MAIN...61900
     1   ' RESTART OF SIMULATION')                                       SUTRA_MAIN...62000
C.....OUTPUT DATASET 3                                                   SUTRA_MAIN...62100
      IF(ME.EQ.-1)                                                       SUTRA_MAIN...62200
     1   WRITE(K3,245) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                    SUTRA_MAIN...62300
  245 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...62400
     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...62500
     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...62600
     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...62700
     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...62800
     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...62900
     6   'SOLUTE CONCENTRATION IS A SPECIFIED CONSTANT OR ',             SUTRA_MAIN...63000
     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...63100
     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...63200
     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...63300
     A   ' WHICH A SOURCE OR SINK OF SOLUTE MASS IS A SPECIFIED ',       SUTRA_MAIN...63400
     B   'CONSTANT OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF ',   SUTRA_MAIN...63500
     C   'NODES AT WHICH PRESSURE AND CONCENTRATION WILL BE OBSERVED')   SUTRA_MAIN...63600
C                                                                        SUTRA_MAIN...63700
      IF(ME.EQ.+1)                                                       SUTRA_MAIN...63800
     1    WRITE(K3,247) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                   SUTRA_MAIN...63900
  247 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...64000
     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...64100
     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...64200
     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...64300
     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...64400
     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...64500
     6   'TEMPERATURE IS A SPECIFIED CONSTANT OR ',                      SUTRA_MAIN...64600
     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...64700
     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...64800
     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...64900
     A   ' WHICH A SOURCE OR SINK OF ENERGY IS A SPECIFIED CONSTANT',    SUTRA_MAIN...65000
     B   ' OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES ',     SUTRA_MAIN...65100
     C   'AT WHICH PRESSURE AND TEMPERATURE WILL BE OBSERVED')           SUTRA_MAIN...65200
C                                                                        SUTRA_MAIN...65300
C.....INPUT DATASETS 5 - 7 (NUMERICAL, TEMPORAL, AND ITERATION CONTROLS) SUTRA_MAIN...65400
      CALL INDAT0()                                                      SUTRA_MAIN...65500
C.....KSOLVP AND KSOLVU HAVE BEEN SET ACCORDING TO THE SOLVERS SELECTED: SUTRA_MAIN...65600
C        BANDED GAUSSIAN ELIMINATION (DIRECT)   ==>   0                  SUTRA_MAIN...65700
C        IC-PRECONDITIONED CG                   ==>   1                  SUTRA_MAIN...65800
C        ILU-PRECONDITIONED GMRES               ==>   2                  SUTRA_MAIN...65900
C        ILU-PRECONDITIONED ORTHOMIN            ==>   3                  SUTRA_MAIN...66000
C                                                                        SUTRA_MAIN...66100
C.....OUTPUT DATASETS 7B & 7C                                            SUTRA_MAIN...66200
      WRITE(K3,261)                                                      SUTRA_MAIN...66300
  261 FORMAT(////11X,'S O L V E R - R E L A T E D   ',                   SUTRA_MAIN...66400
     1   'P A R A M E T E R S')                                          SUTRA_MAIN...66500
C.....OUTPUT DATASETS 3B & 3C                                            SUTRA_MAIN...66600
  266 IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...66700
         WRITE(K3,268)                                                   SUTRA_MAIN...66800
     1      SOLNAM(KSOLVP), ITRMXP, TOLP,                                SUTRA_MAIN...66900
     2      SOLNAM(KSOLVU), ITRMXU, TOLU                                 SUTRA_MAIN...67000
  268    FORMAT(                                                         SUTRA_MAIN...67100
     1      /13X,'SOLVER FOR P: ',A40                                    SUTRA_MAIN...67200
     2      //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',    SUTRA_MAIN...67300
     3           ' DURING P SOLUTION'                                    SUTRA_MAIN...67400
     4      /11X,1PE15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',          SUTRA_MAIN...67500
     5           ' SOLVER ITERATIONS DURING P SOLUTION'                  SUTRA_MAIN...67600
     6      //13X,'SOLVER FOR U: ',A40                                   SUTRA_MAIN...67700
     7      //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',    SUTRA_MAIN...67800
     8           ' DURING U SOLUTION'                                    SUTRA_MAIN...67900
     9      /11X,1PE15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',          SUTRA_MAIN...68000
     A           ' SOLVER ITERATIONS DURING U SOLUTION' )                SUTRA_MAIN...68100
      ELSE                                                               SUTRA_MAIN...68200
         WRITE(K3,269) SOLNAM(KSOLVP)                                    SUTRA_MAIN...68300
  269    FORMAT(/13X,'SOLVER FOR P AND U: ',A40)                         SUTRA_MAIN...68400
      END IF                                                             SUTRA_MAIN...68500
C                                                                        SUTRA_MAIN...68600
C.....CALCULATE ARRAY DIMENSIONS, EXCEPT THOSE THAT DEPEND ON            SUTRA_MAIN...68700
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...68800
C                                                                        SUTRA_MAIN...68900
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...69000
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...69100
         NNNX = 1                                                        SUTRA_MAIN...69200
         NDIMJA = 1                                                      SUTRA_MAIN...69300
         NNVEC = NN                                                      SUTRA_MAIN...69400
      ELSE                                                               SUTRA_MAIN...69500
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...69600
         NNNX = NN                                                       SUTRA_MAIN...69700
         NDIMJA = NN + 1                                                 SUTRA_MAIN...69800
         NNVEC = NN                                                      SUTRA_MAIN...69900
      END IF                                                             SUTRA_MAIN...70000
      NBCN=NPBC+NUBC+1                                                   SUTRA_MAIN...70100
      NSOP=NSOP+1                                                        SUTRA_MAIN...70200
      NSOU=NSOU+1                                                        SUTRA_MAIN...70300
      NOBSN=NOBS+1                                                       SUTRA_MAIN...70400
      IF (KTYPE(1).EQ.3) THEN                                            SUTRA_MAIN...70500
         N48 = 8                                                         SUTRA_MAIN...70600
         NEX = NE                                                        SUTRA_MAIN...70700
      ELSE                                                               SUTRA_MAIN...70800
         N48 = 4                                                         SUTRA_MAIN...70900
         NEX = 1                                                         SUTRA_MAIN...71000
      END IF                                                             SUTRA_MAIN...71100
      NIN=NE*N48                                                         SUTRA_MAIN...71200
C                                                                        SUTRA_MAIN...71300
C.....ALLOCATE REAL ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH        SUTRA_MAIN...71400
      ALLOCATE(PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN),  SUTRA_MAIN...71500
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA_MAIN...71600
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA_MAIN...71700
     3   QIN(NN),UIN(NN),QUIN(NN),QINITR(NN),RCIT(NN),RCITM1(NN))        SUTRA_MAIN...71800
      ALLOCATE(PVEC(NNVEC),UVEC(NNVEC))                                  SUTRA_MAIN...71900
      ALLOCATE(ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),         SUTRA_MAIN...72000
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA_MAIN...72100
     2   PANGL1(NE))                                                     SUTRA_MAIN...72200
      ALLOCATE(ALMID(NEX),ATMID(NEX),                                    SUTRA_MAIN...72300
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA_MAIN...72400
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX))                SUTRA_MAIN...72500
      ALLOCATE(PBC(NBCN),UBC(NBCN),QPLITR(NBCN))                         SUTRA_MAIN...72600
      ALLOCATE(GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48))                  SUTRA_MAIN...72700
      ALLOCATE(B(NNNX))                                                  SUTRA_MAIN...72800
C.....ALLOCATE INTEGER ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH     SUTRA_MAIN...72900
C        OR NELT                                                         SUTRA_MAIN...73000
      ALLOCATE(IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),    SUTRA_MAIN...73100
     1   NREG(NN),LREG(NE),JA(NDIMJA))                                   SUTRA_MAIN...73200
C.....ALLOCATE ARRAYS OF DERIVED TYPE, EXCEPT THOSE THAT DEPEND ON       SUTRA_MAIN...73300
C        BANDWIDTH OR NELT                                               SUTRA_MAIN...73400
      ALLOCATE(OBSPTS(NOBSN))                                            SUTRA_MAIN...73500
      ALLO1 = .TRUE.                                                     SUTRA_MAIN...73600
C                                                                        SUTRA_MAIN...73700
C.....INPUT DATASETS 8 - 15 (OUTPUT CONTROLS; FLUID AND SOLID MATRIX     SUTRA_MAIN...73800
C        PROPERTIES; ADSORPTION PARAMETERS; PRODUCTION OF ENERGY OR      SUTRA_MAIN...73900
C        SOLUTE MASS; GRAVITY; AND NODEWISE AND ELEMENTWISE DATA)        SUTRA_MAIN...74000
      CALL INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,         SUTRA_MAIN...74100
     1   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,                      SUTRA_MAIN...74200
     2   PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG,        SUTRA_MAIN...74300
     3   OBSPTS)                                                         SUTRA_MAIN...74400
C                                                                        SUTRA_MAIN...74500
C.....KEEP TRACK IF OUTPUT ROUTINES HAVE BEEN EXECUTED, TO PRINT         SUTRA_MAIN...74600
C        HEADERS ONLY ONCE.                                              SUTRA_MAIN...74700
      ONCEK5 = .FALSE.                                                   SUTRA_MAIN...74800
      ONCEK6 = .FALSE.                                                   SUTRA_MAIN...74900
      ONCEK7 = .FALSE.                                                   SUTRA_MAIN...75000
      ONCEK8 = .FALSE.                                                   SUTRA_MAIN...75100
      ALLOCATE(ONCK78(NFLOMX))                                           SUTRA_MAIN...75200
      DO 400 J=1,NFLOMX                                                  SUTRA_MAIN...75300
         ONCK78(J) = .FALSE.                                             SUTRA_MAIN...75400
  400 CONTINUE                                                           SUTRA_MAIN...75500
C                                                                        SUTRA_MAIN...75600
C.....INPUT DATASETS 17 & 18 (SOURCES OF FLUID MASS AND ENERGY OR        SUTRA_MAIN...75700
C        SOLUTE MASS)                                                    SUTRA_MAIN...75800
      CALL ZERO(QIN,NN,0.0D0)                                            SUTRA_MAIN...75900
      CALL ZERO(UIN,NN,0.0D0)                                            SUTRA_MAIN...76000
      CALL ZERO(QUIN,NN,0.0D0)                                           SUTRA_MAIN...76100
      IF(NSOP-1.GT.0.OR.NSOU-1.GT.0)                                     SUTRA_MAIN...76200
     1   CALL SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT)             SUTRA_MAIN...76300
C                                                                        SUTRA_MAIN...76400
C.....INPUT DATASETS 19 & 20 (SPECIFIED P AND U BOUNDARY CONDITIONS)     SUTRA_MAIN...76500
      IF(NBCN-1.GT.0) CALL BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT)          SUTRA_MAIN...76600
C                                                                        SUTRA_MAIN...76700
C.....INPUT DATASET 22 (ELEMENT INCIDENCE [MESH CONNECTION] DATA)        SUTRA_MAIN...76800
      CALL CONNEC(IN)                                                    SUTRA_MAIN...76900
C                                                                        SUTRA_MAIN...77000
C.....IF USING OLD (VERSION 2D3D.1) OBSERVATION INPUT FORMAT, LOOK UP    SUTRA_MAIN...77100
C        COORDINATES FOR OBSERVATION POINTS (NODES).                     SUTRA_MAIN...77200
      IF (NOBCYC.NE.-1) THEN                                             SUTRA_MAIN...77300
         DO 710 K=1,NOBS                                                 SUTRA_MAIN...77400
            I = OBSPTS(K)%L                                              SUTRA_MAIN...77500
            OBSPTS(K)%X = X(I)                                           SUTRA_MAIN...77600
            OBSPTS(K)%Y = Y(I)                                           SUTRA_MAIN...77700
            IF (N48.EQ.8) OBSPTS(K)%Z = Z(I)                             SUTRA_MAIN...77800
  710    CONTINUE                                                        SUTRA_MAIN...77900
      END IF                                                             SUTRA_MAIN...78000
C                                                                        SUTRA_MAIN...78100
C.....FIND THE ELEMENT EACH OBSERVATION POINT IS IN.  IN COMPONENTS OF   SUTRA_MAIN...78200
C        OBSPTS, OVERWRITE NODE NUMBERS AND GLOBAL COORDINATES WITH      SUTRA_MAIN...78300
C        ELEMENT NUMBERS AND LOCAL COORDINATES.                          SUTRA_MAIN...78400
      DO 900 K=1,NOBS                                                    SUTRA_MAIN...78500
         XK = OBSPTS(K)%X                                                SUTRA_MAIN...78600
         YK = OBSPTS(K)%Y                                                SUTRA_MAIN...78700
         IF (N48.EQ.8) ZK = OBSPTS(K)%Z                                  SUTRA_MAIN...78800
         DO 800 LL=1,NE                                                  SUTRA_MAIN...78900
            IF (N48.EQ.8) THEN                                           SUTRA_MAIN...79000
               CALL FINDL3(X,Y,Z,IN,LL,XK,YK,ZK,XSI,ETA,ZET,INOUT)       SUTRA_MAIN...79100
            ELSE                                                         SUTRA_MAIN...79200
               CALL FINDL2(X,Y,IN,LL,XK,YK,XSI,ETA,INOUT)                SUTRA_MAIN...79300
            END IF                                                       SUTRA_MAIN...79400
            IF (INOUT.EQ.1) THEN                                         SUTRA_MAIN...79500
               L = LL                                                    SUTRA_MAIN...79600
               GOTO 820                                                  SUTRA_MAIN...79700
            END IF                                                       SUTRA_MAIN...79800
  800    CONTINUE                                                        SUTRA_MAIN...79900
         ERRCOD = 'INP-8D-3'                                             SUTRA_MAIN...80000
         CHERR(1) = OBSPTS(K)%NAME                                       SUTRA_MAIN...80100
         WRITE(UNIT=CHERR(2), FMT=805)                                   SUTRA_MAIN...80200
     1      OBSPTS(K)%X, OBSPTS(K)%Y, OBSPTS(K)%Z                        SUTRA_MAIN...80300
  805    FORMAT('(',2(1PE14.7,','),1PE14.7,')')                          SUTRA_MAIN...80400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...80500
  820    OBSPTS(K)%L = L                                                 SUTRA_MAIN...80600
         OBSPTS(K)%XSI = XSI                                             SUTRA_MAIN...80700
         OBSPTS(K)%ETA = ETA                                             SUTRA_MAIN...80800
         IF (N48.EQ.8) OBSPTS(K)%ZET = ZET                               SUTRA_MAIN...80900
  900 CONTINUE                                                           SUTRA_MAIN...81000
C                                                                        SUTRA_MAIN...81100
C.....IF ITERATIVE SOLVER IS USED, SET UP POINTER ARRAYS IA AND JA THAT  SUTRA_MAIN...81200
C        SPECIFY MATRIX STRUCTURE IN "SLAP COLUMN" FORMAT.  DIMENSION    SUTRA_MAIN...81300
C        NELT GETS SET HERE.                                             SUTRA_MAIN...81400
      IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...81500
         CALL PTRSET()                                                   SUTRA_MAIN...81600
      ELSE                                                               SUTRA_MAIN...81700
         NELT = NN                                                       SUTRA_MAIN...81800
         NDIMIA = 1                                                      SUTRA_MAIN...81900
         ALLOCATE(IA(NDIMIA))                                            SUTRA_MAIN...82000
      END IF                                                             SUTRA_MAIN...82100
      ALLO3 = .TRUE.                                                     SUTRA_MAIN...82200
C                                                                        SUTRA_MAIN...82300
C.....CALCULATE BANDWIDTH                                                SUTRA_MAIN...82400
      CALL BANWID(IN)                                                    SUTRA_MAIN...82500
C                                                                        SUTRA_MAIN...82600
C.....CALCULATE ARRAY DIMENSIONS THAT DEPEND ON BANDWIDTH OR NELT        SUTRA_MAIN...82700
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...82800
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...82900
         NCBI = NBI                                                      SUTRA_MAIN...83000
         NELTA = NELT                                                    SUTRA_MAIN...83100
         NWI = 1                                                         SUTRA_MAIN...83200
         NWF = 1                                                         SUTRA_MAIN...83300
      ELSE                                                               SUTRA_MAIN...83400
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...83500
         NCBI = 1                                                        SUTRA_MAIN...83600
         NELTA = NELT                                                    SUTRA_MAIN...83700
         KSOLVR = KSOLVP                                                 SUTRA_MAIN...83800
         NSAVE = NSAVEP                                                  SUTRA_MAIN...83900
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIP, NWFP)               SUTRA_MAIN...84000
         KSOLVR = KSOLVU                                                 SUTRA_MAIN...84100
         NSAVE = NSAVEU                                                  SUTRA_MAIN...84200
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIU, NWFU)               SUTRA_MAIN...84300
         NWI = MAX(NWIP, NWIU)                                           SUTRA_MAIN...84400
         NWF = MAX(NWFP, NWFU)                                           SUTRA_MAIN...84500
      END IF                                                             SUTRA_MAIN...84600
      MATDIM=NELT*NCBI                                                   SUTRA_MAIN...84700
C                                                                        SUTRA_MAIN...84800
C.....ALLOCATE REAL AND INTEGER ARRAYS THAT DEPEND ON BANDWIDTH OR NELT  SUTRA_MAIN...84900
      ALLOCATE(PMAT(NELT,NCBI),UMAT(NELT,NCBI),FWK(NWF))                 SUTRA_MAIN...85000
      ALLOCATE(IWK(NWI))                                                 SUTRA_MAIN...85100
      ALLO2 = .TRUE.                                                     SUTRA_MAIN...85200
C                                                                        SUTRA_MAIN...85300
C.....INPUT INITIAL OR RESTART CONDITIONS FROM THE ICS FILE AND          SUTRA_MAIN...85400
C        INITIALIZE PARAMETERS                                           SUTRA_MAIN...85500
      CALL INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,SW,DSWDP, SUTRA_MAIN...85600
     1   PBC,IPBC,IPBCT,NREG,QIN,DPDTITR)                                SUTRA_MAIN...85700
C                                                                        SUTRA_MAIN...85800
C.....COMPUTE AND OUTPUT DIMENSIONS OF SIMULATION                        SUTRA_MAIN...85900
      RMVDIM = 27*NN + 11*NE + 10*NEX + 3*NBCN + N48*(2*NE + NEX)        SUTRA_MAIN...86000
     1   + NNNX + 2*NELT*NCBI + NWF + 6*NOBSN + 3*NSCH                   SUTRA_MAIN...86100
      IMVDIM = NIN + NSOP + NSOU + 2*NBCN + NN + NE                      SUTRA_MAIN...86200
     1   + NDIMJA + NDIMIA + NWI + NOBSN + 3*NSCH                        SUTRA_MAIN...86300
      CMVDIM = 73*NOBS + 89*NSCH                                         SUTRA_MAIN...86400
      PMVDIM = 2*NSCH                                                    SUTRA_MAIN...86500
      TOTMB = (DBLE(RMVDIM)*8D0 + DBLE(IMVDIM)*4D0 + DBLE(CMVDIM))/1D6   SUTRA_MAIN...86600
      WRITE(K3,3000) RMVDIM, IMVDIM, CMVDIM, PMVDIM, TOTMB               SUTRA_MAIN...86700
 3000 FORMAT(////11X,'S I M U L A T I O N   D I M E N S I O N S'//       SUTRA_MAIN...86800
     1   13X,'REAL        ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN...86900
     2   13X,'INTEGER     ARRAYS WERE ALLOCATED ',I12/                   SUTRA_MAIN...87000
     3   13X,'CHARACTER   ARRAYS WERE ALLOCATED ',I12,                   SUTRA_MAIN...87100
     4       ' (SUM OF ARRAY_DIMENSION*CHARACTER_LENGTH)'/               SUTRA_MAIN...87200
     5   13X,'ARRAYS OF POINTERS WERE ALLOCATED ',I12//                  SUTRA_MAIN...87300
     6   13X,F10.3,' Mbytes MEMORY USED FOR MAIN ARRAYS'/                SUTRA_MAIN...87400
     7   13X,'- assuming 1 byte/character'/                              SUTRA_MAIN...87500
     8   13X,'- pointer storage not included')                           SUTRA_MAIN...87600
C                                                                        SUTRA_MAIN...87700
      WRITE(K3,4000)                                                     SUTRA_MAIN...87800
 4000 FORMAT(////////8(132("-")/))                                       SUTRA_MAIN...87900
C                                                                        SUTRA_MAIN...88000
C.....CALL MAIN CONTROL ROUTINE, SUTRA                                   SUTRA_MAIN...88100
      CALL SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,        SUTRA_MAIN...88200
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA_MAIN...88300
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA_MAIN...88400
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA_MAIN...88500
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA_MAIN...88600
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA_MAIN...88700
     6   IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,IWK,IA,JA,            SUTRA_MAIN...88800
     7   IQSOPT,IQSOUT,IPBCT,IUBCT)                                      SUTRA_MAIN...88900
C                                                                        SUTRA_MAIN...89000
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND END      SUTRA_MAIN...89100
9000  CONTINUE                                                           SUTRA_MAIN...89200
      CALL TERSEQ()                                                      SUTRA_MAIN...89300
      END                                                                SUTRA_MAIN...89400
C                                                                        SUTRA_MAIN...89500
C     SUBROUTINE        A  D  S  O  R  B           SUTRA VERSION 2.1     ADSORB.........100
C                                                                        ADSORB.........200
C *** PURPOSE :                                                          ADSORB.........300
C ***  TO CALCULATE VALUES OF EQUILIBRIUM SORPTION PARAMETERS FOR        ADSORB.........400
C ***  LINEAR, FREUNDLICH, AND LANGMUIR MODELS.                          ADSORB.........500
C                                                                        ADSORB.........600
      SUBROUTINE ADSORB(CS1,CS2,CS3,SL,SR,U)                             ADSORB.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ADSORB.........800
      CHARACTER*10 ADSMOD                                                ADSORB.........900
      DIMENSION CS1(NN),CS2(NN),CS3(NN),SL(NN),SR(NN),U(NN)              ADSORB........1000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              ADSORB........1100
     1   NSOP,NSOU,NBCN                                                  ADSORB........1200
      COMMON /MODSOR/ ADSMOD                                             ADSORB........1300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      ADSORB........1400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        ADSORB........1500
C                                                                        ADSORB........1600
C.....NOTE THAT THE CONCENTRATION OF ADSORBATE, CS(I), IS GIVEN BY       ADSORB........1700
C        CS(I) = SL(I)*U(I) + SR(I)                                      ADSORB........1800
C                                                                        ADSORB........1900
C.....NO SORPTION                                                        ADSORB........2000
      IF(ADSMOD.NE.'NONE      ') GOTO 450                                ADSORB........2100
      DO 250 I=1,NN                                                      ADSORB........2200
      CS1(I)=0.D0                                                        ADSORB........2300
      CS2(I)=0.D0                                                        ADSORB........2400
      CS3(I)=0.D0                                                        ADSORB........2500
      SL(I)=0.D0                                                         ADSORB........2600
      SR(I)=0.D0                                                         ADSORB........2700
  250 CONTINUE                                                           ADSORB........2800
      GOTO 2000                                                          ADSORB........2900
C                                                                        ADSORB........3000
C.....LINEAR SORPTION MODEL                                              ADSORB........3100
  450 IF(ADSMOD.NE.'LINEAR    ') GOTO 700                                ADSORB........3200
      DO 500 I=1,NN                                                      ADSORB........3300
      CS1(I)=CHI1*RHOW0                                                  ADSORB........3400
      CS2(I)=0.D0                                                        ADSORB........3500
      CS3(I)=0.D0                                                        ADSORB........3600
      SL(I)=CHI1*RHOW0                                                   ADSORB........3700
      SR(I)=0.D0                                                         ADSORB........3800
  500 CONTINUE                                                           ADSORB........3900
      GOTO 2000                                                          ADSORB........4000
C                                                                        ADSORB........4100
C.....FREUNDLICH SORPTION MODEL                                          ADSORB........4200
  700 IF(ADSMOD.NE.'FREUNDLICH') GOTO 950                                ADSORB........4300
      CHCH=CHI1/CHI2                                                     ADSORB........4400
      DCHI2=1.D0/CHI2                                                    ADSORB........4500
      RH2=RHOW0**DCHI2                                                   ADSORB........4600
      CHI2F=((1.D0-CHI2)/CHI2)                                           ADSORB........4700
      DO 750 I=1,NN                                                      ADSORB........4800
      IF(U(I)) 720,720,730                                               ADSORB........4900
  720 UCH=1.0D0                                                          ADSORB........5000
      GOTO 740                                                           ADSORB........5100
  730 UCH=U(I)**CHI2F                                                    ADSORB........5200
  740 RU=RH2*UCH                                                         ADSORB........5300
      CS1(I)=CHCH*RU                                                     ADSORB........5400
      CS2(I)=0.D0                                                        ADSORB........5500
      CS3(I)=0.D0                                                        ADSORB........5600
      SL(I)=CHI1*RU                                                      ADSORB........5700
      SR(I)=0.D0                                                         ADSORB........5800
  750 CONTINUE                                                           ADSORB........5900
      GOTO 2000                                                          ADSORB........6000
C                                                                        ADSORB........6100
C.....LANGMUIR SORPTION MODEL                                            ADSORB........6200
  950 IF(ADSMOD.NE.'LANGMUIR  ') GOTO 2000                               ADSORB........6300
      DO 1000 I=1,NN                                                     ADSORB........6400
      DD=1.D0+CHI2*RHOW0*U(I)                                            ADSORB........6500
      CS1(I)=(CHI1*RHOW0)/(DD*DD)                                        ADSORB........6600
      CS2(I)=0.D0                                                        ADSORB........6700
      CS3(I)=0.D0                                                        ADSORB........6800
      SL(I)=CS1(I)                                                       ADSORB........6900
      SR(I)=CS1(I)*CHI2*RHOW0*U(I)*U(I)                                  ADSORB........7000
 1000 CONTINUE                                                           ADSORB........7100
C                                                                        ADSORB........7200
 2000 RETURN                                                             ADSORB........7300
      END                                                                ADSORB........7400
C                                                                        ADSORB........7500
C     SUBROUTINE        B  A  N  W  I  D           SUTRA VERSION 2.1     BANWID.........100
C                                                                        BANWID.........200
C *** PURPOSE :                                                          BANWID.........300
C ***  TO CALCULATE THE BANDWIDTH OF THE FINITE ELEMENT MESH.            BANWID.........400
C                                                                        BANWID.........500
      SUBROUTINE BANWID(IN)                                              BANWID.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BANWID.........700
      CHARACTER*80 UNAME,FNAME(0:8)                                      BANWID.........800
      DIMENSION IN(NIN)                                                  BANWID.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BANWID........1000
     1   NSOP,NSOU,NBCN                                                  BANWID........1100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        BANWID........1200
      COMMON /FNAMES/ UNAME,FNAME                                        BANWID........1300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     BANWID........1400
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       BANWID........1500
C                                                                        BANWID........1600
      NDIF=0                                                             BANWID........1700
      II=0                                                               BANWID........1800
      WRITE(K3,100)                                                      BANWID........1900
  100 FORMAT(////11X,'**** MESH ANALYSIS ****'//)                        BANWID........2000
C                                                                        BANWID........2100
C.....FIND ELEMENT WITH MAXIMUM DIFFERENCE IN NODE NUMBERS               BANWID........2200
      DO 2000 L=1,NE                                                     BANWID........2300
      II=II+1                                                            BANWID........2400
      IELO=IN(II)                                                        BANWID........2500
      IEHI=IN(II)                                                        BANWID........2600
      DO 1000 I=2,N48                                                    BANWID........2700
      II=II+1                                                            BANWID........2800
      IF(IN(II).LT.IELO) IELO=IN(II)                                     BANWID........2900
 1000 IF(IN(II).GT.IEHI) IEHI=IN(II)                                     BANWID........3000
      NDIFF=IEHI-IELO                                                    BANWID........3100
      IF(NDIFF.GT.NDIF) THEN                                             BANWID........3200
       NDIF=NDIFF                                                        BANWID........3300
       LEM=L                                                             BANWID........3400
      ENDIF                                                              BANWID........3500
 2000 CONTINUE                                                           BANWID........3600
C                                                                        BANWID........3700
C.....CALCULATE FULL BANDWIDTH, NB.                                      BANWID........3800
      NB=2*NDIF+1                                                        BANWID........3900
      NBHALF=NDIF+1                                                      BANWID........4000
C.....NBI IS USED TO DIMENSION ARRAYS WHOSE SIZE DEPENDS ON THE          BANWID........4100
C        BANDWIDTH.  IT IS THE SAME AS THE ACTUAL BANDWIDTH, NB.         BANWID........4200
      NBI = NB                                                           BANWID........4300
      WRITE(K3,2500) NB,LEM                                              BANWID........4400
 2500 FORMAT(//13X,'MAXIMUM FULL BANDWIDTH, ',I9,                        BANWID........4500
     1   ', WAS CALCULATED IN ELEMENT ',I9)                              BANWID........4600
C                                                                        BANWID........4700
      RETURN                                                             BANWID........4800
      END                                                                BANWID........4900
C                                                                        BANWID........5000
C     SUBROUTINE        B  A  S  I  S  2           SUTRA VERSION 2.1     BASIS2.........100
C                                                                        BASIS2.........200
C *** PURPOSE :                                                          BASIS2.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS2.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS2.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS2.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 2D         BASIS2.........700
C ***  CALCULATIONS ONLY; 3D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS2.........800
C ***  BASIS3.                                                           BASIS2.........900
C                                                                        BASIS2........1000
      SUBROUTINE BASIS2(ICALL,L,XLOC,YLOC,IN,X,Y,F,W,DET,                BASIS2........1100
     1   DFDXG,DFDYG,DWDXG,DWDYG,PITER,UITER,PVEL,POR,THICK,THICKG,      BASIS2........1200
     2   VXG,VYG,SWG,RHOG,VISCG,PORG,VGMAG,RELKG,                        BASIS2........1300
     3   PERMXX,PERMXY,PERMYX,PERMYY,CJ11,CJ12,CJ21,CJ22,                BASIS2........1400
     4   GXSI,GETA,RCIT,RCITM1,RGXG,RGYG,LREG)                           BASIS2........1500
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BASIS2........1600
      DOUBLE PRECISION XLOC,YLOC                                         BASIS2........1700
      DIMENSION IN(NIN),X(NN),Y(NN),UITER(NN),PITER(NN),PVEL(NN),        BASIS2........1800
     1   POR(NN),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),THICK(NN)   BASIS2........1900
      DIMENSION GXSI(NE,4),GETA(NE,4),RCIT(NN),RCITM1(NN),LREG(NE)       BASIS2........2000
      DIMENSION F(4),W(4),DFDXG(4),DFDYG(4),DWDXG(4),DWDYG(4)            BASIS2........2100
      DIMENSION FX(4),FY(4),AFX(4),AFY(4),                               BASIS2........2200
     1   DFDXL(4),DFDYL(4),DWDXL(4),DWDYL(4),                            BASIS2........2300
     2   XDW(4),YDW(4),XIIX(4),YIIY(4)                                   BASIS2........2400
      DIMENSION KTYPE(2)                                                 BASIS2........2500
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BASIS2........2600
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BASIS2........2700
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BASIS2........2800
     1   NSOP,NSOU,NBCN                                                  BASIS2........2900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BASIS2........3000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BASIS2........3100
      DATA XIIX/-1.D0,+1.D0,+1.D0,-1.D0/,                                BASIS2........3200
     1     YIIY/-1.D0,-1.D0,+1.D0,+1.D0/                                 BASIS2........3300
      SAVE XIIX,YIIY                                                     BASIS2........3400
C                                                                        BASIS2........3500
C                                                                        BASIS2........3600
C.....AT THIS LOCATION IN LOCAL COORDINATES, (XLOC,YLOC),                BASIS2........3700
C        CALCULATE SYMMETRIC WEIGHTING FUNCTIONS, F(I),                  BASIS2........3800
C        SPACE DERIVATIVES, DFDXG(I) AND DFDYG(I), AND                   BASIS2........3900
C        DETERMINANT OF JACOBIAN, DET.                                   BASIS2........4000
C                                                                        BASIS2........4100
      XF1=1.D0-XLOC                                                      BASIS2........4200
      XF2=1.D0+XLOC                                                      BASIS2........4300
      YF1=1.D0-YLOC                                                      BASIS2........4400
      YF2=1.D0+YLOC                                                      BASIS2........4500
C                                                                        BASIS2........4600
C.....CALCULATE BASIS FUNCTION, F.                                       BASIS2........4700
      FX(1)=XF1                                                          BASIS2........4800
      FX(2)=XF2                                                          BASIS2........4900
      FX(3)=XF2                                                          BASIS2........5000
      FX(4)=XF1                                                          BASIS2........5100
      FY(1)=YF1                                                          BASIS2........5200
      FY(2)=YF1                                                          BASIS2........5300
      FY(3)=YF2                                                          BASIS2........5400
      FY(4)=YF2                                                          BASIS2........5500
      DO 10 I=1,4                                                        BASIS2........5600
   10 F(I)=0.250D0*FX(I)*FY(I)                                           BASIS2........5700
C                                                                        BASIS2........5800
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS2........5900
      DO 20 I=1,4                                                        BASIS2........6000
      DFDXL(I)=XIIX(I)*0.250D0*FY(I)                                     BASIS2........6100
   20 DFDYL(I)=YIIY(I)*0.250D0*FX(I)                                     BASIS2........6200
C                                                                        BASIS2........6300
C.....CALCULATE ELEMENTS OF JACOBIAN MATRIX, CJ.                         BASIS2........6400
      CJ11=0.D0                                                          BASIS2........6500
      CJ12=0.D0                                                          BASIS2........6600
      CJ21=0.D0                                                          BASIS2........6700
      CJ22=0.D0                                                          BASIS2........6800
      DO 100 IL=1,4                                                      BASIS2........6900
      II=(L-1)*4+IL                                                      BASIS2........7000
      I=IN(II)                                                           BASIS2........7100
      CJ11=CJ11+DFDXL(IL)*X(I)                                           BASIS2........7200
      CJ12=CJ12+DFDXL(IL)*Y(I)                                           BASIS2........7300
      CJ21=CJ21+DFDYL(IL)*X(I)                                           BASIS2........7400
  100 CJ22=CJ22+DFDYL(IL)*Y(I)                                           BASIS2........7500
C                                                                        BASIS2........7600
C.....CALCULATE DETERMINANT OF JACOBIAN MATRIX.                          BASIS2........7700
      DET=CJ11*CJ22-CJ21*CJ12                                            BASIS2........7800
C                                                                        BASIS2........7900
C.....RETURN TO ELEMEN2 WITH JACOBIAN MATRIX ON FIRST TIME STEP.         BASIS2........8000
      IF(ICALL.EQ.0) RETURN                                              BASIS2........8100
C                                                                        BASIS2........8200
C.....CALCULATE ELEMENTS OF INVERSE JACOBIAN MATRIX, CIJ.                BASIS2........8300
      ODET=1.D0/DET                                                      BASIS2........8400
      CIJ11=+ODET*CJ22                                                   BASIS2........8500
      CIJ12=-ODET*CJ12                                                   BASIS2........8600
      CIJ21=-ODET*CJ21                                                   BASIS2........8700
      CIJ22=+ODET*CJ11                                                   BASIS2........8800
C                                                                        BASIS2........8900
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES           BASIS2........9000
      DO 200 I=1,4                                                       BASIS2........9100
      DFDXG(I)=CIJ11*DFDXL(I)+CIJ12*DFDYL(I)                             BASIS2........9200
  200 DFDYG(I)=CIJ21*DFDXL(I)+CIJ22*DFDYL(I)                             BASIS2........9300
C                                                                        BASIS2........9400
C.....CALCULATE CONSISTENT COMPONENTS OF (RHO*GRAV) TERM IN LOCAL        BASIS2........9500
C        COORDINATES AT THIS LOCATION, (XLOC,YLOC)                       BASIS2........9600
      RGXL=0.D0                                                          BASIS2........9700
      RGYL=0.D0                                                          BASIS2........9800
      RGXLM1=0.D0                                                        BASIS2........9900
      RGYLM1=0.D0                                                        BASIS2.......10000
      DO 800 IL=1,4                                                      BASIS2.......10100
      II=(L-1)*4+IL                                                      BASIS2.......10200
      I=IN(II)                                                           BASIS2.......10300
      ADFDXL=DABS(DFDXL(IL))                                             BASIS2.......10400
      ADFDYL=DABS(DFDYL(IL))                                             BASIS2.......10500
      RGXL=RGXL+RCIT(I)*GXSI(L,IL)*ADFDXL                                BASIS2.......10600
      RGYL=RGYL+RCIT(I)*GETA(L,IL)*ADFDYL                                BASIS2.......10700
      RGXLM1=RGXLM1+RCITM1(I)*GXSI(L,IL)*ADFDXL                          BASIS2.......10800
      RGYLM1=RGYLM1+RCITM1(I)*GETA(L,IL)*ADFDYL                          BASIS2.......10900
  800 CONTINUE                                                           BASIS2.......11000
C                                                                        BASIS2.......11100
C.....TRANSFORM CONSISTENT COMPONENTS OF (RHO*GRAV) TERM TO              BASIS2.......11200
C        GLOBAL COORDINATES                                              BASIS2.......11300
      RGXG=CIJ11*RGXL+CIJ12*RGYL                                         BASIS2.......11400
      RGYG=CIJ21*RGXL+CIJ22*RGYL                                         BASIS2.......11500
      RGXGM1=CIJ11*RGXLM1+CIJ12*RGYLM1                                   BASIS2.......11600
      RGYGM1=CIJ21*RGXLM1+CIJ22*RGYLM1                                   BASIS2.......11700
C                                                                        BASIS2.......11800
C.....CALCULATE PARAMETER VALUES AT THIS LOCATION, (XLOC,YLOC)           BASIS2.......11900
      PITERG=0.D0                                                        BASIS2.......12000
      UITERG=0.D0                                                        BASIS2.......12100
      DPDXG=0.D0                                                         BASIS2.......12200
      DPDYG=0.D0                                                         BASIS2.......12300
      PORG=0.D0                                                          BASIS2.......12400
      THICKG=0.0D0                                                       BASIS2.......12500
      DO 1000 IL=1,4                                                     BASIS2.......12600
      II=(L-1)*4 +IL                                                     BASIS2.......12700
      I=IN(II)                                                           BASIS2.......12800
      DPDXG=DPDXG+PVEL(I)*DFDXG(IL)                                      BASIS2.......12900
      DPDYG=DPDYG+PVEL(I)*DFDYG(IL)                                      BASIS2.......13000
      PORG=PORG+POR(I)*F(IL)                                             BASIS2.......13100
      THICKG=THICKG+THICK(I)*F(IL)                                       BASIS2.......13200
      PITERG=PITERG+PITER(I)*F(IL)                                       BASIS2.......13300
      UITERG=UITERG+UITER(I)*F(IL)                                       BASIS2.......13400
 1000 CONTINUE                                                           BASIS2.......13500
C                                                                        BASIS2.......13600
C.....SET VALUES FOR DENSITY AND VISCOSITY.                              BASIS2.......13700
C.....RHOG = FUNCTION(UITER)                                             BASIS2.......13800
      RHOG=RHOW0+DRWDU*(UITERG-URHOW0)                                   BASIS2.......13900
C.....VISCG = FUNCTION(UITER); VISCOSITY IN UNITS OF VISC0*(KG/(M*SEC))  BASIS2.......14000
      IF(ME) 1300,1300,1200                                              BASIS2.......14100
 1200 VISCG=VISC0*239.4D-7*(10.D0**(248.37D0/(UITERG+133.15D0)))         BASIS2.......14200
      GOTO 1400                                                          BASIS2.......14300
C.....FOR SOLUTE TRANSPORT, VISCG IS TAKEN TO BE CONSTANT                BASIS2.......14400
 1300 VISCG=VISC0                                                        BASIS2.......14500
 1400 CONTINUE                                                           BASIS2.......14600
C                                                                        BASIS2.......14700
C.....SET UNSATURATED FLOW PARAMETERS SWG AND RELKG                      BASIS2.......14800
      IF(IUNSAT-2) 1600,1500,1600                                        BASIS2.......14900
 1500 IF(PITERG) 1550,1600,1600                                          BASIS2.......15000
 1550 CALL UNSAT(SWG,DSWDPG,RELKG,PITERG,LREG(L))                        BASIS2.......15100
      GOTO 1700                                                          BASIS2.......15200
 1600 SWG=1.0D0                                                          BASIS2.......15300
      RELKG=1.0D0                                                        BASIS2.......15400
 1700 CONTINUE                                                           BASIS2.......15500
C                                                                        BASIS2.......15600
C.....CALCULATE CONSISTENT FLUID VELOCITIES WITH RESPECT TO GLOBAL       BASIS2.......15700
C        COORDINATES, VXG, VYG, AND VGMAG, AT THIS LOCATION, (XLOC,YLOC) BASIS2.......15800
      DENOM=1.D0/(PORG*SWG*VISCG)                                        BASIS2.......15900
      PGX=DPDXG-RGXGM1                                                   BASIS2.......16000
      PGY=DPDYG-RGYGM1                                                   BASIS2.......16100
C.....ZERO OUT RANDOM BOUYANT DRIVING FORCES DUE TO DIFFERENCING         BASIS2.......16200
C        NUMBERS PAST PRECISION LIMIT.  MINIMUM DRIVING FORCE IS         BASIS2.......16300
C        1.D-10 OF PRESSURE GRADIENT.  (THIS VALUE MAY BE CHANGED        BASIS2.......16400
C        DEPENDING ON MACHINE PRECISION.)                                BASIS2.......16500
      IF(DPDXG) 1720,1730,1720                                           BASIS2.......16600
 1720 IF(DABS(PGX/DPDXG)-1.0D-10) 1725,1725,1730                         BASIS2.......16700
 1725 PGX=0.0D0                                                          BASIS2.......16800
 1730 IF(DPDYG) 1750,1760,1750                                           BASIS2.......16900
 1750 IF(DABS(PGY/DPDYG)-1.0D-10) 1755,1755,1760                         BASIS2.......17000
 1755 PGY=0.0D0                                                          BASIS2.......17100
 1760 VXG=-DENOM*(PERMXX(L)*PGX+PERMXY(L)*PGY)*RELKG                     BASIS2.......17200
      VYG=-DENOM*(PERMYX(L)*PGX+PERMYY(L)*PGY)*RELKG                     BASIS2.......17300
      VXG2=VXG*VXG                                                       BASIS2.......17400
      VYG2=VYG*VYG                                                       BASIS2.......17500
      VGMAG=DSQRT(VXG2+VYG2)                                             BASIS2.......17600
C                                                                        BASIS2.......17700
C.....AT THIS POINT IN LOCAL COORDINATES, (XLOC,YLOC),                   BASIS2.......17800
C        CALCULATE ASYMMETRIC WEIGHTING FUNCTIONS, W(I),                 BASIS2.......17900
C        AND SPACE DERIVATIVES, DWDXG(I) AND DWDYG(I).                   BASIS2.......18000
C                                                                        BASIS2.......18100
C.....ASYMMETRIC FUNCTIONS SIMPLIFY WHEN  UP=0.0                         BASIS2.......18200
      IF(UP.GT.1.0D-6.AND.NOUMAT.EQ.0) GOTO 1790                         BASIS2.......18300
      DO 1780 I=1,4                                                      BASIS2.......18400
      W(I)=F(I)                                                          BASIS2.......18500
      DWDXG(I)=DFDXG(I)                                                  BASIS2.......18600
      DWDYG(I)=DFDYG(I)                                                  BASIS2.......18700
 1780 CONTINUE                                                           BASIS2.......18800
C.....RETURN WHEN ONLY SYMMETRIC WEIGHTING FUNCTIONS ARE USED            BASIS2.......18900
      RETURN                                                             BASIS2.......19000
C                                                                        BASIS2.......19100
C.....CALCULATE FLUID VELOCITIES WITH RESPECT TO LOCAL COORDINATES,      BASIS2.......19200
C        VXL, VYL, AND VLMAG, AT THIS LOCATION, (XLOC,YLOC).             BASIS2.......19300
 1790 VXL=CIJ11*VXG+CIJ21*VYG                                            BASIS2.......19400
      VYL=CIJ12*VXG+CIJ22*VYG                                            BASIS2.......19500
      VLMAG=DSQRT(VXL*VXL+VYL*VYL)                                       BASIS2.......19600
C                                                                        BASIS2.......19700
      AA=0.0D0                                                           BASIS2.......19800
      BB=0.0D0                                                           BASIS2.......19900
      IF(VLMAG) 1900,1900,1800                                           BASIS2.......20000
 1800 AA=UP*VXL/VLMAG                                                    BASIS2.......20100
      BB=UP*VYL/VLMAG                                                    BASIS2.......20200
C                                                                        BASIS2.......20300
 1900 XIXI=.750D0*AA*XF1*XF2                                             BASIS2.......20400
      YIYI=.750D0*BB*YF1*YF2                                             BASIS2.......20500
      DO 2000 I=1,4                                                      BASIS2.......20600
      AFX(I)=.50D0*FX(I)+XIIX(I)*XIXI                                    BASIS2.......20700
 2000 AFY(I)=.50D0*FY(I)+YIIY(I)*YIYI                                    BASIS2.......20800
C                                                                        BASIS2.......20900
C.....CALCULATE ASYMMETRIC WEIGHTING FUNCTION, W.                        BASIS2.......21000
      DO 3000 I=1,4                                                      BASIS2.......21100
 3000 W(I)=AFX(I)*AFY(I)                                                 BASIS2.......21200
C                                                                        BASIS2.......21300
      THAAX=0.50D0-1.50D0*AA*XLOC                                        BASIS2.......21400
      THBBY=0.50D0-1.50D0*BB*YLOC                                        BASIS2.......21500
      DO 4000 I=1,4                                                      BASIS2.......21600
      XDW(I)=XIIX(I)*THAAX                                               BASIS2.......21700
 4000 YDW(I)=YIIY(I)*THBBY                                               BASIS2.......21800
C                                                                        BASIS2.......21900
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS2.......22000
      DO 5000 I=1,4                                                      BASIS2.......22100
      DWDXL(I)=XDW(I)*AFY(I)                                             BASIS2.......22200
 5000 DWDYL(I)=YDW(I)*AFX(I)                                             BASIS2.......22300
C                                                                        BASIS2.......22400
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES.          BASIS2.......22500
      DO 6000 I=1,4                                                      BASIS2.......22600
      DWDXG(I)=CIJ11*DWDXL(I)+CIJ12*DWDYL(I)                             BASIS2.......22700
 6000 DWDYG(I)=CIJ21*DWDXL(I)+CIJ22*DWDYL(I)                             BASIS2.......22800
C                                                                        BASIS2.......22900
C                                                                        BASIS2.......23000
      RETURN                                                             BASIS2.......23100
      END                                                                BASIS2.......23200
C                                                                        BASIS2.......23300
C     SUBROUTINE        B  A  S  I  S  3           SUTRA VERSION 2.1     BASIS3.........100
C                                                                        BASIS3.........200
C *** PURPOSE :                                                          BASIS3.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS3.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS3.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS3.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 3D         BASIS3.........700
C ***  CALCULATIONS ONLY; 2D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS3.........800
C ***  BASIS2.                                                           BASIS3.........900
C                                                                        BASIS3........1000
      SUBROUTINE BASIS3(ICALL,L,XLOC,YLOC,ZLOC,IN,X,Y,Z,F,W,DET,         BASIS3........1100
     1   DFDXG,DFDYG,DFDZG,DWDXG,DWDYG,DWDZG,PITER,UITER,PVEL,POR,       BASIS3........1200
     2   VXG,VYG,VZG,SWG,RHOG,VISCG,PORG,VGMAG,RELKG,                    BASIS3........1300
     3   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, BASIS3........1400
     4   CJ11,CJ12,CJ13,CJ21,CJ22,CJ23,CJ31,CJ32,CJ33,                   BASIS3........1500
     4   GXSI,GETA,GZET,RCIT,RCITM1,RGXG,RGYG,RGZG,LREG)                 BASIS3........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BASIS3........1700
      DOUBLE PRECISION XLOC,YLOC,ZLOC                                    BASIS3........1800
      DIMENSION IN(NIN),X(NN),Y(NN),Z(NN),UITER(NN),PITER(NN),PVEL(NN),  BASIS3........1900
     1   POR(NN),PERMXX(NE),PERMXY(NE),PERMXZ(NE),PERMYX(NE),            BASIS3........2000
     2   PERMYY(NE),PERMYZ(NE),PERMZX(NE),PERMZY(NE),PERMZZ(NE)          BASIS3........2100
      DIMENSION GXSI(NE,8),GETA(NE,8),GZET(NE,8)                         BASIS3........2200
      DIMENSION RCIT(NN),RCITM1(NN),LREG(NE)                             BASIS3........2300
      DIMENSION F(8),DFDXG(8),DFDYG(8),DFDZG(8)                          BASIS3........2400
      DIMENSION W(8),DWDXG(8),DWDYG(8),DWDZG(8)                          BASIS3........2500
      DIMENSION FX(8),FY(8),FZ(8),AFX(8),AFY(8),AFZ(8),                  BASIS3........2600
     1   DFDXL(8),DFDYL(8),DFDZL(8),DWDXL(8),DWDYL(8),DWDZL(8),          BASIS3........2700
     2   XDW(8),YDW(8),ZDW(8),XIIX(8),YIIY(8),ZIIZ(8)                    BASIS3........2800
      DIMENSION KTYPE(2)                                                 BASIS3........2900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BASIS3........3000
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BASIS3........3100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BASIS3........3200
     1   NSOP,NSOU,NBCN                                                  BASIS3........3300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BASIS3........3400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BASIS3........3500
      DATA XIIX/-1.D0,+1.D0,+1.D0,-1.D0,-1.D0,+1.D0,+1.D0,-1.D0/         BASIS3........3600
      DATA YIIY/-1.D0,-1.D0,+1.D0,+1.D0,-1.D0,-1.D0,+1.D0,+1.D0/         BASIS3........3700
      DATA ZIIZ/-1.D0,-1.D0,-1.D0,-1.D0,+1.D0,+1.D0,+1.D0,+1.D0/         BASIS3........3800
      SAVE XIIX,YIIY,ZIIZ                                                BASIS3........3900
C                                                                        BASIS3........4000
C                                                                        BASIS3........4100
C.....AT THIS LOCATION IN LOCAL COORDINATES, (XLOC,YLOC,ZLOC),           BASIS3........4200
C        CALCULATE SYMMETRIC WEIGHTING FUNCTIONS, F(I),                  BASIS3........4300
C        SPACE DERIVATIVES, DFDXG(I), DFDYX(I), AND DFDZG(I),            BASIS3........4400
C        AND DETERMINANT OF JACOBIAN, DET.                               BASIS3........4500
C                                                                        BASIS3........4600
      XF1=1.D0-XLOC                                                      BASIS3........4700
      XF2=1.D0+XLOC                                                      BASIS3........4800
      YF1=1.D0-YLOC                                                      BASIS3........4900
      YF2=1.D0+YLOC                                                      BASIS3........5000
      ZF1=1.D0-ZLOC                                                      BASIS3........5100
      ZF2=1.D0+ZLOC                                                      BASIS3........5200
C                                                                        BASIS3........5300
C.....CALCULATE BASIS FUNCTION, F.                                       BASIS3........5400
      FX(1)=XF1                                                          BASIS3........5500
      FX(2)=XF2                                                          BASIS3........5600
      FX(3)=XF2                                                          BASIS3........5700
      FX(4)=XF1                                                          BASIS3........5800
      FX(5)=XF1                                                          BASIS3........5900
      FX(6)=XF2                                                          BASIS3........6000
      FX(7)=XF2                                                          BASIS3........6100
      FX(8)=XF1                                                          BASIS3........6200
      FY(1)=YF1                                                          BASIS3........6300
      FY(2)=YF1                                                          BASIS3........6400
      FY(3)=YF2                                                          BASIS3........6500
      FY(4)=YF2                                                          BASIS3........6600
      FY(5)=YF1                                                          BASIS3........6700
      FY(6)=YF1                                                          BASIS3........6800
      FY(7)=YF2                                                          BASIS3........6900
      FY(8)=YF2                                                          BASIS3........7000
      FZ(1)=ZF1                                                          BASIS3........7100
      FZ(2)=ZF1                                                          BASIS3........7200
      FZ(3)=ZF1                                                          BASIS3........7300
      FZ(4)=ZF1                                                          BASIS3........7400
      FZ(5)=ZF2                                                          BASIS3........7500
      FZ(6)=ZF2                                                          BASIS3........7600
      FZ(7)=ZF2                                                          BASIS3........7700
      FZ(8)=ZF2                                                          BASIS3........7800
      DO 10 I=1,8                                                        BASIS3........7900
   10 F(I)=0.125D0*FX(I)*FY(I)*FZ(I)                                     BASIS3........8000
C                                                                        BASIS3........8100
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS3........8200
      DO 20 I=1,8                                                        BASIS3........8300
      DFDXL(I)=XIIX(I)*0.125D0*FY(I)*FZ(I)                               BASIS3........8400
      DFDYL(I)=YIIY(I)*0.125D0*FX(I)*FZ(I)                               BASIS3........8500
   20 DFDZL(I)=ZIIZ(I)*0.125D0*FX(I)*FY(I)                               BASIS3........8600
C                                                                        BASIS3........8700
C.....CALCULATE ELEMENTS OF JACOBIAN MATRIX, CJ.                         BASIS3........8800
      CJ11=0.D0                                                          BASIS3........8900
      CJ12=0.D0                                                          BASIS3........9000
      CJ13=0.D0                                                          BASIS3........9100
      CJ21=0.D0                                                          BASIS3........9200
      CJ22=0.D0                                                          BASIS3........9300
      CJ23=0.D0                                                          BASIS3........9400
      CJ31=0.D0                                                          BASIS3........9500
      CJ32=0.D0                                                          BASIS3........9600
      CJ33=0.D0                                                          BASIS3........9700
      DO 100 IL=1,8                                                      BASIS3........9800
      II=(L-1)*8+IL                                                      BASIS3........9900
      I=IN(II)                                                           BASIS3.......10000
      CJ11=CJ11+DFDXL(IL)*X(I)                                           BASIS3.......10100
      CJ12=CJ12+DFDXL(IL)*Y(I)                                           BASIS3.......10200
      CJ13=CJ13+DFDXL(IL)*Z(I)                                           BASIS3.......10300
      CJ21=CJ21+DFDYL(IL)*X(I)                                           BASIS3.......10400
      CJ22=CJ22+DFDYL(IL)*Y(I)                                           BASIS3.......10500
      CJ23=CJ23+DFDYL(IL)*Z(I)                                           BASIS3.......10600
      CJ31=CJ31+DFDZL(IL)*X(I)                                           BASIS3.......10700
      CJ32=CJ32+DFDZL(IL)*Y(I)                                           BASIS3.......10800
  100 CJ33=CJ33+DFDZL(IL)*Z(I)                                           BASIS3.......10900
C                                                                        BASIS3.......11000
C.....CALCULATE DETERMINANT OF JACOBIAN MATRIX.                          BASIS3.......11100
      DET=CJ11*(CJ22*CJ33-CJ32*CJ23)                                     BASIS3.......11200
     1   -CJ21*(CJ12*CJ33-CJ32*CJ13)                                     BASIS3.......11300
     2   +CJ31*(CJ12*CJ23-CJ22*CJ13)                                     BASIS3.......11400
C                                                                        BASIS3.......11500
C.....RETURN TO ELEMEN3 WITH JACOBIAN MATRIX ON FIRST TIME STEP.         BASIS3.......11600
      IF(ICALL.EQ.0) RETURN                                              BASIS3.......11700
C                                                                        BASIS3.......11800
C                                                                        BASIS3.......11900
C.....CALCULATE ELEMENTS OF INVERSE JACOBIAN MATRIX, CIJ.                BASIS3.......12000
      ODET=1.D0/DET                                                      BASIS3.......12100
      CIJ11=+ODET*(CJ22*CJ33-CJ32*CJ23)                                  BASIS3.......12200
      CIJ12=-ODET*(CJ12*CJ33-CJ32*CJ13)                                  BASIS3.......12300
      CIJ13=+ODET*(CJ12*CJ23-CJ22*CJ13)                                  BASIS3.......12400
      CIJ21=-ODET*(CJ21*CJ33-CJ31*CJ23)                                  BASIS3.......12500
      CIJ22=+ODET*(CJ11*CJ33-CJ31*CJ13)                                  BASIS3.......12600
      CIJ23=-ODET*(CJ11*CJ23-CJ21*CJ13)                                  BASIS3.......12700
      CIJ31=+ODET*(CJ21*CJ32-CJ31*CJ22)                                  BASIS3.......12800
      CIJ32=-ODET*(CJ11*CJ32-CJ31*CJ12)                                  BASIS3.......12900
      CIJ33=+ODET*(CJ11*CJ22-CJ21*CJ12)                                  BASIS3.......13000
C                                                                        BASIS3.......13100
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES           BASIS3.......13200
      DO 200 I=1,8                                                       BASIS3.......13300
      DFDXG(I)=CIJ11*DFDXL(I)+CIJ12*DFDYL(I)+CIJ13*DFDZL(I)              BASIS3.......13400
      DFDYG(I)=CIJ21*DFDXL(I)+CIJ22*DFDYL(I)+CIJ23*DFDZL(I)              BASIS3.......13500
  200 DFDZG(I)=CIJ31*DFDXL(I)+CIJ32*DFDYL(I)+CIJ33*DFDZL(I)              BASIS3.......13600
C                                                                        BASIS3.......13700
C.....CALCULATE CONSISTENT COMPONENTS OF (RHO*GRAV) TERM IN LOCAL        BASIS3.......13800
C        COORDINATES AT THIS LOCATION, (XLOC,YLOC,ZLOC)                  BASIS3.......13900
      RGXL=0.D0                                                          BASIS3.......14000
      RGYL=0.D0                                                          BASIS3.......14100
      RGZL=0.D0                                                          BASIS3.......14200
      RGXLM1=0.D0                                                        BASIS3.......14300
      RGYLM1=0.D0                                                        BASIS3.......14400
      RGZLM1=0.D0                                                        BASIS3.......14500
      DO 800 IL=1,8                                                      BASIS3.......14600
      II=(L-1)*8+IL                                                      BASIS3.......14700
      I=IN(II)                                                           BASIS3.......14800
      ADFDXL=DABS(DFDXL(IL))                                             BASIS3.......14900
      ADFDYL=DABS(DFDYL(IL))                                             BASIS3.......15000
      ADFDZL=DABS(DFDZL(IL))                                             BASIS3.......15100
      RGXL=RGXL+RCIT(I)*GXSI(L,IL)*ADFDXL                                BASIS3.......15200
      RGYL=RGYL+RCIT(I)*GETA(L,IL)*ADFDYL                                BASIS3.......15300
      RGZL=RGZL+RCIT(I)*GZET(L,IL)*ADFDZL                                BASIS3.......15400
      RGXLM1=RGXLM1+RCITM1(I)*GXSI(L,IL)*ADFDXL                          BASIS3.......15500
      RGYLM1=RGYLM1+RCITM1(I)*GETA(L,IL)*ADFDYL                          BASIS3.......15600
      RGZLM1=RGZLM1+RCITM1(I)*GZET(L,IL)*ADFDZL                          BASIS3.......15700
  800 CONTINUE                                                           BASIS3.......15800
C                                                                        BASIS3.......15900
C.....TRANSFORM CONSISTENT COMPONENTS OF (RHO*GRAV) TERM TO              BASIS3.......16000
C        GLOBAL COORDINATES                                              BASIS3.......16100
      RGXG=CIJ11*RGXL+CIJ12*RGYL+CIJ13*RGZL                              BASIS3.......16200
      RGYG=CIJ21*RGXL+CIJ22*RGYL+CIJ23*RGZL                              BASIS3.......16300
      RGZG=CIJ31*RGXL+CIJ32*RGYL+CIJ33*RGZL                              BASIS3.......16400
      RGXGM1=CIJ11*RGXLM1+CIJ12*RGYLM1+CIJ13*RGZLM1                      BASIS3.......16500
      RGYGM1=CIJ21*RGXLM1+CIJ22*RGYLM1+CIJ23*RGZLM1                      BASIS3.......16600
      RGZGM1=CIJ31*RGXLM1+CIJ32*RGYLM1+CIJ33*RGZLM1                      BASIS3.......16700
C                                                                        BASIS3.......16800
C.....CALCULATE PARAMETER VALUES AT THIS LOCATION, (XLOC,YLOC,ZLOC)      BASIS3.......16900
      PITERG=0.D0                                                        BASIS3.......17000
      UITERG=0.D0                                                        BASIS3.......17100
      DPDXG=0.D0                                                         BASIS3.......17200
      DPDYG=0.D0                                                         BASIS3.......17300
      DPDZG=0.D0                                                         BASIS3.......17400
      PORG=0.D0                                                          BASIS3.......17500
      DO 1000 IL=1,8                                                     BASIS3.......17600
      II=(L-1)*8 +IL                                                     BASIS3.......17700
      I=IN(II)                                                           BASIS3.......17800
      DPDXG=DPDXG+PVEL(I)*DFDXG(IL)                                      BASIS3.......17900
      DPDYG=DPDYG+PVEL(I)*DFDYG(IL)                                      BASIS3.......18000
      DPDZG=DPDZG+PVEL(I)*DFDZG(IL)                                      BASIS3.......18100
      PORG=PORG+POR(I)*F(IL)                                             BASIS3.......18200
      PITERG=PITERG+PITER(I)*F(IL)                                       BASIS3.......18300
      UITERG=UITERG+UITER(I)*F(IL)                                       BASIS3.......18400
 1000 CONTINUE                                                           BASIS3.......18500
C                                                                        BASIS3.......18600
C.....SET VALUES FOR DENSITY AND VISCOSITY.                              BASIS3.......18700
C.....RHOG = FUNCTION(UITER)                                             BASIS3.......18800
      RHOG=RHOW0+DRWDU*(UITERG-URHOW0)                                   BASIS3.......18900
C.....VISCG = FUNCTION(UITER); VISCOSITY IN UNITS OF VISC0*(KG/(M*SEC))  BASIS3.......19000
      IF(ME) 1300,1300,1200                                              BASIS3.......19100
 1200 VISCG=VISC0*239.4D-7*(10.D0**(248.37D0/(UITERG+133.15D0)))         BASIS3.......19200
      GOTO 1400                                                          BASIS3.......19300
C.....FOR SOLUTE TRANSPORT, VISCG IS TAKEN TO BE CONSTANT                BASIS3.......19400
 1300 VISCG=VISC0                                                        BASIS3.......19500
 1400 CONTINUE                                                           BASIS3.......19600
C                                                                        BASIS3.......19700
C.....SET UNSATURATED FLOW PARAMETERS SWG AND RELKG                      BASIS3.......19800
      IF(IUNSAT-2) 1600,1500,1600                                        BASIS3.......19900
 1500 IF(PITERG) 1550,1600,1600                                          BASIS3.......20000
 1550 CALL UNSAT(SWG,DSWDPG,RELKG,PITERG,LREG(L))                        BASIS3.......20100
      GOTO 1700                                                          BASIS3.......20200
 1600 SWG=1.0D0                                                          BASIS3.......20300
      RELKG=1.0D0                                                        BASIS3.......20400
 1700 CONTINUE                                                           BASIS3.......20500
C                                                                        BASIS3.......20600
C.....CALCULATE CONSISTENT FLUID VELOCITIES WITH RESPECT TO GLOBAL       BASIS3.......20700
C        COORDINATES, VXG, VYG, VZG, AND VGMAG, AT THIS LOCATION,        BASIS3.......20800
C        (XLOC,YLOC,ZLOC)                                                BASIS3.......20900
      DENOM=1.D0/(PORG*SWG*VISCG)                                        BASIS3.......21000
      PGX=DPDXG-RGXGM1                                                   BASIS3.......21100
      PGY=DPDYG-RGYGM1                                                   BASIS3.......21200
      PGZ=DPDZG-RGZGM1                                                   BASIS3.......21300
C.....ZERO OUT RANDOM BOUYANT DRIVING FORCES DUE TO DIFFERENCING         BASIS3.......21400
C        NUMBERS PAST PRECISION LIMIT.  MINIMUM DRIVING FORCE IS         BASIS3.......21500
C        1.D-10 OF PRESSURE GRADIENT.  (THIS VALUE MAY BE CHANGED        BASIS3.......21600
C        DEPENDING ON MACHINE PRECISION.)                                BASIS3.......21700
      IF(DPDXG) 1720,1727,1720                                           BASIS3.......21800
 1720 IF(DABS(PGX/DPDXG)-1.0D-10) 1725,1725,1727                         BASIS3.......21900
 1725 PGX=0.0D0                                                          BASIS3.......22000
 1727 IF(DPDYG) 1730,1737,1730                                           BASIS3.......22100
 1730 IF(DABS(PGY/DPDYG)-1.0D-10) 1735,1735,1737                         BASIS3.......22200
 1735 PGY=0.0D0                                                          BASIS3.......22300
 1737 IF(DPDZG) 1740,1760,1740                                           BASIS3.......22400
 1740 IF(DABS(PGZ/DPDZG)-1.0D-10) 1745,1745,1760                         BASIS3.......22500
 1745 PGZ=0.0D0                                                          BASIS3.......22600
 1760 VXG=-DENOM*(PERMXX(L)*PGX+PERMXY(L)*PGY+PERMXZ(L)*PGZ)*RELKG       BASIS3.......22700
      VYG=-DENOM*(PERMYX(L)*PGX+PERMYY(L)*PGY+PERMYZ(L)*PGZ)*RELKG       BASIS3.......22800
      VZG=-DENOM*(PERMZX(L)*PGX+PERMZY(L)*PGY+PERMZZ(L)*PGZ)*RELKG       BASIS3.......22900
      VXG2=VXG*VXG                                                       BASIS3.......23000
      VYG2=VYG*VYG                                                       BASIS3.......23100
      VZG2=VZG*VZG                                                       BASIS3.......23200
      VGMAG=DSQRT(VXG2+VYG2+VZG2)                                        BASIS3.......23300
C                                                                        BASIS3.......23400
C.....AT THIS POINT IN LOCAL COORDINATES, (XLOC,YLOC,ZLOC),              BASIS3.......23500
C        CALCULATE ASYMMETRIC WEIGHTING FUNCTIONS, W(I),                 BASIS3.......23600
C        AND SPACE DERIVATIVES, DWDXG(I), DWDYG(I), AND DWDZG(I).        BASIS3.......23700
C                                                                        BASIS3.......23800
C.....ASYMMETRIC FUNCTIONS SIMPLIFY WHEN  UP=0.0                         BASIS3.......23900
      IF(UP.GT.1.0D-6.AND.NOUMAT.EQ.0) GOTO 1790                         BASIS3.......24000
      DO 1780 I=1,8                                                      BASIS3.......24100
      W(I)=F(I)                                                          BASIS3.......24200
      DWDXG(I)=DFDXG(I)                                                  BASIS3.......24300
      DWDYG(I)=DFDYG(I)                                                  BASIS3.......24400
      DWDZG(I)=DFDZG(I)                                                  BASIS3.......24500
 1780 CONTINUE                                                           BASIS3.......24600
C.....RETURN WHEN ONLY SYMMETRIC WEIGHTING FUNCTIONS ARE USED            BASIS3.......24700
      RETURN                                                             BASIS3.......24800
C                                                                        BASIS3.......24900
C.....CALCULATE FLUID VELOCITIES WITH RESPECT TO LOCAL COORDINATES,      BASIS3.......25000
C        VXL, VYL, VZL, AND VLMAG, AT THIS LOCATION, (XLOC,YLOC,ZLOC).   BASIS3.......25100
 1790 VXL=CIJ11*VXG+CIJ21*VYG+CIJ31*VZG                                  BASIS3.......25200
      VYL=CIJ12*VXG+CIJ22*VYG+CIJ32*VZG                                  BASIS3.......25300
      VZL=CIJ13*VXG+CIJ23*VYG+CIJ33*VZG                                  BASIS3.......25400
      VLMAG=DSQRT(VXL*VXL+VYL*VYL+VZL*VZL)                               BASIS3.......25500
C                                                                        BASIS3.......25600
      AA=0.0D0                                                           BASIS3.......25700
      BB=0.0D0                                                           BASIS3.......25800
      GG=0.0D0                                                           BASIS3.......25900
      IF(VLMAG) 1900,1900,1800                                           BASIS3.......26000
 1800 AA=UP*VXL/VLMAG                                                    BASIS3.......26100
      BB=UP*VYL/VLMAG                                                    BASIS3.......26200
      GG=UP*VZL/VLMAG                                                    BASIS3.......26300
C                                                                        BASIS3.......26400
 1900 XIXI=.750D0*AA*XF1*XF2                                             BASIS3.......26500
      YIYI=.750D0*BB*YF1*YF2                                             BASIS3.......26600
      ZIZI=.750D0*GG*ZF1*ZF2                                             BASIS3.......26700
      DO 2000 I=1,8                                                      BASIS3.......26800
      AFX(I)=.50D0*FX(I)+XIIX(I)*XIXI                                    BASIS3.......26900
      AFY(I)=.50D0*FY(I)+YIIY(I)*YIYI                                    BASIS3.......27000
 2000 AFZ(I)=.50D0*FZ(I)+ZIIZ(I)*ZIZI                                    BASIS3.......27100
C                                                                        BASIS3.......27200
C.....CALCULATE ASYMMETRIC WEIGHTING FUNCTION, W.                        BASIS3.......27300
      DO 3000 I=1,8                                                      BASIS3.......27400
 3000 W(I)=AFX(I)*AFY(I)*AFZ(I)                                          BASIS3.......27500
C                                                                        BASIS3.......27600
      THAAX=0.50D0-1.50D0*AA*XLOC                                        BASIS3.......27700
      THBBY=0.50D0-1.50D0*BB*YLOC                                        BASIS3.......27800
      THGGZ=0.50D0-1.50D0*GG*ZLOC                                        BASIS3.......27900
      DO 4000 I=1,8                                                      BASIS3.......28000
      XDW(I)=XIIX(I)*THAAX                                               BASIS3.......28100
      YDW(I)=YIIY(I)*THBBY                                               BASIS3.......28200
 4000 ZDW(I)=ZIIZ(I)*THGGZ                                               BASIS3.......28300
C                                                                        BASIS3.......28400
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS3.......28500
      DO 5000 I=1,8                                                      BASIS3.......28600
      DWDXL(I)=XDW(I)*AFY(I)*AFZ(I)                                      BASIS3.......28700
      DWDYL(I)=YDW(I)*AFX(I)*AFZ(I)                                      BASIS3.......28800
 5000 DWDZL(I)=ZDW(I)*AFX(I)*AFY(I)                                      BASIS3.......28900
C                                                                        BASIS3.......29000
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES.          BASIS3.......29100
      DO 6000 I=1,8                                                      BASIS3.......29200
      DWDXG(I)=CIJ11*DWDXL(I)+CIJ12*DWDYL(I)+CIJ13*DWDZL(I)              BASIS3.......29300
      DWDYG(I)=CIJ21*DWDXL(I)+CIJ22*DWDYL(I)+CIJ23*DWDZL(I)              BASIS3.......29400
 6000 DWDZG(I)=CIJ31*DWDXL(I)+CIJ32*DWDYL(I)+CIJ33*DWDZL(I)              BASIS3.......29500
C                                                                        BASIS3.......29600
C                                                                        BASIS3.......29700
      RETURN                                                             BASIS3.......29800
      END                                                                BASIS3.......29900
C                                                                        BASIS3.......30000
C     SUBROUTINE        B  C                       SUTRA VERSION 2.1     BC.............100
C                                                                        BC.............200
C *** PURPOSE :                                                          BC.............300
C ***  TO IMPLEMENT SPECIFIED PRESSURE AND SPECIFIED TEMPERATURE OR      BC.............400
C ***  CONCENTRATION CONDITIONS BY MODIFYING THE GLOBAL FLOW AND         BC.............500
C ***  TRANSPORT MATRIX EQUATIONS.                                       BC.............600
C                                                                        BC.............700
      SUBROUTINE BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,JA)  BC.............800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BC.............900
      DIMENSION PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),UVEC(NNVEC), BC............1000
     1   IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN),QPLITR(NBCN)          BC............1100
      DIMENSION JA(NDIMJA)                                               BC............1200
      DIMENSION KTYPE(2)                                                 BC............1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BC............1400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BC............1500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BC............1600
     1   NSOP,NSOU,NBCN                                                  BC............1700
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        BC............1800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           BC............1900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BC............2000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BC............2100
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           BC............2200
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BC............2300
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  BC............2400
C                                                                        BC............2500
C                                                                        BC............2600
C.....SET UP MATRIX STRUCTURE INFORMATION                                BC............2700
      IF (KSOLVP.EQ.0) THEN                                              BC............2800
         JMID = NBHALF                                                   BC............2900
      ELSE                                                               BC............3000
         JMID = 1                                                        BC............3100
      END IF                                                             BC............3200
C                                                                        BC............3300
      IF(NPBC.EQ.0) GOTO 1050                                            BC............3400
C.....SPECIFIED P BOUNDARY CONDITIONS                                    BC............3500
      DO 1000 IP=1,NPBC                                                  BC............3600
      I=IABS(IPBC(IP))                                                   BC............3700
      IF (KSOLVP.EQ.0) THEN                                              BC............3800
         IMID = I                                                        BC............3900
      ELSE                                                               BC............4000
         IMID = JA(I)                                                    BC............4100
      END IF                                                             BC............4200
C                                                                        BC............4300
      IF(ML-1) 100,100,200                                               BC............4400
C.....MODIFY EQUATION FOR P BY ADDING FLUID SOURCE AT SPECIFIED          BC............4500
C        PRESSURE NODE                                                   BC............4600
  100 GPINL=-GNUP                                                        BC............4700
      GPINR=GNUP*PBC(IP)                                                 BC............4800
      PMAT(IMID,JMID)=PMAT(IMID,JMID)-GPINL                              BC............4900
      PVEC(I)=PVEC(I)+GPINR                                              BC............5000
C                                                                        BC............5100
      IF(ML-1) 200,1000,200                                              BC............5200
C.....MODIFY EQUATION FOR U BY ADDING U SOURCE WHEN FLUID FLOWS IN       BC............5300
C        AT SPECIFIED PRESSURE NODE                                      BC............5400
  200 GUR=0.0D0                                                          BC............5500
      GUL=0.0D0                                                          BC............5600
      IF(QPLITR(IP)) 360,360,340                                         BC............5700
  340 GUL=-CW*QPLITR(IP)                                                 BC............5800
      GUR=-GUL*UBC(IP)                                                   BC............5900
  360 IF(NOUMAT) 370,370,380                                             BC............6000
  370 UMAT(IMID,JMID)=UMAT(IMID,JMID)-GUL                                BC............6100
  380 UVEC(I)=UVEC(I)+GUR                                                BC............6200
 1000 CONTINUE                                                           BC............6300
C                                                                        BC............6400
C                                                                        BC............6500
 1050 IF(ML-1) 1100,3000,1100                                            BC............6600
 1100 IF(NUBC.EQ.0) GOTO 3000                                            BC............6700
C.....SPECIFIED U BOUNDARY CONDITIONS.                                   BC............6800
C        MODIFY EQUATION FOR U BY ADDING ENERGY/SOLUTE MASS SOURCE       BC............6900
C        AT SPECIFIED U NODE                                             BC............7000
      DO 2500 IU=1,NUBC                                                  BC............7100
      IUP=IU+NPBC                                                        BC............7200
      I=IABS(IUBC(IUP))                                                  BC............7300
      IF (KSOLVP.EQ.0) THEN                                              BC............7400
         IMID = I                                                        BC............7500
      ELSE                                                               BC............7600
         IMID = JA(I)                                                    BC............7700
      END IF                                                             BC............7800
      IF(NOUMAT) 1200,1200,2000                                          BC............7900
 1200 GUINL=-GNUU                                                        BC............8000
      UMAT(IMID,JMID)=UMAT(IMID,JMID)-GUINL                              BC............8100
 2000 GUINR=GNUU*UBC(IUP)                                                BC............8200
 2500 UVEC(I)=UVEC(I)+GUINR                                              BC............8300
C                                                                        BC............8400
 3000 CONTINUE                                                           BC............8500
C                                                                        BC............8600
C                                                                        BC............8700
      RETURN                                                             BC............8800
      END                                                                BC............8900
C                                                                        BC............9000
C     SUBPROGRAM        B  D  I  N  I  T           SUTRA VERSION 2.1     BDINIT.........100
C                                                                        BDINIT.........200
C *** PURPOSE :                                                          BDINIT.........300
C ***  BLOCK-DATA SUBPROGRAM FOR INITIALIZING VARIABLES NAMED IN         BDINIT.........400
C ***  COMMON BLOCKS.                                                    BDINIT.........500
C                                                                        BDINIT.........600
      BLOCK DATA BDINIT                                                  BDINIT.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BDINIT.........800
      CHARACTER*40 SOLNAM(0:10)                                          BDINIT.........900
      CHARACTER*10 SOLWRD(0:10)                                          BDINIT........1000
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      BDINIT........1100
      COMMON /SOLVN/ NSLVRS                                              BDINIT........1200
C.....SET THE NUMBER OF SOLVERS AVAILABLE                                BDINIT........1300
      DATA NSLVRS /4/                                                    BDINIT........1400
C.....DEFINE KEYWORDS AND NAMES FOR SOLVERS                              BDINIT........1500
      DATA (SOLWRD(M),SOLNAM(M),M=0,10) /                                BDINIT........1600
     1   'DIRECT', 'BANDED GAUSSIAN ELIMINATION (DIRECT)',               BDINIT........1700
     2   'CG', 'IC-PRECONDITIONED CONJUGATE GRADIENT',                   BDINIT........1800
     3   'GMRES', 'ILU-PRECONDITIONED GMRES',                            BDINIT........1900
     4   'ORTHOMIN', 'ILU-PRECONDITIONED ORTHOMIN',                      BDINIT........2000
     5   '', '',                                                         BDINIT........2100
     6   '', '',                                                         BDINIT........2200
     7   '', '',                                                         BDINIT........2300
     8   '', '',                                                         BDINIT........2400
     9   '', '',                                                         BDINIT........2500
     T   '', '',                                                         BDINIT........2600
     1   '', ''/                                                         BDINIT........2700
      END                                                                BDINIT........2800
C                                                                        BDINIT........2900
C     SUBROUTINE        B  O  U  N  D              SUTRA VERSION 2.1     BOUND..........100
C                                                                        BOUND..........200
C *** PURPOSE :                                                          BOUND..........300
C ***  TO READ AND ORGANIZE SPECIFIED PRESSURE DATA AND                  BOUND..........400
C ***  SPECIFIED TEMPERATURE OR CONCENTRATION DATA.                      BOUND..........500
C                                                                        BOUND..........600
      SUBROUTINE BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT)                    BOUND..........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BOUND..........800
      CHARACTER INTFIL*1000                                              BOUND..........900
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     BOUND.........1000
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN)                BOUND.........1100
      DIMENSION INERR(10),RLERR(10)                                      BOUND.........1200
      DIMENSION KTYPE(2)                                                 BOUND.........1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BOUND.........1400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BOUND.........1500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BOUND.........1600
     1   NSOP,NSOU,NBCN                                                  BOUND.........1700
      COMMON /FNAMES/ UNAME,FNAME                                        BOUND.........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     BOUND.........1900
C                                                                        BOUND.........2000
C                                                                        BOUND.........2100
      IPBCT=1                                                            BOUND.........2200
      IUBCT=1                                                            BOUND.........2300
      IP=0                                                               BOUND.........2400
      IPU=0                                                              BOUND.........2500
      WRITE(K3,50)                                                       BOUND.........2600
   50 FORMAT('1'////11X,'B O U N D A R Y   C O N D I T I O N S')         BOUND.........2700
      IF(NPBC.EQ.0) GOTO 400                                             BOUND.........2800
      WRITE(K3,100)                                                      BOUND.........2900
  100 FORMAT(//11X,'**** NODES AT WHICH PRESSURES ARE',                  BOUND.........3000
     1   ' SPECIFIED ****'/)                                             BOUND.........3100
      IF(ME) 107,107,114                                                 BOUND.........3200
  107 WRITE(K3,108)                                                      BOUND.........3300
  108 FORMAT(11X,'     (AS WELL AS SOLUTE CONCENTRATION OF ANY'          BOUND.........3400
     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........3500
     2   /16X,' OF SPECIFIED PRESSURE)'//12X,'NODE',18X,'PRESSURE',      BOUND.........3600
     3   13X,'CONCENTRATION'//)                                          BOUND.........3700
      GOTO 125                                                           BOUND.........3800
  114 WRITE(K3,115)                                                      BOUND.........3900
  115 FORMAT(11X,'     (AS WELL AS TEMPERATURE {DEGREES CELSIUS} OF ANY' BOUND.........4000
     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........4100
     2   /16X,' OF SPECIFIED PRESSURE)'//12X,'NODE',18X,                 BOUND.........4200
     2   'PRESSURE',13X,'  TEMPERATURE'//)                               BOUND.........4300
C                                                                        BOUND.........4400
C.....INPUT DATASET 19:  DATA FOR SPECIFIED PRESSURE NODES               BOUND.........4500
  125 IPU=IPU+1                                                          BOUND.........4600
      ERRCOD = 'REA-INP-19'                                              BOUND.........4700
      CALL READIF(K1, INTFIL, ERRCOD)                                    BOUND.........4800
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND.........4900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        BOUND.........5000
      IDUMA = IABS(IDUM)                                                 BOUND.........5100
      IF (IDUM.EQ.0) THEN                                                BOUND.........5200
         GOTO 180                                                        BOUND.........5300
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND.........5400
         ERRCOD = 'INP-19-1'                                             BOUND.........5500
         INERR(1) = IDUMA                                                BOUND.........5600
         INERR(2) = NN                                                   BOUND.........5700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        BOUND.........5800
      ELSE IF (IPU.GT.NPBC) THEN                                         BOUND.........5900
         GOTO 125                                                        BOUND.........6000
      END IF                                                             BOUND.........6100
      IPBC(IPU) = IDUM                                                   BOUND.........6200
      IF (IPBC(IPU).GT.0) THEN                                           BOUND.........6300
         ERRCOD = 'REA-INP-19'                                           BOUND.........6400
         READ(INTFIL,*,IOSTAT=INERR(1)) IPBC(IPU),PBC(IPU),UBC(IPU)      BOUND.........6500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     BOUND.........6600
         WRITE(K3,160) IPBC(IPU),PBC(IPU),UBC(IPU)                       BOUND.........6700
      ELSE IF (IPBC(IPU).LT.0) THEN                                      BOUND.........6800
         IPBCT = -1                                                      BOUND.........6900
         WRITE(K3,160) IPBC(IPU)                                         BOUND.........7000
      ELSE                                                               BOUND.........7100
         GOTO 180                                                        BOUND.........7200
      END IF                                                             BOUND.........7300
  160 FORMAT(7X,I9,6X,1PE20.13,6X,1PE20.13)                              BOUND.........7400
      GOTO 125                                                           BOUND.........7500
  180 IPU=IPU-1                                                          BOUND.........7600
      IP=IPU                                                             BOUND.........7700
      IF(IP.EQ.NPBC) GOTO 200                                            BOUND.........7800
      ERRCOD = 'INP-3,19-1'                                              BOUND.........7900
      INERR(1) = IP                                                      BOUND.........8000
      INERR(2) = NPBC                                                    BOUND.........8100
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           BOUND.........8200
  200 IF(IPBCT.NE.-1) GOTO 400                                           BOUND.........8300
      IF(ME) 205,205,215                                                 BOUND.........8400
  205 WRITE(K3,206)                                                      BOUND.........8500
  206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED PRESSURE'/12X,'OR INFLOW ', BOUND.........8600
     1   'CONCENTRATION INDICATED'/12X,'BY NEGATIVE NODE NUMBER')        BOUND.........8700
      GOTO 400                                                           BOUND.........8800
  215 WRITE(K3,216)                                                      BOUND.........8900
  216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED PRESSURE'/12X,'OR INFLOW ', BOUND.........9000
     1   'TEMPERATURE INDICATED'/12X,'BY NEGATIVE NODE NUMBER')          BOUND.........9100
  400 IF(NUBC.EQ.0) GOTO 6000                                            BOUND.........9200
C                                                                        BOUND.........9300
      IF(ME) 500,500,550                                                 BOUND.........9400
  500 WRITE(K3,1000)                                                     BOUND.........9500
 1000 FORMAT(////11X,'**** NODES AT WHICH SOLUTE CONCENTRATIONS ARE ',   BOUND.........9600
     1   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND.........9700
     2   ' ****'//12X,'NODE',13X,'CONCENTRATION'//)                      BOUND.........9800
      GOTO 1125                                                          BOUND.........9900
  550 WRITE(K3,1001)                                                     BOUND........10000
 1001 FORMAT(////11X,'**** NODES AT WHICH TEMPERATURES ARE ',            BOUND........10100
     1   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND........10200
     2   ' ****'//12X,'NODE',15X,'TEMPERATURE'//)                        BOUND........10300
C                                                                        BOUND........10400
C.....INPUT DATASET 20:  DATA FOR SPECIFIED CONCENTRATION OR             BOUND........10500
C        TEMPERATURE NODES                                               BOUND........10600
 1125 IPU=IPU+1                                                          BOUND........10700
      ERRCOD = 'REA-INP-20'                                              BOUND........10800
      CALL READIF(K1, INTFIL, ERRCOD)                                    BOUND........10900
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND........11000
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        BOUND........11100
      IDUMA = IABS(IDUM)                                                 BOUND........11200
      IF (IDUM.EQ.0) THEN                                                BOUND........11300
         GOTO 1180                                                       BOUND........11400
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND........11500
         ERRCOD = 'INP-20-1'                                             BOUND........11600
         INERR(1) = IDUMA                                                BOUND........11700
         INERR(2) = NN                                                   BOUND........11800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        BOUND........11900
      ELSE IF (IPU.GT.NPBC+NUBC) THEN                                    BOUND........12000
         GOTO 1125                                                       BOUND........12100
      END IF                                                             BOUND........12200
      IUBC(IPU) = IDUM                                                   BOUND........12300
      IF (IUBC(IPU).GT.0) THEN                                           BOUND........12400
         ERRCOD = 'REA-INP-20'                                           BOUND........12500
         READ(INTFIL,*,IOSTAT=INERR(1)) IUBC(IPU),UBC(IPU)               BOUND........12600
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     BOUND........12700
         WRITE(K3,1150) IUBC(IPU),UBC(IPU)                               BOUND........12800
      ELSE IF (IUBC(IPU).LT.0) THEN                                      BOUND........12900
         IUBCT = -1                                                      BOUND........13000
         WRITE(K3,1150) IUBC(IPU)                                        BOUND........13100
      ELSE                                                               BOUND........13200
         GOTO 1180                                                       BOUND........13300
      END IF                                                             BOUND........13400
 1150 FORMAT(11X,I9,6X,1PE20.13)                                         BOUND........13500
      GOTO 1125                                                          BOUND........13600
 1180 IPU=IPU-1                                                          BOUND........13700
      IU=IPU-IP                                                          BOUND........13800
      IF(IU.EQ.NUBC) GOTO 1200                                           BOUND........13900
      IF (ME.EQ.1) THEN                                                  BOUND........14000
         ERRCOD = 'INP-3,20-2'                                           BOUND........14100
      ELSE                                                               BOUND........14200
         ERRCOD = 'INP-3,20-1'                                           BOUND........14300
      END IF                                                             BOUND........14400
      INERR(1) = IU                                                      BOUND........14500
      INERR(2) = NUBC                                                    BOUND........14600
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           BOUND........14700
 1200 IF(IUBCT.NE.-1) GOTO 6000                                          BOUND........14800
      IF(ME) 1205,1205,1215                                              BOUND........14900
 1205 WRITE(K3,1206)                                                     BOUND........15000
 1206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED CONCENTRATION'/12X,'IS ',   BOUND........15100
     1   'INDICATED BY NEGATIVE NODE NUMBER')                            BOUND........15200
      GOTO 6000                                                          BOUND........15300
 1215 WRITE(K3,1216)                                                     BOUND........15400
 1216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED TEMPERATURE'/12X,'IS ',     BOUND........15500
     1   'INDICATED BY NEGATIVE NODE NUMBER')                            BOUND........15600
C                                                                        BOUND........15700
 6000 IF(IPBCT.EQ.-1.OR.IUBCT.EQ.-1) WRITE(K3,7000)                      BOUND........15800
 7000 FORMAT(////11X,'THE SPECIFIED TIME VARIATIONS ARE ',               BOUND........15900
     1   'USER-PROGRAMMED IN SUBROUTINE  B C T I M E .')                 BOUND........16000
C                                                                        BOUND........16100
C                                                                        BOUND........16200
      RETURN                                                             BOUND........16300
      END                                                                BOUND........16400
C                                                                        BOUND........16500
C     SUBROUTINE        B  U  D  G  E  T           SUTRA VERSION 2.1     BUDGET.........100
C                                                                        BUDGET.........200
C *** PURPOSE :                                                          BUDGET.........300
C ***  TO CALCULATE AND OUTPUT FLUID MASS AND SOLUTE MASS OR             BUDGET.........400
C ***  ENERGY BUDGETS.                                                   BUDGET.........500
C                                                                        BUDGET.........600
      SUBROUTINE BUDGET(ML,IBCT,VOL,SW,DSWDP,RHO,SOP,QIN,PVEC,PM1,       BUDGET.........700
     1   DPDTITR,PBC,QPLITR,IPBC,IQSOP,POR,UVEC,UM1,UM2,UIN,QUIN,QINITR, BUDGET.........800
     2   IQSOU,UBC,IUBC,CS1,CS2,CS3,SL,SR,NREG)                          BUDGET.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BUDGET........1000
      CHARACTER*10 ADSMOD                                                BUDGET........1100
      CHARACTER*13 ULABL(2)                                              BUDGET........1200
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),QINITR(NN),         BUDGET........1300
     1   IQSOU(NSOU)                                                     BUDGET........1400
      DIMENSION IPBC(NBCN),IUBC(NBCN),UBC(NBCN),QPLITR(NBCN),PBC(NBCN)   BUDGET........1500
      DIMENSION POR(NN),VOL(NN),PVEC(NNVEC),UVEC(NNVEC),SW(NN),          BUDGET........1600
     1   DSWDP(NN),RHO(NN),SOP(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN),  BUDGET........1700
     2   CS1(NN),CS2(NN),CS3(NN),SL(NN),SR(NN),NREG(NN)                  BUDGET........1800
      DIMENSION KTYPE(2)                                                 BUDGET........1900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BUDGET........2000
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BUDGET........2100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BUDGET........2200
     1   NSOP,NSOU,NBCN                                                  BUDGET........2300
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           BUDGET........2400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     BUDGET........2500
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      BUDGET........2600
      COMMON /MODSOR/ ADSMOD                                             BUDGET........2700
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BUDGET........2800
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BUDGET........2900
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BUDGET........3000
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  BUDGET........3100
      DATA ULABL(1)/'CONCENTRATION'/,ULABL(2)/' TEMPERATURE '/           BUDGET........3200
      SAVE ULABL                                                         BUDGET........3300
C                                                                        BUDGET........3400
C                                                                        BUDGET........3500
      MN=2                                                               BUDGET........3600
      IF(IUNSAT.NE.0) IUNSAT=1                                           BUDGET........3700
      IF(ME.EQ.-1) MN=1                                                  BUDGET........3800
      WRITE(K3,10)                                                       BUDGET........3900
   10 FORMAT('1')                                                        BUDGET........4000
C.....SET UNSATURATED FLOW PARAMETERS, SW(I) AND DSWDP(I)                BUDGET........4100
      IF(IUNSAT-1) 40,20,40                                              BUDGET........4200
   20 DO 30 I=1,NN                                                       BUDGET........4300
      IF(PVEC(I)) 25,27,27                                               BUDGET........4400
   25 CALL UNSAT(SW(I),DSWDP(I),RELK,PVEC(I),NREG(I))                    BUDGET........4500
      GOTO 30                                                            BUDGET........4600
   27 SW(I)=1.0D0                                                        BUDGET........4700
      DSWDP(I)=0.0D0                                                     BUDGET........4800
   30 CONTINUE                                                           BUDGET........4900
C                                                                        BUDGET........5000
C.....CALCULATE COMPONENTS OF FLUID MASS BUDGET                          BUDGET........5100
   40 IF(ML-1) 50,50,1000                                                BUDGET........5200
   50 CONTINUE                                                           BUDGET........5300
      STPPOS = 0D0                                                       BUDGET........5400
      STPNEG = 0D0                                                       BUDGET........5500
      STUPOS = 0D0                                                       BUDGET........5600
      STUNEG = 0D0                                                       BUDGET........5700
      QINPOS = 0D0                                                       BUDGET........5800
      QINNEG = 0D0                                                       BUDGET........5900
      DO 100 I=1,NN                                                      BUDGET........6000
      TERM = (1-ISSFLO/2)*RHO(I)*VOL(I)*                                 BUDGET........6100
     1   (SW(I)*SOP(I)+POR(I)*DSWDP(I))*(PVEC(I)-PM1(I))/DELTP           BUDGET........6200
      STPPOS = STPPOS + MAX(0D0, TERM)                                   BUDGET........6300
      STPNEG = STPNEG + MIN(0D0, TERM)                                   BUDGET........6400
      TERM = (1-ISSFLO/2)*POR(I)*SW(I)*DRWDU*VOL(I)*                     BUDGET........6500
     1   (UM1(I)-UM2(I))/DLTUM1                                          BUDGET........6600
      STUPOS = STUPOS + MAX(0D0, TERM)                                   BUDGET........6700
      STUNEG = STUNEG + MIN(0D0, TERM)                                   BUDGET........6800
      TERM = QIN(I)                                                      BUDGET........6900
      QINPOS = QINPOS + MAX(0D0, TERM)                                   BUDGET........7000
      QINNEG = QINNEG + MIN(0D0, TERM)                                   BUDGET........7100
  100 CONTINUE                                                           BUDGET........7200
      STPTOT = STPPOS + STPNEG                                           BUDGET........7300
      STUTOT = STUPOS + STUNEG                                           BUDGET........7400
      STFPOS = STPPOS + STUPOS                                           BUDGET........7500
      STFNEG = STPNEG + STUNEG                                           BUDGET........7600
      STFTOT = STPTOT + STUTOT                                           BUDGET........7700
      QINTOT = QINPOS + QINNEG                                           BUDGET........7800
C                                                                        BUDGET........7900
      QPLPOS = 0D0                                                       BUDGET........8000
      QPLNEG = 0D0                                                       BUDGET........8100
      DO 200 IP=1,NPBC                                                   BUDGET........8200
      I=IABS(IPBC(IP))                                                   BUDGET........8300
      TERM = GNUP*(PBC(IP)-PVEC(I))                                      BUDGET........8400
      QPLPOS = QPLPOS + MAX(0D0, TERM)                                   BUDGET........8500
      QPLNEG = QPLNEG + MIN(0D0, TERM)                                   BUDGET........8600
  200 CONTINUE                                                           BUDGET........8700
      QPLTOT = QPLPOS + QPLNEG                                           BUDGET........8800
      QFFPOS = QINPOS + QPLPOS                                           BUDGET........8900
      QFFNEG = QINNEG + QPLNEG                                           BUDGET........9000
      QFFTOT = QINTOT + QPLTOT                                           BUDGET........9100
C                                                                        BUDGET........9200
C.....OUTPUT FLUID MASS BUDGET                                           BUDGET........9300
      ACTFMB = 5D-1*(STFPOS - STFNEG + QFFPOS - QFFNEG)                  BUDGET........9400
      ERFMBA = STFTOT - QFFTOT                                           BUDGET........9500
      WRITE(K3,300) IT,STPPOS,STPNEG,STPTOT,                             BUDGET........9600
     1   ULABL(MN),STUPOS,STUNEG,STUTOT,STFPOS,STFNEG,STFTOT,            BUDGET........9700
     2   QINPOS,QINNEG,QINTOT,QPLPOS,QPLNEG,QPLTOT,                      BUDGET........9800
     3   QFFPOS,QFFNEG,QFFTOT,ACTFMB,ERFMBA                              BUDGET........9900
  300 FORMAT(//11X,'F L U I D   M A S S   B U D G E T      AFTER TIME',  BUDGET.......10000
     1   ' STEP ',I8,',     IN (MASS/SECOND)'                            BUDGET.......10100
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'INCREASES(+)',4X,    BUDGET.......10200
     3   'DECREASES(-)',7X,'CHANGE'/84X,3(2X,14('='))                    BUDGET.......10300
     4   /13X,'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO PRESSURE',    BUDGET.......10400
     5   ' CHANGE',12X,3(1X,1PE15.7)                                     BUDGET.......10500
     6   /13X,'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO ',A13,        BUDGET.......10600
     7   ' CHANGE',7X,3(1X,1PE15.7)/84X,3(2X,14('-'))                    BUDGET.......10700
     8   /13X,'TOTAL RATE OF CHANGE IN STORED FLUID [ S+, S-, S ]',      BUDGET.......10800
     9   21X,3(1X,1PE15.7)                                               BUDGET.......10900
     T   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/89X,'GAINS(+)',7X,        BUDGET.......11000
     1   'LOSSES(-)',7X,'GAIN/LOSS'/84X,3(2X,14('='))                    BUDGET.......11100
     2   /13X,'GAIN/LOSS OF FLUID THROUGH FLUID SOURCES AND SINKS',      BUDGET.......11200
     3   21X,3(1X,1PE15.7)                                               BUDGET.......11300
     4   /13X,'GAIN/LOSS OF FLUID THROUGH INFLOWS/OUTFLOWS AT'           BUDGET.......11400
     5   ' SPECIFIED P NODES',7X,3(1X,1PE15.7)/84X,3(2X,14('-'))         BUDGET.......11500
     6   /13X,'TOTAL RATE OF GAIN/LOSS OF FLUID THROUGH FLOWS',          BUDGET.......11600
     7   ' [ F+, F-, F ]',11X,3(1X,1PE15.7)                              BUDGET.......11700
     8   ///13X,'FLUID MASS BALANCE ACTIVITY',                           BUDGET.......11800
     9   ' [ A = ((S+) - (S-) + (F+) - (F-))/2 ]',14X,1PE15.7            BUDGET.......11900
     T   /13X,'ABSOLUTE FLUID MASS BALANCE ERROR [ S - F ]',36X,1PE15.7) BUDGET.......12000
      IF (ACTFMB.NE.0D0) THEN                                            BUDGET.......12100
         ERFMBR = 1D2*ERFMBA/ACTFMB                                      BUDGET.......12200
         WRITE(K3,301) ERFMBR                                            BUDGET.......12300
      ELSE                                                               BUDGET.......12400
         WRITE(K3,302)                                                   BUDGET.......12500
      END IF                                                             BUDGET.......12600
  301 FORMAT(13X,'RELATIVE FLUID MASS BALANCE ERROR',                    BUDGET.......12700
     1   ' [ 100*(S - F)/A ]',28X,1PE15.7,' (PERCENT)')                  BUDGET.......12800
  302 FORMAT(13X,'RELATIVE FLUID MASS BALANCE ERROR',                    BUDGET.......12900
     1   ' [ 100*(S - F)/A ]',28X,'  UNDEFINED')                         BUDGET.......13000
C                                                                        BUDGET.......13100
      IF(IBCT.EQ.4) GOTO 600                                             BUDGET.......13200
      NSOPI=NSOP-1                                                       BUDGET.......13300
      INEGCT=0                                                           BUDGET.......13400
      DO 500 IQP=1,NSOPI                                                 BUDGET.......13500
      I=IQSOP(IQP)                                                       BUDGET.......13600
      IF(I) 325,500,500                                                  BUDGET.......13700
  325 INEGCT=INEGCT+1                                                    BUDGET.......13800
      IF(INEGCT.EQ.1) WRITE(K3,350)                                      BUDGET.......13900
  350 FORMAT(///22X,'TIME-DEPENDENT FLUID SOURCES OR SINKS'//22X,        BUDGET.......14000
     1   ' NODE',5X,'INFLOW(+)/OUTFLOW(-)'/37X,'  (MASS/SECOND)'//)      BUDGET.......14100
      WRITE(K3,450) -I,QIN(-I)                                           BUDGET.......14200
  450 FORMAT(18X,I9,10X,1PE15.7)                                         BUDGET.......14300
  500 CONTINUE                                                           BUDGET.......14400
C                                                                        BUDGET.......14500
  600 IF(NPBC.EQ.0) GOTO 800                                             BUDGET.......14600
      WRITE(K3,650)                                                      BUDGET.......14700
  650 FORMAT(///22X,'FLUID SOURCES OR SINKS DUE TO SPECIFIED PRESSURES', BUDGET.......14800
     1   //22X,' NODE',5X,'INFLOW(+)/OUTFLOW(-)'/37X,'  (MASS/SECOND)'/) BUDGET.......14900
      DO 700 IP=1,NPBC                                                   BUDGET.......15000
      I=IABS(IPBC(IP))                                                   BUDGET.......15100
      WRITE(K3,450) I, GNUP*(PBC(IP)-PVEC(I))                            BUDGET.......15200
  700 CONTINUE                                                           BUDGET.......15300
C                                                                        BUDGET.......15400
C.....CALCULATE COMPONENTS OF ENERGY OR SOLUTE MASS BUDGET               BUDGET.......15500
  800 IF(ML-1) 1000,5500,1000                                            BUDGET.......15600
 1000 CONTINUE                                                           BUDGET.......15700
      FLDPOS = 0D0                                                       BUDGET.......15800
      FLDNEG = 0D0                                                       BUDGET.......15900
      SLDPOS = 0D0                                                       BUDGET.......16000
      SLDNEG = 0D0                                                       BUDGET.......16100
      DNSPOS = 0D0                                                       BUDGET.......16200
      DNSNEG = 0D0                                                       BUDGET.......16300
      P1FPOS = 0D0                                                       BUDGET.......16400
      P1FNEG = 0D0                                                       BUDGET.......16500
      P1SPOS = 0D0                                                       BUDGET.......16600
      P1SNEG = 0D0                                                       BUDGET.......16700
      P0FPOS = 0D0                                                       BUDGET.......16800
      P0FNEG = 0D0                                                       BUDGET.......16900
      P0SPOS = 0D0                                                       BUDGET.......17000
      P0SNEG = 0D0                                                       BUDGET.......17100
      QQUPOS = 0D0                                                       BUDGET.......17200
      QQUNEG = 0D0                                                       BUDGET.......17300
      QIUPOS = 0D0                                                       BUDGET.......17400
      QIUNEG = 0D0                                                       BUDGET.......17500
C.....SET ADSORPTION PARAMETERS                                          BUDGET.......17600
      IF(ME.EQ.-1.AND.ADSMOD.NE.'NONE      ')                            BUDGET.......17700
     1   CALL ADSORB(CS1,CS2,CS3,SL,SR,UVEC)                             BUDGET.......17800
      DO 1300 I=1,NN                                                     BUDGET.......17900
      ESRV=POR(I)*SW(I)*RHO(I)*VOL(I)                                    BUDGET.......18000
      EPRSV=(1.D0-POR(I))*RHOS*VOL(I)                                    BUDGET.......18100
      DUDT=(1-ISSTRA)*(UVEC(I)-UM1(I))/DELTU                             BUDGET.......18200
      TERM = ESRV*CW*DUDT                                                BUDGET.......18300
      FLDPOS = FLDPOS + MAX(0D0, TERM)                                   BUDGET.......18400
      FLDNEG = FLDNEG + MIN(0D0, TERM)                                   BUDGET.......18500
      TERM = EPRSV*CS1(I)*DUDT                                           BUDGET.......18600
      SLDPOS = SLDPOS + MAX(0D0, TERM)                                   BUDGET.......18700
      SLDNEG = SLDNEG + MIN(0D0, TERM)                                   BUDGET.......18800
      TERM = CW*UVEC(I)*(1-ISSFLO/2)*VOL(I)*                             BUDGET.......18900
     1   (RHO(I)*(SW(I)*SOP(I)+POR(I)*DSWDP(I))*DPDTITR(I)               BUDGET.......19000
     2   +POR(I)*SW(I)*DRWDU*(UM1(I)-UM2(I))/DLTUM1)                     BUDGET.......19100
      DNSPOS = DNSPOS + MAX(0D0, TERM)                                   BUDGET.......19200
      DNSNEG = DNSNEG + MIN(0D0, TERM)                                   BUDGET.......19300
      TERM = ESRV*PRODF1*UVEC(I)                                         BUDGET.......19400
      P1FPOS = P1FPOS + MAX(0D0, TERM)                                   BUDGET.......19500
      P1FNEG = P1FNEG + MIN(0D0, TERM)                                   BUDGET.......19600
      TERM = EPRSV*PRODS1*(SL(I)*UVEC(I)+SR(I))                          BUDGET.......19700
      P1SPOS = P1SPOS + MAX(0D0, TERM)                                   BUDGET.......19800
      P1SNEG = P1SNEG + MIN(0D0, TERM)                                   BUDGET.......19900
      TERM = ESRV*PRODF0                                                 BUDGET.......20000
      P0FPOS = P0FPOS + MAX(0D0, TERM)                                   BUDGET.......20100
      P0FNEG = P0FNEG + MIN(0D0, TERM)                                   BUDGET.......20200
      TERM = EPRSV*PRODS0                                                BUDGET.......20300
      P0SPOS = P0SPOS + MAX(0D0, TERM)                                   BUDGET.......20400
      P0SNEG = P0SNEG + MIN(0D0, TERM)                                   BUDGET.......20500
      TERM = QUIN(I)                                                     BUDGET.......20600
      QQUPOS = QQUPOS + MAX(0D0, TERM)                                   BUDGET.......20700
      QQUNEG = QQUNEG + MIN(0D0, TERM)                                   BUDGET.......20800
      IF (QINITR(I).LE.0D0) THEN                                         BUDGET.......20900
         TERM = QINITR(I)*CW*UVEC(I)                                     BUDGET.......21000
      ELSE                                                               BUDGET.......21100
         TERM = QINITR(I)*CW*UIN(I)                                      BUDGET.......21200
      END IF                                                             BUDGET.......21300
      QIUPOS = QIUPOS + MAX(0D0, TERM)                                   BUDGET.......21400
      QIUNEG = QIUNEG + MIN(0D0, TERM)                                   BUDGET.......21500
 1300 CONTINUE                                                           BUDGET.......21600
      FLDTOT = FLDPOS + FLDNEG                                           BUDGET.......21700
      SLDTOT = SLDPOS + SLDNEG                                           BUDGET.......21800
      DNSTOT = DNSPOS + DNSNEG                                           BUDGET.......21900
      STSPOS = FLDPOS + SLDPOS + DNSPOS                                  BUDGET.......22000
      STSNEG = FLDNEG + SLDNEG + DNSNEG                                  BUDGET.......22100
      STSTOT = FLDTOT + SLDTOT + DNSTOT                                  BUDGET.......22200
      P1FTOT = P1FPOS + P1FNEG                                           BUDGET.......22300
      P1STOT = P1SPOS + P1SNEG                                           BUDGET.......22400
      P0FTOT = P0FPOS + P0FNEG                                           BUDGET.......22500
      P0STOT = P0SPOS + P0SNEG                                           BUDGET.......22600
      PRSPOS = P1FPOS + P1SPOS + P0FPOS + P0SPOS                         BUDGET.......22700
      PRSNEG = P1FNEG + P1SNEG + P0FNEG + P0SNEG                         BUDGET.......22800
      PRSTOT = P1FTOT + P1STOT + P0FTOT + P0STOT                         BUDGET.......22900
      QQUTOT = QQUPOS + QQUNEG                                           BUDGET.......23000
      QIUTOT = QIUPOS + QIUNEG                                           BUDGET.......23100
C                                                                        BUDGET.......23200
      QPUPOS = 0D0                                                       BUDGET.......23300
      QPUNEG = 0D0                                                       BUDGET.......23400
      DO 1500 IP=1,NPBC                                                  BUDGET.......23500
      IF (QPLITR(IP).LE.0D0) THEN                                        BUDGET.......23600
         I=IABS(IPBC(IP))                                                BUDGET.......23700
         TERM = QPLITR(IP)*CW*UVEC(I)                                    BUDGET.......23800
      ELSE                                                               BUDGET.......23900
         TERM = QPLITR(IP)*CW*UBC(IP)                                    BUDGET.......24000
      END IF                                                             BUDGET.......24100
      QPUPOS = QPUPOS + MAX(0D0, TERM)                                   BUDGET.......24200
      QPUNEG = QPUNEG + MIN(0D0, TERM)                                   BUDGET.......24300
 1500 CONTINUE                                                           BUDGET.......24400
      QPUTOT = QPUPOS + QPUNEG                                           BUDGET.......24500
C                                                                        BUDGET.......24600
      QULPOS = 0D0                                                       BUDGET.......24700
      QULNEG = 0D0                                                       BUDGET.......24800
      QULTOT = 0D0                                                       BUDGET.......24900
      IF(NUBC.EQ.0) GOTO 1520                                            BUDGET.......25000
      DO 1510 IU=1,NUBC                                                  BUDGET.......25100
      IUP=IU+NPBC                                                        BUDGET.......25200
      I=IABS(IUBC(IUP))                                                  BUDGET.......25300
      QPLITR(IUP)=GNUU*(UBC(IUP)-UVEC(I))                                BUDGET.......25400
      TERM = QPLITR(IUP)                                                 BUDGET.......25500
      QULPOS = QULPOS + MAX(0D0, TERM)                                   BUDGET.......25600
      QULNEG = QULNEG + MIN(0D0, TERM)                                   BUDGET.......25700
 1510 CONTINUE                                                           BUDGET.......25800
 1520 QULTOT = QULPOS + QULNEG                                           BUDGET.......25900
      QFSPOS = QIUPOS + QPUPOS + QQUPOS + QULPOS                         BUDGET.......26000
      QFSNEG = QIUNEG + QPUNEG + QQUNEG + QULNEG                         BUDGET.......26100
      QFSTOT = QIUTOT + QPUTOT + QQUTOT + QULTOT                         BUDGET.......26200
C                                                                        BUDGET.......26300
 1540 IF(ME) 1550,1550,1615                                              BUDGET.......26400
C                                                                        BUDGET.......26500
C.....OUTPUT SOLUTE MASS BUDGET                                          BUDGET.......26600
 1550 ACTSMB = 5D-1*(STSPOS - STSNEG + PRSPOS - PRSNEG                   BUDGET.......26700
     1   + QFSPOS - QFSNEG)                                              BUDGET.......26800
      ERSMBA = STSTOT - PRSTOT - QFSTOT                                  BUDGET.......26900
      WRITE(K3,1600) IT,FLDPOS,FLDNEG,FLDTOT,SLDPOS,SLDNEG,SLDTOT,       BUDGET.......27000
     1   DNSPOS,DNSNEG,DNSTOT,STSPOS,STSNEG,STSTOT,                      BUDGET.......27100
     2   P1FPOS,P1FNEG,P1FTOT,P1SPOS,P1SNEG,P1STOT,                      BUDGET.......27200
     3   P0FPOS,P0FNEG,P0FTOT,P0SPOS,P0SNEG,P0STOT,PRSPOS,PRSNEG,PRSTOT, BUDGET.......27300
     4   QIUPOS,QIUNEG,QIUTOT,QPUPOS,QPUNEG,QPUTOT,                      BUDGET.......27400
     5   QQUPOS,QQUNEG,QQUTOT,QULPOS,QULNEG,QULTOT,QFSPOS,QFSNEG,QFSTOT, BUDGET.......27500
     6   ACTSMB,ERSMBA                                                   BUDGET.......27600
 1600 FORMAT(//11X,'S O L U T E   B U D G E T      AFTER TIME STEP ',I8, BUDGET.......27700
     1   ',   IN (SOLUTE MASS/SECOND)'                                   BUDGET.......27800
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'INCREASES(+)',4X,    BUDGET.......27900
     3   'DECREASES(-)',7X,'CHANGE'/84X,3(2X,14('='))                    BUDGET.......28000
     4   /13X,'RATE OF CHANGE IN SOLUTE DUE TO CONCENTRATION CHANGE',    BUDGET.......28100
     5   19X,3(1X,1PE15.7)                                               BUDGET.......28200
     6   /13X,'RATE OF CHANGE OF ADSORBATE',44X,3(1X,1PE15.7)            BUDGET.......28300
     7   /13X,'RATE OF CHANGE IN SOLUTE DUE TO CHANGE IN MASS OF FLUID', BUDGET.......28400
     8   16X,3(1X,1PE15.7)/84X,3(2X,14('-'))                             BUDGET.......28500
     9   /13X,'TOTAL RATE OF CHANGE OF SOLUTE [ S+, S-, S ]',            BUDGET.......28600
     T   27X,3(1X,1PE15.7)                                               BUDGET.......28700
     1   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'PRODUCTION(+)',5X,   BUDGET.......28800
     2   'DECAY(-)',7X,'PROD./DECAY'/84X,3(2X,14('='))                   BUDGET.......28900
     3   /13X,'FIRST-ORDER PRODUCTION/DECAY OF SOLUTE',33X,3(1X,1PE15.7) BUDGET.......29000
     4   /13X,'FIRST-ORDER PRODUCTION/DECAY OF ADSORBATE',               BUDGET.......29100
     5   30X,3(1X,1PE15.7)                                               BUDGET.......29200
     6   /13X,'ZERO-ORDER PRODUCTION/DECAY OF SOLUTE',34X,3(1X,1PE15.7)  BUDGET.......29300
     7   /13X,'ZERO-ORDER PRODUCTION/DECAY OF ADSORBATE',                BUDGET.......29400
     8   31X,3(1X,1PE15.7)/84X,3(2X,14('-'))                             BUDGET.......29500
     9   /13X,'TOTAL RATE OF PRODUCTION/DECAY OF SOLUTE AND ADSORBATE',  BUDGET.......29600
     T   ' [ P+, P-, P ]',3X,3(1X,1PE15.7)                               BUDGET.......29700
     1   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/89X,'GAINS(+)',7X,        BUDGET.......29800
     2   'LOSSES(-)',7X,'GAIN/LOSS'/84X,3(2X,14('='))                    BUDGET.......29900
     3   /13X,'GAIN/LOSS OF SOLUTE THROUGH FLUID SOURCES AND SINKS',     BUDGET.......30000
     4   20X,3(1X,1PE15.7)                                               BUDGET.......30100
     5   /13X,'GAIN/LOSS OF SOLUTE THROUGH INFLOWS/OUTFLOWS AT'          BUDGET.......30200
     6   ' SPECIFIED P NODES',6X,3(1X,1PE15.7)                           BUDGET.......30300
     7   /13X,'GAIN/LOSS OF SOLUTE THROUGH SOLUTE SOURCES AND SINKS',    BUDGET.......30400
     8   19X,3(1X,1PE15.7)                                               BUDGET.......30500
     9   /13X,'GAIN/LOSS OF SOLUTE AT SPECIFIED CONCENTRATION NODES',    BUDGET.......30600
     T   19X,3(1X,1PE15.7)/84X,3(2X,14('-'))                             BUDGET.......30700
     1   /13X,'TOTAL RATE OF GAIN/LOSS OF SOLUTE',38X,3(1X,1PE15.7)      BUDGET.......30800
     2   /16X,' THROUGH FLOWS & SOURCES/SINKS [ F+, F-, F ]'             BUDGET.......30900
     3   ///13X,'SOLUTE MASS BAL. ACTIVITY [ A = ((S+) - (S-)',          BUDGET.......31000
     4   ' + (P+) - (P-) + (F+) - (F-))/2 ]',2X,1PE15.7                  BUDGET.......31100
     5   /13X,'ABSOLUTE SOLUTE MASS BALANCE ERROR [ S - P - F ]',        BUDGET.......31200
     6   31X,1PE15.7)                                                    BUDGET.......31300
      IF (ACTSMB.NE.0D0) THEN                                            BUDGET.......31400
         ERSMBR = 1D2*ERSMBA/ACTSMB                                      BUDGET.......31500
         WRITE(K3,1601) ERSMBR                                           BUDGET.......31600
      ELSE                                                               BUDGET.......31700
         WRITE(K3,1602)                                                  BUDGET.......31800
      END IF                                                             BUDGET.......31900
 1601 FORMAT(13X,'RELATIVE SOLUTE MASS BALANCE ERROR',                   BUDGET.......32000
     1   ' [ 100*(S - P - F)/A ]',23X,1PE15.7,' (PERCENT)')              BUDGET.......32100
 1602 FORMAT(13X,'RELATIVE SOLUTE MASS BALANCE ERROR',                   BUDGET.......32200
     1   ' [ 100*(S - P - F)/A ]',23X,'  UNDEFINED')                     BUDGET.......32300
      GOTO 1645                                                          BUDGET.......32400
C                                                                        BUDGET.......32500
C.....OUTPUT ENERGY BUDGET                                               BUDGET.......32600
 1615 ACTSMB = 5D-1*(STSPOS - STSNEG + PRSPOS - PRSNEG                   BUDGET.......32700
     1   + QFSPOS - QFSNEG)                                              BUDGET.......32800
      ERSMBA = STSTOT - PRSTOT - QFSTOT                                  BUDGET.......32900
      WRITE(K3,1635) IT,FLDPOS,FLDNEG,FLDTOT,SLDPOS,SLDNEG,SLDTOT,       BUDGET.......33000
     1   DNSPOS,DNSNEG,DNSTOT,STSPOS,STSNEG,STSTOT,                      BUDGET.......33100
     2   P0FPOS,P0FNEG,P0FTOT,P0SPOS,P0SNEG,P0STOT,PRSPOS,PRSNEG,PRSTOT, BUDGET.......33200
     3   QIUPOS,QIUNEG,QIUTOT,QPUPOS,QPUNEG,QPUTOT,                      BUDGET.......33300
     4   QQUPOS,QQUNEG,QQUTOT,QULPOS,QULNEG,QULTOT,QFSPOS,QFSNEG,QFSTOT, BUDGET.......33400
     5   ACTSMB,ERSMBA                                                   BUDGET.......33500
 1635 FORMAT(//11X,'E N E R G Y   B U D G E T      AFTER TIME STEP ',I8, BUDGET.......33600
     1   ',   IN (ENERGY/SECOND)'                                        BUDGET.......33700
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'INCREASES(+)',4X,    BUDGET.......33800
     3   'DECREASES(-)',7X,'CHANGE'/84X,3(2X,14('='))                    BUDGET.......33900
     4   /13X,'RATE OF CHANGE OF ENERGY IN FLUID DUE TO TEMPERATURE',    BUDGET.......34000
     5   ' CHANGE',12X,3(1X,1PE15.7)                                     BUDGET.......34100
     6   /13X,'RATE OF CHANGE OF ENERGY IN SOLID GRAINS',                BUDGET.......34200
     7   31X,3(1X,1PE15.7)                                               BUDGET.......34300
     8   /13X,'RATE OF CHANGE OF ENERGY DUE TO CHANGE IN MASS OF FLUID', BUDGET.......34400
     9   16X,3(1X,1PE15.7)/84X,3(2X,14('-'))                             BUDGET.......34500
     T   /13X,'TOTAL RATE OF CHANGE OF ENERGY [ S+, S-, S ]',            BUDGET.......34600
     1   27X,3(1X,1PE15.7)                                               BUDGET.......34700
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'PRODUCTION(+)',5X,   BUDGET.......34800
     3   'DECAY(-)',7X,'PROD./DECAY'/84X,3(2X,14('='))                   BUDGET.......34900
     4   /13X,'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN FLUID',          BUDGET.......35000
     5   25X,3(1X,1PE15.7)                                               BUDGET.......35100
     6   /13X,'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN SOLID GRAINS',   BUDGET.......35200
     7   18X,3(1X,1PE15.7)/84X,3(2X,14('-'))                             BUDGET.......35300
     8   /13X,'TOTAL RATE OF PRODUCTION/DECAY OF ENERGY',                BUDGET.......35400
     9   ' [ P+, P-, P ]',17X,3(1X,1PE15.7)                              BUDGET.......35500
     T   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/89X,'GAINS(+)',7X,        BUDGET.......35600
     1   'LOSSES(-)',7X,'GAIN/LOSS'/84X,3(2X,14('='))                    BUDGET.......35700
     2   /13X,'GAIN/LOSS OF ENERGY THROUGH FLUID SOURCES AND SINKS',     BUDGET.......35800
     3   20X,3(1X,1PE15.7)                                               BUDGET.......35900
     4   /13X,'GAIN/LOSS OF ENERGY THROUGH INFLOWS/OUTFLOWS AT'          BUDGET.......36000
     5   ' SPECIFIED P NODES',6X,3(1X,1PE15.7)                           BUDGET.......36100
     6   /13X,'GAIN/LOSS OF ENERGY THROUGH ENERGY SOURCES AND SINKS',    BUDGET.......36200
     7   19X,3(1X,1PE15.7)                                               BUDGET.......36300
     8   /13X,'GAIN/LOSS OF ENERGY AT SPECIFIED TEMPERATURE NODES',      BUDGET.......36400
     9   21X,3(1X,1PE15.7)/84X,3(2X,14('-'))                             BUDGET.......36500
     T   /13X,'TOTAL RATE OF GAIN/LOSS OF ENERGY',38X,3(1X,1PE15.7)      BUDGET.......36600
     1   /16X,' THROUGH FLOWS & SOURCES/SINKS [ F+, F-, F ]'             BUDGET.......36700
     2   ///13X,'ENERGY BALANCE ACTIVITY [ A = ((S+) - (S-)',            BUDGET.......36800
     3   ' + (P+) - (P-) + (F+) - (F-))/2 ]',4X,1PE15.7                  BUDGET.......36900
     4   /13X,'ABSOLUTE ENERGY BALANCE ERROR [ S - P - F ]',             BUDGET.......37000
     5   36X,1PE15.7)                                                    BUDGET.......37100
      IF (ACTSMB.NE.0D0) THEN                                            BUDGET.......37200
         ERSMBR = 1D2*ERSMBA/ACTSMB                                      BUDGET.......37300
         WRITE(K3,1641) ERSMBR                                           BUDGET.......37400
      ELSE                                                               BUDGET.......37500
         WRITE(K3,1642)                                                  BUDGET.......37600
      END IF                                                             BUDGET.......37700
 1641 FORMAT(13X,'RELATIVE ENERGY BALANCE ERROR',                        BUDGET.......37800
     1   ' [ 100*(S - P - F)/A ]',28X,1PE15.7,' (PERCENT)')              BUDGET.......37900
 1642 FORMAT(13X,'RELATIVE ENERGY BALANCE ERROR',                        BUDGET.......38000
     1   ' [ 100*(S - P - F)/A ]',28X,'  UNDEFINED')                     BUDGET.......38100
C                                                                        BUDGET.......38200
 1645 IF ((IT.EQ.1).AND.(ITER.EQ.1).AND.(ISSTRA.NE.1)) WRITE(K3,1646)    BUDGET.......38300
 1646 FORMAT(/13X,'******** NOTE: ON THE FIRST ITERATION OF THE ',       BUDGET.......38400
     1   'FIRST TIME STEP, A LARGE RELATIVE ERROR IN THE  ********'      BUDGET.......38500
     2   /13X,'******** SOLUTE MASS OR ENERGY BUDGET DOES NOT ',         BUDGET.......38600
     3   'NECESSARILY INDICATE AN INACCURATE TRANSPORT  ********'        BUDGET.......38700
     4   /13X,'******** SOLUTION. THE BUDGET CALCULATION WILL ',         BUDGET.......38800
     5   'NOT YIELD A MEANINGFUL RESULT UNLESS THE      ********'        BUDGET.......38900
     6   /13X,'******** INITIAL CONDITIONS REPRESENT MUTUALLY ',         BUDGET.......39000
     7   'CONSISTENT SOLUTIONS FOR FLOW AND TRANSPORT   ********'        BUDGET.......39100
     8   /13X,'******** FROM A PREVIOUS SUTRA SIMULATION THAT ',         BUDGET.......39200
     9   'ARE ALSO CONSISTENT WITH THE PRESENT SOURCES  ********'        BUDGET.......39300
     T   /13X,'******** AND BOUNDARY CONDITIONS.',60X,'********')        BUDGET.......39400
C                                                                        BUDGET.......39500
      NSOPI=NSOP-1                                                       BUDGET.......39600
      IF(NSOPI.EQ.0) GOTO 2000                                           BUDGET.......39700
      IF(ME) 1649,1649,1659                                              BUDGET.......39800
 1649 WRITE(K3,1650)                                                     BUDGET.......39900
 1650 FORMAT(///22X,'SOLUTE SOURCES OR SINKS AT FLUID SOURCES AND ',     BUDGET.......40000
     1   'SINKS'//22X,' NODE',8X,'SOURCE(+)/SINK(-)'/32X,                BUDGET.......40100
     2   '(SOLUTE MASS/SECOND)'/)                                        BUDGET.......40200
      GOTO 1680                                                          BUDGET.......40300
 1659 WRITE(K3,1660)                                                     BUDGET.......40400
 1660 FORMAT(///22X,'ENERGY SOURCES OR SINKS AT FLUID SOURCES AND ',     BUDGET.......40500
     1   'SINKS'//22X,' NODE',8X,'SOURCE(+)/SINK(-)'/37X,                BUDGET.......40600
     2   '(ENERGY/SECOND)'/)                                             BUDGET.......40700
 1680 DO 1900 IQP=1,NSOPI                                                BUDGET.......40800
      I=IABS(IQSOP(IQP))                                                 BUDGET.......40900
      IF(QINITR(I)) 1700,1700,1750                                       BUDGET.......41000
 1700 QU=QINITR(I)*CW*UVEC(I)                                            BUDGET.......41100
      GOTO 1800                                                          BUDGET.......41200
 1750 QU=QINITR(I)*CW*UIN(I)                                             BUDGET.......41300
 1800 WRITE(K3,450) I,QU                                                 BUDGET.......41400
 1900 CONTINUE                                                           BUDGET.......41500
C                                                                        BUDGET.......41600
 2000 IF(NPBC.EQ.0) GOTO 4500                                            BUDGET.......41700
      IF(ME) 2090,2090,2150                                              BUDGET.......41800
 2090 WRITE(K3,2100)                                                     BUDGET.......41900
 2100 FORMAT(///22X,'SOLUTE SOURCES OR SINKS DUE TO FLUID INFLOWS OR ',  BUDGET.......42000
     1   'OUTFLOWS AT POINTS OF SPECIFIED PRESSURE'//22X,' NODE',8X,     BUDGET.......42100
     2   'SOURCE(+)/SINK(-)'/32X,'(SOLUTE MASS/SECOND)'/)                BUDGET.......42200
      GOTO 2190                                                          BUDGET.......42300
 2150 WRITE(K3,2160)                                                     BUDGET.......42400
 2160 FORMAT(///22X,'ENERGY SOURCES OR SINKS DUE TO FLUID INFLOWS OR ',  BUDGET.......42500
     1   'OUTFLOWS AT POINTS OF SPECIFIED PRESSURE'//22X,' NODE',8X,     BUDGET.......42600
     2   'SOURCE(+)/SINK(-)'/37X,'(ENERGY/SECOND)'/)                     BUDGET.......42700
 2190 DO 2400 IP=1,NPBC                                                  BUDGET.......42800
      I=IABS(IPBC(IP))                                                   BUDGET.......42900
      IF(QPLITR(IP)) 2200,2200,2250                                      BUDGET.......43000
 2200 QPU=QPLITR(IP)*CW*UVEC(I)                                          BUDGET.......43100
      GOTO 2300                                                          BUDGET.......43200
 2250 QPU=QPLITR(IP)*CW*UBC(IP)                                          BUDGET.......43300
 2300 WRITE(K3,450) I,QPU                                                BUDGET.......43400
 2400 CONTINUE                                                           BUDGET.......43500
C                                                                        BUDGET.......43600
      IF(IBCT.EQ.4) GOTO 4500                                            BUDGET.......43700
      NSOUI=NSOU-1                                                       BUDGET.......43800
      INEGCT=0                                                           BUDGET.......43900
      DO 3500 IQU=1,NSOUI                                                BUDGET.......44000
      I=IQSOU(IQU)                                                       BUDGET.......44100
      IF(I) 3400,3500,3500                                               BUDGET.......44200
 3400 INEGCT=INEGCT+1                                                    BUDGET.......44300
      IF(ME) 3450,3450,3460                                              BUDGET.......44400
 3450 IF(INEGCT.EQ.1) WRITE(K3,3455)                                     BUDGET.......44500
 3455 FORMAT(///22X,'TIME-DEPENDENT SOLUTE SOURCES AND SINKS'//22X,      BUDGET.......44600
     1   ' NODE',10X,'GAIN(+)/LOSS(-)'/30X,'  (SOLUTE MASS/SECOND)'//)   BUDGET.......44700
      GOTO 3475                                                          BUDGET.......44800
 3460 IF(INEGCT.EQ.1) WRITE(K3,3465)                                     BUDGET.......44900
 3465 FORMAT(///22X,'TIME-DEPENDENT ENERGY SOURCES AND SINKS'//22X,      BUDGET.......45000
     1   ' NODE',10X,'GAIN(+)/LOSS(-)'/35X,'  (ENERGY/SECOND)'//)        BUDGET.......45100
 3475 CONTINUE                                                           BUDGET.......45200
      WRITE(K3,3490) -I,QUIN(-I)                                         BUDGET.......45300
 3490 FORMAT(22X,I9,10X,1PE15.7)                                         BUDGET.......45400
 3500 CONTINUE                                                           BUDGET.......45500
C                                                                        BUDGET.......45600
 4500 IF(NUBC.EQ.0) GOTO 5500                                            BUDGET.......45700
      IF(ME) 4600,4600,4655                                              BUDGET.......45800
 4600 WRITE(K3,4650)                                                     BUDGET.......45900
 4650 FORMAT(///22X,'SOLUTE SOURCES OR SINKS DUE TO SPECIFIED ',         BUDGET.......46000
     1   'CONCENTRATIONS'//22X,' NODE',10X,'GAIN(+)/LOSS(-)'/30X,        BUDGET.......46100
     2   '  (SOLUTE MASS/SECOND)'/)                                      BUDGET.......46200
      GOTO 4690                                                          BUDGET.......46300
 4655 WRITE(K3,4660)                                                     BUDGET.......46400
 4660 FORMAT(///22X,'ENERGY SOURCES OR SINKS DUE TO SPECIFIED ',         BUDGET.......46500
     1   'TEMPERATURES'//22X,' NODE',10X,'GAIN(+)/LOSS(-)'/35X,          BUDGET.......46600
     2   '  (ENERGY/SECOND)'/)                                           BUDGET.......46700
 4690 CONTINUE                                                           BUDGET.......46800
      DO 4700 IU=1,NUBC                                                  BUDGET.......46900
      IUP=IU+NPBC                                                        BUDGET.......47000
      I=IABS(IUBC(IUP))                                                  BUDGET.......47100
      WRITE(K3,450) I,QPLITR(IUP)                                        BUDGET.......47200
 4700 CONTINUE                                                           BUDGET.......47300
C                                                                        BUDGET.......47400
C                                                                        BUDGET.......47500
 5500 CONTINUE                                                           BUDGET.......47600
C                                                                        BUDGET.......47700
      RETURN                                                             BUDGET.......47800
      END                                                                BUDGET.......47900
C                                                                        BUDGET.......48000
C     SUBROUTINE        C  O  N  N  E  C           SUTRA VERSION 2.1     CONNEC.........100
C                                                                        CONNEC.........200
C *** PURPOSE :                                                          CONNEC.........300
C ***  TO READ, ORGANIZE, AND CHECK DATA ON NODE INCIDENCES.             CONNEC.........400
C                                                                        CONNEC.........500
      SUBROUTINE CONNEC(IN)                                              CONNEC.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                CONNEC.........700
      CHARACTER INTFIL*1000                                              CONNEC.........800
      CHARACTER CDUM10*10                                                CONNEC.........900
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     CONNEC........1000
      DIMENSION IN(NIN)                                                  CONNEC........1100
      DIMENSION IIN(8)                                                   CONNEC........1200
      DIMENSION INERR(10),RLERR(10)                                      CONNEC........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              CONNEC........1400
     1   NSOP,NSOU,NBCN                                                  CONNEC........1500
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        CONNEC........1600
      COMMON /FNAMES/ UNAME,FNAME                                        CONNEC........1700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     CONNEC........1800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     CONNEC........1900
     1   KSCRN,KPAUSE                                                    CONNEC........2000
C                                                                        CONNEC........2100
      IPIN=0                                                             CONNEC........2200
      IF(KINCID.EQ.0) WRITE(K3,1)                                        CONNEC........2300
    1 FORMAT('1'////11X,'M E S H   C O N N E C T I O N   D A T A'//      CONNEC........2400
     1   16X,'PRINTOUT OF NODAL INCIDENCES CANCELLED.')                  CONNEC........2500
      IF(KINCID.EQ.+1) WRITE(K3,2)                                       CONNEC........2600
    2 FORMAT('1'////11X,'M E S H   C O N N E C T I O N   D A T A',       CONNEC........2700
     1   ///11X,'**** NODAL INCIDENCES ****'///)                         CONNEC........2800
C                                                                        CONNEC........2900
C.....INPUT DATASET 22 AND CHECK FOR ERRORS                              CONNEC........3000
      ERRCOD = 'REA-INP-22'                                              CONNEC........3100
      CALL READIF(K1, INTFIL, ERRCOD)                                    CONNEC........3200
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              CONNEC........3300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        CONNEC........3400
      IF (CDUM10.NE.'INCIDENCE ') THEN                                   CONNEC........3500
         ERRCOD = 'INP-22-1'                                             CONNEC........3600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        CONNEC........3700
      END IF                                                             CONNEC........3800
      DO 1000 L=1,NE                                                     CONNEC........3900
      ERRCOD = 'REA-INP-22'                                              CONNEC........4000
      CALL READIF(K1, INTFIL, ERRCOD)                                    CONNEC........4100
      READ(INTFIL,*,IOSTAT=INERR(1)) LL,(IIN(II),II=1,N48)               CONNEC........4200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        CONNEC........4300
C.....PREPARE NODE INCIDENCE LIST FOR MESH, IN.                          CONNEC........4400
      DO 5 II=1,N48                                                      CONNEC........4500
      III=II+(L-1)*N48                                                   CONNEC........4600
    5 IN(III)=IIN(II)                                                    CONNEC........4700
      IF(IABS(LL).EQ.L) GOTO 500                                         CONNEC........4800
      ERRCOD = 'INP-22-2'                                                CONNEC........4900
      INERR(1) = LL                                                      CONNEC........5000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           CONNEC........5100
C                                                                        CONNEC........5200
C                                                                        CONNEC........5300
  500 M1=(L-1)*N48+1                                                     CONNEC........5400
      M8=M1+N48-1                                                        CONNEC........5500
      IF(KINCID.EQ.0) GOTO 1000                                          CONNEC........5600
      WRITE(K3,650) L,(IN(M),M=M1,M8)                                    CONNEC........5700
  650 FORMAT(11X,'ELEMENT',I9,5X,' NODES AT : ',6X,'CORNERS ',           CONNEC........5800
     1   5('*'),8I9,1X,5('*'))                                           CONNEC........5900
C                                                                        CONNEC........6000
 1000 CONTINUE                                                           CONNEC........6100
C                                                                        CONNEC........6200
C                                                                        CONNEC........6300
 5000 RETURN                                                             CONNEC........6400
      END                                                                CONNEC........6500
C                                                                        CONNEC........6600
C     FUNCTION          C  U  T  S  M  L           SUTRA VERSION 2.1     CUTSML.........100
C                                                                        CUTSML.........200
C *** PURPOSE :                                                          CUTSML.........300
C ***  TO RETURN ARGUMENT DPNUM IF ITS MAGNITUDE IS GREATER THAN OR      CUTSML.........400
C ***  EQUAL TO 1.D-99, AND ZERO OTHERWISE.                              CUTSML.........500
C                                                                        CUTSML.........600
      DOUBLE PRECISION FUNCTION CUTSML(DPNUM)                            CUTSML.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               CUTSML.........800
C                                                                        CUTSML.........900
C.....RETURN DPNUM IF ITS ABSOLUTE VALUE IS >= 1.D-99, OTHERWISE         CUTSML........1000
C        RETURN ZERO                                                     CUTSML........1100
      IF (DABS(DPNUM).LT.1.D-99) THEN                                    CUTSML........1200
         CUTSML = 0D0                                                    CUTSML........1300
      ELSE                                                               CUTSML........1400
         CUTSML = DPNUM                                                  CUTSML........1500
      END IF                                                             CUTSML........1600
C                                                                        CUTSML........1700
      RETURN                                                             CUTSML........1800
      END                                                                CUTSML........1900
C                                                                        CUTSML........2000
C     SUBROUTINE        D  I  M  W  R  K           SUTRA VERSION 2.1     DIMWRK.........100
C                                                                        DIMWRK.........200
C *** PURPOSE :                                                          DIMWRK.........300
C ***  TO RETURN DIMENSIONS FOR THE SOLVER WORK ARRAYS, WHICH DEPEND ON  DIMWRK.........400
C ***  THE PARTICULAR SOLVER CHOSEN.                                     DIMWRK.........500
C                                                                        DIMWRK.........600
      SUBROUTINE DIMWRK(KSOLVR, NSAVE, NN, NELT, NWI, NWF)               DIMWRK.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                DIMWRK.........800
C                                                                        DIMWRK.........900
C.....COMPUTE SOLVER WORK ARRAY DIMENSIONS                               DIMWRK........1000
      IF (KSOLVR.EQ.1) THEN                                              DIMWRK........1100
         NL = (NELT + NN)/2                                              DIMWRK........1200
         NWI = 11 + 2*NL                                                 DIMWRK........1300
         NWF = NL + 5*NN + 1                                             DIMWRK........1400
      ELSE IF (KSOLVR.EQ.2) THEN                                         DIMWRK........1500
         NWI = 31 + 2*NELT                                               DIMWRK........1600
         NWF = 2 + NN*(NSAVE + 7) + NSAVE*(NSAVE + 3) + (NELT - NN)      DIMWRK........1700
      ELSE IF (KSOLVR.EQ.3) THEN                                         DIMWRK........1800
         NWI = 11 + 2*NELT                                               DIMWRK........1900
         NWF = 1 + 3*NN*(NSAVE + 1) + 7*NN + NSAVE + (NELT - NN)         DIMWRK........2000
      END IF                                                             DIMWRK........2100
C                                                                        DIMWRK........2200
      RETURN                                                             DIMWRK........2300
      END                                                                DIMWRK........2400
C                                                                        DIMWRK........2500
C     SUBROUTINE        D  I  S  P  R  3           SUTRA VERSION 2.1     DISPR3.........100
C                                                                        DISPR3.........200
C *** PURPOSE :                                                          DISPR3.........300
C ***  TO COMPUTE THE COMPONENTS OF THE 3D DISPERSION TENSOR IN          DISPR3.........400
C ***  X,Y,Z-COORDINATES USING AN AD HOC, 3D ANISOTROPIC DISPERSION      DISPR3.........500
C ***  MODEL.                                                            DISPR3.........600
C                                                                        DISPR3.........700
      SUBROUTINE DISPR3(VX,VY,VZ,VMAG,ANG1,ANG2,ANG3,ALMAX,ALMID,ALMIN,  DISPR3.........800
     1   ATMAX,ATMID,ATMIN,DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ)          DISPR3.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                DISPR3........1000
      LOGICAL LISO,TISO                                                  DISPR3........1100
      DIMENSION AL(3),AT(3),VN(3),UN(3),WN(3)                            DISPR3........1200
      DIMENSION J(3)                                                     DISPR3........1300
C                                                                        DISPR3........1400
C.....HANDLE CASE OF ZERO VELOCITY.  (THIS CASE IS ALREADY HANDLED       DISPR3........1500
C        BY SUBROUTINE ELEMN3.  THE BLOCK IF STATEMENT BELOW CAN BE      DISPR3........1600
C        UNCOMMENTED IF NEEDED IN THE FUTURE.)                           DISPR3........1700
C     IF (VMAG.EQ.0D0) THEN                                              DISPR3........1800
C        DXX = 0D0                                                       DISPR3........1900
C        DXY = 0D0                                                       DISPR3........2000
C        DXZ = 0D0                                                       DISPR3........2100
C        DYX = 0D0                                                       DISPR3........2200
C        DYY = 0D0                                                       DISPR3........2300
C        DYZ = 0D0                                                       DISPR3........2400
C        DZX = 0D0                                                       DISPR3........2500
C        DZY = 0D0                                                       DISPR3........2600
C        DZZ = 0D0                                                       DISPR3........2700
C        RETURN                                                          DISPR3........2800
C     END IF                                                             DISPR3........2900
C                                                                        DISPR3........3000
C.....SET TOLERANCES USED TO DETERMINE WHETHER CERTAIN DEGENERATE        DISPR3........3100
C        CONDITIONS ARE TRUE:                                            DISPR3........3200
C        TOLISO -- IS DISPERSION ESSENTIALLY ISOTROPIC?                  DISPR3........3300
C        TOLVRT -- IS FLOW ESSENTIALLY VERTICAL?                         DISPR3........3400
C        TOLCIR -- IS SLICING ELLIPSE ESSENTIALLY A CIRCLE?              DISPR3........3500
      TOLISO = 1D-7                                                      DISPR3........3600
      TOLVRT = 1D-7                                                      DISPR3........3700
      TOLCIR = 9.999999D-1                                               DISPR3........3800
C                                                                        DISPR3........3900
C.....NORMALIZE THE VELOCITY VECTOR.                                     DISPR3........4000
      VNX = VX/VMAG                                                      DISPR3........4100
      VNY = VY/VMAG                                                      DISPR3........4200
      VNZ = VZ/VMAG                                                      DISPR3........4300
C                                                                        DISPR3........4400
C.....DETERMINE WHETHER LONGITUDINAL DISPERSION IS ESSENTIALLY           DISPR3........4500
C        ISOTROPIC.                                                      DISPR3........4600
      AL(1) = ALMAX                                                      DISPR3........4700
      AL(2) = ALMID                                                      DISPR3........4800
      AL(3) = ALMIN                                                      DISPR3........4900
      ALMXVL = MAXVAL(AL)                                                DISPR3........5000
      ALMNVL = MINVAL(AL)                                                DISPR3........5100
      IF (ALMXVL.EQ.0D0) THEN                                            DISPR3........5200
         LISO = .TRUE.                                                   DISPR3........5300
      ELSE                                                               DISPR3........5400
         LISO = ((ALMXVL - ALMNVL)/ALMXVL.LT.TOLISO)                     DISPR3........5500
      END IF                                                             DISPR3........5600
C                                                                        DISPR3........5700
C.....COMPUTE THE LONGITUDINAL DISPERSION COEFFICIENT.                   DISPR3........5800
      IF (LISO) THEN                                                     DISPR3........5900
C........ISOTROPIC CASE.                                                 DISPR3........6000
         DL = ALMAX*VMAG                                                 DISPR3........6100
      ELSE                                                               DISPR3........6200
C........ANISOTROPIC CASE.                                               DISPR3........6300
C........ROTATE V TO "MAX-MID-MIN" COORDINATES.                          DISPR3........6400
         CALL ROTMAT(ANG1,ANG2,ANG3,G11,G12,G13,G21,G22,G23,             DISPR3........6500
     1      G31,G32,G33)                                                 DISPR3........6600
         CALL ROTATE(G11,G21,G31,G12,G22,G32,G13,G23,G33,                DISPR3........6700
     1      VNX,VNY,VNZ,VNXX,VNYY,VNZZ)                                  DISPR3........6800
C........EVALUATE DL FROM THE LONGITUDINAL DISPERSIVITY ELLIPSOID.       DISPR3........6900
         DL = VMAG/(VNXX*VNXX/ALMAX+VNYY*VNYY/ALMID+VNZZ*VNZZ/ALMIN)     DISPR3........7000
      END IF                                                             DISPR3........7100
C                                                                        DISPR3........7200
C.....DETERMINE WHETHER TRANSVERSE DISPERSION IS ESSENTIALLY             DISPR3........7300
C        ISOTROPIC.                                                      DISPR3........7400
      AT(1) = ATMAX                                                      DISPR3........7500
      AT(2) = ATMID                                                      DISPR3........7600
      AT(3) = ATMIN                                                      DISPR3........7700
      ATMXVL = MAXVAL(AT)                                                DISPR3........7800
      ATMNVL = MINVAL(AT)                                                DISPR3........7900
      IF (ATMXVL.EQ.0D0) THEN                                            DISPR3........8000
         TISO = .TRUE.                                                   DISPR3........8100
      ELSE                                                               DISPR3........8200
         TISO = ((ATMXVL - ATMNVL)/ATMXVL.LT.TOLISO)                     DISPR3........8300
      END IF                                                             DISPR3........8400
C                                                                        DISPR3........8500
C.....COMPUTE THE TRANSVERSE DISPERSION DIRECTIONS AND COEFFICIENTS.     DISPR3........8600
      IF (TISO) THEN                                                     DISPR3........8700
C........ISOTROPIC CASE.                                                 DISPR3........8800
         TERM = 1D0 - VNZ*VNZ                                            DISPR3........8900
         IF (TERM.LT.TOLVRT) THEN                                        DISPR3........9000
C...........FLOW IS ESSENTIALLY IN Z-DIRECTION (VERTICAL)                DISPR3........9100
            UNX = 1D0                                                    DISPR3........9200
            UNY = 0D0                                                    DISPR3........9300
            UNZ = 0D0                                                    DISPR3........9400
            WNX = 0D0                                                    DISPR3........9500
            WNY = 1D0                                                    DISPR3........9600
            WNZ = 0D0                                                    DISPR3........9700
         ELSE                                                            DISPR3........9800
C...........FLOW IS NOT IN Z-DIRECTION (NOT VERTICAL)                    DISPR3........9900
            TERMH = DSQRT(TERM)                                          DISPR3.......10000
            UNX = -VNY/TERMH                                             DISPR3.......10100
            UNY = VNX/TERMH                                              DISPR3.......10200
            UNZ = 0D0                                                    DISPR3.......10300
            WNX = -VNZ*UNY                                               DISPR3.......10400
            WNY = VNZ*UNX                                                DISPR3.......10500
            WNZ = TERMH                                                  DISPR3.......10600
         END IF                                                          DISPR3.......10700
         AT1 = ATMAX                                                     DISPR3.......10800
         AT2 = AT1                                                       DISPR3.......10900
      ELSE                                                               DISPR3.......11000
C........ANISOTROPIC CASE.                                               DISPR3.......11100
C........ROTATE V TO "MAX-MID-MIN" COORDINATES, IF NOT DONE PREVIOUSLY.  DISPR3.......11200
         IF (LISO) THEN                                                  DISPR3.......11300
            CALL ROTMAT(ANG1,ANG2,ANG3,G11,G12,G13,G21,G22,G23,          DISPR3.......11400
     1         G31,G32,G33)                                              DISPR3.......11500
            CALL ROTATE(G11,G21,G31,G12,G22,G32,G13,G23,G33,             DISPR3.......11600
     1         VNX,VNY,VNZ,VNXX,VNYY,VNZZ)                               DISPR3.......11700
         END IF                                                          DISPR3.......11800
C........TRANSPOSE AXES SO THAT THE LONGEST AXIS OF THE TRANSVERSE       DISPR3.......11900
C           DISPERSIVITY ELLIPSOID IS "MAX", THE SECOND LONGEST IS       DISPR3.......12000
C           "MID", AND THE SHORTEST IS "MIN".                            DISPR3.......12100
         J(1:1) = MAXLOC(AT)                                             DISPR3.......12200
         J(3:3) = MINLOC(AT)                                             DISPR3.......12300
         J(2) = 6 - J(1) - J(3)                                          DISPR3.......12400
         VN(1) = VNXX                                                    DISPR3.......12500
         VN(2) = VNYY                                                    DISPR3.......12600
         VN(3) = VNZZ                                                    DISPR3.......12700
         VNTXX = VN(J(1))                                                DISPR3.......12800
         VNTYY = VN(J(2))                                                DISPR3.......12900
         VNTZZ = VN(J(3))                                                DISPR3.......13000
         A2 = AT(J(1))                                                   DISPR3.......13100
         B2 = AT(J(2))                                                   DISPR3.......13200
         C2 = AT(J(3))                                                   DISPR3.......13300
C........APPLY THE BIOT-FRESNEL CONSTRUCTION TO THE TRANSVERSE           DISPR3.......13400
C           DISPERSIVITY ELLIPSOID.                                      DISPR3.......13500
         A2B2 = A2*B2                                                    DISPR3.......13600
         A2C2 = A2*C2                                                    DISPR3.......13700
         B2C2 = B2*C2                                                    DISPR3.......13800
         COS2AV = (A2C2 - B2C2)/(A2B2 - B2C2)                            DISPR3.......13900
         SIN2AV = 1D0 - COS2AV                                           DISPR3.......14000
         COSAV = DSQRT(COS2AV)                                           DISPR3.......14100
         SINAV = DSQRT(SIN2AV)                                           DISPR3.......14200
         TERM1 = COSAV*VNTXX                                             DISPR3.......14300
         TERM2 = SINAV*VNTZZ                                             DISPR3.......14400
         OA1V = TERM1 + TERM2                                            DISPR3.......14500
         OA2V = TERM1 - TERM2                                            DISPR3.......14600
         IF (MAX(DABS(OA1V),DABS(OA2V)).GT.TOLCIR) THEN                  DISPR3.......14700
C...........SLICING ELLIPSE IS ESSENTIALLY A CIRCLE                      DISPR3.......14800
            UNTXX = -VNTZZ                                               DISPR3.......14900
            UNTYY = 0D0                                                  DISPR3.......15000
            UNTZZ = VNTXX                                                DISPR3.......15100
            WNTXX = 0D0                                                  DISPR3.......15200
            WNTYY = 1D0                                                  DISPR3.......15300
            WNTZZ = 0D0                                                  DISPR3.......15400
            AT1 = B2                                                     DISPR3.......15500
            AT2 = B2                                                     DISPR3.......15600
         ELSE                                                            DISPR3.......15700
C...........SLICING ELLIPSE IS NOT A CIRCLE                              DISPR3.......15800
            RVJ1MG = 1D0/DSQRT(1D0 - OA1V*OA1V)                          DISPR3.......15900
            RVJ2MG = 1D0/DSQRT(1D0 - OA2V*OA2V)                          DISPR3.......16000
            RSUM = RVJ1MG + RVJ2MG                                       DISPR3.......16100
            RDIF = RVJ1MG - RVJ2MG                                       DISPR3.......16200
            OAUXX = COSAV*RSUM                                           DISPR3.......16300
            OAUZZ = SINAV*RDIF                                           DISPR3.......16400
            OAWXX = COSAV*RDIF                                           DISPR3.......16500
            OAWZZ = SINAV*RSUM                                           DISPR3.......16600
            OAUV = OAUXX*VNTXX + OAUZZ*VNTZZ                             DISPR3.......16700
            OAWV = OAWXX*VNTXX + OAWZZ*VNTZZ                             DISPR3.......16800
            OAUOAU = OAUXX*OAUXX + OAUZZ*OAUZZ                           DISPR3.......16900
            OAWOAW = OAWXX*OAWXX + OAWZZ*OAWZZ                           DISPR3.......17000
            UMTERM = OAUOAU - OAUV*OAUV                                  DISPR3.......17100
            WMTERM = OAWOAW - OAWV*OAWV                                  DISPR3.......17200
C...........COMPUTE THE LARGER OF U AND W DIRECTLY, THEN COMPUTE THE     DISPR3.......17300
C              OTHER BY CROSS-PRODUCT WITH V.                            DISPR3.......17400
            IF (UMTERM.GT.WMTERM) THEN                                   DISPR3.......17500
               RUMAGH = 1D0/DSQRT(UMTERM)                                DISPR3.......17600
               UNTXX = (OAUXX - OAUV*VNTXX)*RUMAGH                       DISPR3.......17700
               UNTYY = -OAUV*VNTYY*RUMAGH                                DISPR3.......17800
               UNTZZ = (OAUZZ - OAUV*VNTZZ)*RUMAGH                       DISPR3.......17900
               WNTXX = UNTYY*VNTZZ - UNTZZ*VNTYY                         DISPR3.......18000
               WNTYY = UNTZZ*VNTXX - UNTXX*VNTZZ                         DISPR3.......18100
               WNTZZ = UNTXX*VNTYY - UNTYY*VNTXX                         DISPR3.......18200
            ELSE                                                         DISPR3.......18300
               RWMAGH = 1D0/DSQRT(WMTERM)                                DISPR3.......18400
               WNTXX = (OAWXX - OAWV*VNTXX)*RWMAGH                       DISPR3.......18500
               WNTYY = -OAWV*VNTYY*RWMAGH                                DISPR3.......18600
               WNTZZ = (OAWZZ - OAWV*VNTZZ)*RWMAGH                       DISPR3.......18700
               UNTXX = WNTYY*VNTZZ - WNTZZ*VNTYY                         DISPR3.......18800
               UNTYY = WNTZZ*VNTXX - WNTXX*VNTZZ                         DISPR3.......18900
               UNTZZ = WNTXX*VNTYY - WNTYY*VNTXX                         DISPR3.......19000
            END IF                                                       DISPR3.......19100
            A2B2C2 = A2B2*C2                                             DISPR3.......19200
            DEN1 = B2C2*UNTXX*UNTXX+A2C2*UNTYY*UNTYY+A2B2*UNTZZ*UNTZZ    DISPR3.......19300
            DEN2 = B2C2*WNTXX*WNTXX+A2C2*WNTYY*WNTYY+A2B2*WNTZZ*WNTZZ    DISPR3.......19400
            AT1 = A2B2C2/DEN1                                            DISPR3.......19500
            AT2 = A2B2C2/DEN2                                            DISPR3.......19600
         END IF                                                          DISPR3.......19700
C........TRANSPOSE AXES BACK TO ORIGINAL "MAX-MID-MIN" AXES.             DISPR3.......19800
         UN(J(1)) = UNTXX                                                DISPR3.......19900
         UN(J(2)) = UNTYY                                                DISPR3.......20000
         UN(J(3)) = UNTZZ                                                DISPR3.......20100
         UNXX = UN(1)                                                    DISPR3.......20200
         UNYY = UN(2)                                                    DISPR3.......20300
         UNZZ = UN(3)                                                    DISPR3.......20400
         WN(J(1)) = WNTXX                                                DISPR3.......20500
         WN(J(2)) = WNTYY                                                DISPR3.......20600
         WN(J(3)) = WNTZZ                                                DISPR3.......20700
         WNXX = WN(1)                                                    DISPR3.......20800
         WNYY = WN(2)                                                    DISPR3.......20900
         WNZZ = WN(3)                                                    DISPR3.......21000
C........ROTATE THE TRANSVERSE DISPERSION DIRECTIONS FROM "MAX-MID-MIN"  DISPR3.......21100
C           COORDINATES TO X,Y,Z-COORDINATES.                            DISPR3.......21200
         CALL ROTATE(G11,G12,G13,G21,G22,G23,G31,G32,G33,UNXX,UNYY,UNZZ, DISPR3.......21300
     1      UNX,UNY,UNZ)                                                 DISPR3.......21400
         CALL ROTATE(G11,G12,G13,G21,G22,G23,G31,G32,G33,WNXX,WNYY,WNZZ, DISPR3.......21500
     1      WNX,WNY,WNZ)                                                 DISPR3.......21600
      END IF                                                             DISPR3.......21700
C.....COMPUTE TRANSVERSE DISPERSION COEFFICIENTS FROM DISPERSIVITIES     DISPR3.......21800
      DT1 = AT1*VMAG                                                     DISPR3.......21900
      DT2 = AT2*VMAG                                                     DISPR3.......22000
C                                                                        DISPR3.......22100
C.....ROTATE THE DISPERSION TENSOR FROM EIGENVECTOR COORDINATES TO       DISPR3.......22200
C     X,Y,Z-COORDINATES.                                                 DISPR3.......22300
      CALL TENSYM(DL,DT1,DT2,VNX,UNX,WNX,VNY,UNY,WNY,VNZ,UNZ,WNZ,        DISPR3.......22400
     1   DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ)                            DISPR3.......22500
C                                                                        DISPR3.......22600
      RETURN                                                             DISPR3.......22700
      END                                                                DISPR3.......22800
C                                                                        DISPR3.......22900
C     FUNCTION          D  P  3  S  T  R           SUTRA VERSION 2.1     DP3STR.........100
C                                                                        DP3STR.........200
C *** PURPOSE :                                                          DP3STR.........300
C ***  TO RETURN THREE DOUBLE-PRECISION NUMBERS IN THE FORM OF A         DP3STR.........400
C ***  STRING.  THE THREE NUMBERS ARE PASSED IN THROUGH ARRAY DPA        DP3STR.........500
C ***  AND ARE ROUNDED USING FUNCTION CUTSML IN PREPARATION FOR OUTPUT.  DP3STR.........600
C                                                                        DP3STR.........700
      FUNCTION DP3STR(DPA)                                               DP3STR.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               DP3STR.........900
      DIMENSION DPA(3)                                                   DP3STR........1000
      CHARACTER DP3STR*45                                                DP3STR........1100
C                                                                        DP3STR........1200
C.....WRITE NUMBERS TO STRING                                            DP3STR........1300
      WRITE(UNIT=DP3STR,FMT="(3(1PE15.7))")                              DP3STR........1400
     1   CUTSML(DPA(1)), CUTSML(DPA(2)), CUTSML(DPA(3))                  DP3STR........1500
C                                                                        DP3STR........1600
      RETURN                                                             DP3STR........1700
      END                                                                DP3STR........1800
C                                                                        DP3STR........1900
C     SUBROUTINE        E  L  E  M  N  2           SUTRA VERSION 2.1     ELEMN2.........100
C                                                                        ELEMN2.........200
C *** PURPOSE :                                                          ELEMN2.........300
C ***  TO CONTROL AND CARRY OUT ALL CALCULATIONS FOR EACH ELEMENT BY     ELEMN2.........400
C ***  OBTAINING ELEMENT INFORMATION FROM THE BASIS FUNCTION ROUTINE,    ELEMN2.........500
C ***  CARRYING OUT GAUSSIAN INTEGRATION OF FINITE ELEMENT INTEGRALS,    ELEMN2.........600
C ***  AND ASSEMBLING RESULTS OF ELEMENTWISE INTEGRATIONS INTO           ELEMN2.........700
C ***  A GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW AND TRANSPORT     ELEMN2.........800
C ***  EQUATIONS. ALSO CALCULATES VELOCITY AT EACH ELEMENT CENTROID FOR  ELEMN2.........900
C ***  PRINTED OUTPUT. THIS SUBROUTINE HANDLES 2D CALCULATIONS ONLY;     ELEMN2........1000
C ***  3D CALCULATIONS ARE PERFORMED IN SUBROUTINE ELEMN3.               ELEMN2........1100
C                                                                        ELEMN2........1200
C                                                                        ELEMN2........1300
      SUBROUTINE ELEMN2(ML,IN,X,Y,THICK,PITER,UITER,RCIT,RCITM1,POR,     ELEMN2........1400
     1   ALMAX,ALMIN,ATMAX,ATMIN,PERMXX,PERMXY,PERMYX,PERMYY,PANGLE,     ELEMN2........1500
     2   VMAG,VANG,VOL,PMAT,PVEC,UMAT,UVEC,GXSI,GETA,PVEL,LREG,IA,JA)    ELEMN2........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ELEMN2........1700
      PARAMETER (NCOLMX=9)                                               ELEMN2........1800
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     ELEMN2........1900
      DIMENSION IN(NIN),X(NN),Y(NN),THICK(NN),PITER(NN),                 ELEMN2........2000
     1   UITER(NN),RCIT(NN),RCITM1(NN),POR(NN),PVEL(NN)                  ELEMN2........2100
      DIMENSION PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),PANGLE(NE),  ELEMN2........2200
     1   ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),VANG(NE),      ELEMN2........2300
     2   GXSI(NE,4),GETA(NE,4),LREG(NE)                                  ELEMN2........2400
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),     ELEMN2........2500
     1   UVEC(NNVEC)                                                     ELEMN2........2600
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    ELEMN2........2700
      DIMENSION F(4,4),W(4,4),DET(4),DFDXG(4,4),DFDYG(4,4),              ELEMN2........2800
     1   DWDXG(4,4),DWDYG(4,4)                                           ELEMN2........2900
      DIMENSION SWG(4),RHOG(4),VISCG(4),PORG(4),VXG(4),VYG(4),           ELEMN2........3000
     1   RELKG(4),RGXG(4),RGYG(4),VGMAG(4),THICKG(4)                     ELEMN2........3100
      DIMENSION RXXG(4),RXYG(4),RYXG(4),RYYG(4)                          ELEMN2........3200
      DIMENSION BXXG(4),BXYG(4),BYXG(4),BYYG(4),EXG(4),EYG(4)            ELEMN2........3300
      DIMENSION GXLOC(4),GYLOC(4)                                        ELEMN2........3400
      DIMENSION IA(NDIMIA),JA(NDIMJA)                                    ELEMN2........3500
      DIMENSION INERR(10), RLERR(10)                                     ELEMN2........3600
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             ELEMN2........3700
      DIMENSION KTYPE(2)                                                 ELEMN2........3800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  ELEMN2........3900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             ELEMN2........4000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              ELEMN2........4100
     1   NSOP,NSOU,NBCN                                                  ELEMN2........4200
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        ELEMN2........4300
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           ELEMN2........4400
      COMMON /FNAMES/ UNAME,FNAME                                        ELEMN2........4500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     ELEMN2........4600
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  ELEMN2........4700
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      ELEMN2........4800
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             ELEMN2........4900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     ELEMN2........5000
     1   KSCRN,KPAUSE                                                    ELEMN2........5100
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      ELEMN2........5200
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        ELEMN2........5300
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           ELEMN2........5400
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       ELEMN2........5500
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  ELEMN2........5600
      DATA GLOC/0.577350269189626D0/                                     ELEMN2........5700
      DATA INTIM/0/,ISTOP/0/,GXLOC/-1.D0,1.D0,1.D0,-1.D0/,               ELEMN2........5800
     1   GYLOC/-1.D0,-1.D0,1.D0,1.D0/                                    ELEMN2........5900
      SAVE GLOC,INTIM,ISTOP,GXLOC,GYLOC                                  ELEMN2........6000
C                                                                        ELEMN2........6100
C.....DECIDE WHETHER TO CALCULATE CENTROID VELOCITIES ON THIS CALL       ELEMN2........6200
      IVCALC = 0                                                         ELEMN2........6300
      JVCALC = 0                                                         ELEMN2........6400
      IF ((ML.NE.2).AND.(ITER.EQ.1)) IVCALC = 1                          ELEMN2........6500
      IF (IT.EQ.1) IVCALC = 1                                            ELEMN2........6600
      IF ((KVEL.EQ.1).OR.(K6.NE.-1)) JVCALC = 1                          ELEMN2........6700
      KVCALC = IVCALC + JVCALC                                           ELEMN2........6800
C                                                                        ELEMN2........6900
C.....ON FIRST TIME STEP, PREPARE GRAVITY VECTOR COMPONENTS,             ELEMN2........7000
C        GXSI AND GETA, FOR CONSISTENT VELOCITIES,                       ELEMN2........7100
C        AND CHECK ELEMENT SHAPES                                        ELEMN2........7200
      IF(INTIM) 100,100,2000                                             ELEMN2........7300
  100 INTIM=1                                                            ELEMN2........7400
C.....LOOP THROUGH ALL ELEMENTS TO OBTAIN THE JACOBIAN                   ELEMN2........7500
C        AT EACH OF THE FOUR NODES IN EACH ELEMENT                       ELEMN2........7600
      DO 1000 L=1,NE                                                     ELEMN2........7700
       DO 500 IL=1,4                                                     ELEMN2........7800
        XLOC=GXLOC(IL)                                                   ELEMN2........7900
        YLOC=GYLOC(IL)                                                   ELEMN2........8000
        CALL BASIS2(0000,L,XLOC,YLOC,IN,X,Y,F(1,IL),W(1,IL),DET(IL),     ELEMN2........8100
     1     DFDXG(1,IL),DFDYG(1,IL),DWDXG(1,IL),DWDYG(1,IL),              ELEMN2........8200
     2     PITER,UITER,PVEL,POR,THICK,THICKG(IL),VXG(IL),VYG(IL),        ELEMN2........8300
     3     SWG(IL),RHOG(IL),VISCG(IL),PORG(IL),VGMAG(IL),RELKG(IL),      ELEMN2........8400
     4     PERMXX,PERMXY,PERMYX,PERMYY,CJ11,CJ12,CJ21,CJ22,              ELEMN2........8500
     5     GXSI,GETA,RCIT,RCITM1,RGXG(IL),RGYG(IL),LREG)                 ELEMN2........8600
        GXSI(L,IL)=CJ11*GRAVX+CJ12*GRAVY                                 ELEMN2........8700
        GETA(L,IL)=CJ21*GRAVX+CJ22*GRAVY                                 ELEMN2........8800
C.....CHECK FOR NEGATIVE- OR ZERO-AREA ERRORS IN ELEMENT SHAPES          ELEMN2........8900
        IF(DET(IL)) 200,200,500                                          ELEMN2........9000
  200   ISTOP=ISTOP+1                                                    ELEMN2........9100
        WRITE(K3,400) IN((L-1)*4+IL),L,DET(IL)                           ELEMN2........9200
        WRITE(K00,401) IN((L-1)*4+IL),L,DET(IL)                          ELEMN2........9300
  400   FORMAT(11X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,        ELEMN2........9400
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN2........9500
  401   FORMAT(1X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,         ELEMN2........9600
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN2........9700
  500  CONTINUE                                                          ELEMN2........9800
 1000 CONTINUE                                                           ELEMN2........9900
C                                                                        ELEMN2.......10000
      IF(ISTOP.EQ.0) GOTO 2000                                           ELEMN2.......10100
      ERRCOD = 'INP-14B,22-1'                                            ELEMN2.......10200
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           ELEMN2.......10300
C                                                                        ELEMN2.......10400
C.....LOOP THROUGH ALL ELEMENTS TO CARRY OUT SPATIAL INTEGRATION         ELEMN2.......10500
C        OF FLUX TERMS IN P AND/OR U EQUATIONS                           ELEMN2.......10600
 2000 IF(IUNSAT.NE.0) IUNSAT=2                                           ELEMN2.......10700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......10800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......10900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......11000
      DO 9999 L=1,NE                                                     ELEMN2.......11100
       XIX=-1.D0                                                         ELEMN2.......11200
       YIY=-1.D0                                                         ELEMN2.......11300
       KG=0                                                              ELEMN2.......11400
C.....OBTAIN BASIS FUNCTION AND RELATED INFORMATION AT EACH OF           ELEMN2.......11500
C        FOUR GAUSS POINTS IN THE ELEMENT                                ELEMN2.......11600
       DO 2200 IYL=1,2                                                   ELEMN2.......11700
        DO 2100 IXL=1,2                                                  ELEMN2.......11800
         KG=KG+1                                                         ELEMN2.......11900
         XLOC=XIX*GLOC                                                   ELEMN2.......12000
         YLOC=YIY*GLOC                                                   ELEMN2.......12100
         CALL BASIS2(0001,L,XLOC,YLOC,IN,X,Y,F(1,KG),W(1,KG),DET(KG),    ELEMN2.......12200
     1      DFDXG(1,KG),DFDYG(1,KG),DWDXG(1,KG),DWDYG(1,KG),             ELEMN2.......12300
     2      PITER,UITER,PVEL,POR,THICK,THICKG(KG),VXG(KG),VYG(KG),       ELEMN2.......12400
     3      SWG(KG),RHOG(KG),VISCG(KG),PORG(KG),VGMAG(KG),RELKG(KG),     ELEMN2.......12500
     4      PERMXX,PERMXY,PERMYX,PERMYY,CJ11,CJ12,CJ21,CJ22,             ELEMN2.......12600
     5      GXSI,GETA,RCIT,RCITM1,RGXG(KG),RGYG(KG),LREG)                ELEMN2.......12700
 2100    XIX=-XIX                                                        ELEMN2.......12800
 2200   YIY=-YIY                                                         ELEMN2.......12900
C                                                                        ELEMN2.......13000
C.....CALCULATE VELOCITY AT ELEMENT CENTROID WHEN REQUIRED               ELEMN2.......13100
       IF(KVCALC-2) 3000,2300,3000                                       ELEMN2.......13200
 2300  AXSUM=0.0D0                                                       ELEMN2.......13300
       AYSUM=0.0D0                                                       ELEMN2.......13400
       DO 2400 KG=1,4                                                    ELEMN2.......13500
        AXSUM=AXSUM+VXG(KG)                                              ELEMN2.......13600
 2400   AYSUM=AYSUM+VYG(KG)                                              ELEMN2.......13700
       VMAG(L)=DSQRT(AXSUM*AXSUM+AYSUM*AYSUM)                            ELEMN2.......13800
       IF (VMAG(L).NE.0D0) THEN                                          ELEMN2.......13900
          VMAG(L)=VMAG(L)*2.5D-1                                         ELEMN2.......14000
          VANG(L)=DATAN2(AYSUM,AXSUM)*5.729577951308232D+1               ELEMN2.......14100
       ELSE                                                              ELEMN2.......14200
          VANG(L)=0D0                                                    ELEMN2.......14300
       END IF                                                            ELEMN2.......14400
C                                                                        ELEMN2.......14500
C.....INCLUDE MESH THICKNESS IN NUMERICAL INTEGRATION                    ELEMN2.......14600
 3000  DO 3300 KG=1,4                                                    ELEMN2.......14700
 3300   DET(KG)=THICKG(KG)*DET(KG)                                       ELEMN2.......14800
C                                                                        ELEMN2.......14900
C.....CALCULATE PARAMETERS FOR FLUID MASS BALANCE AT GAUSS POINTS        ELEMN2.......15000
       IF(ML-1) 3400,3400,6100                                           ELEMN2.......15100
 3400  SWTEST=0.D0                                                       ELEMN2.......15200
       DO 4000 KG=1,4                                                    ELEMN2.......15300
        SWTEST=SWTEST+SWG(KG)                                            ELEMN2.......15400
        ROMG=RHOG(KG)*RELKG(KG)/VISCG(KG)                                ELEMN2.......15500
        RXXG(KG)=PERMXX(L)*ROMG                                          ELEMN2.......15600
        RXYG(KG)=PERMXY(L)*ROMG                                          ELEMN2.......15700
        RYXG(KG)=PERMYX(L)*ROMG                                          ELEMN2.......15800
        RYYG(KG)=PERMYY(L)*ROMG                                          ELEMN2.......15900
 4000   CONTINUE                                                         ELEMN2.......16000
C                                                                        ELEMN2.......16100
C.....INTEGRATE FLUID MASS BALANCE IN AN UNSATURATED ELEMENT             ELEMN2.......16200
C        USING ASYMMETRIC WEIGHTING FUNCTIONS                            ELEMN2.......16300
       IF(UP.LE.1.0D-6) GOTO 5200                                        ELEMN2.......16400
       IF(SWTEST-3.999D0) 4200,5200,5200                                 ELEMN2.......16500
 4200  DO 4300 I=1,4                                                     ELEMN2.......16600
        VOLE(I) = 0.D0                                                   ELEMN2.......16700
        DFLOWE(I) = 0.D0                                                 ELEMN2.......16800
        DO 4300 J=1,4                                                    ELEMN2.......16900
         BFLOWE(I,J) = 0.D0                                              ELEMN2.......17000
 4300  CONTINUE                                                          ELEMN2.......17100
       DO 5000 KG=1,4                                                    ELEMN2.......17200
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN2.......17300
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN2.......17400
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN2.......17500
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN2.......17600
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG)                           ELEMN2.......17700
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG)                           ELEMN2.......17800
        DO 4400 I=1,4                                                    ELEMN2.......17900
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN2.......18000
         DFLOWE(I) = DFLOWE(I) + RDRX*DWDXG(I,KG) + RDRY*DWDYG(I,KG)     ELEMN2.......18100
 4400   CONTINUE                                                         ELEMN2.......18200
        DO 5000 J=1,4                                                    ELEMN2.......18300
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN2.......18400
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN2.......18500
         DO 5000 I=1,4                                                   ELEMN2.......18600
          BFLOWE(I,J) = BFLOWE(I,J) + DWDXG(I,KG)*RDDFJX                 ELEMN2.......18700
     1                  + DWDYG(I,KG)*RDDFJY                             ELEMN2.......18800
 5000  CONTINUE                                                          ELEMN2.......18900
       GOTO 6050                                                         ELEMN2.......19000
C                                                                        ELEMN2.......19100
C.....INTEGRATE FLUID MASS BALANCE IN A SATURATED OR UNSATURATED         ELEMN2.......19200
C        ELEMENT USING SYMMETRIC WEIGHTING FUNCTIONS                     ELEMN2.......19300
 5200  DO 5300 I=1,4                                                     ELEMN2.......19400
        VOLE(I) = 0.D0                                                   ELEMN2.......19500
        DFLOWE(I) = 0.D0                                                 ELEMN2.......19600
        DO 5300 J=1,4                                                    ELEMN2.......19700
         BFLOWE(I,J) = 0.D0                                              ELEMN2.......19800
 5300  CONTINUE                                                          ELEMN2.......19900
       DO 6000 KG=1,4                                                    ELEMN2.......20000
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN2.......20100
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN2.......20200
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN2.......20300
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN2.......20400
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG)                           ELEMN2.......20500
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG)                           ELEMN2.......20600
        DO 5400 I=1,4                                                    ELEMN2.......20700
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN2.......20800
         DFLOWE(I) = DFLOWE(I) + RDRX*DFDXG(I,KG) + RDRY*DFDYG(I,KG)     ELEMN2.......20900
 5400   CONTINUE                                                         ELEMN2.......21000
        DO 6000 J=1,4                                                    ELEMN2.......21100
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN2.......21200
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN2.......21300
         DO 6000 I=1,4                                                   ELEMN2.......21400
          BFLOWE(I,J) = BFLOWE(I,J) + DFDXG(I,KG)*RDDFJX                 ELEMN2.......21500
     1                  + DFDYG(I,KG)*RDDFJY                             ELEMN2.......21600
 6000  CONTINUE                                                          ELEMN2.......21700
 6050  CONTINUE                                                          ELEMN2.......21800
       IF(ML-1) 6100,9000,6100                                           ELEMN2.......21900
 6100  IF(NOUMAT.EQ.1) GOTO 9000                                         ELEMN2.......22000
C                                                                        ELEMN2.......22100
C                                                                        ELEMN2.......22200
C.....CALCULATE PARAMETERS FOR ENERGY BALANCE OR SOLUTE MASS BALANCE     ELEMN2.......22300
C        AT GAUSS POINTS                                                 ELEMN2.......22400
       DO 7000 KG=1,4                                                    ELEMN2.......22500
        ESWG=PORG(KG)*SWG(KG)                                            ELEMN2.......22600
        RHOCWG=RHOG(KG)*CW                                               ELEMN2.......22700
        ESRCG=ESWG*RHOCWG                                                ELEMN2.......22800
        IF(VGMAG(KG)) 6300,6300,6600                                     ELEMN2.......22900
 6300   EXG(KG)=0.0D0                                                    ELEMN2.......23000
        EYG(KG)=0.0D0                                                    ELEMN2.......23100
        DXXG=0.0D0                                                       ELEMN2.......23200
        DXYG=0.0D0                                                       ELEMN2.......23300
        DYXG=0.0D0                                                       ELEMN2.......23400
        DYYG=0.0D0                                                       ELEMN2.......23500
        GOTO 6900                                                        ELEMN2.......23600
 6600   EXG(KG)=ESRCG*VXG(KG)                                            ELEMN2.......23700
        EYG(KG)=ESRCG*VYG(KG)                                            ELEMN2.......23800
C                                                                        ELEMN2.......23900
C.....DISPERSIVITY MODEL FOR 2D ANISOTROPIC MEDIA                        ELEMN2.......24000
C        WITH PRINCIPAL DISPERSIVITIES ALMAX, ALMIN, ATMAX, AND ATMIN    ELEMN2.......24100
        VANGG=1.570796327D0                                              ELEMN2.......24200
        IF(VXG(KG)*VXG(KG).GT.0.D0) VANGG=DATAN(VYG(KG)/VXG(KG))         ELEMN2.......24300
        VKANGG=VANGG-PANGLE(L)                                           ELEMN2.......24400
        DCO=DCOS(VKANGG)                                                 ELEMN2.......24500
        DSI=DSIN(VKANGG)                                                 ELEMN2.......24600
C.....EFFECTIVE LONGITUDINAL DISPERSIVITY IN FLOW DIRECTION, ALEFF       ELEMN2.......24700
        ALEFF=0.0D0                                                      ELEMN2.......24800
        IF(ALMAX(L)+ALMIN(L)) 6800,6800,6700                             ELEMN2.......24900
 6700   ALEFF=ALMAX(L)*ALMIN(L)/(ALMIN(L)*DCO*DCO+ALMAX(L)*DSI*DSI)      ELEMN2.......25000
 6800   DLG=ALEFF*VGMAG(KG)                                              ELEMN2.......25100
C.....EFFECTIVE TRANSVERSE DISPERSIVITY IN FLOW DIRECTION, ATEFF.        ELEMN2.......25200
C        NOTE THAT, STARTING WITH VERSION 2D3D.1, ATMAX AND ATMIN        ELEMN2.......25300
C        HAVE EXCHANGED IDENTITIES TO MAKE THE 2D DISPERSION MODEL       ELEMN2.......25400
C        CONCEPTUALLY CONSISTENT WITH THE 3D MODEL.                      ELEMN2.......25500
        ATEFF=0.0D0                                                      ELEMN2.......25600
        IF(ATMAX(L)+ATMIN(L)) 6860,6860,6840                             ELEMN2.......25700
 6840   ATEFF=ATMAX(L)*ATMIN(L)/(ATMAX(L)*DCO*DCO+ATMIN(L)*DSI*DSI)      ELEMN2.......25800
 6860   DTG=ATEFF*VGMAG(KG)                                              ELEMN2.......25900
C                                                                        ELEMN2.......26000
        VXVG=VXG(KG)/VGMAG(KG)                                           ELEMN2.......26100
        VYVG=VYG(KG)/VGMAG(KG)                                           ELEMN2.......26200
        VXVG2=VXVG*VXVG                                                  ELEMN2.......26300
        VYVG2=VYVG*VYVG                                                  ELEMN2.......26400
C.....DISPERSION TENSOR                                                  ELEMN2.......26500
        DXXG=DLG*VXVG2+DTG*VYVG2                                         ELEMN2.......26600
        DYYG=DTG*VXVG2+DLG*VYVG2                                         ELEMN2.......26700
        DXYG=(DLG-DTG)*VXVG*VYVG                                         ELEMN2.......26800
        DYXG=DXYG                                                        ELEMN2.......26900
C                                                                        ELEMN2.......27000
C.....IN-PARALLEL CONDUCTIVITIES (DIFFUSIVITIES) FORMULA                 ELEMN2.......27100
 6900   IF (ME.EQ.1) THEN                                                ELEMN2.......27200
C..........FOR ENERGY TRANSPORT:                                         ELEMN2.......27300
           ESE = ESWG*SIGMAW + (1D0-PORG(KG))*SIGMAS                     ELEMN2.......27400
        ELSE                                                             ELEMN2.......27500
C..........FOR SOLUTE TRANSPORT:                                         ELEMN2.......27600
           ESE = ESRCG*SIGMAW                                            ELEMN2.......27700
        END IF                                                           ELEMN2.......27800
C.....ADD DIFFUSION AND DISPERSION TERMS TO TOTAL DISPERSION TENSOR      ELEMN2.......27900
        BXXG(KG)=ESRCG*DXXG+ESE                                          ELEMN2.......28000
        BXYG(KG)=ESRCG*DXYG                                              ELEMN2.......28100
        BYXG(KG)=ESRCG*DYXG                                              ELEMN2.......28200
 7000   BYYG(KG)=ESRCG*DYYG+ESE                                          ELEMN2.......28300
C                                                                        ELEMN2.......28400
C.....INTEGRATE SOLUTE MASS BALANCE OR ENERGY BALANCE                    ELEMN2.......28500
C        USING SYMMETRIC WEIGHTING FUNCTIONS FOR DISPERSION TERM AND     ELEMN2.......28600
C        USING EITHER SYMMETRIC OR ASYMMETRIC WEIGHTING FUNCTIONS        ELEMN2.......28700
C        FOR ADVECTION TERM                                              ELEMN2.......28800
         DO 7400 I=1,4                                                   ELEMN2.......28900
         DO 7400 J=1,4                                                   ELEMN2.......29000
            BTRANE(I,J) = 0.D0                                           ELEMN2.......29100
            DTRANE(I,J) = 0.D0                                           ELEMN2.......29200
 7400    CONTINUE                                                        ELEMN2.......29300
         DO 8000 KG=1,4                                                  ELEMN2.......29400
            BXXGD = BXXG(KG)*DET(KG)                                     ELEMN2.......29500
            BXYGD = BXYG(KG)*DET(KG)                                     ELEMN2.......29600
            BYXGD = BYXG(KG)*DET(KG)                                     ELEMN2.......29700
            BYYGD = BYYG(KG)*DET(KG)                                     ELEMN2.......29800
            EXGD = EXG(KG)*DET(KG)                                       ELEMN2.......29900
            EYGD = EYG(KG)*DET(KG)                                       ELEMN2.......30000
            DO 8000 J=1,4                                                ELEMN2.......30100
               BDDFJX = BXXGD*DFDXG(J,KG) + BXYGD*DFDYG(J,KG)            ELEMN2.......30200
               BDDFJY = BYXGD*DFDXG(J,KG) + BYYGD*DFDYG(J,KG)            ELEMN2.......30300
               EDDFJ = EXGD*DFDXG(J,KG) + EYGD*DFDYG(J,KG)               ELEMN2.......30400
               DO 8000 I=1,4                                             ELEMN2.......30500
                  BTRANE(I,J) = BTRANE(I,J) + DFDXG(I,KG)*BDDFJX         ELEMN2.......30600
     1               + DFDYG(I,KG)*BDDFJY                                ELEMN2.......30700
                  DTRANE(I,J) = DTRANE(I,J) + EDDFJ*W(I,KG)              ELEMN2.......30800
 8000    CONTINUE                                                        ELEMN2.......30900
 9000  CONTINUE                                                          ELEMN2.......31000
C                                                                        ELEMN2.......31100
C                                                                        ELEMN2.......31200
C.....SEND RESULTS OF INTEGRATIONS FOR THIS ELEMENT                      ELEMN2.......31300
C        TO GLOBAL ASSEMBLY ROUTINE:                                     ELEMN2.......31400
C        GLOBAN -- SUTRA'S ORIGINAL BANDED FORMAT                        ELEMN2.......31500
C        GLOCOL -- SLAP COLUMN FORMAT                                    ELEMN2.......31600
      IF (KSOLVP.EQ.0) THEN                                              ELEMN2.......31700
         CALL GLOBAN(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN2.......31800
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC)                                  ELEMN2.......31900
      ELSE                                                               ELEMN2.......32000
         CALL GLOCOL(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN2.......32100
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC,IA,JA)                            ELEMN2.......32200
      END IF                                                             ELEMN2.......32300
 9999 CONTINUE                                                           ELEMN2.......32400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......32500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......32600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......32700
C                                                                        ELEMN2.......32800
C                                                                        ELEMN2.......32900
      RETURN                                                             ELEMN2.......33000
      END                                                                ELEMN2.......33100
C                                                                        ELEMN2.......33200
C     SUBROUTINE        E  L  E  M  N  3           SUTRA VERSION 2.1     ELEMN3.........100
C                                                                        ELEMN3.........200
C *** PURPOSE :                                                          ELEMN3.........300
C ***  TO CONTROL AND CARRY OUT ALL CALCULATIONS FOR EACH ELEMENT BY     ELEMN3.........400
C ***  OBTAINING ELEMENT INFORMATION FROM THE BASIS FUNCTION ROUTINE,    ELEMN3.........500
C ***  CARRYING OUT GAUSSIAN INTEGRATION OF FINITE ELEMENT INTEGRALS,    ELEMN3.........600
C ***  AND ASSEMBLING RESULTS OF ELEMENTWISE INTEGRATIONS INTO           ELEMN3.........700
C ***  A GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW AND TRANSPORT     ELEMN3.........800
C ***  EQUATIONS. ALSO CALCULATES VELOCITY AT EACH ELEMENT CENTROID FOR  ELEMN3.........900
C ***  PRINTED OUTPUT. THIS SUBROUTINE HANDLES 3D CALCULATIONS ONLY.     ELEMN3........1000
C ***  2D CALCULATIONS ARE PERFORMED IN SUBROUTINE ELEMN2.               ELEMN3........1100
C                                                                        ELEMN3........1200
C                                                                        ELEMN3........1300
      SUBROUTINE ELEMN3(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,         ELEMN3........1400
     1   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,                            ELEMN3........1500
     2   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, ELEMN3........1600
     3   PANGL1,PANGL2,PANGL3,VMAG,VANG1,VANG2,VOL,PMAT,PVEC,            ELEMN3........1700
     4   UMAT,UVEC,GXSI,GETA,GZET,PVEL,LREG,IA,JA)                       ELEMN3........1800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ELEMN3........1900
      PARAMETER (NCOLMX=9)                                               ELEMN3........2000
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     ELEMN3........2100
      DIMENSION IN(NIN),X(NN),Y(NN),Z(NN),PITER(NN),                     ELEMN3........2200
     1   UITER(NN),RCIT(NN),RCITM1(NN),POR(NN),PVEL(NN)                  ELEMN3........2300
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NE),PERMYX(NE),             ELEMN3........2400
     1   PERMYY(NE),PERMYZ(NE),PERMZX(NE),PERMZY(NE),PERMZZ(NE),         ELEMN3........2500
     2   PANGL1(NE),PANGL2(NE),PANGL3(NE),                               ELEMN3........2600
     3   ALMAX(NE),ALMID(NE),ALMIN(NE),ATMAX(NE),ATMID(NE),ATMIN(NE),    ELEMN3........2700
     4   VMAG(NE),VANG1(NE),VANG2(NE),                                   ELEMN3........2800
     5   GXSI(NE,8),GETA(NE,8),GZET(NE,8),LREG(NE)                       ELEMN3........2900
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),     ELEMN3........3000
     1   UVEC(NNVEC)                                                     ELEMN3........3100
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    ELEMN3........3200
      DIMENSION F(8,8),W(8,8),DET(8),DFDXG(8,8),DFDYG(8,8),DFDZG(8,8),   ELEMN3........3300
     1   DWDXG(8,8),DWDYG(8,8),DWDZG(8,8)                                ELEMN3........3400
      DIMENSION SWG(8),RHOG(8),VISCG(8),PORG(8),VXG(8),VYG(8),VZG(8),    ELEMN3........3500
     1   RELKG(8),RGXG(8),RGYG(8),RGZG(8),VGMAG(8)                       ELEMN3........3600
      DIMENSION RXXG(8),RXYG(8),RXZG(8),RYXG(8),RYYG(8),RYZG(8),         ELEMN3........3700
     1   RZXG(8),RZYG(8),RZZG(8)                                         ELEMN3........3800
      DIMENSION BXXG(8),BXYG(8),BXZG(8),BYXG(8),BYYG(8),BYZG(8),         ELEMN3........3900
     1   BZXG(8),BZYG(8),BZZG(8),EXG(8),EYG(8),EZG(8)                    ELEMN3........4000
      DIMENSION GXLOC(8),GYLOC(8),GZLOC(8)                               ELEMN3........4100
      DIMENSION IA(NDIMIA),JA(NDIMJA)                                    ELEMN3........4200
      DIMENSION INERR(10),RLERR(10)                                      ELEMN3........4300
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             ELEMN3........4400
      DIMENSION KTYPE(2)                                                 ELEMN3........4500
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  ELEMN3........4600
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             ELEMN3........4700
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              ELEMN3........4800
     1   NSOP,NSOU,NBCN                                                  ELEMN3........4900
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        ELEMN3........5000
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        ELEMN3........5100
      COMMON /FNAMES/ UNAME,FNAME                                        ELEMN3........5200
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     ELEMN3........5300
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  ELEMN3........5400
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      ELEMN3........5500
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        ELEMN3........5600
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     ELEMN3........5700
     1   KSCRN,KPAUSE                                                    ELEMN3........5800
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      ELEMN3........5900
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        ELEMN3........6000
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           ELEMN3........6100
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       ELEMN3........6200
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  ELEMN3........6300
      DATA GLOC/0.577350269189626D0/                                     ELEMN3........6400
      DATA INTIM/0/,ISTOP/0/                                             ELEMN3........6500
      DATA GXLOC/-1.D0,1.D0,1.D0,-1.D0,-1.D0,1.D0,1.D0,-1.D0/            ELEMN3........6600
      DATA GYLOC/-1.D0,-1.D0,1.D0,1.D0,-1.D0,-1.D0,1.D0,1.D0/            ELEMN3........6700
      DATA GZLOC/-1.D0,-1.D0,-1.D0,-1.D0,1.D0,1.D0,1.D0,1.D0/            ELEMN3........6800
      SAVE GLOC,INTIM,ISTOP,GXLOC,GYLOC,GZLOC                            ELEMN3........6900
C                                                                        ELEMN3........7000
C.....DECIDE WHETHER TO CALCULATE CENTROID VELOCITIES ON THIS CALL       ELEMN3........7100
      IVCALC = 0                                                         ELEMN3........7200
      JVCALC = 0                                                         ELEMN3........7300
      IF ((ML.NE.2).AND.(ITER.EQ.1)) IVCALC = 1                          ELEMN3........7400
      IF (IT.EQ.1) IVCALC = 1                                            ELEMN3........7500
      IF ((KVEL.EQ.1).OR.(K6.NE.-1)) JVCALC = 1                          ELEMN3........7600
      KVCALC = IVCALC + JVCALC                                           ELEMN3........7700
C                                                                        ELEMN3........7800
C.....ON FIRST TIME STEP, PREPARE GRAVITY VECTOR COMPONENTS,             ELEMN3........7900
C        GXSI, GETA, AND GZET FOR CONSISTENT VELOCITIES,                 ELEMN3........8000
C        AND CHECK ELEMENT SHAPES                                        ELEMN3........8100
      IF(INTIM) 100,100,2000                                             ELEMN3........8200
  100 INTIM=1                                                            ELEMN3........8300
C.....LOOP THROUGH ALL ELEMENTS TO OBTAIN THE JACOBIAN                   ELEMN3........8400
C        AT EACH OF THE EIGHT NODES IN EACH ELEMENT                      ELEMN3........8500
      DO 1000 L=1,NE                                                     ELEMN3........8600
       DO 500 IL=1,8                                                     ELEMN3........8700
        XLOC=GXLOC(IL)                                                   ELEMN3........8800
        YLOC=GYLOC(IL)                                                   ELEMN3........8900
        ZLOC=GZLOC(IL)                                                   ELEMN3........9000
        CALL BASIS3(0000,L,XLOC,YLOC,ZLOC,IN,X,Y,Z,F(1,IL),W(1,IL),      ELEMN3........9100
     1     DET(IL),DFDXG(1,IL),DFDYG(1,IL),DFDZG(1,IL),                  ELEMN3........9200
     2     DWDXG(1,IL),DWDYG(1,IL),DWDZG(1,IL),PITER,UITER,PVEL,POR,     ELEMN3........9300
     3     VXG(IL),VYG(IL),VZG(IL),SWG(IL),RHOG(IL),VISCG(IL),           ELEMN3........9400
     4     PORG(IL),VGMAG(IL),RELKG(IL),PERMXX,PERMXY,PERMXZ,            ELEMN3........9500
     5     PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ,                    ELEMN3........9600
     6     CJ11,CJ12,CJ13,CJ21,CJ22,CJ23,CJ31,CJ32,CJ33,                 ELEMN3........9700
     7     GXSI,GETA,GZET,RCIT,RCITM1,RGXG(IL),RGYG(IL),RGZG(IL),LREG)   ELEMN3........9800
        GXSI(L,IL)=CJ11*GRAVX+CJ12*GRAVY+CJ13*GRAVZ                      ELEMN3........9900
        GETA(L,IL)=CJ21*GRAVX+CJ22*GRAVY+CJ23*GRAVZ                      ELEMN3.......10000
        GZET(L,IL)=CJ31*GRAVX+CJ32*GRAVY+CJ33*GRAVZ                      ELEMN3.......10100
C.....CHECK FOR NEGATIVE- OR ZERO-AREA ERRORS IN ELEMENT SHAPES          ELEMN3.......10200
        IF(DET(IL)) 200,200,500                                          ELEMN3.......10300
  200   ISTOP=ISTOP+1                                                    ELEMN3.......10400
        WRITE(K3,400) IN((L-1)*8+IL),L,DET(IL)                           ELEMN3.......10500
        WRITE(K00,401) IN((L-1)*8+IL),L,DET(IL)                          ELEMN3.......10600
  400   FORMAT(11X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,        ELEMN3.......10700
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN3.......10800
  401   FORMAT(1X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,         ELEMN3.......10900
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN3.......11000
  500  CONTINUE                                                          ELEMN3.......11100
 1000 CONTINUE                                                           ELEMN3.......11200
C                                                                        ELEMN3.......11300
      IF(ISTOP.EQ.0) GOTO 2000                                           ELEMN3.......11400
      ERRCOD = 'INP-14B,22-1'                                            ELEMN3.......11500
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           ELEMN3.......11600
C                                                                        ELEMN3.......11700
C.....LOOP THROUGH ALL ELEMENTS TO CARRY OUT SPATIAL INTEGRATION         ELEMN3.......11800
C        OF FLUX TERMS IN P AND/OR U EQUATIONS                           ELEMN3.......11900
 2000 IF(IUNSAT.NE.0) IUNSAT=2                                           ELEMN3.......12000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......12100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......12200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......12300
      DO 9999 L=1,NE                                                     ELEMN3.......12400
       XIX=-1.D0                                                         ELEMN3.......12500
       YIY=-1.D0                                                         ELEMN3.......12600
       ZIZ=-1.D0                                                         ELEMN3.......12700
       KG=0                                                              ELEMN3.......12800
C.....OBTAIN BASIS FUNCTION AND RELATED INFORMATION AT EACH OF           ELEMN3.......12900
C        EIGHT GAUSS POINTS IN THE ELEMENT                               ELEMN3.......13000
       DO 2250 IZL=1,2                                                   ELEMN3.......13100
        DO 2200 IYL=1,2                                                  ELEMN3.......13200
         DO 2100 IXL=1,2                                                 ELEMN3.......13300
          KG=KG+1                                                        ELEMN3.......13400
          XLOC=XIX*GLOC                                                  ELEMN3.......13500
          YLOC=YIY*GLOC                                                  ELEMN3.......13600
          ZLOC=ZIZ*GLOC                                                  ELEMN3.......13700
          CALL BASIS3(0001,L,XLOC,YLOC,ZLOC,IN,X,Y,Z,F(1,KG),W(1,KG),    ELEMN3.......13800
     1       DET(KG),DFDXG(1,KG),DFDYG(1,KG),DFDZG(1,KG),                ELEMN3.......13900
     2       DWDXG(1,KG),DWDYG(1,KG),DWDZG(1,KG),PITER,UITER,PVEL,POR,   ELEMN3.......14000
     3       VXG(KG),VYG(KG),VZG(KG),SWG(KG),RHOG(KG),VISCG(KG),         ELEMN3.......14100
     4       PORG(KG),VGMAG(KG),RELKG(KG),PERMXX,PERMXY,PERMXZ,          ELEMN3.......14200
     5       PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ,                  ELEMN3.......14300
     6       CJ11,CJ12,CJ13,CJ21,CJ22,CJ23,CJ31,CJ32,CJ33,               ELEMN3.......14400
     7       GXSI,GETA,GZET,RCIT,RCITM1,RGXG(KG),RGYG(KG),RGZG(KG),LREG) ELEMN3.......14500
 2100     XIX=-XIX                                                       ELEMN3.......14600
 2200    YIY=-YIY                                                        ELEMN3.......14700
 2250   ZIZ=-ZIZ                                                         ELEMN3.......14800
C                                                                        ELEMN3.......14900
C.....CALCULATE VELOCITY AT ELEMENT CENTROID WHEN REQUIRED               ELEMN3.......15000
       IF(KVCALC-2) 3000,2300,3000                                       ELEMN3.......15100
 2300  AXSUM=0.0D0                                                       ELEMN3.......15200
       AYSUM=0.0D0                                                       ELEMN3.......15300
       AZSUM=0.0D0                                                       ELEMN3.......15400
       DO 2400 KG=1,8                                                    ELEMN3.......15500
        AXSUM=AXSUM+VXG(KG)                                              ELEMN3.......15600
        AYSUM=AYSUM+VYG(KG)                                              ELEMN3.......15700
 2400   AZSUM=AZSUM+VZG(KG)                                              ELEMN3.......15800
       VMAG(L)=DSQRT(AXSUM*AXSUM+AYSUM*AYSUM+AZSUM*AZSUM)                ELEMN3.......15900
       IF (VMAG(L).NE.0D0) THEN                                          ELEMN3.......16000
          VANG2(L)=DASIN(AZSUM/VMAG(L))*5.729577951308232D+1             ELEMN3.......16100
          VMAG(L)=VMAG(L)*1.25D-1                                        ELEMN3.......16200
          VANG1(L)=DATAN2(AYSUM,AXSUM)*5.729577951308232D+1              ELEMN3.......16300
       ELSE                                                              ELEMN3.......16400
          VANG1(L)=0D0                                                   ELEMN3.......16500
          VANG2(L)=0D0                                                   ELEMN3.......16600
       END IF                                                            ELEMN3.......16700
C                                                                        ELEMN3.......16800
 3000  CONTINUE                                                          ELEMN3.......16900
C                                                                        ELEMN3.......17000
C.....CALCULATE PARAMETERS FOR FLUID MASS BALANCE AT GAUSS POINTS        ELEMN3.......17100
       IF(ML-1) 3400,3400,6100                                           ELEMN3.......17200
 3400  SWTEST=0.D0                                                       ELEMN3.......17300
       DO 4000 KG=1,8                                                    ELEMN3.......17400
        SWTEST=SWTEST+SWG(KG)                                            ELEMN3.......17500
        ROMG=RHOG(KG)*RELKG(KG)/VISCG(KG)                                ELEMN3.......17600
        RXXG(KG)=PERMXX(L)*ROMG                                          ELEMN3.......17700
        RXYG(KG)=PERMXY(L)*ROMG                                          ELEMN3.......17800
        RXZG(KG)=PERMXZ(L)*ROMG                                          ELEMN3.......17900
        RYXG(KG)=PERMYX(L)*ROMG                                          ELEMN3.......18000
        RYYG(KG)=PERMYY(L)*ROMG                                          ELEMN3.......18100
        RYZG(KG)=PERMYZ(L)*ROMG                                          ELEMN3.......18200
        RZXG(KG)=PERMZX(L)*ROMG                                          ELEMN3.......18300
        RZYG(KG)=PERMZY(L)*ROMG                                          ELEMN3.......18400
        RZZG(KG)=PERMZZ(L)*ROMG                                          ELEMN3.......18500
 4000   CONTINUE                                                         ELEMN3.......18600
C                                                                        ELEMN3.......18700
C.....INTEGRATE FLUID MASS BALANCE IN AN UNSATURATED ELEMENT             ELEMN3.......18800
C        USING ASYMMETRIC WEIGHTING FUNCTIONS                            ELEMN3.......18900
       IF(UP.LE.1.0D-6) GOTO 5200                                        ELEMN3.......19000
       IF(SWTEST-3.999D0) 4200,5200,5200                                 ELEMN3.......19100
 4200  DO 4300 I=1,8                                                     ELEMN3.......19200
        VOLE(I) = 0.D0                                                   ELEMN3.......19300
        DFLOWE(I) = 0.D0                                                 ELEMN3.......19400
        DO 4300 J=1,8                                                    ELEMN3.......19500
         BFLOWE(I,J) = 0.D0                                              ELEMN3.......19600
 4300  CONTINUE                                                          ELEMN3.......19700
       DO 5000 KG=1,8                                                    ELEMN3.......19800
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN3.......19900
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN3.......20000
        RXZGD = RXZG(KG)*DET(KG)                                         ELEMN3.......20100
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN3.......20200
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN3.......20300
        RYZGD = RYZG(KG)*DET(KG)                                         ELEMN3.......20400
        RZXGD = RZXG(KG)*DET(KG)                                         ELEMN3.......20500
        RZYGD = RZYG(KG)*DET(KG)                                         ELEMN3.......20600
        RZZGD = RZZG(KG)*DET(KG)                                         ELEMN3.......20700
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG) + RXZGD*RGZG(KG)          ELEMN3.......20800
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG) + RYZGD*RGZG(KG)          ELEMN3.......20900
        RDRZ = RZXGD*RGXG(KG) + RZYGD*RGYG(KG) + RZZGD*RGZG(KG)          ELEMN3.......21000
        DO 4400 I=1,8                                                    ELEMN3.......21100
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN3.......21200
         DFLOWE(I) = DFLOWE(I) + RDRX*DWDXG(I,KG) + RDRY*DWDYG(I,KG)     ELEMN3.......21300
     1               + RDRZ*DWDZG(I,KG)                                  ELEMN3.......21400
 4400   CONTINUE                                                         ELEMN3.......21500
        DO 5000 J=1,8                                                    ELEMN3.......21600
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN3.......21700
     1            + RXZGD*DFDZG(J,KG)                                    ELEMN3.......21800
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN3.......21900
     1            + RYZGD*DFDZG(J,KG)                                    ELEMN3.......22000
         RDDFJZ = RZXGD*DFDXG(J,KG) + RZYGD*DFDYG(J,KG)                  ELEMN3.......22100
     1            + RZZGD*DFDZG(J,KG)                                    ELEMN3.......22200
         DO 5000 I=1,8                                                   ELEMN3.......22300
          BFLOWE(I,J) = BFLOWE(I,J) + DWDXG(I,KG)*RDDFJX                 ELEMN3.......22400
     1                  + DWDYG(I,KG)*RDDFJY + DWDZG(I,KG)*RDDFJZ        ELEMN3.......22500
 5000  CONTINUE                                                          ELEMN3.......22600
       GOTO 6050                                                         ELEMN3.......22700
C                                                                        ELEMN3.......22800
C.....INTEGRATE FLUID MASS BALANCE IN A SATURATED OR UNSATURATED         ELEMN3.......22900
C        ELEMENT USING SYMMETRIC WEIGHTING FUNCTIONS                     ELEMN3.......23000
 5200  DO 5300 I=1,8                                                     ELEMN3.......23100
        VOLE(I) = 0.D0                                                   ELEMN3.......23200
        DFLOWE(I) = 0.D0                                                 ELEMN3.......23300
        DO 5300 J=1,8                                                    ELEMN3.......23400
         BFLOWE(I,J) = 0.D0                                              ELEMN3.......23500
 5300  CONTINUE                                                          ELEMN3.......23600
       DO 6000 KG=1,8                                                    ELEMN3.......23700
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN3.......23800
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN3.......23900
        RXZGD = RXZG(KG)*DET(KG)                                         ELEMN3.......24000
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN3.......24100
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN3.......24200
        RYZGD = RYZG(KG)*DET(KG)                                         ELEMN3.......24300
        RZXGD = RZXG(KG)*DET(KG)                                         ELEMN3.......24400
        RZYGD = RZYG(KG)*DET(KG)                                         ELEMN3.......24500
        RZZGD = RZZG(KG)*DET(KG)                                         ELEMN3.......24600
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG) + RXZGD*RGZG(KG)          ELEMN3.......24700
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG) + RYZGD*RGZG(KG)          ELEMN3.......24800
        RDRZ = RZXGD*RGXG(KG) + RZYGD*RGYG(KG) + RZZGD*RGZG(KG)          ELEMN3.......24900
        DO 5400 I=1,8                                                    ELEMN3.......25000
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN3.......25100
         DFLOWE(I) = DFLOWE(I) + RDRX*DFDXG(I,KG) + RDRY*DFDYG(I,KG)     ELEMN3.......25200
     1               + RDRZ*DFDZG(I,KG)                                  ELEMN3.......25300
 5400   CONTINUE                                                         ELEMN3.......25400
        DO 6000 J=1,8                                                    ELEMN3.......25500
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN3.......25600
     1            + RXZGD*DFDZG(J,KG)                                    ELEMN3.......25700
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN3.......25800
     1            + RYZGD*DFDZG(J,KG)                                    ELEMN3.......25900
         RDDFJZ = RZXGD*DFDXG(J,KG) + RZYGD*DFDYG(J,KG)                  ELEMN3.......26000
     1            + RZZGD*DFDZG(J,KG)                                    ELEMN3.......26100
         DO 6000 I=1,8                                                   ELEMN3.......26200
          BFLOWE(I,J) = BFLOWE(I,J) + DFDXG(I,KG)*RDDFJX                 ELEMN3.......26300
     1                  + DFDYG(I,KG)*RDDFJY + DFDZG(I,KG)*RDDFJZ        ELEMN3.......26400
 6000  CONTINUE                                                          ELEMN3.......26500
 6050  CONTINUE                                                          ELEMN3.......26600
       IF(ML-1) 6100,9000,6100                                           ELEMN3.......26700
 6100  IF(NOUMAT.EQ.1) GOTO 9000                                         ELEMN3.......26800
C                                                                        ELEMN3.......26900
C                                                                        ELEMN3.......27000
C.....CALCULATE PARAMETERS FOR ENERGY BALANCE OR SOLUTE MASS BALANCE     ELEMN3.......27100
C        AT GAUSS POINTS                                                 ELEMN3.......27200
       DO 7000 KG=1,8                                                    ELEMN3.......27300
        ESWG=PORG(KG)*SWG(KG)                                            ELEMN3.......27400
        RHOCWG=RHOG(KG)*CW                                               ELEMN3.......27500
        ESRCG=ESWG*RHOCWG                                                ELEMN3.......27600
        IF(VGMAG(KG)) 6300,6300,6600                                     ELEMN3.......27700
 6300   EXG(KG)=0.0D0                                                    ELEMN3.......27800
        EYG(KG)=0.0D0                                                    ELEMN3.......27900
        EZG(KG)=0.0D0                                                    ELEMN3.......28000
        DXXG=0.0D0                                                       ELEMN3.......28100
        DXYG=0.0D0                                                       ELEMN3.......28200
        DXZG=0.0D0                                                       ELEMN3.......28300
        DYXG=0.0D0                                                       ELEMN3.......28400
        DYYG=0.0D0                                                       ELEMN3.......28500
        DYZG=0.0D0                                                       ELEMN3.......28600
        DZXG=0.0D0                                                       ELEMN3.......28700
        DZYG=0.0D0                                                       ELEMN3.......28800
        DZZG=0.0D0                                                       ELEMN3.......28900
        GOTO 6900                                                        ELEMN3.......29000
 6600   EXG(KG)=ESRCG*VXG(KG)                                            ELEMN3.......29100
        EYG(KG)=ESRCG*VYG(KG)                                            ELEMN3.......29200
        EZG(KG)=ESRCG*VZG(KG)                                            ELEMN3.......29300
C                                                                        ELEMN3.......29400
C                                                                        ELEMN3.......29500
C.....DISPERSIVITY MODEL FOR 3D ANISOTROPIC MEDIA                        ELEMN3.......29600
C        WITH PRINCIPAL DISPERSIVITIES ALMAX, ALMID, ALMIN,              ELEMN3.......29700
C        ATMAX, ATMID, ATMIN                                             ELEMN3.......29800
        CALL DISPR3(VXG(KG),VYG(KG),VZG(KG),VGMAG(KG),PANGL1(L),         ELEMN3.......29900
     1     PANGL2(L),PANGL3(L),ALMAX(L),ALMID(L),ALMIN(L),               ELEMN3.......30000
     2     ATMAX(L),ATMID(L),ATMIN(L),DXXG,DXYG,DXZG,DYXG,DYYG,DYZG,     ELEMN3.......30100
     3     DZXG,DZYG,DZZG)                                               ELEMN3.......30200
C                                                                        ELEMN3.......30300
C.....IN-PARALLEL CONDUCTIVITIES (DIFFUSIVITIES) FORMULA                 ELEMN3.......30400
 6900   IF (ME.EQ.1) THEN                                                ELEMN3.......30500
C..........FOR ENERGY TRANSPORT:                                         ELEMN3.......30600
           ESE = ESWG*SIGMAW + (1D0-PORG(KG))*SIGMAS                     ELEMN3.......30700
        ELSE                                                             ELEMN3.......30800
C..........FOR SOLUTE TRANSPORT:                                         ELEMN3.......30900
           ESE = ESRCG*SIGMAW + (1D0-PORG(KG))*RHOCWG*SIGMAS             ELEMN3.......31000
        END IF                                                           ELEMN3.......31100
C.....ADD DIFFUSION AND DISPERSION TERMS TO TOTAL DISPERSION TENSOR      ELEMN3.......31200
        BXXG(KG)=ESRCG*DXXG+ESE                                          ELEMN3.......31300
        BXYG(KG)=ESRCG*DXYG                                              ELEMN3.......31400
        BXZG(KG)=ESRCG*DXZG                                              ELEMN3.......31500
        BYXG(KG)=ESRCG*DYXG                                              ELEMN3.......31600
        BYYG(KG)=ESRCG*DYYG+ESE                                          ELEMN3.......31700
        BYZG(KG)=ESRCG*DYZG                                              ELEMN3.......31800
        BZXG(KG)=ESRCG*DZXG                                              ELEMN3.......31900
        BZYG(KG)=ESRCG*DZYG                                              ELEMN3.......32000
 7000   BZZG(KG)=ESRCG*DZZG+ESE                                          ELEMN3.......32100
C                                                                        ELEMN3.......32200
C.....INTEGRATE SOLUTE MASS BALANCE OR ENERGY BALANCE                    ELEMN3.......32300
C        USING SYMMETRIC WEIGHTING FUNCTIONS FOR DISPERSION TERM AND     ELEMN3.......32400
C        USING EITHER SYMMETRIC OR ASYMMETRIC WEIGHTING FUNCTIONS        ELEMN3.......32500
C        FOR ADVECTION TERM                                              ELEMN3.......32600
         DO 7400 I=1,8                                                   ELEMN3.......32700
         DO 7400 J=1,8                                                   ELEMN3.......32800
            BTRANE(I,J) = 0.D0                                           ELEMN3.......32900
            DTRANE(I,J) = 0.D0                                           ELEMN3.......33000
 7400    CONTINUE                                                        ELEMN3.......33100
         DO 8000 KG=1,8                                                  ELEMN3.......33200
            BXXGD = BXXG(KG)*DET(KG)                                     ELEMN3.......33300
            BXYGD = BXYG(KG)*DET(KG)                                     ELEMN3.......33400
            BXZGD = BXZG(KG)*DET(KG)                                     ELEMN3.......33500
            BYXGD = BYXG(KG)*DET(KG)                                     ELEMN3.......33600
            BYYGD = BYYG(KG)*DET(KG)                                     ELEMN3.......33700
            BYZGD = BYZG(KG)*DET(KG)                                     ELEMN3.......33800
            BZXGD = BZXG(KG)*DET(KG)                                     ELEMN3.......33900
            BZYGD = BZYG(KG)*DET(KG)                                     ELEMN3.......34000
            BZZGD = BZZG(KG)*DET(KG)                                     ELEMN3.......34100
            EXGD = EXG(KG)*DET(KG)                                       ELEMN3.......34200
            EYGD = EYG(KG)*DET(KG)                                       ELEMN3.......34300
            EZGD = EZG(KG)*DET(KG)                                       ELEMN3.......34400
            DO 8000 J=1,8                                                ELEMN3.......34500
               BDDFJX = BXXGD*DFDXG(J,KG) + BXYGD*DFDYG(J,KG)            ELEMN3.......34600
     1                 + BXZGD*DFDZG(J,KG)                               ELEMN3.......34700
               BDDFJY = BYXGD*DFDXG(J,KG) + BYYGD*DFDYG(J,KG)            ELEMN3.......34800
     1                 + BYZGD*DFDZG(J,KG)                               ELEMN3.......34900
               BDDFJZ = BZXGD*DFDXG(J,KG) + BZYGD*DFDYG(J,KG)            ELEMN3.......35000
     1                 + BZZGD*DFDZG(J,KG)                               ELEMN3.......35100
               EDDFJ = EXGD*DFDXG(J,KG) + EYGD*DFDYG(J,KG)               ELEMN3.......35200
     1                 + EZGD*DFDZG(J,KG)                                ELEMN3.......35300
               DO 8000 I=1,8                                             ELEMN3.......35400
                  BTRANE(I,J) = BTRANE(I,J) + DFDXG(I,KG)*BDDFJX         ELEMN3.......35500
     1               + DFDYG(I,KG)*BDDFJY + DFDZG(I,KG)*BDDFJZ           ELEMN3.......35600
                  DTRANE(I,J) = DTRANE(I,J) + EDDFJ*W(I,KG)              ELEMN3.......35700
 8000    CONTINUE                                                        ELEMN3.......35800
 9000  CONTINUE                                                          ELEMN3.......35900
C                                                                        ELEMN3.......36000
C                                                                        ELEMN3.......36100
C.....SEND RESULTS OF INTEGRATIONS FOR THIS ELEMENT                      ELEMN3.......36200
C        TO GLOBAL ASSEMBLY ROUTINE:                                     ELEMN3.......36300
C        GLOBAN -- SUTRA'S ORIGINAL BANDED FORMAT                        ELEMN3.......36400
C        GLOCOL -- SLAP COLUMN FORMAT                                    ELEMN3.......36500
      IF (KSOLVP.EQ.0) THEN                                              ELEMN3.......36600
         CALL GLOBAN(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN3.......36700
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC)                                  ELEMN3.......36800
      ELSE                                                               ELEMN3.......36900
         CALL GLOCOL(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN3.......37000
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC,IA,JA)                            ELEMN3.......37100
      END IF                                                             ELEMN3.......37200
 9999 CONTINUE                                                           ELEMN3.......37300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......37400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......37500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......37600
C                                                                        ELEMN3.......37700
C                                                                        ELEMN3.......37800
      RETURN                                                             ELEMN3.......37900
      END                                                                ELEMN3.......38000
C                                                                        ELEMN3.......38100
C     SUBROUTINE        F  I  N  D  L  2           SUTRA VERSION 2.1     FINDL2.........100
C                                                                        FINDL2.........200
C *** PURPOSE :                                                          FINDL2.........300
C ***  TO DETERMINE WHETHER POINT (XK, YK) IN 2D GLOBAL COORDINATES      FINDL2.........400
C ***  IS CONTAINED WITHIN ELEMENT LL.  IF THE POINT IS INSIDE THE       FINDL2.........500
C ***  ELEMENT, SET INOUT = 1; IF OUTSIDE, SET INOUT = 0.  CONDITION     FINDL2.........600
C ***  INOUT = 99 SIGNALS CONVERGENCE FAILURE.  ADAPTED FROM SUTRAPLOT   FINDL2.........700
C ***  SUBROUTINE ITER2D.                                                FINDL2.........800
C                                                                        FINDL2.........900
      SUBROUTINE FINDL2(X,Y,IN,LL,XK,YK,XSI,ETA,INOUT)                   FINDL2........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FINDL2........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              FINDL2........1200
     1   NSOP,NSOU,NBCN                                                  FINDL2........1300
      DIMENSION IN(NE*4)                                                 FINDL2........1400
      DIMENSION X(NN), Y(NN)                                             FINDL2........1500
      DATA TOL /0.001/, ITRMAX /25/, EPSILON /0.001/                     FINDL2........1600
C                                                                        FINDL2........1700
C.....DEFINE OPE = (1. + EPSILON) FOR CONVENIENCE.                       FINDL2........1800
      OPE = 1D0 + EPSILON                                                FINDL2........1900
C                                                                        FINDL2........2000
C.....SET CORNER COORDINATES.                                            FINDL2........2100
      M0 = (LL - 1)*4                                                    FINDL2........2200
      X1 = X(IN(M0+1))                                                   FINDL2........2300
      X2 = X(IN(M0+2))                                                   FINDL2........2400
      X3 = X(IN(M0+3))                                                   FINDL2........2500
      X4 = X(IN(M0+4))                                                   FINDL2........2600
      Y1 = Y(IN(M0+1))                                                   FINDL2........2700
      Y2 = Y(IN(M0+2))                                                   FINDL2........2800
      Y3 = Y(IN(M0+3))                                                   FINDL2........2900
      Y4 = Y(IN(M0+4))                                                   FINDL2........3000
C                                                                        FINDL2........3100
C.....CALCULATE COEFFICIENTS.                                            FINDL2........3200
      AX = +X1+X2+X3+X4                                                  FINDL2........3300
      BX = -X1+X2+X3-X4                                                  FINDL2........3400
      CX = -X1-X2+X3+X4                                                  FINDL2........3500
      DX = +X1-X2+X3-X4                                                  FINDL2........3600
      AY = +Y1+Y2+Y3+Y4                                                  FINDL2........3700
      BY = -Y1+Y2+Y3-Y4                                                  FINDL2........3800
      CY = -Y1-Y2+Y3+Y4                                                  FINDL2........3900
      DY = +Y1-Y2+Y3-Y4                                                  FINDL2........4000
                                                                         FINDL2........4100
C                                                                        FINDL2........4200
C.....INITIAL GUESS OF ZERO FOR XSI AND ETA.                             FINDL2........4300
      XSI=0.0                                                            FINDL2........4400
      ETA=0.0                                                            FINDL2........4500
C                                                                        FINDL2........4600
C.....ITERATION LOOP TO SOLVE FOR LOCAL COORDINATES.                     FINDL2........4700
C                                                                        FINDL2........4800
      DO 800 I=1,ITRMAX                                                  FINDL2........4900
C                                                                        FINDL2........5000
         F10 = AX - 4.*XK + BX*XSI + CX*ETA + DX*XSI*ETA                 FINDL2........5100
         F20 = AY - 4.*YK + BY*XSI + CY*ETA + DY*XSI*ETA                 FINDL2........5200
         FP11 = BX + DX*ETA                                              FINDL2........5300
         FP12 = CX + DX*XSI                                              FINDL2........5400
         FP21 = BY + DY*ETA                                              FINDL2........5500
         FP22 = CY + DY*XSI                                              FINDL2........5600
C                                                                        FINDL2........5700
         DETXSI = -F10*FP22 + F20*FP12                                   FINDL2........5800
         DETETA = -F20*FP11 + F10*FP21                                   FINDL2........5900
         DETERM = FP11*FP22 - FP12*FP21                                  FINDL2........6000
         DELXSI = DETXSI/DETERM                                          FINDL2........6100
         DELETA = DETETA/DETERM                                          FINDL2........6200
C                                                                        FINDL2........6300
         XSI = XSI + DELXSI                                              FINDL2........6400
         ETA = ETA + DELETA                                              FINDL2........6500
C                                                                        FINDL2........6600
C........STOP ITERATING IF CHANGE IN XSI AND ETA < TOL.                  FINDL2........6700
         IF ((ABS(DELXSI).LT.TOL).AND.(ABS(DELETA).LT.TOL)) GOTO 900     FINDL2........6800
C                                                                        FINDL2........6900
  800 CONTINUE                                                           FINDL2........7000
C                                                                        FINDL2........7100
C.....ITERATONS FAILED TO CONVERGE.  SET INOUT = 99 AND RETURN.          FINDL2........7200
      INOUT = 99                                                         FINDL2........7300
      GOTO 1000                                                          FINDL2........7400
C                                                                        FINDL2........7500
C.....ITERATIONS CONVERGED.  IF POINT IS INSIDE THE ELEMENT,             FINDL2........7600
C        SET INOUT = 1.  IF OUTSIDE, SET INOUT = 0.                      FINDL2........7700
  900 INOUT = 1                                                          FINDL2........7800
      IF ((ABS(XSI).GT.OPE).OR.(ABS(ETA).GT.OPE)) INOUT = 0              FINDL2........7900
C                                                                        FINDL2........8000
 1000 RETURN                                                             FINDL2........8100
      END                                                                FINDL2........8200
C                                                                        FINDL2........8300
C     SUBROUTINE        F  I  N  D  L  3           SUTRA VERSION 2.1     FINDL3.........100
C                                                                        FINDL3.........200
C *** PURPOSE :                                                          FINDL3.........300
C ***  TO DETERMINE WHETHER POINT (XK, YK, ZK) IN 3D GLOBAL COORDINATES  FINDL3.........400
C ***  IS CONTAINED WITHIN ELEMENT LL.  IF THE POINT IS INSIDE THE       FINDL3.........500
C ***  ELEMENT, SET INOUT = 1; IF OUTSIDE, SET INOUT = 0.  CONDITION     FINDL3.........600
C ***  INOUT = 99 SIGNALS CONVERGENCE FAILURE.  ADAPTED FROM SUTRAPLOT   FINDL3.........700
C ***  SUBROUTINE ITER3D.                                                FINDL3.........800
C                                                                        FINDL3.........900
      SUBROUTINE FINDL3(X,Y,Z,IN,LL,XK,YK,ZK,XSI,ETA,ZET,INOUT)          FINDL3........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FINDL3........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              FINDL3........1200
     1   NSOP,NSOU,NBCN                                                  FINDL3........1300
      DIMENSION IN(NE*8)                                                 FINDL3........1400
      DIMENSION X(NN), Y(NN), Z(NN)                                      FINDL3........1500
      DATA TOL /0.001/, ITRMAX /25/, EPSILON /0.001/                     FINDL3........1600
C                                                                        FINDL3........1700
C.....DEFINE OPE = (1. + EPSILON) FOR CONVENIENCE.                       FINDL3........1800
      OPE = 1D0 + EPSILON                                                FINDL3........1900
C                                                                        FINDL3........2000
C.....SET CORNER COORDINATES.                                            FINDL3........2100
      M0 = (LL - 1)*8                                                    FINDL3........2200
      X1 = X(IN(M0+1))                                                   FINDL3........2300
      X2 = X(IN(M0+2))                                                   FINDL3........2400
      X3 = X(IN(M0+3))                                                   FINDL3........2500
      X4 = X(IN(M0+4))                                                   FINDL3........2600
      X5 = X(IN(M0+5))                                                   FINDL3........2700
      X6 = X(IN(M0+6))                                                   FINDL3........2800
      X7 = X(IN(M0+7))                                                   FINDL3........2900
      X8 = X(IN(M0+8))                                                   FINDL3........3000
      Y1 = Y(IN(M0+1))                                                   FINDL3........3100
      Y2 = Y(IN(M0+2))                                                   FINDL3........3200
      Y3 = Y(IN(M0+3))                                                   FINDL3........3300
      Y4 = Y(IN(M0+4))                                                   FINDL3........3400
      Y5 = Y(IN(M0+5))                                                   FINDL3........3500
      Y6 = Y(IN(M0+6))                                                   FINDL3........3600
      Y7 = Y(IN(M0+7))                                                   FINDL3........3700
      Y8 = Y(IN(M0+8))                                                   FINDL3........3800
      Z1 = Z(IN(M0+1))                                                   FINDL3........3900
      Z2 = Z(IN(M0+2))                                                   FINDL3........4000
      Z3 = Z(IN(M0+3))                                                   FINDL3........4100
      Z4 = Z(IN(M0+4))                                                   FINDL3........4200
      Z5 = Z(IN(M0+5))                                                   FINDL3........4300
      Z6 = Z(IN(M0+6))                                                   FINDL3........4400
      Z7 = Z(IN(M0+7))                                                   FINDL3........4500
      Z8 = Z(IN(M0+8))                                                   FINDL3........4600
C                                                                        FINDL3........4700
C.....CALCULATE COEFFICIENTS.                                            FINDL3........4800
      AX = +X1+X2+X3+X4+X5+X6+X7+X8                                      FINDL3........4900
      BX = -X1+X2+X3-X4-X5+X6+X7-X8                                      FINDL3........5000
      CX = -X1-X2+X3+X4-X5-X6+X7+X8                                      FINDL3........5100
      DX = -X1-X2-X3-X4+X5+X6+X7+X8                                      FINDL3........5200
      EX = +X1-X2+X3-X4+X5-X6+X7-X8                                      FINDL3........5300
      FX = +X1-X2-X3+X4-X5+X6+X7-X8                                      FINDL3........5400
      GX = +X1+X2-X3-X4-X5-X6+X7+X8                                      FINDL3........5500
      HX = -X1+X2-X3+X4+X5-X6+X7-X8                                      FINDL3........5600
      AY = +Y1+Y2+Y3+Y4+Y5+Y6+Y7+Y8                                      FINDL3........5700
      BY = -Y1+Y2+Y3-Y4-Y5+Y6+Y7-Y8                                      FINDL3........5800
      CY = -Y1-Y2+Y3+Y4-Y5-Y6+Y7+Y8                                      FINDL3........5900
      DY = -Y1-Y2-Y3-Y4+Y5+Y6+Y7+Y8                                      FINDL3........6000
      EY = +Y1-Y2+Y3-Y4+Y5-Y6+Y7-Y8                                      FINDL3........6100
      FY = +Y1-Y2-Y3+Y4-Y5+Y6+Y7-Y8                                      FINDL3........6200
      GY = +Y1+Y2-Y3-Y4-Y5-Y6+Y7+Y8                                      FINDL3........6300
      HY = -Y1+Y2-Y3+Y4+Y5-Y6+Y7-Y8                                      FINDL3........6400
      AZ = +Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8                                      FINDL3........6500
      BZ = -Z1+Z2+Z3-Z4-Z5+Z6+Z7-Z8                                      FINDL3........6600
      CZ = -Z1-Z2+Z3+Z4-Z5-Z6+Z7+Z8                                      FINDL3........6700
      DZ = -Z1-Z2-Z3-Z4+Z5+Z6+Z7+Z8                                      FINDL3........6800
      EZ = +Z1-Z2+Z3-Z4+Z5-Z6+Z7-Z8                                      FINDL3........6900
      FZ = +Z1-Z2-Z3+Z4-Z5+Z6+Z7-Z8                                      FINDL3........7000
      GZ = +Z1+Z2-Z3-Z4-Z5-Z6+Z7+Z8                                      FINDL3........7100
      HZ = -Z1+Z2-Z3+Z4+Z5-Z6+Z7-Z8                                      FINDL3........7200
C                                                                        FINDL3........7300
C.....INITIAL GUESS OF ZERO FOR XSI, ETA, AND ZETA.                      FINDL3........7400
      XSI=0.0                                                            FINDL3........7500
      ETA=0.0                                                            FINDL3........7600
      ZET=0.0                                                            FINDL3........7700
C                                                                        FINDL3........7800
C.....ITERATION LOOP TO SOLVE FOR LOCAL COORDINATES.                     FINDL3........7900
C                                                                        FINDL3........8000
      DO 800 I=1,ITRMAX                                                  FINDL3........8100
C                                                                        FINDL3........8200
         F10 = AX - 8.*XK + BX*XSI + CX*ETA + DX*ZET + EX*XSI*ETA        FINDL3........8300
     1        + FX*XSI*ZET + GX*ETA*ZET + HX*XSI*ETA*ZET                 FINDL3........8400
         F20 = AY - 8.*YK + BY*XSI + CY*ETA + DY*ZET + EY*XSI*ETA        FINDL3........8500
     1        + FY*XSI*ZET + GY*ETA*ZET + HY*XSI*ETA*ZET                 FINDL3........8600
         F30 = AZ - 8.*ZK + BZ*XSI + CZ*ETA + DZ*ZET + EZ*XSI*ETA        FINDL3........8700
     1        + FZ*XSI*ZET + GZ*ETA*ZET + HZ*XSI*ETA*ZET                 FINDL3........8800
         FP11 = BX + EX*ETA + FX*ZET + HX*ETA*ZET                        FINDL3........8900
         FP12 = CX + EX*XSI + GX*ZET + HX*XSI*ZET                        FINDL3........9000
         FP13 = DX + FX*XSI + GX*ETA + HX*XSI*ETA                        FINDL3........9100
         FP21 = BY + EY*ETA + FY*ZET + HY*ETA*ZET                        FINDL3........9200
         FP22 = CY + EY*XSI + GY*ZET + HY*XSI*ZET                        FINDL3........9300
         FP23 = DY + FY*XSI + GY*ETA + HY*XSI*ETA                        FINDL3........9400
         FP31 = BZ + EZ*ETA + FZ*ZET + HZ*ETA*ZET                        FINDL3........9500
         FP32 = CZ + EZ*XSI + GZ*ZET + HZ*XSI*ZET                        FINDL3........9600
         FP33 = DZ + FZ*XSI + GZ*ETA + HZ*XSI*ETA                        FINDL3........9700
C                                                                        FINDL3........9800
         S11 = FP22*FP33 - FP32*FP23                                     FINDL3........9900
         S12 = FP21*FP33 - FP31*FP23                                     FINDL3.......10000
         S13 = FP21*FP32 - FP31*FP22                                     FINDL3.......10100
         CF12 = -F20*FP33 + F30*FP23                                     FINDL3.......10200
         CF34 = -F20*FP32 + F30*FP22                                     FINDL3.......10300
         CF43 = -CF34                                                    FINDL3.......10400
         CF56 = -F30*FP21 + F20*FP31                                     FINDL3.......10500
C                                                                        FINDL3.......10600
         DETXSI = -F10*S11 - FP12*CF12 + FP13*CF34                       FINDL3.......10700
         DETETA = FP11*CF12 + F10*S12 + FP13*CF56                        FINDL3.......10800
         DETZET = FP11*CF43 - FP12*CF56 - F10*S13                        FINDL3.......10900
         DETERM = FP11*S11 - FP12*S12 + FP13*S13                         FINDL3.......11000
         DELXSI = DETXSI/DETERM                                          FINDL3.......11100
         DELETA = DETETA/DETERM                                          FINDL3.......11200
         DELZET = DETZET/DETERM                                          FINDL3.......11300
C                                                                        FINDL3.......11400
         XSI = XSI + DELXSI                                              FINDL3.......11500
         ETA = ETA + DELETA                                              FINDL3.......11600
         ZET = ZET + DELZET                                              FINDL3.......11700
C                                                                        FINDL3.......11800
C........STOP ITERATING IF CHANGE IN XSI, ETA, AND ZETA < TOL.           FINDL3.......11900
         IF ((ABS(DELXSI).LT.TOL).AND.(ABS(DELETA).LT.TOL).AND.          FINDL3.......12000
     1       (ABS(DELZET).LT.TOL)) GOTO 900                              FINDL3.......12100
C                                                                        FINDL3.......12200
  800 CONTINUE                                                           FINDL3.......12300
C                                                                        FINDL3.......12400
C.....ITERATONS FAILED TO CONVERGE.  SET INOUT = 99 AND RETURN.          FINDL3.......12500
      INOUT = 99                                                         FINDL3.......12600
      GOTO 1000                                                          FINDL3.......12700
C                                                                        FINDL3.......12800
C.....ITERATIONS CONVERGED.  IF POINT IS INSIDE THE ELEMENT,             FINDL3.......12900
C        SET INOUT = 1.  IF OUTSIDE, SET INOUT = 0.                      FINDL3.......13000
  900 INOUT = 1                                                          FINDL3.......13100
      IF ((ABS(XSI).GT.OPE).OR.(ABS(ETA).GT.OPE).OR.(ABS(ZET).GT.OPE))   FINDL3.......13200
     1   INOUT = 0                                                       FINDL3.......13300
C                                                                        FINDL3.......13400
 1000 RETURN                                                             FINDL3.......13500
      END                                                                FINDL3.......13600
C                                                                        FINDL3.......13700
C     SUBROUTINE        F  O  P  E  N              SUTRA VERSION 2.1     FOPEN..........100
C                                                                        FOPEN..........200
C *** PURPOSE :                                                          FOPEN..........300
C ***  OPENS FILES FOR SUTRA SIMULATION.  READS AND PROCESSES FILE       FOPEN..........400
C ***  SPECIFICATIONS FROM "SUTRA.FIL" AND OPENS INPUT AND OUTPUT FILES. FOPEN..........500
C                                                                        FOPEN..........600
      SUBROUTINE FOPEN()                                                 FOPEN..........700
      USE EXPINT                                                         FOPEN..........800
      USE SCHDEF                                                         FOPEN..........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FOPEN.........1000
      PARAMETER (IUNMIN=11)                                              FOPEN.........1100
      CHARACTER*80 FT,FN,UNAME,FNAME,ENAME,ENDEF,FTSTR                   FOPEN.........1200
      CHARACTER*80 FNROOT,FNEXTN                                         FOPEN.........1300
      CHARACTER*80 ERRCOD,CHERR(10)                                      FOPEN.........1400
      CHARACTER*8 VERNUM, VERNIN                                         FOPEN.........1500
      CHARACTER INTFIL*1000                                              FOPEN.........1600
      LOGICAL IS                                                         FOPEN.........1700
      LOGICAL EXST                                                       FOPEN.........1800
      LOGICAL ONCEFO                                                     FOPEN.........1900
      DIMENSION FTYPE(0:8),FNAME(0:8),IUNIT(0:8)                         FOPEN.........2000
      DIMENSION FTSTR(0:8)                                               FOPEN.........2100
      DIMENSION INERR(10),RLERR(10)                                      FOPEN.........2200
      COMMON /FNAMES/ UNAME,FNAME                                        FOPEN.........2300
      COMMON /FO/ONCEFO                                                  FOPEN.........2400
      COMMON /FUNITA/ IUNIT                                              FOPEN.........2500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     FOPEN.........2600
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      FOPEN.........2700
      COMMON /SCH/ NSCH,ISCHTS                                           FOPEN.........2800
      COMMON /VER/ VERNUM, VERNIN                                        FOPEN.........2900
      DATA (FTSTR(NFT),NFT=0,8)/'SMY','INP','ICS','LST','RST',           FOPEN.........3000
     1   'NOD','ELE','OBS','OBC'/                                        FOPEN.........3100
C                                                                        FOPEN.........3200
C.....IF THIS IS THE FIRST PASS, READ AND PROCESS FILE SPECIFICATIONS    FOPEN.........3300
C        FROM "SUTRA.FIL" AND OPEN ALL OUTPUT FILES EXCEPT OBSERVATION   FOPEN.........3400
C        OUTPUT.  OBSERVATION OUTPUT FILES ARE CREATED ON THE SECOND     FOPEN.........3500
C        PASS, AFTER DATASET 8D HAS BEEN READ.                           FOPEN.........3600
      IF (.NOT.ONCEFO) THEN                                              FOPEN.........3700
C                                                                        FOPEN.........3800
C........INITIALIZE UNIT NUMBERS AND FILENAMES                           FOPEN.........3900
         K1 = -1                                                         FOPEN.........4000
         K2 = -1                                                         FOPEN.........4100
         K3 = -1                                                         FOPEN.........4200
         K4 = -1                                                         FOPEN.........4300
         K5 = -1                                                         FOPEN.........4400
         K6 = -1                                                         FOPEN.........4500
         K7 = -1                                                         FOPEN.........4600
         K8 = -1                                                         FOPEN.........4700
         DO 20 NF=0,8                                                    FOPEN.........4800
            IUNIT(NF) = -1                                               FOPEN.........4900
            FNAME(NF) = ""                                               FOPEN.........5000
   20    CONTINUE                                                        FOPEN.........5100
C                                                                        FOPEN.........5200
C........SET DEFAULT VALUES FOR THE SMY FILE.  THE DEFAULT FILE WILL     FOPEN.........5300
C           NOT ACTUALLY BE CREATED UNLESS IT IS NEEDED.                 FOPEN.........5400
         K00 = K0 + 1                                                    FOPEN.........5500
         ENDEF = 'SUTRA.SMY'                                             FOPEN.........5600
C                                                                        FOPEN.........5700
C........OPEN FILE UNIT CONTAINING UNIT NUMBERS AND FILE ASSIGNMENTS     FOPEN.........5800
         IU=K0                                                           FOPEN.........5900
         FN=UNAME                                                        FOPEN.........6000
         INQUIRE(FILE=UNAME,EXIST=IS)                                    FOPEN.........6100
         IF (IS) THEN                                                    FOPEN.........6200
            OPEN(UNIT=IU,FILE=UNAME,STATUS='OLD',FORM='FORMATTED',       FOPEN.........6300
     1         IOSTAT=KERR)                                              FOPEN.........6400
            IF(KERR.GT.0) GOTO 9000                                      FOPEN.........6500
         ELSE                                                            FOPEN.........6600
            CALL NAFU(K00,0,ENDEF)                                       FOPEN.........6700
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN.........6800
            GOTO 8000                                                    FOPEN.........6900
         ENDIF                                                           FOPEN.........7000
C                                                                        FOPEN.........7100
C........READ IN UNIT NUMBERS AND FILE ASSIGNMENTS.  ASSIGN COMPATIBLE   FOPEN.........7200
C           UNIT NUMBERS.  CLOSE UNIT K0.                                FOPEN.........7300
         NFILE = 0                                                       FOPEN.........7400
         DO 90 NF=0,8                                                    FOPEN.........7500
C...........READ A FILE SPECIFICATION                                    FOPEN.........7600
            READ(K0,'(A)',IOSTAT=INERR(1),END=99) INTFIL                 FOPEN.........7700
            IF (INERR(1).NE.0) THEN                                      FOPEN.........7800
               CALL NAFU(K00,0,ENDEF)                                    FOPEN.........7900
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN.........8000
               ERRCOD = 'REA-FIL'                                        FOPEN.........8100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  FOPEN.........8200
            END IF                                                       FOPEN.........8300
            IF (VERIFY(INTFIL,' ').EQ.0) GOTO 99                         FOPEN.........8400
            READ(INTFIL,*,IOSTAT=INERR(1)) FT, IU, FN                    FOPEN.........8500
            IF (INERR(1).NE.0) THEN                                      FOPEN.........8600
               CALL NAFU(K00,0,ENDEF)                                    FOPEN.........8700
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN.........8800
               ERRCOD = 'REA-FIL'                                        FOPEN.........8900
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  FOPEN.........9000
            END IF                                                       FOPEN.........9100
C...........CHECK FOR ILLEGAL SPECIFICATIONS                             FOPEN.........9200
            IF (FN.EQ.UNAME) THEN                                        FOPEN.........9300
               CALL NAFU(K00,0,ENDEF)                                    FOPEN.........9400
               OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                FOPEN.........9500
               ERRCOD = 'FIL-9'                                          FOPEN.........9600
               CHERR(1) = UNAME                                          FOPEN.........9700
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  FOPEN.........9800
            END IF                                                       FOPEN.........9900
C...........IF THE SPECIFIED UNIT NUMBER IS LESS THAN IUNMIN,            FOPEN........10000
C              SET IT TO IUNMIN                                          FOPEN........10100
            IU = MAX(IU, IUNMIN)                                         FOPEN........10200
C...........STORE THE FILE INFORMATION, CHECKING FOR INVALID AND         FOPEN........10300
C              REPEATED FILE TYPE SPECIFICATIONS AND ASSIGNING UNIT      FOPEN........10400
C              NUMBERS TO NON-OBSERVATION FILES ALONG THE WAY            FOPEN........10500
            DO 50 NFT=0,8                                                FOPEN........10600
               IF (FT.EQ.FTSTR(NFT)) THEN                                FOPEN........10700
                  IF (IUNIT(NFT).EQ.-1) THEN                             FOPEN........10800
                     IF (NFT.LE.6) CALL NAFU(IU,0,FN)                    FOPEN........10900
                     IUNIT(NFT) = IU                                     FOPEN........11000
                     FNAME(NFT) = FN                                     FOPEN........11100
                     GOTO 60                                             FOPEN........11200
                  ELSE                                                   FOPEN........11300
                     CALL NAFU(K00,0,ENDEF)                              FOPEN........11400
                     OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')          FOPEN........11500
                     ERRCOD = 'FIL-6'                                    FOPEN........11600
                     CHERR(1) = UNAME                                    FOPEN........11700
                     CHERR(2) = FT                                       FOPEN........11800
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)            FOPEN........11900
                  END IF                                                 FOPEN........12000
               END IF                                                    FOPEN........12100
   50       CONTINUE                                                     FOPEN........12200
            CALL NAFU(K00,0,ENDEF)                                       FOPEN........12300
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN........12400
            ERRCOD = 'FIL-5'                                             FOPEN........12500
            CHERR(1) = UNAME                                             FOPEN........12600
            CHERR(2) = FT                                                FOPEN........12700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FOPEN........12800
   60       CONTINUE                                                     FOPEN........12900
   90    CONTINUE                                                        FOPEN........13000
   99    CLOSE(K0)                                                       FOPEN........13100
C                                                                        FOPEN........13200
C........OPEN THE SMY FILE.                                              FOPEN........13300
C                                                                        FOPEN........13400
C........IF NO SMY SPECIFICATION, USE THE DEFAULT.                       FOPEN........13500
         IF (IUNIT(0).EQ.-1) THEN                                        FOPEN........13600
            CALL NAFU(K00,0,ENDEF)                                       FOPEN........13700
            IUNIT(0) = K00                                               FOPEN........13800
            FNAME(0) = ENDEF                                             FOPEN........13900
         END IF                                                          FOPEN........14000
         IU = IUNIT(0)                                                   FOPEN........14100
         FN = FNAME(0)                                                   FOPEN........14200
         OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',IOSTAT=KERR)              FOPEN........14300
C........IN CASE OF ERROR WHILE OPENING SMY FILE, WRITE ERROR            FOPEN........14400
C           MESSAGE TO DEFAULT FILE                                      FOPEN........14500
         IF (KERR.GT.0) THEN                                             FOPEN........14600
            CALL NAFU(K00,0,ENDEF)                                       FOPEN........14700
            OPEN(UNIT=K00,FILE=ENDEF,STATUS='REPLACE')                   FOPEN........14800
            GOTO 9000                                                    FOPEN........14900
         END IF                                                          FOPEN........15000
C........SET K00 AND ENAME                                               FOPEN........15100
         K00 = IU                                                        FOPEN........15200
         ENAME = FN                                                      FOPEN........15300
C                                                                        FOPEN........15400
C........CHECK FOR REPEATED FILENAMES (EXCEPT OBS AND OBC FILES)         FOPEN........15500
C           AND MISSING SPECIFICATIONS FOR REQUIRED FILE TYPES           FOPEN........15600
         DO 260 NF=0,6                                                   FOPEN........15700
            IF (IUNIT(NF).EQ.-1) THEN                                    FOPEN........15800
               IF ((NF.GE.1).AND.(NF.LE.4)) THEN                         FOPEN........15900
                  ERRCOD = 'FIL-7'                                       FOPEN........16000
                  CHERR(1) = UNAME                                       FOPEN........16100
                  CHERR(2) = FTSTR(NF)                                   FOPEN........16200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               FOPEN........16300
               ELSE                                                      FOPEN........16400
                  CYCLE                                                  FOPEN........16500
               END IF                                                    FOPEN........16600
            END IF                                                       FOPEN........16700
            DO 250 NF2=NF+1,6                                            FOPEN........16800
               IF (FNAME(NF2).EQ.FNAME(NF)) THEN                         FOPEN........16900
                  ERRCOD = 'FIL-4'                                       FOPEN........17000
                  CHERR(1) = UNAME                                       FOPEN........17100
                  CHERR(2) = FNAME(NF)                                   FOPEN........17200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               FOPEN........17300
               END IF                                                    FOPEN........17400
  250       CONTINUE                                                     FOPEN........17500
  260    CONTINUE                                                        FOPEN........17600
C                                                                        FOPEN........17700
C........SET UNIT NUMBERS K1 - K7.  (K00 HAS BEEN SET PREVIOUSLY.)       FOPEN........17800
         K1=IUNIT(1)                                                     FOPEN........17900
         K2=IUNIT(2)                                                     FOPEN........18000
         K3=IUNIT(3)                                                     FOPEN........18100
         K4=IUNIT(4)                                                     FOPEN........18200
         K5=IUNIT(5)                                                     FOPEN........18300
         K6=IUNIT(6)                                                     FOPEN........18400
         K7=IUNIT(7)                                                     FOPEN........18500
         K8=IUNIT(8)                                                     FOPEN........18600
C                                                                        FOPEN........18700
C........CHECK FOR EXISTENCE OF INPUT FILES AND OPEN INPUT AND OUTPUT    FOPEN........18800
C           FILES (EXCEPT SMY, OBS, AND OBC)                             FOPEN........18900
         DO 300 NF=1,6                                                   FOPEN........19000
            IU=IUNIT(NF)                                                 FOPEN........19100
            FN=FNAME(NF)                                                 FOPEN........19200
            IF (IU.EQ.-1) GOTO 300                                       FOPEN........19300
            IF(NF.LE.2) THEN                                             FOPEN........19400
               INQUIRE(FILE=FN,EXIST=IS)                                 FOPEN........19500
               IF(IS) THEN                                               FOPEN........19600
                  OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED',    FOPEN........19700
     1               IOSTAT=KERR)                                        FOPEN........19800
               ELSE                                                      FOPEN........19900
                  GOTO 8000                                              FOPEN........20000
               ENDIF                                                     FOPEN........20100
            ELSE                                                         FOPEN........20200
               OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',   FOPEN........20300
     1            IOSTAT=KERR)                                           FOPEN........20400
            ENDIF                                                        FOPEN........20500
            IF(KERR.GT.0) GOTO 9000                                      FOPEN........20600
  300    CONTINUE                                                        FOPEN........20700
C                                                                        FOPEN........20800
C........SET FLAG TO INDICATE THAT FIRST PASS IS COMPLETED, THEN RETURN  FOPEN........20900
         ONCEFO = .TRUE.                                                 FOPEN........21000
         RETURN                                                          FOPEN........21100
C                                                                        FOPEN........21200
      ELSE                                                               FOPEN........21300
C                                                                        FOPEN........21400
C........INITIALIZE OBSERVATION-RELATED UNIT NUMBERS AND FILENAMES       FOPEN........21500
         DO 330 NFO=1,NFLOMX                                             FOPEN........21600
            IUNIO(NFO) = -1                                              FOPEN........21700
            FNAMO(NFO) = ""                                              FOPEN........21800
  330    CONTINUE                                                        FOPEN........21900
C                                                                        FOPEN........22000
C........OPEN OBS AND OBC FILES, AUTOMATICALLY GENERATING UNIT NUMBERS   FOPEN........22100
C           AND FILENAMES                                                FOPEN........22200
C                                                                        FOPEN........22300
C........LOOP OVER THE TWO FILE TYPES                                    FOPEN........22400
         DO 400 NF=7,8                                                   FOPEN........22500
C...........IF NO FILE SPECIFICATION OF THIS TYPE, MOVE ON               FOPEN........22600
            IF (IUNIT(NF).EQ.-1) CYCLE                                   FOPEN........22700
C...........DETERMINE LENGTH OF THE SPECIFIED FILENAME AND ITS ROOT      FOPEN........22800
            LNAME = LEN_TRIM(FNAME(NF))                                  FOPEN........22900
            LROOT = SCAN(FNAME(NF),'.',BACK=.TRUE.) - 1                  FOPEN........23000
C...........SET THE ROOT NAME AND EXTENSION THAT WILL BE USED FOR FILES  FOPEN........23100
C              OF THIS TYPE                                              FOPEN........23200
            IF (LROOT.NE.-1) THEN                                        FOPEN........23300
               IF (LROOT.NE.0) THEN                                      FOPEN........23400
                  FNROOT = FNAME(NF)(1:LROOT)                            FOPEN........23500
               ELSE                                                      FOPEN........23600
                  FNROOT = "SUTRA"                                       FOPEN........23700
               END IF                                                    FOPEN........23800
               IF (LROOT.NE.LNAME-1) THEN                                FOPEN........23900
                  FNEXTN = FNAME(NF)(LROOT+1:LNAME)                      FOPEN........24000
               ELSE                                                      FOPEN........24100
                  FNEXTN = "." // FTSTR(NF)                              FOPEN........24200
               END IF                                                    FOPEN........24300
            ELSE                                                         FOPEN........24400
               IF (LNAME.NE.0) THEN                                      FOPEN........24500
                  FNROOT = FNAME(NF)                                     FOPEN........24600
               ELSE                                                      FOPEN........24700
                  FNROOT = "SUTRA"                                       FOPEN........24800
               END IF                                                    FOPEN........24900
               FNEXTN = "." // FTSTR(NF)                                 FOPEN........25000
            END IF                                                       FOPEN........25100
C...........INITIALIZE UNIT NUMBER                                       FOPEN........25200
            IUNEXT = IUNIT(NF)                                           FOPEN........25300
C...........LOOP OVER OBSERVATION OUTPUT FILES                           FOPEN........25400
            DO 380 J=1,NFLOMX                                            FOPEN........25500
               JM1 = J - 1                                               FOPEN........25600
C..............IF FILE IS NOT OF THE TYPE CURRENTLY BEING PROCESSED,     FOPEN........25700
C                 SKIP FILE                                              FOPEN........25800
               IF (OFP(J)%FRMT.NE.FTSTR(NF)) CYCLE                       FOPEN........25900
C..............CONSTRUCT FILENAME FROM ROOT NAME, SCHEDULE NAME,         FOPEN........26000
C                 AND EXTENSION                                          FOPEN........26100
               IF (SCHDLS(OFP(J)%ISCHED)%NAME.NE."-") THEN               FOPEN........26200
                  FN = TRIM(FNROOT) // "_"                               FOPEN........26300
     1               // TRIM(SCHDLS(OFP(J)%ISCHED)%NAME) // FNEXTN       FOPEN........26400
               ELSE                                                      FOPEN........26500
                  FN = TRIM(FNROOT) // FNEXTN                            FOPEN........26600
               END IF                                                    FOPEN........26700
C..............CHECK FOR DUPLICATE FILENAME AMONG NON-OBSERVATION        FOPEN........26800
C                 FILES                                                  FOPEN........26900
               DO 350 NFF=0,6                                            FOPEN........27000
                  IF (FN.EQ.FNAME(NFF)) THEN                             FOPEN........27100
                     ERRCOD = 'FIL-4'                                    FOPEN........27200
                     CHERR(1) = UNAME                                    FOPEN........27300
                     CHERR(2) = FN                                       FOPEN........27400
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)            FOPEN........27500
                  END IF                                                 FOPEN........27600
  350          CONTINUE                                                  FOPEN........27700
C..............CHECK FOR DUPLICATE FILENAME AMONG PREVIOUSLY DEFINED     FOPEN........27800
C                 OBSERVATION OUTPUT FILES                               FOPEN........27900
               DO 355 NJ=1,J-1                                           FOPEN........28000
                  IF (FN.EQ.FNAMO(NJ)) THEN                              FOPEN........28100
                     ERRCOD = 'FIL-4'                                    FOPEN........28200
                     CHERR(1) = UNAME                                    FOPEN........28300
                     CHERR(2) = FN                                       FOPEN........28400
                     CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)            FOPEN........28500
                  END IF                                                 FOPEN........28600
  355          CONTINUE                                                  FOPEN........28700
C..............ASSIGN NEXT AVAILABLE UNIT NUMBER, RECORD FILE            FOPEN........28800
C                 INFORMATION, AND OPEN THE FILE                         FOPEN........28900
               CALL NAFU(IUNEXT,JM1,FN)                                  FOPEN........29000
               IU = IUNEXT                                               FOPEN........29100
               IUNIO(J) = IU                                             FOPEN........29200
               FNAMO(J) = FN                                             FOPEN........29300
               INQUIRE(UNIT=IU, OPENED=IS)                               FOPEN........29400
               OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',   FOPEN........29500
     1            IOSTAT=KERR)                                           FOPEN........29600
               IF(KERR.GT.0) GOTO 9000                                   FOPEN........29700
  380       CONTINUE                                                     FOPEN........29800
  400    CONTINUE                                                        FOPEN........29900
C                                                                        FOPEN........30000
C........SECOND PASS IS COMPLETED, SO RETURN                             FOPEN........30100
         RETURN                                                          FOPEN........30200
C                                                                        FOPEN........30300
      END IF                                                             FOPEN........30400
C                                                                        FOPEN........30500
 8000 CONTINUE                                                           FOPEN........30600
C.....GENERATE ERROR                                                     FOPEN........30700
      ERRCOD = 'FIL-1'                                                   FOPEN........30800
      CHERR(1) = UNAME                                                   FOPEN........30900
      CHERR(2) = FN                                                      FOPEN........31000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           FOPEN........31100
C                                                                        FOPEN........31200
 9000 CONTINUE                                                           FOPEN........31300
C.....GENERATE ERROR                                                     FOPEN........31400
      ERRCOD = 'FIL-2'                                                   FOPEN........31500
      CHERR(1) = UNAME                                                   FOPEN........31600
      CHERR(2) = FN                                                      FOPEN........31700
      INERR(1) = IU                                                      FOPEN........31800
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           FOPEN........31900
C                                                                        FOPEN........32000
      END                                                                FOPEN........32100
C                                                                        FOPEN........32200
C     FUNCTION          F  R  C  S  T  P           SUTRA VERSION 2.1     FRCSTP.........100
C                                                                        FRCSTP.........200
C *** PURPOSE :                                                          FRCSTP.........300
C ***  TO RETURN THE FRACTIONAL TIME STEP FOR A GIVEN TIME.  IF THE      FRCSTP.........400
C ***  SPECIFIED TIME IS GREATER THAN THE MAXIMUM, A VALUE OF            FRCSTP.........500
C ***  +HUGE(1D0) (THE LARGEST NUMBER THAT CAN BE REPRESENTED IN DOUBLE  FRCSTP.........600
C ***  PRECISION) IS RETURNED.  IF THE SPECIFIED TIME IS LESS THAN       FRCSTP.........700
C ***  TSTART, A VALUE OF -HUGE(1D0) IS RETURNED.  IF THE TIME STEP      FRCSTP.........800
C ***  SCHEDULE HAS NOT YET BEEN DEFINED, A VALUE OF ZERO IS RETURNED.   FRCSTP.........900
C                                                                        FRCSTP........1000
      DOUBLE PRECISION FUNCTION FRCSTP(TIME)                             FRCSTP........1100
      USE LLDEF                                                          FRCSTP........1200
      USE SCHDEF                                                         FRCSTP........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FRCSTP........1400
      TYPE (LLD), POINTER :: DEN                                         FRCSTP........1500
      COMMON /SCH/ NSCH,ISCHTS                                           FRCSTP........1600
C                                                                        FRCSTP........1700
      IF (ISCHTS.EQ.0) THEN                                              FRCSTP........1800
         FRCSTP = DNINT(DBLE(0))                                         FRCSTP........1900
         RETURN                                                          FRCSTP........2000
      END IF                                                             FRCSTP........2100
C                                                                        FRCSTP........2200
      NSMAX = SCHDLS(ISCHTS)%LLEN - 1                                    FRCSTP........2300
C                                                                        FRCSTP........2400
      DEN => SCHDLS(ISCHTS)%SLIST                                        FRCSTP........2500
      T1 = DEN%DVALU1                                                    FRCSTP........2600
      IF (TIME.EQ.T1) THEN                                               FRCSTP........2700
         FRCSTP = DNINT(DBLE(0))                                         FRCSTP........2800
         RETURN                                                          FRCSTP........2900
      ELSE IF (TIME.LT.T1) THEN                                          FRCSTP........3000
         FRCSTP = -HUGE(1D0)                                             FRCSTP........3100
         RETURN                                                          FRCSTP........3200
      END IF                                                             FRCSTP........3300
      DO 100 NS=1,NSMAX                                                  FRCSTP........3400
         DEN => DEN%NENT                                                 FRCSTP........3500
         T2 = DEN%DVALU1                                                 FRCSTP........3600
         IF (TIME.EQ.T2) THEN                                            FRCSTP........3700
            FRCSTP = DNINT(DBLE(NS))                                     FRCSTP........3800
            RETURN                                                       FRCSTP........3900
         ELSE IF (TIME.LT.T2) THEN                                       FRCSTP........4000
            WT = (TIME - T1)/(T2 - T1)                                   FRCSTP........4100
            S1 = DBLE(NS - 1)                                            FRCSTP........4200
            S2 = DBLE(NS)                                                FRCSTP........4300
            FRCSTP = (1D0 - WT)*S1 + WT*S2                               FRCSTP........4400
            RETURN                                                       FRCSTP........4500
         END IF                                                          FRCSTP........4600
  100 CONTINUE                                                           FRCSTP........4700
      FRCSTP = +HUGE(1D0)                                                FRCSTP........4800
C                                                                        FRCSTP........4900
      RETURN                                                             FRCSTP........5000
      END                                                                FRCSTP........5100
C                                                                        FRCSTP........5200
C     SUBROUTINE        G  L  O  B  A  N           SUTRA VERSION 2.1     GLOBAN.........100
C                                                                        GLOBAN.........200
C *** PURPOSE :                                                          GLOBAN.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOBAN.........400
C ***  A GLOBAL BANDED MATRIX AND GLOBAL VECTOR FOR BOTH                 GLOBAN.........500
C ***  FLOW AND TRANSPORT EQUATIONS.                                     GLOBAN.........600
C                                                                        GLOBAN.........700
      SUBROUTINE GLOBAN(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,           GLOBAN.........800
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC)                                  GLOBAN.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                GLOBAN........1000
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    GLOBAN........1100
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC)                      GLOBAN........1200
      DIMENSION UMAT(NELT,NCBI),UVEC(NNVEC)                              GLOBAN........1300
      DIMENSION IN(NIN)                                                  GLOBAN........1400
      DIMENSION KTYPE(2)                                                 GLOBAN........1500
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  GLOBAN........1600
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             GLOBAN........1700
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              GLOBAN........1800
     1   NSOP,NSOU,NBCN                                                  GLOBAN........1900
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        GLOBAN........2000
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           GLOBAN........2100
C                                                                        GLOBAN........2200
      N1=(L-1)*N48+1                                                     GLOBAN........2300
      N8=N1+N48-1                                                        GLOBAN........2400
C                                                                        GLOBAN........2500
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOBAN........2600
C        P-MATRIX AND P-VECTOR                                           GLOBAN........2700
      IF(ML-1) 9050,9050,9150                                            GLOBAN........2800
 9050 IE=0                                                               GLOBAN........2900
      DO 9100 II=N1,N8                                                   GLOBAN........3000
      IE=IE+1                                                            GLOBAN........3100
      IB=IN(II)                                                          GLOBAN........3200
      VOL(IB)=VOL(IB)+VOLE(IE)                                           GLOBAN........3300
      PVEC(IB)=PVEC(IB)+DFLOWE(IE)                                       GLOBAN........3400
      JE=0                                                               GLOBAN........3500
      DO 9100 JJ=N1,N8                                                   GLOBAN........3600
      JE=JE+1                                                            GLOBAN........3700
      JB=IN(JJ)-IB+NBHALF                                                GLOBAN........3800
 9100 PMAT(IB,JB)=PMAT(IB,JB)+BFLOWE(IE,JE)                              GLOBAN........3900
      IF(ML-1) 9150,9300,9150                                            GLOBAN........4000
C                                                                        GLOBAN........4100
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOBAN........4200
C        U-MATRIX                                                        GLOBAN........4300
 9150 IF(NOUMAT.EQ.1) GOTO 9300                                          GLOBAN........4400
      IE=0                                                               GLOBAN........4500
      DO 9200 II=N1,N8                                                   GLOBAN........4600
      IE=IE+1                                                            GLOBAN........4700
      IB=IN(II)                                                          GLOBAN........4800
C.....POSITION FOR ADDITION TO U-VECTOR                                  GLOBAN........4900
C        UVEC(IB)=UVEC(IB)+ ((   ))                                      GLOBAN........5000
      JE=0                                                               GLOBAN........5100
      DO 9200 JJ=N1,N8                                                   GLOBAN........5200
      JE=JE+1                                                            GLOBAN........5300
      JB=IN(JJ)-IB+NBHALF                                                GLOBAN........5400
 9200 UMAT(IB,JB)=UMAT(IB,JB)+DTRANE(IE,JE)+BTRANE(IE,JE)                GLOBAN........5500
C                                                                        GLOBAN........5600
 9300 CONTINUE                                                           GLOBAN........5700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOBAN........5800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOBAN........5900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOBAN........6000
C                                                                        GLOBAN........6100
C                                                                        GLOBAN........6200
      RETURN                                                             GLOBAN........6300
      END                                                                GLOBAN........6400
C                                                                        GLOBAN........6500
C     SUBROUTINE        G  L  O  C  O  L           SUTRA VERSION 2.1     GLOCOL.........100
C                                                                        GLOCOL.........200
C *** PURPOSE :                                                          GLOCOL.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOCOL.........400
C ***  A GLOBAL "SLAP COLUMN"-FORMAT MATRIX AND GLOBAL VECTOR            GLOCOL.........500
C ***  FOR BOTH FLOW AND TRANSPORT EQUATIONS.                            GLOCOL.........600
C                                                                        GLOCOL.........700
      SUBROUTINE GLOCOL(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,           GLOCOL.........800
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC,IA,JA)                            GLOCOL.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                GLOCOL........1000
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    GLOCOL........1100
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC)                      GLOCOL........1200
      DIMENSION UMAT(NELT,NCBI),UVEC(NNVEC)                              GLOCOL........1300
      DIMENSION IN(NIN),IA(NDIMIA),JA(NDIMJA)                            GLOCOL........1400
      DIMENSION KTYPE(2)                                                 GLOCOL........1500
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  GLOCOL........1600
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             GLOCOL........1700
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              GLOCOL........1800
     1   NSOP,NSOU,NBCN                                                  GLOCOL........1900
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        GLOCOL........2000
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        GLOCOL........2100
C                                                                        GLOCOL........2200
      N1=(L-1)*N48+1                                                     GLOCOL........2300
      N8=N1+N48-1                                                        GLOCOL........2400
C                                                                        GLOCOL........2500
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOCOL........2600
C        P-MATRIX AND P-VECTOR                                           GLOCOL........2700
      IF(ML-1) 9050,9050,9150                                            GLOCOL........2800
 9050 IE=0                                                               GLOCOL........2900
      DO 9100 II=N1,N8                                                   GLOCOL........3000
      IE=IE+1                                                            GLOCOL........3100
      IB=IN(II)                                                          GLOCOL........3200
      VOL(IB)=VOL(IB)+VOLE(IE)                                           GLOCOL........3300
      PVEC(IB)=PVEC(IB)+DFLOWE(IE)                                       GLOCOL........3400
      JE=0                                                               GLOCOL........3500
      DO 9100 JJ=N1,N8                                                   GLOCOL........3600
      JE=JE+1                                                            GLOCOL........3700
      JB = IN(JJ)                                                        GLOCOL........3800
      MBEG = JA(JB)                                                      GLOCOL........3900
      MEND = JA(JB + 1) - 1                                              GLOCOL........4000
      DO 9060 MM=MBEG,MEND                                               GLOCOL........4100
         IF (IB.EQ.IA(MM)) THEN                                          GLOCOL........4200
            M = MM                                                       GLOCOL........4300
            GOTO 9100                                                    GLOCOL........4400
         END IF                                                          GLOCOL........4500
 9060 CONTINUE                                                           GLOCOL........4600
 9100 PMAT(M,1)=PMAT(M,1)+BFLOWE(IE,JE)                                  GLOCOL........4700
      IF(ML-1) 9150,9300,9150                                            GLOCOL........4800
C                                                                        GLOCOL........4900
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOCOL........5000
C        U-MATRIX                                                        GLOCOL........5100
 9150 IF(NOUMAT.EQ.1) GOTO 9300                                          GLOCOL........5200
      IE=0                                                               GLOCOL........5300
      DO 9200 II=N1,N8                                                   GLOCOL........5400
      IE=IE+1                                                            GLOCOL........5500
      IB=IN(II)                                                          GLOCOL........5600
C.....POSITION FOR ADDITION TO U-VECTOR                                  GLOCOL........5700
C        UVEC(IB)=UVEC(IB)+ ((   ))                                      GLOCOL........5800
      JE=0                                                               GLOCOL........5900
      DO 9200 JJ=N1,N8                                                   GLOCOL........6000
      JE=JE+1                                                            GLOCOL........6100
      JB = IN(JJ)                                                        GLOCOL........6200
      MBEG = JA(JB)                                                      GLOCOL........6300
      MEND = JA(JB + 1) - 1                                              GLOCOL........6400
      DO 9160 MM=MBEG,MEND                                               GLOCOL........6500
         IF (IB.EQ.IA(MM)) THEN                                          GLOCOL........6600
            M = MM                                                       GLOCOL........6700
            GOTO 9200                                                    GLOCOL........6800
         END IF                                                          GLOCOL........6900
 9160 CONTINUE                                                           GLOCOL........7000
 9200 UMAT(M,1)=UMAT(M,1)+DTRANE(IE,JE)+BTRANE(IE,JE)                    GLOCOL........7100
C                                                                        GLOCOL........7200
 9300 CONTINUE                                                           GLOCOL........7300
 9999 CONTINUE                                                           GLOCOL........7400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOCOL........7500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOCOL........7600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOCOL........7700
C                                                                        GLOCOL........7800
C                                                                        GLOCOL........7900
      RETURN                                                             GLOCOL........8000
      END                                                                GLOCOL........8100
C                                                                        GLOCOL........8200
C     SUBROUTINE        I  N  D  A  T  0           SUTRA VERSION 2.1     INDAT0.........100
C                                                                        INDAT0.........200
C *** PURPOSE :                                                          INDAT0.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A PORTION OF THE INP FILE          INDAT0.........400
C ***  INPUT DATA (DATASETS 5 THROUGH 7)                                 INDAT0.........500
C                                                                        INDAT0.........600
      SUBROUTINE INDAT0()                                                INDAT0.........700
      USE EXPINT                                                         INDAT0.........800
      USE LLDEF                                                          INDAT0.........900
      USE SCHDEF                                                         INDAT0........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT0........1100
      CHARACTER INTFIL*1000                                              INDAT0........1200
      CHARACTER*10 ADSMOD                                                INDAT0........1300
      CHARACTER SOLWRD(0:10)*10,SOLNAM(0:10)*40                          INDAT0........1400
      CHARACTER*10 CSOLVP,CSOLVU                                         INDAT0........1500
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     INDAT0........1600
      CHARACTER SCHTYP*12, CDUM10*10                                     INDAT0........1700
      CHARACTER*10 SCHNAM                                                INDAT0........1800
      CHARACTER CTICS*20, CREFT*8                                        INDAT0........1900
      DIMENSION INERR(10),RLERR(10)                                      INDAT0........2000
      DIMENSION KTYPE(2)                                                 INDAT0........2100
      ALLOCATABLE :: ISLIST(:), TLIST(:), DTMP1(:), DTMP2(:)             INDAT0........2200
      CHARACTER*10, ALLOCATABLE :: CTMP(:)                               INDAT0........2300
      CHARACTER*8 VERNUM, VERNIN                                         INDAT0........2400
      LOGICAL, ALLOCATABLE :: SBASED(:), ELAPSD(:)                       INDAT0........2500
      LOGICAL TSYES                                                      INDAT0........2600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT0........2700
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT0........2800
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT0........2900
     1   NSOP,NSOU,NBCN                                                  INDAT0........3000
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        INDAT0........3100
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           INDAT0........3200
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT0........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     INDAT0........3400
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT0........3500
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT0........3600
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            INDAT0........3700
      COMMON /ITSOLR/ TOLP,TOLU                                          INDAT0........3800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     INDAT0........3900
     1   KSCRN,KPAUSE                                                    INDAT0........4000
      COMMON /MODSOR/ ADSMOD                                             INDAT0........4100
      COMMON /SCH/ NSCH,ISCHTS                                           INDAT0........4200
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT0........4300
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT0........4400
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       INDAT0........4500
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           INDAT0........4600
      COMMON /SOLVN/ NSLVRS                                              INDAT0........4700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT0........4800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT0........4900
      COMMON /VER/ VERNUM, VERNIN                                        INDAT0........5000
C                                                                        INDAT0........5100
      INSTOP=0                                                           INDAT0........5200
C                                                                        INDAT0........5300
C.....INPUT DATASET 5: NUMERICAL CONTROL PARAMETERS                      INDAT0........5400
      ERRCOD = 'REA-INP-5'                                               INDAT0........5500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0........5600
      READ(INTFIL,*,IOSTAT=INERR(1)) UP,GNUP,GNUU                        INDAT0........5700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0........5800
      IF(ME.EQ.-1) WRITE(K3,70) UP,GNUP,GNUU                             INDAT0........5900
   70 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6000
     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6100
     2   11X,1PE15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6200
     3   11X,1PE15.4,5X,'SPECIFIED CONCENTRATION BOUNDARY CONDITION ',   INDAT0........6300
     4   'FACTOR')                                                       INDAT0........6400
      IF(ME.EQ.+1) WRITE(K3,80) UP,GNUP,GNUU                             INDAT0........6500
   80 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6600
     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6700
     2   11X,1PE15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6800
     3   11X,1PE15.4,5X,'SPECIFIED TEMPERATURE BOUNDARY CONDITION ',     INDAT0........6900
     4   'FACTOR')                                                       INDAT0........7000
C                                                                        INDAT0........7100
C.....INPUT DATASET 6: TEMPORAL CONTROL AND SOLUTION CYCLING DATA        INDAT0........7200
      ERRCOD = 'REA-ICS-1'                                               INDAT0........7300
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT0........7400
      READ(INTFIL,*,IOSTAT=INERR(1)) TICS                                INDAT0........7500
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0........7600
      REWIND(K2)                                                         INDAT0........7700
      WRITE(CTICS,'(E20.10)') TICS                                       INDAT0........7800
      WRITE(K3,120)                                                      INDAT0........7900
  120 FORMAT('1'////11X,'T E M P O R A L   C O N T R O L   A N D   ',    INDAT0........8000
     1   'S O L U T I O N   C Y C L I N G   D A T A')                    INDAT0........8100
      ERRCOD = 'REA-INP-6'                                               INDAT0........8200
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0........8300
      READ(INTFIL,*,IOSTAT=INERR(1)) NSCH                                INDAT0........8400
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0........8500
C.....IF VERSION 2.0 INPUT, RE-READ DATASET IN OLD FORMAT.  ELSE IF      INDAT0........8600
C        NSCH>0, RE-READ FIRST LINE OF DATASET IN NEW FORMAT.  ELSE      INDAT0........8700
C        IF NSCH<0, OR NSCH=0 AND TRANSPORT IS NOT STEADY-STATE,         INDAT0........8800
C        GENERATE ERROR.                                                 INDAT0........8900
      IF (VERNIN.EQ."2.0") THEN                                          INDAT0........9000
C........READ TEMPORAL AND SOLUTION CYCLING CONTROLS.                    INDAT0........9100
         READ(INTFIL,*,IOSTAT=INERR(1)) ITMAX,DELT,TMAX,ITCYC,DTMULT,    INDAT0........9200
     1      DTMAX,NPCYC,NUCYC                                            INDAT0........9300
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0........9400
C........ERROR CHECKING SPECIFIC TO OLD FORMAT.                          INDAT0........9500
         IF (DELT.GT.DTMAX) THEN                                         INDAT0........9600
            ERRCOD = 'INP-6-3'                                           INDAT0........9700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0........9800
         END IF                                                          INDAT0........9900
      ELSE IF (NSCH.GT.0) THEN                                           INDAT0.......10000
C........READ FIRST LINE OF DATASET.                                     INDAT0.......10100
         READ(INTFIL,*,IOSTAT=INERR(1)) NSCH, NPCYC, NUCYC               INDAT0.......10200
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0.......10300
      ELSE IF (NSCH.LT.0) THEN                                           INDAT0.......10400
            ERRCOD = 'INP-6-8'                                           INDAT0.......10500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......10600
      ELSE                                                               INDAT0.......10700
         IF (ISSTRA.EQ.0) THEN                                           INDAT0.......10800
            ERRCOD = 'INP-6-13'                                          INDAT0.......10900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......11000
         END IF                                                          INDAT0.......11100
         NPCYC = 1                                                       INDAT0.......11200
         NUCYC = 1                                                       INDAT0.......11300
      END IF                                                             INDAT0.......11400
C.....ERROR CHECKING COMMON TO BOTH FORMATS.                             INDAT0.......11500
      IF (NPCYC.LT.1.OR.NUCYC.LT.1) THEN                                 INDAT0.......11600
         ERRCOD = 'INP-6-1'                                              INDAT0.......11700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......11800
      ELSE IF (NPCYC.NE.1.AND.NUCYC.NE.1) THEN                           INDAT0.......11900
         ERRCOD = 'INP-6-2'                                              INDAT0.......12000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......12100
      END IF                                                             INDAT0.......12200
C.....IF TRANSPORT IS STEADY-STATE, SKIP THROUGH THE REST OF THE         INDAT0.......12300
C        DATASET AND CREATE A TRIVIAL "TIME_STEPS" SCHEDULE.             INDAT0.......12400
C        (NOTE THAT IF TRANSPORT IS STEADY-STATE, SO IS FLOW.)           INDAT0.......12500
C        EVENTUALLY, A TRIVIAL SCHEDULE WILL ALSO BE CREATED FOR         INDAT0.......12600
C        OBSERVATION OUTPUT, SO SET NSCH=2.                              INDAT0.......12700
      IF (ISSTRA.EQ.1) THEN                                              INDAT0.......12800
         TSTART = TICS                                                   INDAT0.......12900
         IF (VERNIN.NE."2.0") THEN                                       INDAT0.......13000
            ERRCOD = 'REA-INP-6'                                         INDAT0.......13100
            CDUM10 = ''                                                  INDAT0.......13200
            DO WHILE (CDUM10.NE.'-')                                     INDAT0.......13300
               CALL READIF(K1, INTFIL, ERRCOD)                           INDAT0.......13400
               IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR,      INDAT0.......13500
     1            RLERR)                                                 INDAT0.......13600
               READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                     INDAT0.......13700
            END DO                                                       INDAT0.......13800
            DELT = MAX(1D-1*DABS(TSTART), 1D0)                           INDAT0.......13900
         END IF                                                          INDAT0.......14000
         NSCH = 2                                                        INDAT0.......14100
         ALLOCATE(SCHDLS(NSCH))                                          INDAT0.......14200
         DO 135 NS=1,NSCH                                                INDAT0.......14300
            ALLOCATE(SCHDLS(NS)%SLIST)                                   INDAT0.......14400
            SCHDLS(NS)%LLEN = 0                                          INDAT0.......14500
  135    CONTINUE                                                        INDAT0.......14600
         TIME = TSTART                                                   INDAT0.......14700
         STEP = 0D0                                                      INDAT0.......14800
         LSTLEN = 0                                                      INDAT0.......14900
         CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)                INDAT0.......15000
         TIME = TIME + DELT                                              INDAT0.......15100
         STEP = 1D0                                                      INDAT0.......15200
         CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)                INDAT0.......15300
         SCHDLS(1)%LLEN = LSTLEN                                         INDAT0.......15400
         ISCHTS = 1                                                      INDAT0.......15500
         ITMAX = 1                                                       INDAT0.......15600
C........WRITE STEADY-STATE OUTPUT INFORMATION.                          INDAT0.......15700
         WRITE(K3,138)                                                   INDAT0.......15800
  138    FORMAT (/13X,'NOTE: BECAUSE FLOW AND TRANSPORT ARE STEADY-',    INDAT0.......15900
     1      'STATE, USER-DEFINED SCHEDULES ARE NOT IN EFFECT.  '         INDAT0.......16000
     2      /13X,'STEADY-STATE RESULTS WILL BE WRITTEN TO THE ',         INDAT0.......16100
     3      'APPROPRIATE OUTPUT FILES.')                                 INDAT0.......16200
C........SKIP OVER PROCESSING AND WRITING OF TEMPORAL DATA.              INDAT0.......16300
         GOTO 850                                                        INDAT0.......16400
      END IF                                                             INDAT0.......16500
C.....IF DATASET IN OLD FORMAT, WRITE SPECIFICATIONS AND                 INDAT0.......16600
C        CREATE A CORRESPONDING SCHEDULE CALLED "TIME_STEPS".            INDAT0.......16700
C        IF NSCH=0, GENERATE AN ERROR.  IF IN NEW FORMAT, READ           INDAT0.......16800
C        AND PROCESS USER-DEFINED SCHEDULES.                             INDAT0.......16900
      IF (VERNIN.EQ."2.0") THEN                                          INDAT0.......17000
         TSTART = TICS                                                   INDAT0.......17100
         WRITE(K3,150) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX                INDAT0.......17200
  150    FORMAT (/13X,'NOTE: BECAUSE TEMPORAL CONTROL AND SOLUTION ',    INDAT0.......17300
     1      'CYCLING DATA WERE ENTERED USING THE OLD (VERSION 2D3D.1) ', INDAT0.......17400
     2      'INPUT FORMAT,'/13X,'A CORRESPONDING SCHEDULE, ',            INDAT0.......17500
     3      '"TIME_STEPS", WAS CREATED AUTOMATICALLY FROM THE ',         INDAT0.......17600
     4      'FOLLOWING PARAMETERS:'                                      INDAT0.......17700
     5      //11X,I15,5X,'MAXIMUM ALLOWED NUMBER OF TIME STEPS'          INDAT0.......17800
     6      /11X,1PE15.4,5X,'INITIAL TIME STEP (IN SECONDS)'             INDAT0.......17900
     7      /11X,1PE15.4,5X,'MAXIMUM ALLOWED SIMULATION TIME ',          INDAT0.......18000
     8      '(IN SECONDS)'                                               INDAT0.......18100
     9      //11X,I15,5X,'TIME STEP MULTIPLIER CYCLE (IN TIME STEPS)'    INDAT0.......18200
     1      /11X,0PF15.5,5X,'MULTIPLICATION FACTOR FOR TIME STEP CHANGE' INDAT0.......18300
     2      /11X,1PE15.4,5X,'MAXIMUM ALLOWED TIME STEP (IN SECONDS)')    INDAT0.......18400
C........TWO DEFAULT SCHEDULES WILL EVENTUALLY BE DEFINED:               INDAT0.......18500
C           "TIME_STEPS", WHICH CONTROLS TIME STEPPING, AND              INDAT0.......18600
C           "OBS", WHICH CONTROLS TIMING OF OBSERVATION OUTPUT.          INDAT0.......18700
C           SET THE NUMBER OF SCHEDULES ACCORDINGLY AND ALLOCATE         INDAT0.......18800
C           THE SCHEDULE ARRAY AND ITS LINKED LISTS.                     INDAT0.......18900
         NSCH = 2                                                        INDAT0.......19000
         ALLOCATE(SCHDLS(NSCH))                                          INDAT0.......19100
         DO 185 NS=1,NSCH                                                INDAT0.......19200
            ALLOCATE(SCHDLS(NS)%SLIST)                                   INDAT0.......19300
            SCHDLS(NS)%LLEN = 0                                          INDAT0.......19400
  185    CONTINUE                                                        INDAT0.......19500
C........DEFINE THE DEFAULT "TIME_STEPS" SCHEDULE BASED ON THE           INDAT0.......19600
C           TEMPORAL CONTROLS.  NOTE THAT, FOR BACKWARD COMPATIBILITY    INDAT0.......19700
C           WITH OLD DATASETS, THE ORIGINAL METHOD OF HANDLING CHANGES   INDAT0.......19800
C           IN TIME STEP SIZE [BASED ON MOD(JT,ITCYC).EQ.0, NOT          INDAT0.......19900
C           MOD(JT-1,ITCYC).EQ.0] HAS BEEN RETAINED.                     INDAT0.......20000
         SCHDLS(1)%NAME = "TIME_STEPS"                                   INDAT0.......20100
         TIME = TSTART                                                   INDAT0.......20200
         STEP = 0D0                                                      INDAT0.......20300
         LSTLEN = 0                                                      INDAT0.......20400
         CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)                INDAT0.......20500
         DTIME = DELT                                                    INDAT0.......20600
         DO 580 JT=1,ITMAX                                               INDAT0.......20700
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DTIME=DTIME*DTMULT     INDAT0.......20800
            IF (DTIME.GT.DTMAX) DTIME = DTMAX                            INDAT0.......20900
            TIME = TIME + DTIME                                          INDAT0.......21000
            STEP = DBLE(JT)                                              INDAT0.......21100
            CALL LLDINS(LSTLEN, SCHDLS(1)%SLIST, TIME, STEP)             INDAT0.......21200
            IF (TIME.GE.TMAX) EXIT                                       INDAT0.......21300
  580    CONTINUE                                                        INDAT0.......21400
         SCHDLS(1)%LLEN = LSTLEN                                         INDAT0.......21500
         ISCHTS = 1                                                      INDAT0.......21600
C........SKIP OVER THE CODE THAT READS SCHEDULE SPECIFICATIONS.          INDAT0.......21700
         GOTO 850                                                        INDAT0.......21800
      END IF                                                             INDAT0.......21900
C.....WRITE SCHEDULE PARAMETERS.                                         INDAT0.......22000
      WRITE(K3,700) NSCH                                                 INDAT0.......22100
  700 FORMAT(/13X,'THE ',I5,' SCHEDULES ARE LISTED BELOW.'               INDAT0.......22200
     1   '  SCHEDULE "TIME_STEPS" CONTROLS TIME STEPPING.')              INDAT0.......22300
C.....ALLOCATE SCHEDULE-RELATED ARRAYS AND INITIALIZE SCHEDULE NUMBER    INDAT0.......22400
C        FOR "TIME_STEPS".                                               INDAT0.......22500
      ALLOCATE(SCHDLS(NSCH), SBASED(NSCH), ELAPSD(NSCH))                 INDAT0.......22600
      ISCHTS = 0                                                         INDAT0.......22700
C.....LOOP THROUGH THE LIST OF SCHEDULE SPECIFICATIONS, CONSTRUCTING     INDAT0.......22800
C        SCHEDULES.                                                      INDAT0.......22900
      DO 800 I=1,NSCH                                                    INDAT0.......23000
C........ALLOCATE HEAD OF LINKED LIST FOR THE CURRENT SCHEDULE AND SET   INDAT0.......23100
C           LIST LENGTH TO ZERO.                                         INDAT0.......23200
         ALLOCATE(SCHDLS(I)%SLIST)                                       INDAT0.......23300
         SCHDLS(I)%LLEN = 0                                              INDAT0.......23400
C........READ SCHEDULE NAME AND DO SOME ERROR CHECKING.                  INDAT0.......23500
         ERRCOD = 'REA-INP-6'                                            INDAT0.......23600
         CALL READIF(K1, INTFIL, ERRCOD)                                 INDAT0.......23700
         READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM                           INDAT0.......23800
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0.......23900
         IF (SCHNAM.EQ."-") THEN                                         INDAT0.......24000
            ERRCOD = 'INP-6-4'                                           INDAT0.......24100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......24200
         ELSE                                                            INDAT0.......24300
            DO 710 II=1,I-1                                              INDAT0.......24400
               IF (SCHNAM.EQ.SCHDLS(II)%NAME) THEN                       INDAT0.......24500
                  ERRCOD = 'INP-6-5'                                     INDAT0.......24600
                  CHERR(1) = SCHNAM                                      INDAT0.......24700
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT0.......24800
               END IF                                                    INDAT0.......24900
  710       CONTINUE                                                     INDAT0.......25000
         END IF                                                          INDAT0.......25100
C........(RE)READ SCHEDULE NAME AND TYPE.                                INDAT0.......25200
         READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP                   INDAT0.......25300
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0.......25400
C........BASED ON THE SCHEDULE TYPE, READ IN THE SPECIFICATIONS AND      INDAT0.......25500
C           CONSTRUCT THE SCHEDULE.                                      INDAT0.......25600
         IF (SCHTYP.EQ."STEP CYCLE") THEN                                INDAT0.......25700
            SBASED(I) = .TRUE.                                           INDAT0.......25800
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......25900
            READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP,               INDAT0.......26000
     1         NSMAX, ISTEPI, ISTEPL, ISTEPC                             INDAT0.......26100
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  INDAT0.......26200
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......26300
            ELAPSD(I) = .FALSE.                                          INDAT0.......26400
C...........CONSTRUCT THE SCHEDULE BY STEPPING THROUGH THE STEP CYCLE    INDAT0.......26500
C              AND STORING THE RESULTS IN THE LINKED LIST.  SET TIME     INDAT0.......26600
C              EQUAL TO STEP FOR NOW SO THAT THE LIST IS CONSTRUCTED     INDAT0.......26700
C              IN THE PROPER ORDER.                                      INDAT0.......26800
            NSTEP = ISTEPI                                               INDAT0.......26900
            NDSTEP = ISTEPC                                              INDAT0.......27000
            LSTLEN = 0                                                   INDAT0.......27100
            STEP = DNINT(DBLE(NSTEP))                                    INDAT0.......27200
            TIME = STEP                                                  INDAT0.......27300
            CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)             INDAT0.......27400
            DO 720 NS=1,NSMAX                                            INDAT0.......27500
               NSTEP = NSTEP + NDSTEP                                    INDAT0.......27600
               STEP = DNINT(DBLE(NSTEP))                                 INDAT0.......27700
               TIME = STEP                                               INDAT0.......27800
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......27900
               IF (NSTEP.GE.ISTEPL) EXIT                                 INDAT0.......28000
  720       CONTINUE                                                     INDAT0.......28100
            SCHDLS(I)%LLEN = LSTLEN                                      INDAT0.......28200
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......28300
            WRITE(K3,722) SCHDLS(I)%NAME, NSMAX, ISTEPI, ISTEPL, ISTEPC  INDAT0.......28400
  722       FORMAT(/16X,'SCHEDULE ',A, 3X,'STEP CYCLE WITH THE ',        INDAT0.......28500
     1         'FOLLOWING SPECIFICATIONS:'                               INDAT0.......28600
     2         /40X, I8, 5X, 'MAXIMUM NUMBER OF TIME STEPS AFTER ',      INDAT0.......28700
     3            'INITIAL TIME STEP NUMBER'                             INDAT0.......28800
     4         /40X, I8, 5X, 'INITIAL TIME STEP NUMBER'                  INDAT0.......28900
     5         /40X, I8, 5X, 'LIMITING TIME STEP NUMBER'                 INDAT0.......29000
     6         /40X, I8, 5X, 'TIME STEP INCREMENT')                      INDAT0.......29100
         ELSE IF (SCHTYP.EQ."TIME CYCLE") THEN                           INDAT0.......29200
            SBASED(I) = .FALSE.                                          INDAT0.......29300
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......29400
            READ(INTFIL,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,        INDAT0.......29500
     1         SCALT, NTMAX, TIMEI, TIMEL, TIMEC, NTCYC,                 INDAT0.......29600
     2         TCMULT, TCMIN, TCMAX                                      INDAT0.......29700
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  INDAT0.......29800
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......29900
            IF (CREFT.EQ.'ELAPSED ') THEN                                INDAT0.......30000
               IF ((SCHNAM.EQ.'TIME_STEPS').AND.(TIMEI.NE.0D0)) THEN     INDAT0.......30100
                  ERRCOD = 'INP-6-7'                                     INDAT0.......30200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT0.......30300
               END IF                                                    INDAT0.......30400
               ELAPSD(I) = .TRUE.                                        INDAT0.......30500
            ELSE IF (CREFT.EQ.'ABSOLUTE') THEN                           INDAT0.......30600
               ELAPSD(I) = .FALSE.                                       INDAT0.......30700
            ELSE                                                         INDAT0.......30800
               ERRCOD = 'INP-6-6'                                        INDAT0.......30900
               CHERR(1) = CREFT                                          INDAT0.......31000
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INDAT0.......31100
            END IF                                                       INDAT0.......31200
C...........SCALE ALL TIME SPECIFICATIONS                                INDAT0.......31300
            TIMEI = TIMEI*SCALT                                          INDAT0.......31400
            TIMEL = TIMEL*SCALT                                          INDAT0.......31500
            TIMEC = TIMEC*SCALT                                          INDAT0.......31600
            TCMIN = TCMIN*SCALT                                          INDAT0.......31700
            TCMAX = TCMAX*SCALT                                          INDAT0.......31800
C...........CONSTRUCT THE SCHEDULE BY STEPPING THROUGH THE TIME CYCLE    INDAT0.......31900
C              AND STORING THE RESULTS IN THE LINKED LIST.               INDAT0.......32000
            TIME = TIMEI                                                 INDAT0.......32100
            STEP = FRCSTP(TIME)                                          INDAT0.......32200
            DTIME = TIMEC                                                INDAT0.......32300
            LSTLEN = 0                                                   INDAT0.......32400
            CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)             INDAT0.......32500
            DO 730 NT=1,NTMAX                                            INDAT0.......32600
               IF (MOD(NT-1,NTCYC).EQ.0 .AND. NT.GT.1)                   INDAT0.......32700
     1            DTIME=DTIME*TCMULT                                     INDAT0.......32800
               IF (DTIME.GT.TCMAX) DTIME = TCMAX                         INDAT0.......32900
               IF (DTIME.LT.TCMIN) DTIME = TCMIN                         INDAT0.......33000
               TIME = TIME + DTIME                                       INDAT0.......33100
               STEP = FRCSTP(TIME)                                       INDAT0.......33200
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......33300
               IF (TIME.GE.TIMEL) EXIT                                   INDAT0.......33400
  730       CONTINUE                                                     INDAT0.......33500
            SCHDLS(I)%LLEN = LSTLEN                                      INDAT0.......33600
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......33700
            WRITE(K3,732) SCHDLS(I)%NAME, TRIM(CREFT), NTMAX, TIMEI,     INDAT0.......33800
     1          TIMEL, TIMEC, NTCYC, TCMULT, TCMIN, TCMAX                INDAT0.......33900
  732       FORMAT(/16X,'SCHEDULE ',A, 3X,'TIME CYCLE WITH THE ',        INDAT0.......34000
     1         'FOLLOWING SPECIFICATIONS IN TERMS OF ', A, ' TIMES:'     INDAT0.......34100
     2         /46X, I8, 5X, 'MAXIMUM NUMBER OF TIMES AFTER ',           INDAT0.......34200
     3            'INITIAL TIME'                                         INDAT0.......34300
     4         /39X, 1PE15.7, 5X, 'INITIAL TIME'                         INDAT0.......34400
     5         /39X, 1PE15.7, 5X, 'LIMITING TIME'                        INDAT0.......34500
     6         /39X, 1PE15.7, 5X, 'INITIAL TIME INCREMENT'               INDAT0.......34600
     7         /46X, I8, 5X, 'TIME INCREMENT CHANGE CYCLE '              INDAT0.......34700
     8         /39X, 1PE15.7, 5X, 'TIME INCREMENT MULTIPLIER'            INDAT0.......34800
     9         /39X, 1PE15.7, 5X, 'MINIMUM TIME INCREMENT'               INDAT0.......34900
     1         /39X, 1PE15.7, 5X, 'MAXIMUM TIME INCREMENT')              INDAT0.......35000
         ELSE IF (SCHTYP.EQ."STEP LIST") THEN                            INDAT0.......35100
            SBASED(I) = .TRUE.                                           INDAT0.......35200
C...........READ THE SCHEDULE NAME, TYPE, AND LENGTH.                    INDAT0.......35300
            BACKSPACE(K1)                                                INDAT0.......35400
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, NSLIST            INDAT0.......35500
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  INDAT0.......35600
C...........ALLOCATE A TEMPORARY ARRAY TO HOLD THE STEP LIST.            INDAT0.......35700
            ALLOCATE (ISLIST(NSLIST))                                    INDAT0.......35800
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......35900
            BACKSPACE(K1)                                                INDAT0.......36000
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP,                   INDAT0.......36100
     1         NSLIST, (ISLIST(NS),NS=1,NSLIST)                          INDAT0.......36200
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  INDAT0.......36300
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......36400
            ELAPSD(I) = .FALSE.                                          INDAT0.......36500
C...........CONSTRUCT THE SCHEDULE BY TRANSFERRING THE LIST FROM ARRAY   INDAT0.......36600
C              ISLIST TO THE LINKED LIST.  SET TIME EQUAL TO STEP FOR    INDAT0.......36700
C              NOW SO THAT THE LIST IS CONSTRUCTED IN THE PROPER ORDER.  INDAT0.......36800
            LSTLEN = 0                                                   INDAT0.......36900
            DO 740 NS=1,NSLIST                                           INDAT0.......37000
               NSTEP = ISLIST(NS)                                        INDAT0.......37100
               STEP = DNINT(DBLE(NSTEP))                                 INDAT0.......37200
               TIME = STEP                                               INDAT0.......37300
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......37400
  740       CONTINUE                                                     INDAT0.......37500
            SCHDLS(I)%LLEN = NSLIST                                      INDAT0.......37600
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......37700
            WRITE(K3,742) SCHDLS(I)%NAME, (ISLIST(NS),NS=1,NSLIST)       INDAT0.......37800
  742       FORMAT(/16X,'SCHEDULE ',A, 3X,'STEP LIST THAT INCLUDES ',    INDAT0.......37900
     1         'THE FOLLOWING TIME STEPS:'/:(38X,8(2X,I8)))              INDAT0.......38000
C...........DEALLOCATE THE TEMPORARY ARRAY.                              INDAT0.......38100
            DEALLOCATE (ISLIST)                                          INDAT0.......38200
         ELSE IF (SCHTYP.EQ."TIME LIST") THEN                            INDAT0.......38300
            SBASED(I) = .FALSE.                                          INDAT0.......38400
C...........READ THE SCHEDULE NAME, TYPE, SCALE FACTOR, AND LENGTH.      INDAT0.......38500
            BACKSPACE(K1)                                                INDAT0.......38600
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,            INDAT0.......38700
     1         SCALT, NTLIST                                             INDAT0.......38800
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  INDAT0.......38900
C...........ALLOCATE A TEMPORARY ARRAY TO HOLD THE TIME LIST.            INDAT0.......39000
            ALLOCATE (TLIST(NTLIST))                                     INDAT0.......39100
C...........READ ALL THE SPECIFICATIONS.                                 INDAT0.......39200
            BACKSPACE(K1)                                                INDAT0.......39300
            READ(K1,*,IOSTAT=INERR(1)) SCHNAM, SCHTYP, CREFT,            INDAT0.......39400
     1         SCALT, NTLIST, (TLIST(NT),NT=1,NTLIST)                    INDAT0.......39500
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  INDAT0.......39600
            SCHDLS(I)%NAME = SCHNAM                                      INDAT0.......39700
            IF (CREFT.EQ.'ELAPSED ') THEN                                INDAT0.......39800
               IF ((SCHNAM.EQ.'TIME_STEPS').AND.(TLIST(1).NE.0D0)) THEN  INDAT0.......39900
                  ERRCOD = 'INP-6-7'                                     INDAT0.......40000
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT0.......40100
               END IF                                                    INDAT0.......40200
               ELAPSD(I) = .TRUE.                                        INDAT0.......40300
            ELSE IF (CREFT.EQ.'ABSOLUTE') THEN                           INDAT0.......40400
               ELAPSD(I) = .FALSE.                                       INDAT0.......40500
            ELSE                                                         INDAT0.......40600
               ERRCOD = 'INP-6-6'                                        INDAT0.......40700
               CHERR(1) = CREFT                                          INDAT0.......40800
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INDAT0.......40900
            END IF                                                       INDAT0.......41000
C...........SCALE ALL TIME SPECIFICATIONS                                INDAT0.......41100
            DO 745 NT=1,NTLIST                                           INDAT0.......41200
               TLIST(NT) = TLIST(NT)*SCALT                               INDAT0.......41300
  745       CONTINUE                                                     INDAT0.......41400
C...........CONSTRUCT THE SCHEDULE BY TRANSFERRING THE LIST FROM ARRAY   INDAT0.......41500
C              TLIST TO THE LINKED LIST.                                 INDAT0.......41600
            LSTLEN = 0                                                   INDAT0.......41700
            DO 750 NT=1,NTLIST                                           INDAT0.......41800
               TIME = TLIST(NT)                                          INDAT0.......41900
               STEP = FRCSTP(TIME)                                       INDAT0.......42000
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......42100
  750       CONTINUE                                                     INDAT0.......42200
            SCHDLS(I)%LLEN = NTLIST                                      INDAT0.......42300
C...........WRITE OUT THE SPECIFICATIONS.                                INDAT0.......42400
            WRITE(K3,752) SCHDLS(I)%NAME, TRIM(CREFT),                   INDAT0.......42500
     1         (TLIST(NT),NT=1,NTLIST)                                   INDAT0.......42600
  752       FORMAT(/16X,'SCHEDULE ',A, 3X,'TIME LIST THAT INCLUDES ',    INDAT0.......42700
     1         'THE FOLLOWING ', A, ' TIMES (SEC):'                      INDAT0.......42800
     2         /:(38X,4(1X,1PE15.7)))                                    INDAT0.......42900
            DEALLOCATE (TLIST)                                           INDAT0.......43000
         ELSE                                                            INDAT0.......43100
C...........THE SPECIFIED SCHEDULE TYPE IS INVALID, SO GENERATE AN       INDAT0.......43200
C              ERROR.                                                    INDAT0.......43300
            ERRCOD = 'INP-6-9'                                           INDAT0.......43400
            CHERR(1) = SCHTYP                                            INDAT0.......43500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......43600
         END IF                                                          INDAT0.......43700
  800 CONTINUE                                                           INDAT0.......43800
C.....READ ONE MORE LINE TO CHECK FOR THE END-OF-LIST MARKER ('-').      INDAT0.......43900
C        IF NOT FOUND, GENERATE AN ERROR.                                INDAT0.......44000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......44100
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              INDAT0.......44200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0.......44300
      IF (CDUM10.NE.'-') THEN                                            INDAT0.......44400
         ERRCOD = 'INP-6-4'                                              INDAT0.......44500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......44600
      END IF                                                             INDAT0.......44700
C.....FIND SCHEDULE "TIME_STEPS".                                        INDAT0.......44800
      DO 810 I=1,NSCH                                                    INDAT0.......44900
         IF (SCHDLS(I)%NAME.EQ."TIME_STEPS") THEN                        INDAT0.......45000
            ISCHTS=I                                                     INDAT0.......45100
            TSYES = .TRUE.                                               INDAT0.......45200
            EXIT                                                         INDAT0.......45300
         END IF                                                          INDAT0.......45400
  810 CONTINUE                                                           INDAT0.......45500
C.....IF TRANSPORT IS TRANSIENT AND SCHEDULE "TIME_STEPS" HAS NOT        INDAT0.......45600
C        BEEN DEFINED, GENERATE ERROR                                    INDAT0.......45700
      IF ((ISSTRA.EQ.0).AND.(.NOT.TSYES)) THEN                           INDAT0.......45800
         ERRCOD = 'INP-6-14'                                             INDAT0.......45900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......46000
      END IF                                                             INDAT0.......46100
C.....IF "TIME_STEPS" LENGTH IS <=1 (SCHEDULE CONTAINS, AT MOST,         INDAT0.......46200
C        ONLY THE INITIAL TIME, AND NO SUBSEQUENT TIME STEPS),           INDAT0.......46300
C        GENERATE AN ERROR.                                              INDAT0.......46400
      IF (SCHDLS(ISCHTS)%LLEN.LE.1) THEN                                 INDAT0.......46500
         ERRCOD = 'INP-6-10'                                             INDAT0.......46600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......46700
      END IF                                                             INDAT0.......46800
C.....IN SCHEDULE TIME STEPS, FILL IN TIME STEP NUMBERS, ADDING          INDAT0.......46900
C        TICS TO ELAPSED TIMES IF NECESSARY.  GENERATE ERROR IF          INDAT0.......47000
C        ANY TIMES ARE REPEATED.                                         INDAT0.......47100
      NSMAX = SCHDLS(ISCHTS)%LLEN                                        INDAT0.......47200
      ALLOCATE(DTMP1(NSMAX),DTMP2(NSMAX))                                INDAT0.......47300
      CALL LLD2AR(NSMAX, SCHDLS(ISCHTS)%SLIST, DTMP1, DTMP2)             INDAT0.......47400
      DEALLOCATE (SCHDLS(ISCHTS)%SLIST)                                  INDAT0.......47500
      ALLOCATE (SCHDLS(ISCHTS)%SLIST)                                    INDAT0.......47600
      IF (ELAPSD(ISCHTS)) THEN                                           INDAT0.......47700
         TREF = TICS                                                     INDAT0.......47800
      ELSE                                                               INDAT0.......47900
         TREF = 0D0                                                      INDAT0.......48000
      END IF                                                             INDAT0.......48100
      ITMAX = NSMAX - 1                                                  INDAT0.......48200
      TSTART = TREF + DTMP1(1)                                           INDAT0.......48300
      TFINSH = TREF + DTMP1(NSMAX)                                       INDAT0.......48400
      DELT = DTMP1(2) - DTMP1(1)                                         INDAT0.......48500
      LSTLEN = 0                                                         INDAT0.......48600
      DO 820 NS=1,NSMAX                                                  INDAT0.......48700
         IF ((NS.GT.1).AND.(DTMP1(NS).EQ.DTMP1(NS-1))) THEN              INDAT0.......48800
            ERRCOD = 'INP-6-12'                                          INDAT0.......48900
            IF (ELAPSD(ISCHTS)) THEN                                     INDAT0.......49000
               CHERR(1) = "elapsed time"                                 INDAT0.......49100
            ELSE                                                         INDAT0.......49200
               CHERR(1) = "absolute time"                                INDAT0.......49300
            END IF                                                       INDAT0.......49400
            CHERR(2) = "TIME_STEPS"                                      INDAT0.......49500
            RLERR(1) = DTMP1(NS)                                         INDAT0.......49600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......49700
         END IF                                                          INDAT0.......49800
         NSTEP = NS - 1                                                  INDAT0.......49900
         TIME = TREF + DTMP1(NS)                                         INDAT0.......50000
         STEP = DNINT(DBLE(NSTEP))                                       INDAT0.......50100
         CALL LLDINS(LSTLEN, SCHDLS(ISCHTS)%SLIST, TIME, STEP)           INDAT0.......50200
  820 CONTINUE                                                           INDAT0.......50300
      DEALLOCATE(DTMP1,DTMP2)                                            INDAT0.......50400
C.....FILL IN TIMES OR STEPS FOR REMAINING SCHEDULES, SHIFTING           INDAT0.......50500
C        ELAPSED TIMES TO ABSOLUTE TIMES IF NECESSARY.  PRUNE ENTRIES    INDAT0.......50600
C        THAT ARE OUTSIDE THE RANGE OF SCHEDULE "TIME_STEPS".            INDAT0.......50700
C        GENERATE ERROR IF ANY TIME STEPS OR TIMES ARE REPEATED.         INDAT0.......50800
      DO 845 I=1,NSCH                                                    INDAT0.......50900
         IF (I.EQ.ISCHTS) CYCLE                                          INDAT0.......51000
         NSMAX = SCHDLS(I)%LLEN                                          INDAT0.......51100
         ALLOCATE(DTMP1(NSMAX),DTMP2(NSMAX))                             INDAT0.......51200
         CALL LLD2AR(NSMAX, SCHDLS(I)%SLIST, DTMP1, DTMP2)               INDAT0.......51300
         DEALLOCATE (SCHDLS(I)%SLIST)                                    INDAT0.......51400
         ALLOCATE (SCHDLS(I)%SLIST)                                      INDAT0.......51500
         IF (ELAPSD(I)) THEN                                             INDAT0.......51600
            TREF = TSTART                                                INDAT0.......51700
         ELSE                                                            INDAT0.......51800
            TREF = 0D0                                                   INDAT0.......51900
         END IF                                                          INDAT0.......52000
         LSTLEN = 0                                                      INDAT0.......52100
         IF (SBASED(I)) THEN                                             INDAT0.......52200
            DO 840 NS=1,NSMAX                                            INDAT0.......52300
               IF ((NS.GT.1).AND.(DTMP2(NS).EQ.DTMP2(NS-1))) THEN        INDAT0.......52400
                  ERRCOD = 'INP-6-12'                                    INDAT0.......52500
                  CHERR(1) = "time step"                                 INDAT0.......52600
                  CHERR(2) = SCHDLS(I)%NAME                              INDAT0.......52700
                  RLERR(1) = DTMP2(NS)                                   INDAT0.......52800
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT0.......52900
               END IF                                                    INDAT0.......53000
               STEP = DTMP2(NS)                                          INDAT0.......53100
               NSTEP = NINT(STEP)                                        INDAT0.......53200
               IF ((NSTEP.LT.0).OR.(NSTEP.GT.ITMAX)) CYCLE               INDAT0.......53300
               TIME = TIMETS(NSTEP)                                      INDAT0.......53400
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......53500
  840       CONTINUE                                                     INDAT0.......53600
         ELSE                                                            INDAT0.......53700
            DO 842 NS=1,NSMAX                                            INDAT0.......53800
               IF ((NS.GT.1).AND.(DTMP1(NS).EQ.DTMP1(NS-1))) THEN        INDAT0.......53900
                  ERRCOD = 'INP-6-12'                                    INDAT0.......54000
                  IF (ELAPSD(I)) THEN                                    INDAT0.......54100
                     CHERR(1) = "elapsed time"                           INDAT0.......54200
                  ELSE                                                   INDAT0.......54300
                     CHERR(1) = "absolute time"                          INDAT0.......54400
                  END IF                                                 INDAT0.......54500
                  CHERR(2) = SCHDLS(I)%NAME                              INDAT0.......54600
                  RLERR(1) = DTMP1(NS)                                   INDAT0.......54700
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT0.......54800
               END IF                                                    INDAT0.......54900
               TIME = TREF + DTMP1(NS)                                   INDAT0.......55000
               IF ((TIME.LT.TSTART).OR.(TIME.GT.TFINSH)) CYCLE           INDAT0.......55100
               STEP = FRCSTP(TIME)                                       INDAT0.......55200
               CALL LLDINS(LSTLEN, SCHDLS(I)%SLIST, TIME, STEP)          INDAT0.......55300
  842       CONTINUE                                                     INDAT0.......55400
         END IF                                                          INDAT0.......55500
         SCHDLS(I)%LLEN = LSTLEN                                         INDAT0.......55600
         DEALLOCATE(DTMP1,DTMP2)                                         INDAT0.......55700
  845 CONTINUE                                                           INDAT0.......55800
C.....DEALLOCATE ARRAY THAT INDICATES METHODS OF SCHEDULE SPECIFICATION  INDAT0.......55900
      DEALLOCATE(SBASED)                                                 INDAT0.......56000
C.....WRITE THE SOLUTION CYCLING CONTROLS.                               INDAT0.......56100
  850 WRITE(K3,874) NPCYC,NUCYC                                          INDAT0.......56200
  874 FORMAT (/13X,'SOLUTION CYCLING DATA:'                              INDAT0.......56300
     1      //11X,I15,5X,'FLOW SOLUTION CYCLE (IN TIME STEPS)'           INDAT0.......56400
     2      /11X,I15,5X,'TRANSPORT SOLUTION CYCLE (IN TIME STEPS)')      INDAT0.......56500
C.....SET SOLUTION CYCLING FOR STEADY-STATE FLOW                         INDAT0.......56600
      IF(ISSFLO.EQ.1) THEN                                               INDAT0.......56700
         NPCYC=ITMAX+1                                                   INDAT0.......56800
         NUCYC=1                                                         INDAT0.......56900
      END IF                                                             INDAT0.......57000
C                                                                        INDAT0.......57100
C.....INPUT DATASET 7A:  ITERATION CONTROLS FOR RESOLVING NONLINEARITIES INDAT0.......57200
      ERRCOD = 'REA-INP-7A'                                              INDAT0.......57300
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......57400
      READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX                              INDAT0.......57500
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0.......57600
      IF (ITRMAX.GT.1) THEN                                              INDAT0.......57700
         ERRCOD = 'REA-INP-7A'                                           INDAT0.......57800
         READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX,RPMAX,RUMAX               INDAT0.......57900
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0.......58000
      END IF                                                             INDAT0.......58100
      IF(ITRMAX-1) 1192,1192,1194                                        INDAT0.......58200
 1192 WRITE(K3,1193)                                                     INDAT0.......58300
 1193 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......58400
     1   //11X,'  NON-ITERATIVE SOLUTION')                               INDAT0.......58500
      GOTO 1196                                                          INDAT0.......58600
 1194 WRITE(K3,1195) ITRMAX,RPMAX,RUMAX                                  INDAT0.......58700
 1195 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......58800
     1   //11X,I15,5X,'MAXIMUM NUMBER OF ITERATIONS PER TIME STEP',      INDAT0.......58900
     2   /11X,1PE15.4,5X,'ABSOLUTE CONVERGENCE CRITERION FOR FLOW',      INDAT0.......59000
     3   ' SOLUTION'/11X,1PE15.4,5X,'ABSOLUTE CONVERGENCE CRITERION',    INDAT0.......59100
     4   ' FOR TRANSPORT SOLUTION')                                      INDAT0.......59200
 1196 CONTINUE                                                           INDAT0.......59300
C                                                                        INDAT0.......59400
C.....INPUT DATASETS 7B & 7C:  MATRIX EQUATION SOLVER CONTROLS FOR       INDAT0.......59500
C        PRESSURE AND TRANSPORT SOLUTIONS                                INDAT0.......59600
      ERRCOD = 'REA-INP-7B'                                              INDAT0.......59700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......59800
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP                              INDAT0.......59900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0.......60000
      IF ((CSOLVP.NE.SOLWRD(0))) THEN                                    INDAT0.......60100
         ERRCOD = 'REA-INP-7B'                                           INDAT0.......60200
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP,ITRMXP,TOLP               INDAT0.......60300
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0.......60400
      END IF                                                             INDAT0.......60500
      ERRCOD = 'REA-INP-7C'                                              INDAT0.......60600
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......60700
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU                              INDAT0.......60800
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT0.......60900
      IF ((CSOLVU.NE.SOLWRD(0))) THEN                                    INDAT0.......61000
         ERRCOD = 'REA-INP-7C'                                           INDAT0.......61100
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU,ITRMXU,TOLU               INDAT0.......61200
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT0.......61300
      END IF                                                             INDAT0.......61400
      KSOLVP = -1                                                        INDAT0.......61500
      KSOLVU = -1                                                        INDAT0.......61600
      DO 1250 M=0,NSLVRS-1                                               INDAT0.......61700
         IF (CSOLVP.EQ.SOLWRD(M)) KSOLVP = M                             INDAT0.......61800
         IF (CSOLVU.EQ.SOLWRD(M)) KSOLVU = M                             INDAT0.......61900
 1250 CONTINUE                                                           INDAT0.......62000
      IF ((KSOLVP.LT.0).OR.(KSOLVU.LT.0)) THEN                           INDAT0.......62100
         ERRCOD = 'INP-7B&C-1'                                           INDAT0.......62200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......62300
      ELSE IF ((KSOLVP*KSOLVU.EQ.0).AND.(KSOLVP+KSOLVU.NE.0)) THEN       INDAT0.......62400
         ERRCOD = 'INP-7B&C-2'                                           INDAT0.......62500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......62600
      ELSE IF ((KSOLVU.EQ.1).OR.((KSOLVP.EQ.1).AND.(UP.NE.0D0))) THEN    INDAT0.......62700
         ERRCOD = 'INP-7B&C-3'                                           INDAT0.......62800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......62900
      END IF                                                             INDAT0.......63000
      IF (KSOLVP.EQ.2) THEN                                              INDAT0.......63100
         ITOLP = 0                                                       INDAT0.......63200
      ELSE                                                               INDAT0.......63300
         ITOLP = 1                                                       INDAT0.......63400
      END IF                                                             INDAT0.......63500
      IF (KSOLVU.EQ.2) THEN                                              INDAT0.......63600
         ITOLU = 0                                                       INDAT0.......63700
      ELSE                                                               INDAT0.......63800
         ITOLU = 1                                                       INDAT0.......63900
      END IF                                                             INDAT0.......64000
      NSAVEP = 10                                                        INDAT0.......64100
      NSAVEU = 10                                                        INDAT0.......64200
C                                                                        INDAT0.......64300
C                                                                        INDAT0.......64400
      RETURN                                                             INDAT0.......64500
      END                                                                INDAT0.......64600
C                                                                        INDAT0.......64700
C     SUBROUTINE        I  N  D  A  T  1           SUTRA VERSION 2.1     INDAT1.........100
C                                                                        INDAT1.........200
C *** PURPOSE :                                                          INDAT1.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A MAJOR PORTION OF INP FILE        INDAT1.........400
C ***  INPUT DATA (DATASETS 8 THROUGH 15)                                INDAT1.........500
C                                                                        INDAT1.........600
      SUBROUTINE INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,         INDAT1.........700
     1   ATMIN,PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,                       INDAT1.........800
     2   PERMYZ,PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG, INDAT1.........900
     3   OBSPTS)                                                         INDAT1........1000
      USE ALLARR, ONLY : OBSDAT                                          INDAT1........1100
      USE LLDEF                                                          INDAT1........1200
      USE EXPINT                                                         INDAT1........1300
      USE SCHDEF                                                         INDAT1........1400
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT1........1500
      PARAMETER (NCOLMX=9)                                               INDAT1........1600
      CHARACTER*10 ADSMOD,CDUM10                                         INDAT1........1700
      CHARACTER*6 STYPE(2)                                               INDAT1........1800
      CHARACTER K5SYM(7)*1, NCOL(NCOLMX)*1, VARNK5(7)*25                 INDAT1........1900
      CHARACTER K6SYM(7)*2, LCOL(NCOLMX)*2, VARNK6(7)*25                 INDAT1........2000
      CHARACTER*1 CNODAL,CELMNT,CINCID,CVEL,CBUDG,CSCRN,CPAUSE           INDAT1........2100
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     INDAT1........2200
      CHARACTER INTFIL*1000,DOTS45*45                                    INDAT1........2300
      CHARACTER OBSNAM*40, OBSSCH*10, OBSFMT*3                           INDAT1........2400
      CHARACTER*8 VERNUM, VERNIN                                         INDAT1........2500
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         INDAT1........2600
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             INDAT1........2700
      DIMENSION X(NN),Y(NN),Z(NN),POR(NN),SOP(NN),NREG(NN)               INDAT1........2800
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NEX),PERMYX(NE),PERMYY(NE), INDAT1........2900
     1   PERMYZ(NEX),PERMZX(NEX),PERMZY(NEX),PERMZZ(NEX),PANGL1(NE),     INDAT1........3000
     2   PANGL2(NEX),PANGL3(NEX),ALMAX(NE),ALMID(NEX),ALMIN(NE),         INDAT1........3100
     3   ATMAX(NE),ATMID(NEX),ATMIN(NE),LREG(NE)                         INDAT1........3200
      DIMENSION INERR(10),RLERR(10)                                      INDAT1........3300
      DIMENSION KTYPE(2)                                                 INDAT1........3400
      DIMENSION IUNIT(0:8)                                               INDAT1........3500
      ALLOCATABLE :: INOB(:)                                             INDAT1........3600
      TYPE (LLD), POINTER :: DENTS                                       INDAT1........3700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT1........3800
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT1........3900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT1........4000
     1   NSOP,NSOU,NBCN                                                  INDAT1........4100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        INDAT1........4200
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT1........4300
      COMMON /FUNITA/ IUNIT                                              INDAT1........4400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     INDAT1........4500
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT1........4600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT1........4700
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        INDAT1........4800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     INDAT1........4900
     1   KSCRN,KPAUSE                                                    INDAT1........5000
      COMMON /MODSOR/ ADSMOD                                             INDAT1........5100
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      INDAT1........5200
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT1........5300
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT1........5400
      COMMON /SCH/ NSCH,ISCHTS                                           INDAT1........5500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT1........5600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT1........5700
      COMMON /VER/ VERNUM, VERNIN                                        INDAT1........5800
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                         INDAT1........5900
      DATA (K5SYM(MM), MM=1,7) /'N', 'X', 'Y', 'Z', 'P', 'U', 'S'/       INDAT1........6000
      DATA (VARNK5(MM), MM=1,7) /'NODE NUMBER',                          INDAT1........6100
     1   'X-COORDINATE', 'Y-COORDINATE', 'Z-COORDINATE',                 INDAT1........6200
     2   'PRESSURE', 'CONCENTRATION/TEMPERATURE', 'SATURATION'/          INDAT1........6300
      DATA (K6SYM(MM), MM=1,7) /'E', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'/    INDAT1........6400
      DATA (VARNK6(MM), MM=1,7) /'ELEMENT NUMBER',                       INDAT1........6500
     1   'X-COORDINATE OF CENTROID', 'Y-COORDINATE OF CENTROID',         INDAT1........6600
     2   'Z-COORDINATE OF CENTROID', 'X-VELOCITY', 'Y-VELOCITY',         INDAT1........6700
     3   'Z-VELOCITY'/                                                   INDAT1........6800
      DATA DOTS45 /'.............................................'/      INDAT1........6900
      SAVE STYPE,K5SYM,VARNK5,K6SYM,VARNK6                               INDAT1........7000
C                                                                        INDAT1........7100
      INSTOP=0                                                           INDAT1........7200
C                                                                        INDAT1........7300
C.....INPUT DATASET 8A:  OUTPUT CONTROLS AND OPTIONS FOR LST FILE        INDAT1........7400
C        AND SCREEN                                                      INDAT1........7500
      ERRCOD = 'REA-INP-8A'                                              INDAT1........7600
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1........7700
      READ(INTFIL,*,IOSTAT=INERR(1)) NPRINT,CNODAL,CELMNT,CINCID,        INDAT1........7800
     1   CVEL,CBUDG,CSCRN,CPAUSE                                         INDAT1........7900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1........8000
      IF (CNODAL.EQ.'Y') THEN                                            INDAT1........8100
         KNODAL = +1                                                     INDAT1........8200
      ELSE IF (CNODAL.EQ.'N') THEN                                       INDAT1........8300
         KNODAL = 0                                                      INDAT1........8400
      ELSE                                                               INDAT1........8500
         ERRCOD = 'INP-8A-1'                                             INDAT1........8600
         CHERR(1) = 'CNODAL '                                            INDAT1........8700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1........8800
      END IF                                                             INDAT1........8900
      IF (CELMNT.EQ.'Y') THEN                                            INDAT1........9000
         KELMNT = +1                                                     INDAT1........9100
      ELSE IF (CELMNT.EQ.'N') THEN                                       INDAT1........9200
         KELMNT = 0                                                      INDAT1........9300
      ELSE                                                               INDAT1........9400
         ERRCOD = 'INP-8A-2'                                             INDAT1........9500
         CHERR(1) = 'CELMNT'                                             INDAT1........9600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1........9700
      END IF                                                             INDAT1........9800
      IF (CINCID.EQ.'Y') THEN                                            INDAT1........9900
         KINCID = +1                                                     INDAT1.......10000
      ELSE IF (CINCID.EQ.'N') THEN                                       INDAT1.......10100
         KINCID = 0                                                      INDAT1.......10200
      ELSE                                                               INDAT1.......10300
         ERRCOD = 'INP-8A-3'                                             INDAT1.......10400
         CHERR(1) = 'CINCID'                                             INDAT1.......10500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......10600
      END IF                                                             INDAT1.......10700
      IF (CVEL.EQ.'Y') THEN                                              INDAT1.......10800
         KVEL = +1                                                       INDAT1.......10900
      ELSE IF (CVEL.EQ.'N') THEN                                         INDAT1.......11000
         KVEL = 0                                                        INDAT1.......11100
      ELSE                                                               INDAT1.......11200
         ERRCOD = 'INP-8A-4'                                             INDAT1.......11300
         CHERR(1) = 'CVEL  '                                             INDAT1.......11400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......11500
      END IF                                                             INDAT1.......11600
      IF (CBUDG.EQ.'Y') THEN                                             INDAT1.......11700
         KBUDG = +1                                                      INDAT1.......11800
      ELSE IF (CBUDG.EQ.'N') THEN                                        INDAT1.......11900
         KBUDG = 0                                                       INDAT1.......12000
      ELSE                                                               INDAT1.......12100
         ERRCOD = 'INP-8A-5'                                             INDAT1.......12200
         CHERR(1) = 'CBUDG '                                             INDAT1.......12300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......12400
      END IF                                                             INDAT1.......12500
      IF (CSCRN.EQ.'Y') THEN                                             INDAT1.......12600
         KSCRN = +1                                                      INDAT1.......12700
      ELSE IF (CSCRN.EQ.'N') THEN                                        INDAT1.......12800
         KSCRN = 0                                                       INDAT1.......12900
      ELSE                                                               INDAT1.......13000
         ERRCOD = 'INP-8A-6'                                             INDAT1.......13100
         CHERR(1) = 'CSCRN '                                             INDAT1.......13200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......13300
      END IF                                                             INDAT1.......13400
      IF (CPAUSE.EQ.'Y') THEN                                            INDAT1.......13500
         KPAUSE = +1                                                     INDAT1.......13600
      ELSE IF (CPAUSE.EQ.'N') THEN                                       INDAT1.......13700
         KPAUSE = 0                                                      INDAT1.......13800
      ELSE                                                               INDAT1.......13900
         ERRCOD = 'INP-8A-7'                                             INDAT1.......14000
         CHERR(1) = 'CPAUSE'                                             INDAT1.......14100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......14200
      END IF                                                             INDAT1.......14300
C                                                                        INDAT1.......14400
      WRITE(K3,72) NPRINT                                                INDAT1.......14500
   72 FORMAT(////11X,'O U T P U T   C O N T R O L S   A N D   ',         INDAT1.......14600
     1   'O P T I O N S'//13X,'.LST FILE'/13X,'---------'                INDAT1.......14700
     2   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)')             INDAT1.......14800
      IF(KNODAL.EQ.+1) WRITE(K3,74)                                      INDAT1.......14900
      IF(KNODAL.EQ.0) WRITE(K3,75)                                       INDAT1.......15000
   74 FORMAT(/13X,'- PRINT NODE COORDINATES, THICKNESSES AND',           INDAT1.......15100
     1   ' POROSITIES')                                                  INDAT1.......15200
   75 FORMAT(/13X,'- CANCEL PRINT OF NODE COORDINATES, THICKNESSES AND', INDAT1.......15300
     1   ' POROSITIES')                                                  INDAT1.......15400
      IF(KELMNT.EQ.+1) WRITE(K3,76)                                      INDAT1.......15500
      IF(KELMNT.EQ.0) WRITE(K3,77)                                       INDAT1.......15600
   76 FORMAT(13X,'- PRINT ELEMENT PERMEABILITIES AND DISPERSIVITIES')    INDAT1.......15700
   77 FORMAT(13X,'- CANCEL PRINT OF ELEMENT PERMEABILITIES AND ',        INDAT1.......15800
     1   'DISPERSIVITIES')                                               INDAT1.......15900
      IF(KINCID.EQ.+1) WRITE(K3,78)                                      INDAT1.......16000
      IF(KINCID.EQ.0) WRITE(K3,79)                                       INDAT1.......16100
   78 FORMAT(13X,'- PRINT NODE INCIDENCES IN EACH ELEMENT')              INDAT1.......16200
   79 FORMAT(13X,'- CANCEL PRINT OF NODE INCIDENCES IN EACH ELEMENT')    INDAT1.......16300
      IME=2                                                              INDAT1.......16400
      IF(ME.EQ.+1) IME=1                                                 INDAT1.......16500
      IF(KVEL.EQ.+1) WRITE(K3,84)                                        INDAT1.......16600
      IF(KVEL.EQ.0) WRITE(K3,85)                                         INDAT1.......16700
   84 FORMAT(/13X,'- CALCULATE AND PRINT VELOCITIES AT ELEMENT ',        INDAT1.......16800
     1   'CENTROIDS ON EACH TIME STEP WITH OUTPUT')                      INDAT1.......16900
   85 FORMAT(/13X,'- CANCEL PRINT OF VELOCITIES')                        INDAT1.......17000
      IF(KBUDG.EQ.+1) WRITE(K3,86) STYPE(IME)                            INDAT1.......17100
      IF(KBUDG.EQ.0) WRITE(K3,87)                                        INDAT1.......17200
   86 FORMAT(/13X,'- CALCULATE AND PRINT FLUID AND ',A6,' BUDGETS ',     INDAT1.......17300
     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......17400
   87 FORMAT(/13X,'- CANCEL PRINT OF BUDGETS')                           INDAT1.......17500
C                                                                        INDAT1.......17600
C.....INPUT DATASET 8B:  OUTPUT CONTROLS AND OPTIONS FOR NOD FILE        INDAT1.......17700
      ERRCOD = 'REA-INP-8B'                                              INDAT1.......17800
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......17900
      READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR                              INDAT1.......18000
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......18100
      DO 140 M=1,NCOLMX                                                  INDAT1.......18200
         READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR, (NCOL(MM), MM=1,M)       INDAT1.......18300
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT1.......18400
         IF (NCOL(M).EQ.'-') THEN                                        INDAT1.......18500
            NCOLS5 = M - 1                                               INDAT1.......18600
            GOTO 142                                                     INDAT1.......18700
         END IF                                                          INDAT1.......18800
  140 CONTINUE                                                           INDAT1.......18900
      NCOLS5 = NCOLMX                                                    INDAT1.......19000
  142 CONTINUE                                                           INDAT1.......19100
      WRITE(K3,144) NCOLPR                                               INDAT1.......19200
  144 FORMAT (//13X,'.NOD FILE'/13X,'---------'                          INDAT1.......19300
     1   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......19400
      DO 148 M=1,NCOLS5                                                  INDAT1.......19500
         DO 146 MM=1,7                                                   INDAT1.......19600
            IF (NCOL(M).EQ.K5SYM(MM)) THEN                               INDAT1.......19700
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......19800
                  ERRCOD = 'INP-8B-1'                                    INDAT1.......19900
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......20000
               END IF                                                    INDAT1.......20100
               IF ((MM.EQ.4).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......20200
                  ERRCOD = 'INP-8B-2'                                    INDAT1.......20300
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......20400
               END IF                                                    INDAT1.......20500
               J5COL(M) = MM                                             INDAT1.......20600
               GOTO 148                                                  INDAT1.......20700
            END IF                                                       INDAT1.......20800
  146    CONTINUE                                                        INDAT1.......20900
         ERRCOD = 'INP-8B-3'                                             INDAT1.......21000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......21100
  148 CONTINUE                                                           INDAT1.......21200
      WRITE(K3,150) (M,VARNK5(J5COL(M)),M=1,NCOLS5)                      INDAT1.......21300
  150 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......21400
C                                                                        INDAT1.......21500
C.....INPUT DATASET 8C:  OUTPUT CONTROLS AND OPTIONS FOR ELE FILE        INDAT1.......21600
      ERRCOD = 'REA-INP-8C'                                              INDAT1.......21700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......21800
      READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR                              INDAT1.......21900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......22000
      DO 160 M=1,NCOLMX                                                  INDAT1.......22100
         READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR, (LCOL(MM), MM=1,M)       INDAT1.......22200
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT1.......22300
         IF (LCOL(M).EQ.'-') THEN                                        INDAT1.......22400
            NCOLS6 = M - 1                                               INDAT1.......22500
            GOTO 162                                                     INDAT1.......22600
         END IF                                                          INDAT1.......22700
  160 CONTINUE                                                           INDAT1.......22800
      NCOLS6 = NCOLMX                                                    INDAT1.......22900
  162 CONTINUE                                                           INDAT1.......23000
      WRITE(K3,164) LCOLPR                                               INDAT1.......23100
  164 FORMAT (//13X,'.ELE FILE'/13X,'---------'                          INDAT1.......23200
     1   //13X,I8,3X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......23300
      DO 168 M=1,NCOLS6                                                  INDAT1.......23400
         DO 166 MM=1,7                                                   INDAT1.......23500
            IF (LCOL(M).EQ.K6SYM(MM)) THEN                               INDAT1.......23600
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......23700
                  ERRCOD = 'INP-8C-1'                                    INDAT1.......23800
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......23900
               END IF                                                    INDAT1.......24000
               IF ((MM.EQ.4).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......24100
                  ERRCOD = 'INP-8C-2'                                    INDAT1.......24200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......24300
               END IF                                                    INDAT1.......24400
               IF ((MM.EQ.7).AND.(KTYPE(1).EQ.2)) THEN                   INDAT1.......24500
                  ERRCOD = 'INP-8C-4'                                    INDAT1.......24600
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......24700
               END IF                                                    INDAT1.......24800
               J6COL(M) = MM                                             INDAT1.......24900
               GOTO 168                                                  INDAT1.......25000
            END IF                                                       INDAT1.......25100
  166    CONTINUE                                                        INDAT1.......25200
         ERRCOD = 'INP-8C-3'                                             INDAT1.......25300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......25400
  168 CONTINUE                                                           INDAT1.......25500
      WRITE(K3,170) (M,VARNK6(J6COL(M)),M=1,NCOLS6)                      INDAT1.......25600
  170 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......25700
C                                                                        INDAT1.......25800
C.....INPUT DATASET 8D:  OUTPUT CONTROLS AND OPTIONS FOR OBSERVATIONS    INDAT1.......25900
      NOBCYC = ITMAX + 1                                                 INDAT1.......26000
      IF (NOBSN-1.EQ.0) GOTO 1199                                        INDAT1.......26100
C.....NOBS IS ACTUAL NUMBER OF OBSERVATION POINTS                        INDAT1.......26200
C.....NTOBS IS MAXIMUM NUMBER OF TIME STEPS WITH OBSERVATIONS            INDAT1.......26300
      NOBS=NOBSN-1                                                       INDAT1.......26400
C.....READ IN OBSERVATION POINTS                                         INDAT1.......26500
      ERRCOD = 'REA-INP-8D'                                              INDAT1.......26600
C.....DO THIS READ NOW TO SKIP ANY COMMENTS AND BLANK LINES.             INDAT1.......26700
C        (BACKSPACE LATER IF IT MUST BE REREAD IN OLD FORMAT.)           INDAT1.......26800
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......26900
      READ(INTFIL,*,IOSTAT=INERR(1)) NOBLIN                              INDAT1.......27000
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......27100
C.....IF TRANSPORT IS STEADY-STATE, CONSTRUCT A TRIVIAL SCHEDULE.        INDAT1.......27200
      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......27300
         SCHDLS(2)%NAME = "-"                                            INDAT1.......27400
         TIME = TSTART                                                   INDAT1.......27500
         STEP = 0D0                                                      INDAT1.......27600
         LSTLEN = 0                                                      INDAT1.......27700
         CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)                INDAT1.......27800
         STEP = 1D0                                                      INDAT1.......27900
         CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)                INDAT1.......28000
         SCHDLS(2)%LLEN = LSTLEN                                         INDAT1.......28100
      END IF                                                             INDAT1.......28200
C.....IF OLD (VERSION 2.0) INPUT FORMAT IS BEING USED, CONSTRUCT A       INDAT1.......28300
C        CORRESPONDING OBSERVATION OUTPUT SCHEDULE IF TRANSPORT IS       INDAT1.......28400
C        TRANSIENT.                                                      INDAT1.......28500
      IF (VERNIN.EQ."2.0") THEN                                          INDAT1.......28600
C........SET THE MAX NUMBER OF OBSERVATIONS PER LINE TO THE TOTAL        INDAT1.......28700
C           NUMBER OF OBSERVATIONS.                                      INDAT1.......28800
         NOBLIN = NOBS                                                   INDAT1.......28900
C........SET UP A TEMPORARY ARRAY TO HOLD OBSERVATION NODES.             INDAT1.......29000
         ALLOCATE(INOB(NOBSN))                                           INDAT1.......29100
C........BACKSPACE AND REREAD DATASET IN OLD FORMAT                      INDAT1.......29200
         BACKSPACE(K1)                                                   INDAT1.......29300
         READ(K1,*,IOSTAT=INERR(1)) NOBCYC, (INOB(JJ), JJ=1,NOBSN)       INDAT1.......29400
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT1.......29500
C........IF THE LAST NODE NUMBER IS NOT ZERO, GENERATE AN ERROR.         INDAT1.......29600
         IF (INOB(NOBSN).NE.0) THEN                                      INDAT1.......29700
            ERRCOD = 'INP-8D-1'                                          INDAT1.......29800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......29900
         END IF                                                          INDAT1.......30000
C........IF A NODE NUMBER IS INVALID, GENERATE AN ERROR.                 INDAT1.......30100
         DO 510 JJ=1,NOBS                                                INDAT1.......30200
            IF ((INOB(JJ).LT.1).OR.(INOB(JJ).GT.NN)) THEN                INDAT1.......30300
               ERRCOD = 'INP-8D-2'                                       INDAT1.......30400
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INDAT1.......30500
            END IF                                                       INDAT1.......30600
  510    CONTINUE                                                        INDAT1.......30700
C........IF TRANSPORT IS TRANSIENT, CONSTRUCT A SCHEDULE THAT            INDAT1.......30800
C           CORRESPONDS TO THE CYCLE SPECIFIED BY NOBCYC.                INDAT1.......30900
         IF (ISSTRA.EQ.0) THEN                                           INDAT1.......31000
            SCHDLS(2)%LLEN = 0                                           INDAT1.......31100
            SCHDLS(2)%NAME = "-"                                         INDAT1.......31200
            ISTEPI = 0                                                   INDAT1.......31300
            ISTEPL = ITMAX                                               INDAT1.......31400
            ITERM = ISTEPL - ISTEPI                                      INDAT1.......31500
C...........IF NOBCYC=0, SET THE CYCLE TO THE TOTAL NUMBER OF TIME       INDAT1.......31600
C              STEPS, SO THAT THE SCHEDULE CONSISTS OF TIME STEPS        INDAT1.......31700
C              0, 1, AND ITMAX.  (VERSION 2.0 SIMPLY BOMBS IF            INDAT1.......31800
C              NOBCYC=0.)                                                INDAT1.......31900
            IF (NOBCYC.EQ.0) THEN                                        INDAT1.......32000
               ISTEPC = ITERM                                            INDAT1.......32100
            ELSE                                                         INDAT1.......32200
               ISTEPC = IABS(NOBCYC)                                     INDAT1.......32300
            END IF                                                       INDAT1.......32400
            NTORS = INT(ITERM/ISTEPC) + 1                                INDAT1.......32500
            IF (MOD(ITERM,ISTEPC).NE.0) NTORS = NTORS + 1                INDAT1.......32600
            NSTEP = ISTEPI                                               INDAT1.......32700
            LSTLEN = 0                                                   INDAT1.......32800
            DENTS => SCHDLS(ISCHTS)%SLIST                                INDAT1.......32900
            JT = 0                                                       INDAT1.......33000
            DO 580 NT=1,NTORS                                            INDAT1.......33100
               NSTEP = MIN(ISTEPL, ISTEPI + (NT - 1)*ISTEPC)             INDAT1.......33200
               DO WHILE (NSTEP.GT.JT)                                    INDAT1.......33300
                  DENTS => DENTS%NENT                                    INDAT1.......33400
                  JT = JT + 1                                            INDAT1.......33500
               END DO                                                    INDAT1.......33600
               STEP = DENTS%DVALU2                                       INDAT1.......33700
               TIME = DENTS%DVALU1                                       INDAT1.......33800
               CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)          INDAT1.......33900
  580       CONTINUE                                                     INDAT1.......34000
C...........IF NOBCYC>=0, INCLUDE TIME STEP 1 IF NOT ALREADY INCLUDED.   INDAT1.......34100
            IF ((NOBCYC.GE.0).AND.(NOBCYC.NE.1)) THEN                    INDAT1.......34200
               DENTS => SCHDLS(ISCHTS)%SLIST                             INDAT1.......34300
               STEP = DNINT(DBLE(1))                                     INDAT1.......34400
               TIME = DENTS%NENT%DVALU1                                  INDAT1.......34500
               CALL LLDINS(LSTLEN, SCHDLS(2)%SLIST, TIME, STEP)          INDAT1.......34600
            END IF                                                       INDAT1.......34700
            SCHDLS(2)%LLEN = LSTLEN                                      INDAT1.......34800
         END IF                                                          INDAT1.......34900
C........CONVERT NODES TO GENERALIZED OBSERVATION POINTS.  THE POINTS    INDAT1.......35000
C           ARE NAMED "NODE_#", WHERE # IS THE NODE NUMBER.              INDAT1.......35100
         DO 540 I=1,NOBS                                                 INDAT1.......35200
            WRITE(OBSPTS(I)%NAME,*) INOB(I)                              INDAT1.......35300
            OBSPTS(I)%NAME = "NODE_" // ADJUSTL(OBSPTS(I)%NAME)          INDAT1.......35400
            OBSPTS(I)%SCHED = "-"                                        INDAT1.......35500
            OBSPTS(I)%FRMT = "OBS"                                       INDAT1.......35600
            OBSPTS(I)%L = INOB(I)                                        INDAT1.......35700
  540    CONTINUE                                                        INDAT1.......35800
C........DEALLOCATE TEMPORARY ARRAY.                                     INDAT1.......35900
         DEALLOCATE(INOB)                                                INDAT1.......36000
C........SKIP PAST THE CODE THAT READS A LIST OF GENERALIZED             INDAT1.......36100
C           OBSERVATION POINTS.                                          INDAT1.......36200
         GOTO 820                                                        INDAT1.......36300
      END IF                                                             INDAT1.......36400
C.....READ THE LIST OF GENERALIZED OBSERVATION POINTS.                   INDAT1.......36500
      NOBCYC = -1                                                        INDAT1.......36600
      DO 690 I=1,NOBS                                                    INDAT1.......36700
C........READ THE OBSERVATION NAME.                                      INDAT1.......36800
         CALL READIF(K1, INTFIL, ERRCOD)                                 INDAT1.......36900
         READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM                           INDAT1.......37000
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT1.......37100
C........IF END-OF-LIST MARKER ENCOUNTERED TOO SOON, GENERATE ERROR.     INDAT1.......37200
         IF (OBSNAM.EQ.'-') THEN                                         INDAT1.......37300
            ERRCOD = 'INP-8D-4'                                          INDAT1.......37400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......37500
         END IF                                                          INDAT1.......37600
C........READ IN (X,Y,Z) OR (X,Y) COORDINATES, DEPENDING ON PROBLEM      INDAT1.......37700
C           DIMENSIONALITY, AS WELL AS OUTPUT SCHEDULE AND FORMAT.       INDAT1.......37800
         IF (KTYPE(1).EQ.3) THEN                                         INDAT1.......37900
            READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM, XOBS, YOBS, ZOBS,     INDAT1.......38000
     1         OBSSCH, OBSFMT                                            INDAT1.......38100
         ELSE                                                            INDAT1.......38200
            READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM, XOBS, YOBS,           INDAT1.......38300
     1         OBSSCH, OBSFMT                                            INDAT1.......38400
         END IF                                                          INDAT1.......38500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT1.......38600
         OBSPTS(I)%NAME = OBSNAM                                         INDAT1.......38700
         OBSPTS(I)%X = XOBS                                              INDAT1.......38800
         OBSPTS(I)%Y = YOBS                                              INDAT1.......38900
         OBSPTS(I)%Z = ZOBS                                              INDAT1.......39000
         IF (ISSTRA.EQ.1) THEN                                           INDAT1.......39100
            OBSPTS(I)%SCHED = "-"                                        INDAT1.......39200
         ELSE                                                            INDAT1.......39300
            OBSPTS(I)%SCHED = OBSSCH                                     INDAT1.......39400
         END IF                                                          INDAT1.......39500
         OBSPTS(I)%FRMT = OBSFMT                                         INDAT1.......39600
  690 CONTINUE                                                           INDAT1.......39700
C.....READ ONE MORE LINE TO CHECK FOR THE END-OF-LIST MARKER ('-').      INDAT1.......39800
C        IF NOT FOUND, GENERATE ERROR.                                   INDAT1.......39900
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......40000
      READ(INTFIL,*,IOSTAT=INERR(1)) OBSNAM                              INDAT1.......40100
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......40200
      IF (OBSNAM.NE.'-') THEN                                            INDAT1.......40300
         ERRCOD = 'INP-8D-4'                                             INDAT1.......40400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......40500
      END IF                                                             INDAT1.......40600
C                                                                        INDAT1.......40700
C.....CONDENSE SCHEDULE AND FILE TYPE INFORMATION FROM OBSDAT INTO       INDAT1.......40800
C        ARRAY OFP, CHECKING FOR UNDEFINED SCHEDULES                     INDAT1.......40900
  820 ALLOCATE (OFP(NSCH*2))                                             INDAT1.......41000
      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......41100
         NFLOMX=0                                                        INDAT1.......41200
         DO 840 I=1,NOBS                                                 INDAT1.......41300
            DO 835 J=1,NFLOMX                                            INDAT1.......41400
               IF (OBSPTS(I)%FRMT.EQ.OFP(J)%FRMT) GOTO 840               INDAT1.......41500
  835       CONTINUE                                                     INDAT1.......41600
            NFLOMX = NFLOMX + 1                                          INDAT1.......41700
            OFP(NFLOMX)%ISCHED = 2                                       INDAT1.......41800
            OFP(NFLOMX)%FRMT = OBSPTS(I)%FRMT                            INDAT1.......41900
  840    CONTINUE                                                        INDAT1.......42000
      ELSE                                                               INDAT1.......42100
         NFLOMX = 0                                                      INDAT1.......42200
         DO 860 I=1,NOBS                                                 INDAT1.......42300
            DO 850 NS=1,NSCH                                             INDAT1.......42400
               IF (OBSPTS(I)%SCHED.EQ.SCHDLS(NS)%NAME) THEN              INDAT1.......42500
                  INS = NS                                               INDAT1.......42600
                  GOTO 852                                               INDAT1.......42700
               END IF                                                    INDAT1.......42800
  850       CONTINUE                                                     INDAT1.......42900
            ERRCOD = 'INP-8D-5'                                          INDAT1.......43000
            CHERR(1) = OBSPTS(I)%SCHED                                   INDAT1.......43100
            CHERR(2) = OBSPTS(I)%NAME                                    INDAT1.......43200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......43300
  852       DO 855 J=1,NFLOMX                                            INDAT1.......43400
               IF ((OBSPTS(I)%SCHED.EQ.SCHDLS(OFP(J)%ISCHED)%NAME).AND.  INDAT1.......43500
     1             (OBSPTS(I)%FRMT.EQ.OFP(J)%FRMT)) GOTO 860             INDAT1.......43600
  855       CONTINUE                                                     INDAT1.......43700
            NFLOMX = NFLOMX + 1                                          INDAT1.......43800
            OFP(NFLOMX)%ISCHED = INS                                     INDAT1.......43900
            OFP(NFLOMX)%FRMT = OBSPTS(I)%FRMT                            INDAT1.......44000
  860    CONTINUE                                                        INDAT1.......44100
      END IF                                                             INDAT1.......44200
C                                                                        INDAT1.......44300
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR OBSERVATION OUTPUT     INDAT1.......44400
C        FILES.                                                          INDAT1.......44500
      ALLOCATE(IUNIO(NFLOMX),FNAMO(NFLOMX))                              INDAT1.......44600
      CALL FOPEN()                                                       INDAT1.......44700
C                                                                        INDAT1.......44800
C.....OUTPUT OBSERVATION FILE INFORMATION                                INDAT1.......44900
      IF (ISSTRA.EQ.1) THEN                                              INDAT1.......45000
         WRITE(K3,868) IUNIO(1),FNAMO(1)                                 INDAT1.......45100
  868    FORMAT (//13X,'.OBS AND .OBC FILES'/13X,'-------------------'// INDAT1.......45200
     1      (13X,'UNIT ',I7,4X,'ASSIGNED TO ',A80))                      INDAT1.......45300
         WRITE(K3,869)                                                   INDAT1.......45400
  869    FORMAT (/13X,'NOTE: BECAUSE FLOW AND TRANSPORT ARE STEADY-',    INDAT1.......45500
     1      'STATE, USER-DEFINED SCHEDULES ARE NOT IN EFFECT.  '         INDAT1.......45600
     2      /13X,'STEADY-STATE OBSERVATIONS WILL BE WRITTEN TO THE ',    INDAT1.......45700
     3      'APPROPRIATE OUTPUT FILES.')                                 INDAT1.......45800
      ELSE IF (VERNIN.NE."2.0") THEN                                     INDAT1.......45900
         WRITE(K3,870) (SCHDLS(OFP(J)%ISCHED)%NAME,OFP(J)%FRMT,          INDAT1.......46000
     1      IUNIO(J),FNAMO(J),J=1,NFLOMX)                                INDAT1.......46100
  870    FORMAT (//13X,'.OBS AND .OBC FILES'/13X,'-------------------'// INDAT1.......46200
     1      (13X,'SCHEDULE ',A,', FORMAT ',A,', UNIT ',I7,4X,            INDAT1.......46300
     2      'ASSIGNED TO ',A80))                                         INDAT1.......46400
      ELSE                                                               INDAT1.......46500
         WRITE(K3,868) IUNIO(1),FNAMO(1)                                 INDAT1.......46600
         WRITE(K3,872) NOBCYC, SCHDLS(2)%LLEN                            INDAT1.......46700
  872    FORMAT (/13X,'NOTE: OBSERVATION OUTPUT CYCLING ',               INDAT1.......46800
     1      'INFORMATION WAS ENTERED USING THE OLD (VERSION 2D3D.1) '    INDAT1.......46900
     2      'INPUT FORMAT.'/13X,'OBSERVATIONS WILL BE MADE EVERY ',I8,   INDAT1.......47000
     3      ' TIME STEPS, AS WELL AS ON THE FIRST AND LAST TIME STEP,'   INDAT1.......47100
     4      /13X,'FOR A TOTAL OF ',I8,' TIME STEPS.')                    INDAT1.......47200
      END IF                                                             INDAT1.......47300
C                                                                        INDAT1.......47400
C.....OUTPUT GENERALIZED OBSERVATION POINT INFORMATION.                  INDAT1.......47500
      WRITE(K3,1182)                                                     INDAT1.......47600
 1182 FORMAT(////11X,'O B S E R V A T I O N   P O I N T S')              INDAT1.......47700
C.....3D PROBLEM.                                                        INDAT1.......47800
      IF (KTYPE(1).EQ.3) THEN                                            INDAT1.......47900
C........WRITE HEADER.                                                   INDAT1.......48000
         WRITE(K3,1187)                                                  INDAT1.......48100
 1187    FORMAT(                                                         INDAT1.......48200
     1        //13X,'NAME',42X,'COORDINATES',37X,'SCHEDULE',4X,'FORMAT'  INDAT1.......48300
     2         /13X,'----',42X,'-----------',37X,'--------',4X,'------') INDAT1.......48400
C........PRINT INFORMATION FOR EACH POINT.  IF POINTS WERE CONVERTED     INDAT1.......48500
C           FROM NODES, COORDINATES HAVE YET TO BE READ IN, SO PUT IN    INDAT1.......48600
C           A PLACEHOLDER.                                               INDAT1.......48700
         IF (NOBCYC.NE.-1) THEN                                          INDAT1.......48800
            DO 1189 JJ=1,NOBS                                            INDAT1.......48900
               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......49000
               WRITE(K3,1188) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......49100
     1            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......49200
 1188          FORMAT(13X,A,1X,A,1X,                                     INDAT1.......49300
     1            '( _______ TO BE READ FROM DATASET 14 _______ )',      INDAT1.......49400
     2            3X,A,2X,A)                                             INDAT1.......49500
 1189       CONTINUE                                                     INDAT1.......49600
         ELSE                                                            INDAT1.......49700
            DO 1191 JJ=1,NOBS                                            INDAT1.......49800
               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......49900
               WRITE(K3,1190) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......50000
     1            OBSPTS(JJ)%X,OBSPTS(JJ)%Y,OBSPTS(JJ)%Z,                INDAT1.......50100
     2            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......50200
 1190          FORMAT(13X,A,1X,A,1X,'(',2(1PE14.7,','),1PE14.7,')',      INDAT1.......50300
     1            3X,A,2X,A)                                             INDAT1.......50400
 1191       CONTINUE                                                     INDAT1.......50500
         END IF                                                          INDAT1.......50600
C.....2D PROBLEM.                                                        INDAT1.......50700
      ELSE                                                               INDAT1.......50800
C........WRITE HEADER.                                                   INDAT1.......50900
         WRITE(K3,1193)                                                  INDAT1.......51000
 1193    FORMAT(                                                         INDAT1.......51100
     1        //13X,'NAME',42X,'COORDINATES',22X,'SCHEDULE',4X,'FORMAT'  INDAT1.......51200
     2         /13X,'----',42X,'-----------',22X,'--------',4X,'------') INDAT1.......51300
C........PRINT INFORMATION FOR EACH POINT.  IF POINTS WERE CONVERTED     INDAT1.......51400
C           FROM NODES, COORDINATES HAVE YET TO BE READ IN, SO PUT IN    INDAT1.......51500
C           A PLACEHOLDER.                                               INDAT1.......51600
         IF (NOBCYC.NE.-1) THEN                                          INDAT1.......51700
            DO 1195 JJ=1,NOBS                                            INDAT1.......51800
               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......51900
               WRITE(K3,1194) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......52000
     1            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......52100
 1194          FORMAT(13X,A,1X,A,1X,'( TO BE READ FROM DATASET 14  )',   INDAT1.......52200
     1            3X,A,2X,A)                                             INDAT1.......52300
 1195       CONTINUE                                                     INDAT1.......52400
         ELSE                                                            INDAT1.......52500
            DO 1197 JJ=1,NOBS                                            INDAT1.......52600
               LTOP = LEN_TRIM(OBSPTS(JJ)%NAME)                          INDAT1.......52700
               WRITE(K3,1196) TRIM(OBSPTS(JJ)%NAME),DOTS45(1:43-LTOP),   INDAT1.......52800
     1            OBSPTS(JJ)%X,OBSPTS(JJ)%Y,                             INDAT1.......52900
     2            OBSPTS(JJ)%SCHED,OBSPTS(JJ)%FRMT                       INDAT1.......53000
 1196          FORMAT(13X,A,1X,A,1X,'(',1PE14.7,',',1PE14.7,')',         INDAT1.......53100
     1            3X,A,2X,A)                                             INDAT1.......53200
 1197       CONTINUE                                                     INDAT1.......53300
         END IF                                                          INDAT1.......53400
      END IF                                                             INDAT1.......53500
 1199 CONTINUE                                                           INDAT1.......53600
C                                                                        INDAT1.......53700
C.....INPUT DATASET 9:  FLUID PROPERTIES                                 INDAT1.......53800
      ERRCOD = 'REA-INP-9'                                               INDAT1.......53900
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......54000
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPFL,CW,SIGMAW,RHOW0,URHOW0,      INDAT1.......54100
     1   DRWDU,VISC0                                                     INDAT1.......54200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......54300
C.....INPUT DATASET 10:  SOLID MATRIX PROPERTIES                         INDAT1.......54400
      ERRCOD = 'REA-INP-10'                                              INDAT1.......54500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......54600
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPMA,CS,SIGMAS,RHOS               INDAT1.......54700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......54800
      IF(ME.EQ.+1)                                                       INDAT1.......54900
     1  WRITE(K3,1210) COMPFL,COMPMA,CW,CS,VISC0,RHOS,RHOW0,DRWDU,       INDAT1.......55000
     2     URHOW0,SIGMAW,SIGMAS                                          INDAT1.......55100
 1210 FORMAT('1'////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......55200
     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......55300
     2   //11X,1PE15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PE15.4,5X,     INDAT1.......55400
     3   'COMPRESSIBILITY OF POROUS MATRIX'//11X,1PE15.4,5X,             INDAT1.......55500
     4   'SPECIFIC HEAT CAPACITY OF FLUID',/11X,1PE15.4,5X,              INDAT1.......55600
     5   'SPECIFIC HEAT CAPACITY OF SOLID GRAIN'//13X,'FLUID VISCOSITY', INDAT1.......55700
     6   ' IS CALCULATED BY SUTRA AS A FUNCTION OF TEMPERATURE IN ',     INDAT1.......55800
     7   'UNITS OF {kg/(m*s)}'//11X,1PE15.4,5X,'VISC0, CONVERSION ',     INDAT1.......55900
     8   'FACTOR FOR VISCOSITY UNITS,  {desired units} = VISC0*',        INDAT1.......56000
     9   '{kg/(m*s)}'//11X,1PE15.4,5X,'DENSITY OF A SOLID GRAIN'         INDAT1.......56100
     *   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......56200
     1   'SUTRA IN TERMS OF TEMPERATURE, U, AS:'/13X,'RHOW = RHOW0 + ',  INDAT1.......56300
     2   'DRWDU*(U-URHOW0)'//11X,1PE15.4,5X,'FLUID BASE DENSITY, RHOW0'  INDAT1.......56400
     3   /11X,1PE15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......56500
     4   'TEMPERATURE, DRWDU'/11X,1PE15.4,5X,'TEMPERATURE, URHOW0, ',    INDAT1.......56600
     5   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......56700
     6   //11X,1PE15.4,5X,'THERMAL CONDUCTIVITY OF FLUID'                INDAT1.......56800
     7   /11X,1PE15.4,5X,'THERMAL CONDUCTIVITY OF SOLID GRAIN')          INDAT1.......56900
      IF(ME.EQ.-1)                                                       INDAT1.......57000
     1  WRITE(K3,1220) COMPFL,COMPMA,VISC0,RHOS,RHOW0,DRWDU,             INDAT1.......57100
     2     URHOW0,SIGMAW                                                 INDAT1.......57200
 1220 FORMAT('1'////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......57300
     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......57400
     2   //11X,1PE15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PE15.4,5X,     INDAT1.......57500
     3   'COMPRESSIBILITY OF POROUS MATRIX'                              INDAT1.......57600
     4   //11X,1PE15.4,5X,'FLUID VISCOSITY'                              INDAT1.......57700
     4   //11X,1PE15.4,5X,'DENSITY OF A SOLID GRAIN'                     INDAT1.......57800
     5   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......57900
     6   'SUTRA IN TERMS OF SOLUTE CONCENTRATION, U, AS:',               INDAT1.......58000
     7   /13X,'RHOW = RHOW0 + DRWDU*(U-URHOW0)'                          INDAT1.......58100
     8   //11X,1PE15.4,5X,'FLUID BASE DENSITY, RHOW0'                    INDAT1.......58200
     9   /11X,1PE15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......58300
     *   'SOLUTE CONCENTRATION, DRWDU'                                   INDAT1.......58400
     1   /11X,1PE15.4,5X,'SOLUTE CONCENTRATION, URHOW0, ',               INDAT1.......58500
     4   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......58600
     5   //11X,1PE15.4,5X,'MOLECULAR DIFFUSIVITY OF SOLUTE IN FLUID')    INDAT1.......58700
C                                                                        INDAT1.......58800
C.....INPUT DATASET 11:  ADSORPTION PARAMETERS                           INDAT1.......58900
      ERRCOD = 'REA-INP-11'                                              INDAT1.......59000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......59100
      READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD                              INDAT1.......59200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......59300
      IF (ADSMOD.NE.'NONE      ') THEN                                   INDAT1.......59400
         READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD,CHI1,CHI2                 INDAT1.......59500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INDAT1.......59600
      END IF                                                             INDAT1.......59700
      IF(ME.EQ.+1) GOTO 1248                                             INDAT1.......59800
      IF(ADSMOD.EQ.'NONE      ') GOTO 1234                               INDAT1.......59900
      WRITE(K3,1232) ADSMOD                                              INDAT1.......60000
 1232 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......60100
     1   //16X,A10,5X,'EQUILIBRIUM SORPTION ISOTHERM')                   INDAT1.......60200
      GOTO 1236                                                          INDAT1.......60300
 1234 WRITE(K3,1235)                                                     INDAT1.......60400
 1235 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......60500
     1   //16X,'NON-SORBING SOLUTE')                                     INDAT1.......60600
 1236 IF((ADSMOD.EQ.'NONE ').OR.(ADSMOD.EQ.'LINEAR    ').OR.             INDAT1.......60700
     1   (ADSMOD.EQ.'FREUNDLICH').OR.(ADSMOD.EQ.'LANGMUIR  ')) GOTO 1238 INDAT1.......60800
      ERRCOD = 'INP-11-1'                                                INDAT1.......60900
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           INDAT1.......61000
 1238 IF(ADSMOD.EQ.'LINEAR    ') WRITE(K3,1242) CHI1                     INDAT1.......61100
 1242 FORMAT(11X,1PE15.4,5X,'LINEAR DISTRIBUTION COEFFICIENT')           INDAT1.......61200
      IF(ADSMOD.EQ.'FREUNDLICH') WRITE(K3,1244) CHI1,CHI2                INDAT1.......61300
 1244 FORMAT(11X,1PE15.4,5X,'FREUNDLICH DISTRIBUTION COEFFICIENT'        INDAT1.......61400
     1   /11X,1PE15.4,5X,'SECOND FREUNDLICH COEFFICIENT')                INDAT1.......61500
      IF(ADSMOD.EQ.'FREUNDLICH'.AND.CHI2.LE.0.D0) THEN                   INDAT1.......61600
         ERRCOD = 'INP-11-2'                                             INDAT1.......61700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......61800
      ENDIF                                                              INDAT1.......61900
      IF(ADSMOD.EQ.'LANGMUIR  ') WRITE(K3,1246) CHI1,CHI2                INDAT1.......62000
 1246 FORMAT(11X,1PE15.4,5X,'LANGMUIR DISTRIBUTION COEFFICIENT'          INDAT1.......62100
     1   /11X,1PE15.4,5X,'SECOND LANGMUIR COEFFICIENT')                  INDAT1.......62200
C                                                                        INDAT1.......62300
C.....INPUT DATASET 12:  PRODUCTION OF ENERGY OR SOLUTE MASS             INDAT1.......62400
 1248 ERRCOD = 'REA-INP-12'                                              INDAT1.......62500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......62600
      READ(INTFIL,*,IOSTAT=INERR(1)) PRODF0,PRODS0,PRODF1,PRODS1         INDAT1.......62700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......62800
      IF(ME.EQ.-1) WRITE(K3,1250) PRODF0,PRODS0,PRODF1,PRODS1            INDAT1.......62900
 1250 FORMAT(////11X,'P R O D U C T I O N   A N D   D E C A Y   O F   ', INDAT1.......63000
     1   'S P E C I E S   M A S S'//13X,'PRODUCTION RATE (+)'/13X,       INDAT1.......63100
     2   'DECAY RATE (-)'//11X,1PE15.4,5X,'ZERO-ORDER RATE OF SOLUTE ',  INDAT1.......63200
     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PE15.4,5X,                INDAT1.......63300
     4   'ZERO-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',       INDAT1.......63400
     5   'IMMOBILE PHASE'/11X,1PE15.4,5X,'FIRST-ORDER RATE OF SOLUTE ',  INDAT1.......63500
     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PE15.4,5X,                INDAT1.......63600
     4   'FIRST-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',      INDAT1.......63700
     5   'IMMOBILE PHASE')                                               INDAT1.......63800
      IF(ME.EQ.+1) WRITE(K3,1260) PRODF0,PRODS0                          INDAT1.......63900
 1260 FORMAT(////11X,'P R O D U C T I O N   A N D   L O S S   O F   ',   INDAT1.......64000
     1   'E N E R G Y'//13X,'PRODUCTION RATE (+)'/13X,                   INDAT1.......64100
     2   'LOSS RATE (-)'//11X,1PE15.4,5X,'ZERO-ORDER RATE OF ENERGY ',   INDAT1.......64200
     3   'PRODUCTION/LOSS IN FLUID'/11X,1PE15.4,5X,                      INDAT1.......64300
     4   'ZERO-ORDER RATE OF ENERGY PRODUCTION/LOSS IN ',                INDAT1.......64400
     5   'SOLID GRAINS')                                                 INDAT1.......64500
C.....SET PARAMETER SWITCHES FOR EITHER ENERGY OR SOLUTE TRANSPORT       INDAT1.......64600
      IF(ME) 1272,1272,1274                                              INDAT1.......64700
C     FOR SOLUTE TRANSPORT:                                              INDAT1.......64800
 1272 CS=0.0D0                                                           INDAT1.......64900
      CW=1.D00                                                           INDAT1.......65000
      SIGMAS=0.0D0                                                       INDAT1.......65100
      GOTO 1278                                                          INDAT1.......65200
C     FOR ENERGY TRANSPORT:                                              INDAT1.......65300
 1274 ADSMOD='NONE      '                                                INDAT1.......65400
      CHI1=0.0D0                                                         INDAT1.......65500
      CHI2=0.0D0                                                         INDAT1.......65600
      PRODF1=0.0D0                                                       INDAT1.......65700
      PRODS1=0.0D0                                                       INDAT1.......65800
 1278 CONTINUE                                                           INDAT1.......65900
C                                                                        INDAT1.......66000
      IF (KTYPE(1).EQ.3) THEN                                            INDAT1.......66100
C.....READ 3D INPUT FROM DATASETS 13 - 15.                               INDAT1.......66200
C                                                                        INDAT1.......66300
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......66400
      ERRCOD = 'REA-INP-13'                                              INDAT1.......66500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......66600
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY,GRAVZ                   INDAT1.......66700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......66800
      WRITE(K3,1320) GRAVX,GRAVY,GRAVZ                                   INDAT1.......66900
 1320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......67000
     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......67100
     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PE15.4,5X,                   INDAT1.......67200
     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......67300
     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PE15.4,5X,          INDAT1.......67400
     5   'GRAVY = -GRAV * D(ELEVATION)/DY'//13X,'COMPONENT OF GRAVITY',  INDAT1.......67500
     6   ' VECTOR'/13X,'IN +Z DIRECTION, GRAVZ'/11X,1PE15.4,5X,          INDAT1.......67600
     7   'GRAVZ = -GRAV * D(ELEVATION)/DZ')                              INDAT1.......67700
C                                                                        INDAT1.......67800
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......67900
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......68000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......68100
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALZ,PORFAC     INDAT1.......68200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......68300
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......68400
         ERRCOD = 'INP-14A-1'                                            INDAT1.......68500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......68600
      END IF                                                             INDAT1.......68700
      NRTEST=1                                                           INDAT1.......68800
      DO 1450 I=1,NN                                                     INDAT1.......68900
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......69000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......69100
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......69200
     1   POR(II)                                                         INDAT1.......69300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......69400
      X(II)=X(II)*SCALX                                                  INDAT1.......69500
      Y(II)=Y(II)*SCALY                                                  INDAT1.......69600
      Z(II)=Z(II)*SCALZ                                                  INDAT1.......69700
      POR(II)=POR(II)*PORFAC                                             INDAT1.......69800
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......69900
      NROLD=NREG(II)                                                     INDAT1.......70000
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......70100
 1450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......70200
 1460 IF(KNODAL.EQ.0) WRITE(K3,1461) SCALX,SCALY,SCALZ,PORFAC            INDAT1.......70300
 1461 FORMAT('1'////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......70400
     1   'PRINTOUT OF NODE COORDINATES AND POROSITIES ',                 INDAT1.......70500
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'X-SCALE'/   INDAT1.......70600
     3   33X,1PE15.4,5X,'Y-SCALE'/33X,1PE15.4,5X,'Z-SCALE'/              INDAT1.......70700
     4   33X,1PE15.4,5X,'POROSITY FACTOR')                               INDAT1.......70800
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,1463)     INDAT1.......70900
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,1465)     INDAT1.......71000
 1463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......71100
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......71200
 1465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......71300
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......71400
      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......71500
     1   WRITE(K3,1470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1.......71600
 1470 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......71700
     1   'NODE',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                  INDAT1.......71800
     2   (9X,I9,3(3X,1PE14.5),6X,0PF8.5))                                INDAT1.......71900
      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......72000
     1   WRITE(K3,1480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1.......72100
 1480 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......72200
     1   'REGION',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                INDAT1.......72300
     2   (9X,I9,3X,I6,3(3X,1PE14.5),6X,0PF8.5))                          INDAT1.......72400
C                                                                        INDAT1.......72500
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......72600
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......72700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......72800
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMIDFA,PMINFA,        INDAT1.......72900
     1   ANG1FA,ANG2FA,ANG3FA,ALMAXF,ALMIDF,ALMINF,                      INDAT1.......73000
     1   ATMXF,ATMDF,ATMNF                                               INDAT1.......73100
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......73200
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......73300
         ERRCOD = 'INP-15A-1'                                            INDAT1.......73400
         CHERR(1) = '3D'                                                 INDAT1.......73500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......73600
      END IF                                                             INDAT1.......73700
      IF(KELMNT.EQ.+1) THEN                                              INDAT1.......73800
         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......73900
            WRITE(K3,1500)                                               INDAT1.......74000
 1500       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......74100
     1         11X,'ELEMENT',3X,'REGION',4X,                             INDAT1.......74200
     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......74300
     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......74400
     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......74500
     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......74600
     3         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......74700
     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......74800
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......74900
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......75000
     4         2(64X),' IN MAX-PERM',4X,'IN MID-PERM',4X,'IN MIN-PERM',  INDAT1.......75100
     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......75200
     1         2(64X),'   DIRECTION',4X,'  DIRECTION',4X,'  DIRECTION',  INDAT1.......75300
     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......75400
         ELSE                                                            INDAT1.......75500
            WRITE(K3,1501)                                               INDAT1.......75600
 1501       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......75700
     1         11X,'ELEMENT',4X,                                         INDAT1.......75800
     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......75900
     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......76000
     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......76100
     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......76200
     3         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......76300
     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......76400
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......76500
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......76600
     4         119X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM',  INDAT1.......76700
     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......76800
     1         119X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION',  INDAT1.......76900
     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......77000
         END IF                                                          INDAT1.......77100
      END IF                                                             INDAT1.......77200
      LRTEST=1                                                           INDAT1.......77300
      DO 1550 LL=1,NE                                                    INDAT1.......77400
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......77500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......77600
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMID,PMIN,           INDAT1.......77700
     1   ANGLE1,ANGLE2,ANGLE3,ALMAX(L),ALMID(L),ALMIN(L),                INDAT1.......77800
     1   ATMAX(L),ATMID(L),ATMIN(L)                                      INDAT1.......77900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......78000
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......78100
      LROLD=LREG(L)                                                      INDAT1.......78200
      PMAX=PMAX*PMAXFA                                                   INDAT1.......78300
      PMID=PMID*PMIDFA                                                   INDAT1.......78400
      PMIN=PMIN*PMINFA                                                   INDAT1.......78500
      ANGLE1=ANGLE1*ANG1FA                                               INDAT1.......78600
      ANGLE2=ANGLE2*ANG2FA                                               INDAT1.......78700
      ANGLE3=ANGLE3*ANG3FA                                               INDAT1.......78800
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......78900
      ALMID(L)=ALMID(L)*ALMIDF                                           INDAT1.......79000
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......79100
      ATMAX(L)=ATMAX(L)*ATMXF                                            INDAT1.......79200
      ATMID(L)=ATMID(L)*ATMDF                                            INDAT1.......79300
      ATMIN(L)=ATMIN(L)*ATMNF                                            INDAT1.......79400
      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,1520) L,                 INDAT1.......79500
     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......79600
     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......79700
 1520 FORMAT(9X,I9,2X,3(1PE14.5,2X),7X,9(G11.4,4X))                      INDAT1.......79800
      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,1530) L,LREG(L),         INDAT1.......79900
     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......80000
     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......80100
 1530 FORMAT(9X,I9,4X,I5,2X,3(1PE14.5,2X),7X,9(G11.4,4X))                INDAT1.......80200
C                                                                        INDAT1.......80300
C.....ROTATE PERMEABILITY FROM MAX/MID/MIN TO X/Y/Z DIRECTIONS.          INDAT1.......80400
C        BASED ON CODE WRITTEN BY DAVID POLLOCK (USGS).                  INDAT1.......80500
      D2R=1.745329252D-2                                                 INDAT1.......80600
      PANGL1(L)=D2R*ANGLE1                                               INDAT1.......80700
      PANGL2(L)=D2R*ANGLE2                                               INDAT1.......80800
      PANGL3(L)=D2R*ANGLE3                                               INDAT1.......80900
      ZERO = 0D0                                                         INDAT1.......81000
      CALL ROTMAT(PANGL1(L),PANGL2(L),PANGL3(L),Q11,Q12,Q13,             INDAT1.......81100
     1   Q21,Q22,Q23,Q31,Q32,Q33)                                        INDAT1.......81200
      CALL TENSYM(PMAX,PMID,PMIN,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,    INDAT1.......81300
     1   PERMXX(L),PERMXY(L),PERMXZ(L),PERMYX(L),PERMYY(L),PERMYZ(L),    INDAT1.......81400
     2   PERMZX(L),PERMZY(L),PERMZZ(L))                                  INDAT1.......81500
 1550 CONTINUE                                                           INDAT1.......81600
      IF(KELMNT.EQ.0)                                                    INDAT1.......81700
     1   WRITE(K3,1569) PMAXFA,PMIDFA,PMINFA,ANG1FA,ANG2FA,ANG3FA,       INDAT1.......81800
     2      ALMAXF,ALMIDF,ALMINF,ATMXF,ATMDF,ATMNF                       INDAT1.......81900
 1569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......82000
     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......82100
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'MAXIMUM ',  INDAT1.......82200
     3   'PERMEABILITY FACTOR'/33X,1PE15.4,5X,'MIDDLE PERMEABILITY ',    INDAT1.......82300
     4   'FACTOR '/33X,1PE15.4,5X,'MINIMUM PERMEABILITY FACTOR'/         INDAT1.......82400
     5   33X,1PE15.4,5X,'ANGLE1 FACTOR'/33X,1PE15.4,5X,'ANGLE2 FACTOR'/  INDAT1.......82500
     6   33X,1PE15.4,5X,'ANGLE3 FACTOR'/                                 INDAT1.......82600
     7   33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY IN ',      INDAT1.......82700
     8   'MAX-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL ', INDAT1.......82800
     9   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR ',  INDAT1.......82900
     T   'FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION'/          INDAT1.......83000
     1   33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY IN ',        INDAT1.......83100
     2   'MAX-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE ',   INDAT1.......83200
     3   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PE15.4,5X,'FACTOR',   INDAT1.......83300
     4   ' FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')           INDAT1.......83400
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,1573)     INDAT1.......83500
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,1575)     INDAT1.......83600
 1573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......83700
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......83800
 1575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......83900
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......84000
C                                                                        INDAT1.......84100
      ELSE                                                               INDAT1.......84200
C.....READ 2D INPUT FROM DATASETS 13 - 15.                               INDAT1.......84300
C.....NOTE THAT Z = THICKNESS AND PANGL1 = PANGLE.                       INDAT1.......84400
C                                                                        INDAT1.......84500
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......84600
      ERRCOD = 'REA-INP-13'                                              INDAT1.......84700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......84800
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY                         INDAT1.......84900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......85000
      GRAVZ = 0D0                                                        INDAT1.......85100
      WRITE(K3,2320) GRAVX,GRAVY                                         INDAT1.......85200
 2320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......85300
     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......85400
     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PE15.4,5X,                   INDAT1.......85500
     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......85600
     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PE15.4,5X,          INDAT1.......85700
     5   'GRAVY = -GRAV * D(ELEVATION)/DY')                              INDAT1.......85800
C                                                                        INDAT1.......85900
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......86000
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......86100
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......86200
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALTH,PORFAC    INDAT1.......86300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......86400
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......86500
         ERRCOD = 'INP-14A-1'                                            INDAT1.......86600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......86700
      END IF                                                             INDAT1.......86800
      NRTEST=1                                                           INDAT1.......86900
      DO 2450 I=1,NN                                                     INDAT1.......87000
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......87100
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......87200
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......87300
     1   POR(II)                                                         INDAT1.......87400
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......87500
      X(II)=X(II)*SCALX                                                  INDAT1.......87600
      Y(II)=Y(II)*SCALY                                                  INDAT1.......87700
      Z(II)=Z(II)*SCALTH                                                 INDAT1.......87800
      POR(II)=POR(II)*PORFAC                                             INDAT1.......87900
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......88000
      NROLD=NREG(II)                                                     INDAT1.......88100
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......88200
 2450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......88300
 2460 IF(KNODAL.EQ.0) WRITE(K3,2461) SCALX,SCALY,SCALTH,PORFAC           INDAT1.......88400
 2461 FORMAT('1'////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......88500
     1   'PRINTOUT OF NODE COORDINATES, THICKNESSES AND POROSITIES ',    INDAT1.......88600
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'X-SCALE'/   INDAT1.......88700
     1   33X,1PE15.4,5X,'Y-SCALE'/33X,1PE15.4,5X,'THICKNESS FACTOR'/     INDAT1.......88800
     2   33X,1PE15.4,5X,'POROSITY FACTOR')                               INDAT1.......88900
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,2463)     INDAT1.......89000
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,2465)     INDAT1.......89100
 2463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......89200
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......89300
 2465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......89400
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......89500
      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......89600
     1   WRITE(K3,2470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1.......89700
 2470 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......89800
     1   'NODE',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//           INDAT1.......89900
     2   (9X,I9,3(3X,1PE14.5),6X,0PF8.5))                                INDAT1.......90000
      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......90100
     1   WRITE(K3,2480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1.......90200
 2480 FORMAT('1'//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......90300
     1   'REGION',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//         INDAT1.......90400
     2   (9X,I9,3X,I6,3(3X,1PE14.5),6X,0PF8.5))                          INDAT1.......90500
C                                                                        INDAT1.......90600
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......90700
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......90800
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......90900
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMINFA,ANGFAC,        INDAT1.......91000
     1   ALMAXF,ALMINF,ATMAXF,ATMINF                                     INDAT1.......91100
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......91200
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......91300
         ERRCOD = 'INP-15A-1'                                            INDAT1.......91400
         CHERR(1) = '2D'                                                 INDAT1.......91500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......91600
      END IF                                                             INDAT1.......91700
      IF (KELMNT.EQ.+1) THEN                                             INDAT1.......91800
         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......91900
            WRITE(K3,2500)                                               INDAT1.......92000
 2500       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......92100
     1         11X,'ELEMENT',3X,'REGION',4X,'MAXIMUM',9X,'MINIMUM',12X,  INDAT1.......92200
     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1.......92300
     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1.......92400
     4         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1.......92500
     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1.......92600
     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1.......92700
     7         59X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1.......92800
     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1.......92900
     9         67X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1.......93000
     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1.......93100
         ELSE                                                            INDAT1.......93200
            WRITE(K3,2501)                                               INDAT1.......93300
 2501       FORMAT('1'//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......93400
     1         11X,'ELEMENT',4X,'MAXIMUM',9X,'MINIMUM',12X,              INDAT1.......93500
     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1.......93600
     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1.......93700
     4         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1.......93800
     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1.......93900
     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1.......94000
     7         50X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1.......94100
     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1.......94200
     9         58X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1.......94300
     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1.......94400
         END IF                                                          INDAT1.......94500
      END IF                                                             INDAT1.......94600
      LRTEST=1                                                           INDAT1.......94700
      DO 2550 LL=1,NE                                                    INDAT1.......94800
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......94900
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......95000
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMIN,ANGLEX,         INDAT1.......95100
     1   ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)                             INDAT1.......95200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT1.......95300
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......95400
      LROLD=LREG(L)                                                      INDAT1.......95500
      PMAX=PMAX*PMAXFA                                                   INDAT1.......95600
      PMIN=PMIN*PMINFA                                                   INDAT1.......95700
      ANGLEX=ANGLEX*ANGFAC                                               INDAT1.......95800
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......95900
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......96000
      ATMAX(L)=ATMAX(L)*ATMAXF                                           INDAT1.......96100
      ATMIN(L)=ATMIN(L)*ATMINF                                           INDAT1.......96200
      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,2520) L,                 INDAT1.......96300
     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1.......96400
 2520 FORMAT(9X,I9,2X,2(1PE14.5,2X),7X,5(G11.4,4X))                      INDAT1.......96500
      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,2530) L,LREG(L),         INDAT1.......96600
     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1.......96700
 2530 FORMAT(9X,I9,4X,I5,2X,2(1PE14.5,2X),7X,5(G11.4,4X))                INDAT1.......96800
C                                                                        INDAT1.......96900
C.....ROTATE PERMEABILITY FROM MAXIMUM/MINIMUM TO X/Y DIRECTIONS         INDAT1.......97000
      RADIAX=1.745329D-2*ANGLEX                                          INDAT1.......97100
      SINA=DSIN(RADIAX)                                                  INDAT1.......97200
      COSA=DCOS(RADIAX)                                                  INDAT1.......97300
      SINA2=SINA*SINA                                                    INDAT1.......97400
      COSA2=COSA*COSA                                                    INDAT1.......97500
      PERMXX(L)=PMAX*COSA2+PMIN*SINA2                                    INDAT1.......97600
      PERMYY(L)=PMAX*SINA2+PMIN*COSA2                                    INDAT1.......97700
      PERMXY(L)=(PMAX-PMIN)*SINA*COSA                                    INDAT1.......97800
      PERMYX(L)=PERMXY(L)                                                INDAT1.......97900
      PANGL1(L)=RADIAX                                                   INDAT1.......98000
 2550 CONTINUE                                                           INDAT1.......98100
      IF(KELMNT.EQ.0)                                                    INDAT1.......98200
     1   WRITE(K3,2569) PMAXFA,PMINFA,ANGFAC,ALMAXF,ALMINF,ATMAXF,ATMINF INDAT1.......98300
 2569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......98400
     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......98500
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PE15.4,5X,'MAXIMUM ',  INDAT1.......98600
     3   'PERMEABILITY FACTOR'/33X,1PE15.4,5X,'MINIMUM PERMEABILITY ',   INDAT1.......98700
     4   'FACTOR'/33X,1PE15.4,5X,'ANGLE FROM +X TO MAXIMUM DIRECTION',   INDAT1.......98800
     5   ' FACTOR'/33X,1PE15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY' INDAT1.......98900
     6  ,' IN MAX-PERM DIRECTION'/33X,1PE15.4,5X,                        INDAT1.......99000
     7   'FACTOR FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION',   INDAT1.......99100
     8   /33X,1PE15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY',           INDAT1.......99200
     9   ' IN MAX-PERM DIRECTION'/33X,1PE15.4,5X,                        INDAT1.......99300
     *   'FACTOR FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')     INDAT1.......99400
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,2573)     INDAT1.......99500
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,2575)     INDAT1.......99600
 2573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......99700
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......99800
 2575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......99900
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1......100000
C                                                                        INDAT1......100100
      END IF                                                             INDAT1......100200
C                                                                        INDAT1......100300
      RETURN                                                             INDAT1......100400
      END                                                                INDAT1......100500
C                                                                        INDAT1......100600
C     SUBROUTINE        I  N  D  A  T  2           SUTRA VERSION 2.1     INDAT2.........100
C                                                                        INDAT2.........200
C *** PURPOSE :                                                          INDAT2.........300
C ***  TO READ INITIAL CONDITIONS FROM ICS FILE, AND TO                  INDAT2.........400
C ***  INITIALIZE DATA FOR EITHER WARM OR COLD START OF                  INDAT2.........500
C ***  THE SIMULATION.                                                   INDAT2.........600
C                                                                        INDAT2.........700
      SUBROUTINE INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,    INDAT2.........800
     1   SW,DSWDP,PBC,IPBC,IPBCT,NREG,QIN,DPDTITR)                       INDAT2.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT2........1000
      CHARACTER*10 CPUNI,CUUNI                                           INDAT2........1100
      CHARACTER INTFIL*1000                                              INDAT2........1200
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     INDAT2........1300
      DIMENSION INERR(10),RLERR(10)                                      INDAT2........1400
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),PM1(NN),UM1(NN),UM2(NN),SL(NN),  INDAT2........1500
     1   SR(NN),CS1(NN),CS2(NN),CS3(NN),RCIT(NN),SW(NN),DSWDP(NN),       INDAT2........1600
     2   PBC(NBCN),IPBC(NBCN),NREG(NN),QIN(NN),DPDTITR(NN)               INDAT2........1700
      DIMENSION KTYPE(2)                                                 INDAT2........1800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT2........1900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT2........2000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT2........2100
     1   NSOP,NSOU,NBCN                                                  INDAT2........2200
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           INDAT2........2300
      COMMON /FNAMES/ UNAME,FNAME                                        INDAT2........2400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     INDAT2........2500
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT2........2600
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT2........2700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT2........2800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT2........2900
C                                                                        INDAT2........3000
C                                                                        INDAT2........3100
      IF(IREAD) 500,500,620                                              INDAT2........3200
C.....INPUT INITIAL CONDITIONS FOR WARM START                            INDAT2........3300
  500 ERRCOD = 'REA-ICS-1'                                               INDAT2........3400
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........3500
      READ(INTFIL,*,IOSTAT=INERR(1)) DUM,DELTP,DELTU                     INDAT2........3600
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........3700
      ERRCOD = 'REA-ICS-2'                                               INDAT2........3800
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........3900
      READ(INTFIL,*,IOSTAT=INERR(1)) CPUNI                               INDAT2........4000
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........4100
      IF (CPUNI.NE.'NONUNIFORM') THEN                                    INDAT2........4200
         ERRCOD = 'ICS-2-2'                                              INDAT2........4300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........4400
      END IF                                                             INDAT2........4500
      ERRCOD = 'REA-ICS-2'                                               INDAT2........4600
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........4700
      BACKSPACE(K2)                                                      INDAT2........4800
      READ(K2,*,IOSTAT=INERR(1)) (PVEC(I),I=1,NN)                        INDAT2........4900
      IF (INERR(1).NE.0) THEN                                            INDAT2........5000
         ERRCOD = 'REA-ICS-2'                                            INDAT2........5100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........5200
      END IF                                                             INDAT2........5300
      ERRCOD = 'REA-ICS-3'                                               INDAT2........5400
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........5500
      READ(INTFIL,*,IOSTAT=INERR(1)) CUUNI                               INDAT2........5600
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........5700
      IF (CUUNI.NE.'NONUNIFORM') THEN                                    INDAT2........5800
         ERRCOD = 'ICS-3-2'                                              INDAT2........5900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........6000
      END IF                                                             INDAT2........6100
      ERRCOD = 'REA-ICS-3'                                               INDAT2........6200
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........6300
      BACKSPACE(K2)                                                      INDAT2........6400
      READ(K2,*,IOSTAT=INERR(1)) (UVEC(I),I=1,NN)                        INDAT2........6500
      IF (INERR(1).NE.0) THEN                                            INDAT2........6600
         ERRCOD = 'REA-ICS-3'                                            INDAT2........6700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........6800
      END IF                                                             INDAT2........6900
      ERRCOD = 'REA-ICS-4'                                               INDAT2........7000
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........7100
      BACKSPACE(K2)                                                      INDAT2........7200
      READ(K2,*,IOSTAT=INERR(1)) (PM1(I),I=1,NN)                         INDAT2........7300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........7400
      READ(K2,*,IOSTAT=INERR(1)) (UM1(I),I=1,NN)                         INDAT2........7500
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........7600
      READ(K2,*,IOSTAT=INERR(1)) (CS1(I),I=1,NN)                         INDAT2........7700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........7800
      READ(K2,*,IOSTAT=INERR(1)) (RCIT(I),I=1,NN)                        INDAT2........7900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........8000
      READ(K2,*,IOSTAT=INERR(1)) (SW(I),I=1,NN)                          INDAT2........8100
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........8200
      READ(K2,*,IOSTAT=INERR(1)) (QIN(I),I=1,NN)                         INDAT2........8300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........8400
      READ(K2,*,IOSTAT=INERR(1)) (PBC(IPU),IPU=1,NBCN)                   INDAT2........8500
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2........8600
C     CALL ZERO(CS2,NN,0.0D0)                                            INDAT2........8700
C     CALL ZERO(CS3,NN,0.0D0)                                            INDAT2........8800
      CALL ZERO(SL,NN,0.0D0)                                             INDAT2........8900
      CALL ZERO(SR,NN,0.0D0)                                             INDAT2........9000
      CALL ZERO(DSWDP,NN,0.0D0)                                          INDAT2........9100
      DO 550 I=1,NN                                                      INDAT2........9200
  550 UM2(I)=UM1(I)                                                      INDAT2........9300
      GOTO 1000                                                          INDAT2........9400
C                                                                        INDAT2........9500
C.....INPUT INITIAL CONDITIONS FOR COLD START                            INDAT2........9600
  620 ERRCOD = 'REA-ICS-1'                                               INDAT2........9700
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........9800
      READ(INTFIL,*,IOSTAT=INERR(1)) DUM                                 INDAT2........9900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2.......10000
      ERRCOD = 'REA-ICS-2'                                               INDAT2.......10100
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2.......10200
      READ(INTFIL,*,IOSTAT=INERR(1)) CPUNI                               INDAT2.......10300
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2.......10400
      IF (CPUNI.EQ.'UNIFORM') THEN                                       INDAT2.......10500
         ERRCOD = 'REA-ICS-2'                                            INDAT2.......10600
         CALL READIF(K2, INTFIL, ERRCOD)                                 INDAT2.......10700
         BACKSPACE(K2)                                                   INDAT2.......10800
         READ(K2,*,IOSTAT=INERR(1)) PUNI                                 INDAT2.......10900
         IF (INERR(1).NE.0) THEN                                         INDAT2.......11000
            ERRCOD = 'REA-ICS-2'                                         INDAT2.......11100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......11200
         END IF                                                          INDAT2.......11300
         DO 625 I=1,NN                                                   INDAT2.......11400
            PVEC(I) = PUNI                                               INDAT2.......11500
  625    CONTINUE                                                        INDAT2.......11600
      ELSE IF (CPUNI.EQ.'NONUNIFORM') THEN                               INDAT2.......11700
         ERRCOD = 'REA-ICS-2'                                            INDAT2.......11800
         CALL READIF(K2, INTFIL, ERRCOD)                                 INDAT2.......11900
         BACKSPACE(K2)                                                   INDAT2.......12000
         READ(K2,*,IOSTAT=INERR(1)) (PVEC(I),I=1,NN)                     INDAT2.......12100
         IF (INERR(1).NE.0) THEN                                         INDAT2.......12200
            ERRCOD = 'REA-ICS-2'                                         INDAT2.......12300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......12400
         END IF                                                          INDAT2.......12500
      ELSE                                                               INDAT2.......12600
         ERRCOD = 'ICS-2-1'                                              INDAT2.......12700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......12800
      END IF                                                             INDAT2.......12900
      ERRCOD = 'REA-ICS-3'                                               INDAT2.......13000
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2.......13100
      READ(INTFIL,*,IOSTAT=INERR(1)) CUUNI                               INDAT2.......13200
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INDAT2.......13300
      IF (CUUNI.EQ.'UNIFORM') THEN                                       INDAT2.......13400
         ERRCOD = 'REA-ICS-3'                                            INDAT2.......13500
         CALL READIF(K2, INTFIL, ERRCOD)                                 INDAT2.......13600
         BACKSPACE(K2)                                                   INDAT2.......13700
         READ(K2,*,IOSTAT=INERR(1)) UUNI                                 INDAT2.......13800
         IF (INERR(1).NE.0) THEN                                         INDAT2.......13900
            ERRCOD = 'REA-ICS-3'                                         INDAT2.......14000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......14100
         END IF                                                          INDAT2.......14200
         DO 630 I=1,NN                                                   INDAT2.......14300
            UVEC(I) = UUNI                                               INDAT2.......14400
  630    CONTINUE                                                        INDAT2.......14500
      ELSE IF (CUUNI.EQ.'NONUNIFORM') THEN                               INDAT2.......14600
         ERRCOD = 'REA-ICS-3'                                            INDAT2.......14700
         CALL READIF(K2, INTFIL, ERRCOD)                                 INDAT2.......14800
         BACKSPACE(K2)                                                   INDAT2.......14900
         READ(K2,*,IOSTAT=INERR(1)) (UVEC(I),I=1,NN)                     INDAT2.......15000
         IF (INERR(1).NE.0) THEN                                         INDAT2.......15100
            ERRCOD = 'REA-ICS-3'                                         INDAT2.......15200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......15300
         END IF                                                          INDAT2.......15400
      ELSE                                                               INDAT2.......15500
         ERRCOD = 'ICS-3-1'                                              INDAT2.......15600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......15700
      END IF                                                             INDAT2.......15800
C.....START-UP WITH NO PROJECTIONS BY SETTING DELTP AND DELTU            INDAT2.......15900
C        SUCH THAT BDELP=BDELU=0.5D-16 IN PROJECTION FORMULAE FOUND      INDAT2.......16000
C        IN SUBROUTINE SUTRA.                                            INDAT2.......16100
      DELTP=DELT*1.D16                                                   INDAT2.......16200
      DELTU=DELT*1.D16                                                   INDAT2.......16300
C.....INITIALIZE SPECIFIED TIME-VARYING PRESSURES TO INITIAL PRESSURE    INDAT2.......16400
C        VALUES FOR START-UP CALCULATION OF INFLOWS OR OUTFLOWS          INDAT2.......16500
C        (SET QPLITR=0)                                                  INDAT2.......16600
      IF(IPBCT) 680,740,740                                              INDAT2.......16700
  680 DO 730 IP=1,NPBC                                                   INDAT2.......16800
      I=IPBC(IP)                                                         INDAT2.......16900
      IF(I) 700,700,730                                                  INDAT2.......17000
  700 PBC(IP)=PVEC(-I)                                                   INDAT2.......17100
  730 CONTINUE                                                           INDAT2.......17200
C.....INITIALIZE P, U, AND CONSISTENT DENSITY                            INDAT2.......17300
  740 DO 800 I=1,NN                                                      INDAT2.......17400
      PM1(I)=PVEC(I)                                                     INDAT2.......17500
      UM1(I)=UVEC(I)                                                     INDAT2.......17600
      UM2(I)=UVEC(I)                                                     INDAT2.......17700
      RCIT(I)=RHOW0+DRWDU*(UVEC(I)-URHOW0)                               INDAT2.......17800
  800 CONTINUE                                                           INDAT2.......17900
C.....INITIALIZE SATURATION, SW(I)                                       INDAT2.......18000
      CALL ZERO(SW,NN,1.0D0)                                             INDAT2.......18100
      CALL ZERO(DSWDP,NN,0.0D0)                                          INDAT2.......18200
      IF(IUNSAT.NE.1) GOTO 990                                           INDAT2.......18300
      IUNSAT=3                                                           INDAT2.......18400
      DO 900 I=1,NN                                                      INDAT2.......18500
  900 IF(PVEC(I).LT.0) CALL UNSAT(SW(I),DSWDP(I),RELK,PVEC(I),NREG(I))   INDAT2.......18600
  990 CONTINUE                                                           INDAT2.......18700
      CALL ZERO(CS1,NN,CS)                                               INDAT2.......18800
C     CALL ZERO(CS2,NN,0.0D0)                                            INDAT2.......18900
C     CALL ZERO(CS3,NN,0.0D0)                                            INDAT2.......19000
      CALL ZERO(SL,NN,0.0D0)                                             INDAT2.......19100
      CALL ZERO(SR,NN,0.0D0)                                             INDAT2.......19200
      CALL ZERO(DPDTITR,NN,0.0D0)                                        INDAT2.......19300
 1000 CONTINUE                                                           INDAT2.......19400
C                                                                        INDAT2.......19500
C.....SET STARTING TIME OF SIMULATION CLOCK, TSEC.  NOTE THAT THE VALUE  INDAT2.......19600
C        OF TSTART WAS SET IN SUBROUTINE INDAT0, WHERE THE "TIME_STEPS"  INDAT2.......19700
C        SCHEDULE IS DEFINED.                                            INDAT2.......19800
      TSEC=TSTART                                                        INDAT2.......19900
C                                                                        INDAT2.......20000
C                                                                        INDAT2.......20100
      RETURN                                                             INDAT2.......20200
      END                                                                INDAT2.......20300
C                                                                        INDAT2.......20400
C     SUBROUTINE        L  L  D  2  A  R           SUTRA VERSION 2.1     LLD2AR.........100
C                                                                        LLD2AR.........200
C *** PURPOSE :                                                          LLD2AR.........300
C ***  TO LOAD A LINKED LIST OF DOUBLE-PRECISION PAIRS INTO TWO ARRAYS.  LLD2AR.........400
C                                                                        LLD2AR.........500
      SUBROUTINE LLD2AR(LSTLEN, DLIST, DARR1, DARR2)                     LLD2AR.........600
      USE LLDEF                                                          LLD2AR.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LLD2AR.........800
      TYPE (LLD), POINTER :: DEN, DLIST                                  LLD2AR.........900
      DIMENSION DARR1(*), DARR2(*)                                       LLD2AR........1000
C                                                                        LLD2AR........1100
      DEN => DLIST                                                       LLD2AR........1200
      DO 100 K=1,LSTLEN                                                  LLD2AR........1300
         DARR1(K) = DEN%DVALU1                                           LLD2AR........1400
         DARR2(K) = DEN%DVALU2                                           LLD2AR........1500
         DEN => DEN%NENT                                                 LLD2AR........1600
  100 CONTINUE                                                           LLD2AR........1700
C                                                                        LLD2AR........1800
      RETURN                                                             LLD2AR........1900
      END                                                                LLD2AR........2000
C                                                                        LLD2AR........2100
C                                                                        LLD2AR........2200
C     SUBROUTINE        L  L  D  I  N  S           SUTRA VERSION 2.1     LLDINS.........100
C                                                                        LLDINS.........200
C *** PURPOSE :                                                          LLDINS.........300
C ***  TO INSERT A PAIR OF DOUBLE-PRECISION VALUES INTO A LINKED         LLDINS.........400
C ***  LIST, IN ASCENDING ORDER BASED ON THE FIRST VALUE IN THE PAIR.    LLDINS.........500
C                                                                        LLDINS.........600
      SUBROUTINE LLDINS(LSTLEN, DLIST, DNUM1, DNUM2)                     LLDINS.........700
      USE LLDEF                                                          LLDINS.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LLDINS.........900
      TYPE (LLD), POINTER :: DEN, DENPV, DENPI, DENNW, DLIST             LLDINS........1000
C                                                                        LLDINS........1100
C.....IF LIST IS EMPTY, PLACE PAIR AT HEAD OF LIST, ELSE INSERT          LLDINS........1200
C        INTO LIST IN ASCENDING ORDER BASED ON FIRST VALUE               LLDINS........1300
      IF (LSTLEN.EQ.0) THEN                                              LLDINS........1400
C........PLACE AT HEAD                                                   LLDINS........1500
         DLIST%DVALU1 = DNUM1                                            LLDINS........1600
         DLIST%DVALU2 = DNUM2                                            LLDINS........1700
         GOTO 780                                                        LLDINS........1800
      ELSE                                                               LLDINS........1900
C........INSERT INTO LISTS                                               LLDINS........2000
         ALLOCATE(DENPV)                                                 LLDINS........2100
         DENPI => DENPV                                                  LLDINS........2200
         DENPV%NENT => DLIST                                             LLDINS........2300
         DO 770 K=1,LSTLEN                                               LLDINS........2400
            DEN => DENPV%NENT                                            LLDINS........2500
            IF (DNUM1.LT.DEN%DVALU1) THEN                                LLDINS........2600
               ALLOCATE(DENNW)                                           LLDINS........2700
               DENNW%DVALU1 = DNUM1                                      LLDINS........2800
               DENNW%DVALU2 = DNUM2                                      LLDINS........2900
               DENNW%NENT => DEN                                         LLDINS........3000
               IF (K.EQ.1) THEN                                          LLDINS........3100
                  DLIST => DENNW                                         LLDINS........3200
               ELSE                                                      LLDINS........3300
                  DENPV%NENT => DENNW                                    LLDINS........3400
               END IF                                                    LLDINS........3500
               DEALLOCATE(DENPI)                                         LLDINS........3600
               GOTO 780                                                  LLDINS........3700
            END IF                                                       LLDINS........3800
            DENPV => DEN                                                 LLDINS........3900
  770    CONTINUE                                                        LLDINS........4000
C........APPEND TO TAIL                                                  LLDINS........4100
         ALLOCATE(DENNW)                                                 LLDINS........4200
         DENNW%DVALU1 = DNUM1                                            LLDINS........4300
         DENNW%DVALU2 = DNUM2                                            LLDINS........4400
         DEN%NENT => DENNW                                               LLDINS........4500
         DEALLOCATE(DENPI)                                               LLDINS........4600
      END IF                                                             LLDINS........4700
C                                                                        LLDINS........4800
  780 LSTLEN = LSTLEN + 1                                                LLDINS........4900
      RETURN                                                             LLDINS........5000
      END                                                                LLDINS........5100
C                                                                        LLDINS........5200
C     SUBROUTINE        L  O  D  O  B  S           SUTRA VERSION 2.1     LODOBS.........100
C                                                                        LODOBS.........200
C *** PURPOSE :                                                          LODOBS.........300
C ***  TO LOAD OBSERVATION POINT INDICES NOBLIN AT A TIME, STARTING      LODOBS.........400
C ***  WITH INDEX JNEXT, INTO ARRAY JSET.  ONLY OBSERVATION POINTS       LODOBS.........500
C ***  WHOSE SCHEDULE AND OUTPUT FORMAT MATCH THOSE THAT CORRESPOND      LODOBS.........600
C ***  TO FILE INDEX NFLO ARE LOADED.  THE NUMBER OF OBSERVATIONS        LODOBS.........700
C ***  LOADED, JLOAD, CAN BE LESS THAN NOBLIN IF THE LIST OF             LODOBS.........800
C ***  OBSERVATION POINT INDICES IS EXHAUSTED.                           LODOBS.........900
C                                                                        LODOBS........1000
      SUBROUTINE LODOBS(NFLO,JNEXT,OBSPTS,JSET,JLOAD)                    LODOBS........1100
      USE ALLARR, ONLY : OBSDAT                                          LODOBS........1200
      USE SCHDEF                                                         LODOBS........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LODOBS........1400
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         LODOBS........1500
      DIMENSION JSET(*)                                                  LODOBS........1600
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      LODOBS........1700
C                                                                        LODOBS........1800
      NOBS = NOBSN - 1                                                   LODOBS........1900
C                                                                        LODOBS........2000
      JLOAD = 0                                                          LODOBS........2100
      DO 300 J=JNEXT,NOBS                                                LODOBS........2200
         IF ((OBSPTS(J)%FRMT.EQ."OBS").AND.                              LODOBS........2300
     1      (OBSPTS(J)%SCHED.EQ.SCHDLS(OFP(NFLO)%ISCHED)%NAME))          LODOBS........2400
     2      THEN                                                         LODOBS........2500
            JLOAD = JLOAD + 1                                            LODOBS........2600
            JSET(JLOAD) = J                                              LODOBS........2700
            IF ((JLOAD.EQ.NOBLIN).OR.(J.EQ.NOBS)) THEN                   LODOBS........2800
               JNEXT = J + 1                                             LODOBS........2900
               RETURN                                                    LODOBS........3000
            END IF                                                       LODOBS........3100
         END IF                                                          LODOBS........3200
  300 CONTINUE                                                           LODOBS........3300
C                                                                        LODOBS........3400
      JNEXT = NOBS + 1                                                   LODOBS........3500
      RETURN                                                             LODOBS........3600
      END                                                                LODOBS........3700
C                                                                        LODOBS........3800
C     SUBROUTINE        N  A  F  U                 SUTRA VERSION 2.1     NAFU...........100
C                                                                        NAFU...........200
C *** PURPOSE :                                                          NAFU...........300
C ***  TO FIND THE NEXT AVAILABLE FORTRAN UNIT.  ON INPUT, IUNEXT IS     NAFU...........400
C ***  THE UNIT NUMBER FROM WHICH THE SEARCH IS TO BEGIN.  ON OUTPUT,    NAFU...........500
C ***  IUNEXT IS THE NEXT AVAILABLE UNIT NUMBER.                         NAFU...........600
C                                                                        NAFU...........700
      SUBROUTINE NAFU(IUNEXT,NJMAX,FN)                                   NAFU...........800
      USE SCHDEF, ONLY : IUNIO                                           NAFU...........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                NAFU..........1000
      CHARACTER*80 FN,UNAME                                              NAFU..........1100
      CHARACTER*80 ERRCOD,CHERR(10)                                      NAFU..........1200
      LOGICAL EXST                                                       NAFU..........1300
      DIMENSION INERR(10),RLERR(10)                                      NAFU..........1400
      DIMENSION IUNIT(0:8), NKS(2), KLIST(2,20)                          NAFU..........1500
      COMMON /FNAMES/ UNAME,FNAME                                        NAFU..........1600
      COMMON /FUNINS/ NKS,KLIST                                          NAFU..........1700
      COMMON /FUNITA/ IUNIT                                              NAFU..........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     NAFU..........1900
C                                                                        NAFU..........2000
C.....CHECK "SUTRA.FIL" (UNIT K0)                                        NAFU..........2100
  100 IF (IUNEXT.EQ.K0) IUNEXT = IUNEXT + 1                              NAFU..........2200
C.....CHECK NON-INSERTED, NON-OBSERVATION FILES                          NAFU..........2300
  200 DO 300 NFF=0,6                                                     NAFU..........2400
         IF (IUNEXT.EQ.IUNIT(NFF)) THEN                                  NAFU..........2500
            IUNEXT = IUNEXT + 1                                          NAFU..........2600
            GOTO 100                                                     NAFU..........2700
         END IF                                                          NAFU..........2800
  300 CONTINUE                                                           NAFU..........2900
C.....CHECK OBSERVATION FILES                                            NAFU..........3000
  400 DO 500 NJ=1,NJMAX                                                  NAFU..........3100
         IF (IUNEXT.EQ.IUNIO(NJ)) THEN                                   NAFU..........3200
            IUNEXT = IUNEXT + 1                                          NAFU..........3300
            GOTO 100                                                     NAFU..........3400
         END IF                                                          NAFU..........3500
  500 CONTINUE                                                           NAFU..........3600
C.....CHECK INSERTED FILES                                               NAFU..........3700
      IF ((IUNEXT.EQ.K1).OR.(IUNEXT.EQ.K2)) THEN                         NAFU..........3800
         IUNEXT = IUNEXT + 1                                             NAFU..........3900
         GOTO 100                                                        NAFU..........4000
      END IF                                                             NAFU..........4100
      DO 600 I=1,2                                                       NAFU..........4200
      DO 600 K=1,NKS(I)                                                  NAFU..........4300
         IF (IUNEXT.EQ.KLIST(I,K)) THEN                                  NAFU..........4400
            IUNEXT = IUNEXT + 1                                          NAFU..........4500
            GOTO 100                                                     NAFU..........4600
         END IF                                                          NAFU..........4700
  600 CONTINUE                                                           NAFU..........4800
C.....IF THE UNIT NUMBER SELECTED IS NOT VALID, GENERATE ERROR           NAFU..........4900
      INQUIRE(UNIT=IUNEXT, EXIST=EXST)                                   NAFU..........5000
      IF (.NOT.EXST) THEN                                                NAFU..........5100
         ERRCOD = 'FIL-10'                                               NAFU..........5200
         INERR(1) = IUNEXT                                               NAFU..........5300
         CHERR(1) = UNAME                                                NAFU..........5400
         CHERR(2) = FN                                                   NAFU..........5500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        NAFU..........5600
      END IF                                                             NAFU..........5700
C                                                                        NAFU..........5800
      RETURN                                                             NAFU..........5900
      END                                                                NAFU..........6000
C                                                                        NAFU..........6100
C     SUBROUTINE        N  O  D  A  L              SUTRA VERSION 2.1     NODAL..........100
C                                                                        NODAL..........200
C *** PURPOSE :                                                          NODAL..........300
C ***  (1) TO CARRY OUT ALL CELLWISE CALCULATIONS AND TO ADD CELLWISE    NODAL..........400
C ***      TERMS TO THE GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW    NODAL..........500
C ***      AND TRANSPORT EQUATIONS.                                      NODAL..........600
C ***  (2) TO ADD FLUID SOURCE AND SOLUTE MASS OR ENERGY SOURCE TERMS    NODAL..........700
C ***      TO THE MATRIX EQUATIONS.                                      NODAL..........800
C                                                                        NODAL..........900
      SUBROUTINE NODAL(ML,VOL,PMAT,PVEC,UMAT,UVEC,PITER,UITER,PM1,UM1,   NODAL.........1000
     1   UM2,POR,QIN,UIN,QUIN,QINITR,CS1,CS2,CS3,SL,SR,SW,DSWDP,RHO,SOP, NODAL.........1100
     1   NREG,JA)                                                        NODAL.........1200
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                NODAL.........1300
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),     NODAL.........1400
     1   UVEC(NNVEC)                                                     NODAL.........1500
      DIMENSION PITER(NN),UITER(NN),PM1(NN),UM1(NN),UM2(NN),             NODAL.........1600
     1   POR(NN),QIN(NN),UIN(NN),QUIN(NN),QINITR(NN),                    NODAL.........1700
     2   CS1(NN),CS2(NN),CS3(NN),SL(NN),SR(NN),SW(NN),RHO(NN),DSWDP(NN), NODAL.........1800
     3   SOP(NN),NREG(NN)                                                NODAL.........1900
      DIMENSION JA(NDIMJA)                                               NODAL.........2000
      DIMENSION KTYPE(2)                                                 NODAL.........2100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  NODAL.........2200
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             NODAL.........2300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              NODAL.........2400
     1   NSOP,NSOU,NBCN                                                  NODAL.........2500
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        NODAL.........2600
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           NODAL.........2700
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      NODAL.........2800
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        NODAL.........2900
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           NODAL.........3000
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       NODAL.........3100
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  NODAL.........3200
C                                                                        NODAL.........3300
C                                                                        NODAL.........3400
      IF(IUNSAT.NE.0) IUNSAT=1                                           NODAL.........3500
C                                                                        NODAL.........3600
C.....SET UP MATRIX STRUCTURE INFORMATION                                NODAL.........3700
      IF (KSOLVP.EQ.0) THEN                                              NODAL.........3800
         JMID = NBHALF                                                   NODAL.........3900
      ELSE                                                               NODAL.........4000
         JMID = 1                                                        NODAL.........4100
      END IF                                                             NODAL.........4200
C                                                                        NODAL.........4300
C.....DO NOT UPDATE NODAL PARAMETERS ON A TIME STEP WHEN ONLY U IS       NODAL.........4400
C        SOLVED FOR BY BACK SUBSTITUTION (I.E., WHEN NOUMAT=1)           NODAL.........4500
      IF(NOUMAT) 50,50,200                                               NODAL.........4600
C.....SET UNSATURATED FLOW PARAMETERS AT NODES, SW(I) AND DSWDP(I)       NODAL.........4700
   50 DO 120 I=1,NN                                                      NODAL.........4800
      IF(IUNSAT-1) 120,100,120                                           NODAL.........4900
  100 IF(PITER(I)) 110,115,115                                           NODAL.........5000
  110 CALL UNSAT(SW(I),DSWDP(I),RELK,PITER(I),NREG(I))                   NODAL.........5100
      GOTO 120                                                           NODAL.........5200
  115 SW(I)=1.0D0                                                        NODAL.........5300
      DSWDP(I)=0.0D0                                                     NODAL.........5400
  120 CONTINUE                                                           NODAL.........5500
C.....SET FLUID DENSITY AT NODES, RHO(I)                                 NODAL.........5600
C        RHO = F (UITER(I))                                              NODAL.........5700
      DO 150 I=1,NN                                                      NODAL.........5800
  150 RHO(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                               NODAL.........5900
  200 CONTINUE                                                           NODAL.........6000
C                                                                        NODAL.........6100
      DO 1000 I=1,NN                                                     NODAL.........6200
      IF (KSOLVP.EQ.0) THEN                                              NODAL.........6300
         IMID = I                                                        NODAL.........6400
      ELSE                                                               NODAL.........6500
         IMID = JA(I)                                                    NODAL.........6600
      END IF                                                             NODAL.........6700
C                                                                        NODAL.........6800
      SWRHON=SW(I)*RHO(I)                                                NODAL.........6900
C                                                                        NODAL.........7000
      IF(ML-1) 220,220,230                                               NODAL.........7100
C                                                                        NODAL.........7200
C.....CALCULATE CELLWISE TERMS FOR P EQUATION.                           NODAL.........7300
C.....FOR STEADY-STATE FLOW, ISSFLO=2; FOR TRANSIENT FLOW, ISSFLO=0.     NODAL.........7400
  220 AFLN=(1-ISSFLO/2)*                                                 NODAL.........7500
     1   (SWRHON*SOP(I)+POR(I)*RHO(I)*DSWDP(I))*VOL(I)/DELTP             NODAL.........7600
      CFLN=POR(I)*SW(I)*DRWDU*VOL(I)                                     NODAL.........7700
      DUDT=(1-ISSFLO/2)*(UM1(I)-UM2(I))/DLTUM1                           NODAL.........7800
      CFLN=CFLN*DUDT                                                     NODAL.........7900
C.....ADD CELLWISE TERMS AND FLUID SOURCES OR FLUXES TO P EQUATION       NODAL.........8000
      PMAT(IMID,JMID) = PMAT(IMID,JMID) + AFLN                           NODAL.........8100
      PVEC(I) = PVEC(I) - CFLN + AFLN*PM1(I) + QIN(I)                    NODAL.........8200
C                                                                        NODAL.........8300
      IF(ML-1) 230,1000,230                                              NODAL.........8400
C                                                                        NODAL.........8500
C.....CALCULATE CELLWISE TERMS FOR U-EQUATION                            NODAL.........8600
  230 EPRS=(1.D0-POR(I))*RHOS                                            NODAL.........8700
      ATRN=(1-ISSTRA)*(POR(I)*SWRHON*CW+EPRS*CS1(I))*VOL(I)/DELTU        NODAL.........8800
      GTRN=POR(I)*SWRHON*PRODF1*VOL(I)                                   NODAL.........8900
      GSV=EPRS*PRODS1*VOL(I)                                             NODAL.........9000
      GSLTRN=GSV*SL(I)                                                   NODAL.........9100
      GSRTRN=GSV*SR(I)                                                   NODAL.........9200
      ETRN=(POR(I)*SWRHON*PRODF0+EPRS*PRODS0)*VOL(I)                     NODAL.........9300
C.....CALCULATE SOURCES OF SOLUTE OR ENERGY CONTAINED IN                 NODAL.........9400
C        SOURCES OF FLUID (ZERO CONTRIBUTION FOR OUTFLOWING FLUID)       NODAL.........9500
      QUR=0.0D0                                                          NODAL.........9600
      QUL=0.0D0                                                          NODAL.........9700
      IF(QINITR(I)) 360,360,340                                          NODAL.........9800
  340 QUL=-CW*QINITR(I)                                                  NODAL.........9900
      QUR=-QUL*UIN(I)                                                    NODAL........10000
C.....ADD CELLWISE TERMS, SOURCES OF SOLUTE OR ENERGY IN FLUID INFLOWS,  NODAL........10100
C        AND PURE SOURCES OR FLUXES OF SOLUTE OR ENERGY TO U-EQUATION    NODAL........10200
  360 IF(NOUMAT) 370,370,380                                             NODAL........10300
  370 UMAT(IMID,JMID) = UMAT(IMID,JMID) + ATRN - GTRN - GSLTRN - QUL     NODAL........10400
  380 UVEC(I) = UVEC(I) + ATRN*UM1(I) + ETRN + GSRTRN + QUR + QUIN(I)    NODAL........10500
C                                                                        NODAL........10600
 1000 CONTINUE                                                           NODAL........10700
C                                                                        NODAL........10800
      RETURN                                                             NODAL........10900
      END                                                                NODAL........11000
C                                                                        NODAL........11100
C     SUBROUTINE        O  U  T  E  L  E           SUTRA VERSION 2.1     OUTELE.........100
C                                                                        OUTELE.........200
C *** PURPOSE :                                                          OUTELE.........300
C ***  TO PRINT ELEMENT CENTROID COORDINATES AND VELOCITY COMPONENTS     OUTELE.........400
C ***  IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE ELE FILE.     OUTELE.........500
C                                                                        OUTELE.........600
      SUBROUTINE OUTELE(VMAG,VANG1,VANG2,IN,X,Y,Z,TITLE1,TITLE2)         OUTELE.........700
      USE EXPINT                                                         OUTELE.........800
      USE SCHDEF                                                         OUTELE.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTELE........1000
      PARAMETER (NCOLMX=9)                                               OUTELE........1100
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTELE........1200
      CHARACTER*15 COLTK6(7)                                             OUTELE........1300
      CHARACTER*1 CPVX,CPVY,CPVZ                                         OUTELE........1400
      CHARACTER*14 CTYPE2                                                OUTELE........1500
      CHARACTER*80 LAYSTR                                                OUTELE........1600
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                OUTELE........1700
      LOGICAL PRINTE                                                     OUTELE........1800
      DIMENSION IN(NIN),IIN(8)                                           OUTELE........1900
      DIMENSION VMAG(NE),VANG1(NE),VANG2(NEX)                            OUTELE........2000
      DIMENSION X(NN),Y(NN),Z(NN)                                        OUTELE........2100
      DIMENSION VCOL(NCOLMX),VVAR(7)                                     OUTELE........2200
      DIMENSION J5COL(NCOLMX),J6COL(NCOLMX)                              OUTELE........2300
      DIMENSION KTYPE(2)                                                 OUTELE........2400
      ALLOCATABLE TT(:),ITT(:),ISVEL(:)                                  OUTELE........2500
      TYPE (LLD), POINTER :: DENTS                                       OUTELE........2600
      COMMON /CLAY/ LAYSTR                                               OUTELE........2700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTELE........2800
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTELE........2900
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTELE........3000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTELE........3100
     1   NSOP,NSOU,NBCN                                                  OUTELE........3200
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTELE........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTELE........3400
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTELE........3500
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTELE........3600
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTELE........3700
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTELE........3800
     1   KSCRN,KPAUSE                                                    OUTELE........3900
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8                          OUTELE........4000
      COMMON /SCH/ NSCH,ISCHTS                                           OUTELE........4100
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTELE........4200
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTELE........4300
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTELE........4400
      DATA (COLTK6(MM), MM=1,7) /'Element',                              OUTELE........4500
     1   '       X origin', '       Y origin', '       Z origin',        OUTELE........4600
     2   '     X velocity', '     Y velocity', '     Z velocity'/        OUTELE........4700
      SAVE COLTK6                                                        OUTELE........4800
C                                                                        OUTELE........4900
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTELE........5000
C                                                                        OUTELE........5100
      DKTM2 = DBLE(KTYPE(1) - 2)                                         OUTELE........5200
C                                                                        OUTELE........5300
      IF (.NOT. ONCEK6)  THEN                                            OUTELE........5400
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTELE........5500
C                                                                        OUTELE........5600
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTELE........5700
         KT=0                                                            OUTELE........5800
         DO 4 JT=1,ITMAX                                                 OUTELE........5900
            IF (MOD(JT,LCOLPR).EQ.0 .OR. JT.EQ.1) KT = KT + 1            OUTELE........6000
    4    CONTINUE                                                        OUTELE........6100
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,LCOLPR).NE.0) KT = KT + 1         OUTELE........6200
         KTMAX = KT                                                      OUTELE........6300
C                                                                        OUTELE........6400
C........ALLOCATE LOCAL ARRAYS                                           OUTELE........6500
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTELE........6600
         ALLOCATE(ISVEL(KTMAX))                                          OUTELE........6700
C                                                                        OUTELE........6800
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTELE........6900
         TS=TSTART                                                       OUTELE........7000
C........TIME STEP VALUE                                                 OUTELE........7100
         JT=0                                                            OUTELE........7200
C........NUMBER OF PRINTED TIME STEPS                                    OUTELE........7300
         KT=0                                                            OUTELE........7400
C........TIME STEP INCREMENT                                             OUTELE........7500
         DELTK=DELT                                                      OUTELE........7600
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTELE........7700
         LCP = 0                                                         OUTELE........7800
         CPVX = 'N'                                                      OUTELE........7900
         CPVY = 'N'                                                      OUTELE........8000
         CPVZ = 'N'                                                      OUTELE........8100
         DO 8 M=1,NCOLS6                                                 OUTELE........8200
            IF (J6COL(M).EQ.5) CPVX = 'Y'                                OUTELE........8300
            IF (J6COL(M).EQ.6) CPVY = 'Y'                                OUTELE........8400
            IF (J6COL(M).EQ.7) CPVZ = 'Y'                                OUTELE........8500
    8    CONTINUE                                                        OUTELE........8600
         DENTS => SCHDLS(ISCHTS)%SLIST                                   OUTELE........8700
         DO 10 JT=1,ITMAX                                                OUTELE........8800
            DENTS => DENTS%NENT                                          OUTELE........8900
            TS = DENTS%DVALU1                                            OUTELE........9000
            LCV = LCP                                                    OUTELE........9100
            IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCP = JT                OUTELE........9200
            IF (MOD(JT,LCOLPR).EQ.0 .OR. JT.EQ.1) THEN                   OUTELE........9300
               KT = KT + 1                                               OUTELE........9400
               TT(KT) = TS                                               OUTELE........9500
               ITT(KT) = JT                                              OUTELE........9600
               ISVEL(KT) = LCV                                           OUTELE........9700
               IF (JT.NE.1 .OR. ISSFLO.EQ.2) THEN                        OUTELE........9800
                  IF (MOD(JT,NUCYC).NE.0) ISVEL(KT) = 0                  OUTELE........9900
               ENDIF                                                     OUTELE.......10000
            ENDIF                                                        OUTELE.......10100
   10    CONTINUE                                                        OUTELE.......10200
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTELE.......10300
C                                                                        OUTELE.......10400
C                                                                        OUTELE.......10500
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTELE.......10600
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,LCOLPR).NE.0) THEN                OUTELE.......10700
            KT = KT + 1                                                  OUTELE.......10800
            TT(KT) = TS                                                  OUTELE.......10900
            ITT(KT) = ITMAX                                              OUTELE.......11000
            ISVEL(KT) = LCV                                              OUTELE.......11100
         ENDIF                                                           OUTELE.......11200
C                                                                        OUTELE.......11300
C........IF STEADY-STATE FLOW, V BASED ON TIME STEP 0 ONLY, AND          OUTELE.......11400
C           OUTPUT OCCURS ONLY ON TIME STEP 1.                           OUTELE.......11500
         IF (ISSFLO.NE.0) THEN                                           OUTELE.......11600
            KTMAX = 1                                                    OUTELE.......11700
            ISVEL(1) = 0                                                 OUTELE.......11800
         END IF                                                          OUTELE.......11900
C........WRITE HEADER INFORMATION                                        OUTELE.......12000
         WRITE(K6,950) TITLE1, TITLE2                                    OUTELE.......12100
         IF (KTYPE(2).GT.1) THEN                                         OUTELE.......12200
            IF (KTYPE(2).EQ.3) THEN                                      OUTELE.......12300
               CTYPE2 = "BLOCKWISE MESH"                                 OUTELE.......12400
            ELSE                                                         OUTELE.......12500
               CTYPE2 = "REGULAR MESH  "                                 OUTELE.......12600
            END IF                                                       OUTELE.......12700
            IF (KTYPE(1).EQ.3) THEN                                      OUTELE.......12800
               WRITE(K6,951) KTYPE(1), CTYPE2, NN1-1, NN2-1, NN3-1,      OUTELE.......12900
     1            NE, " Elems", NN, " Nodes"                             OUTELE.......13000
            ELSE                                                         OUTELE.......13100
               WRITE(K6,952) KTYPE(1), CTYPE2, NN1-1, NN2-1,             OUTELE.......13200
     1            NE, " Elems", NN, " Nodes"                             OUTELE.......13300
            END IF                                                       OUTELE.......13400
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTELE.......13500
            WRITE(K6,953) KTYPE(1), LAYSTR(1:6), NLAYS-1, NELAY,         OUTELE.......13600
     1         NE, " Elems", NN, " Nodes"                                OUTELE.......13700
         ELSE                                                            OUTELE.......13800
            WRITE(K6,954) KTYPE(1), NE, " Elems", NN, " Nodes"           OUTELE.......13900
         END IF                                                          OUTELE.......14000
         WRITE(K6,960) "VELOCITY RESULTS",                               OUTELE.......14100
     1      KTMAX, "Vx", "Vy", "Vz"                                      OUTELE.......14200
         DO 20  KT=1, KTMAX                                              OUTELE.......14300
            WRITE(K6,961) ITT(KT), TT(KT), CPVX, ISVEL(KT),              OUTELE.......14400
     1         CPVY, ISVEL(KT), CPVZ, ISVEL(KT)                          OUTELE.......14500
   20    CONTINUE                                                        OUTELE.......14600
  950    FORMAT("## ", 80A1,                                             OUTELE.......14700
     1         /"## ", 80A1,                                             OUTELE.......14800
     2         /"## ")                                                   OUTELE.......14900
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTELE.......15000
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTELE.......15100
     2                 "(", I9, A, ")"                                   OUTELE.......15200
     3         /"## ")                                                   OUTELE.......15300
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTELE.......15400
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTELE.......15500
     2                 "(", I9, A, ")"                                   OUTELE.......15600
     3         /"## ")                                                   OUTELE.......15700
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTELE.......15800
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTELE.......15900
     2                 "(", I9, A, ")"                                   OUTELE.......16000
     3         /"## ")                                                   OUTELE.......16100
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTELE.......16200
     1                 "(", I9, A, ")"                                   OUTELE.......16300
     2          /"## ")                                                  OUTELE.......16400
  960    FORMAT("## ", 92("="),                                          OUTELE.......16500
     1         /"## ", A, 48X, I9, " Time steps printed",                OUTELE.......16600
     2         /"## ", 92("="),                                          OUTELE.......16700
     3         /"## ",                                                   OUTELE.......16800
     4         /"## ", 4X, "Time steps", 22X,                            OUTELE.......16900
     5                 "[Printed? / Time step on which V is based]"      OUTELE.......17000
     6         /"## ", 3X, "in this file      Time (sec)",               OUTELE.......17100
     7                 10X,A2, 13X,A2, 13X,A2,                           OUTELE.......17200
     8         /"## ", 2X, 14("-"), 3X, 13("-"), 1X, 3(3X, 12("-")) )    OUTELE.......17300
  961    FORMAT ("## ", 7X, I8, 4X, 1PE13.6, 3(5X, A1, 1X, I8))          OUTELE.......17400
C                                                                        OUTELE.......17500
C........DEALLOCATE LOCAL ARRAYS.                                        OUTELE.......17600
         DEALLOCATE(TT,ITT,ISVEL)                                        OUTELE.......17700
C                                                                        OUTELE.......17800
         ONCEK6 = .TRUE.                                                 OUTELE.......17900
      ENDIF                                                              OUTELE.......18000
C                                                                        OUTELE.......18100
C.....OUTPUT VELOCITIES FOR STEADY FLOW ONLY ON THE FIRST TIME STEP      OUTELE.......18200
      IF ((ISSFLO.EQ.2).AND.(IT.GT.1)) GOTO 9999                         OUTELE.......18300
C                                                                        OUTELE.......18400
C.....VELOCITY HEADER INFORMATION REPEATED BEFORE EACH TIME STEP         OUTELE.......18500
      IF ((IT.EQ.1).AND.(ISSTRA.EQ.1)) THEN                              OUTELE.......18600
         DURN = 0D0                                                      OUTELE.......18700
         TOUT = TSTART                                                   OUTELE.......18800
      ELSE                                                               OUTELE.......18900
         DURN = DELT                                                     OUTELE.......19000
         TOUT = TSEC                                                     OUTELE.......19100
      END IF                                                             OUTELE.......19200
      WRITE(K6,966) IT, DURN, TOUT                                       OUTELE.......19300
  966 FORMAT('## ',                                                      OUTELE.......19400
     1      /'## ', 92('='),                                             OUTELE.......19500
     2      /'## TIME STEP ', I8, 22X, 'Duration: ', 1PE11.4, ' sec',    OUTELE.......19600
     3                            6X, 'Time: ', 1PE11.4, ' sec',         OUTELE.......19700
     4      /'## ', 92('='))                                             OUTELE.......19800
      PRINTE = (J6COL(1).EQ.1)                                           OUTELE.......19900
      IF (PRINTE) THEN                                                   OUTELE.......20000
         WRITE(K6,982) (COLTK6(J6COL(M)), M=1,NCOLS6)                    OUTELE.......20100
  982    FORMAT ("## ", A8, 19(A15))                                     OUTELE.......20200
      ELSE                                                               OUTELE.......20300
         WRITE(K6,983) COLTK6(J6COL(1))(3:15),                           OUTELE.......20400
     1      (COLTK6(J6COL(M)), M=2,NCOLS6)                               OUTELE.......20500
  983    FORMAT ("## ", A13, 19(A15))                                    OUTELE.......20600
      END IF                                                             OUTELE.......20700
C                                                                        OUTELE.......20800
C.....VELOCITY DATA FOR THIS TIME STEP                                   OUTELE.......20900
      RN48 = 1D0/DBLE(N48)                                               OUTELE.......21000
      DO 5000 L=1, NE                                                    OUTELE.......21100
         CENTRX = 0D0                                                    OUTELE.......21200
         CENTRY = 0D0                                                    OUTELE.......21300
         CENTRZ = 0D0                                                    OUTELE.......21400
         DO 1400 II=1, N48                                               OUTELE.......21500
            III=II+(L-1)*N48                                             OUTELE.......21600
            IIN(II)=IN(III)                                              OUTELE.......21700
            CENTRX = CENTRX + X(IIN(II))                                 OUTELE.......21800
            CENTRY = CENTRY + Y(IIN(II))                                 OUTELE.......21900
            CENTRZ = CENTRZ + Z(IIN(II))                                 OUTELE.......22000
 1400    CONTINUE                                                        OUTELE.......22100
         CENTRX = CENTRX*RN48                                            OUTELE.......22200
         CENTRY = CENTRY*RN48                                            OUTELE.......22300
         CENTRZ = CENTRZ*RN48                                            OUTELE.......22400
         VA1 = 0.017453292D0*VANG1(L)                                    OUTELE.......22500
         LL = MIN(L, NEX)                                                OUTELE.......22600
         VA2 = 0.017453292D0*VANG2(LL)*DKTM2                             OUTELE.......22700
         CVA2 = DCOS(VA2)                                                OUTELE.......22800
         VECTRX=VMAG(L) * DCOS(VA1)*CVA2                                 OUTELE.......22900
         VECTRY=VMAG(L) * DSIN(VA1)*CVA2                                 OUTELE.......23000
         VECTRZ=VMAG(L) * DSIN(VA2)                                      OUTELE.......23100
         VVAR(1) = DBLE(L)                                               OUTELE.......23200
         VVAR(2) = CENTRX                                                OUTELE.......23300
         VVAR(3) = CENTRY                                                OUTELE.......23400
         VVAR(4) = CENTRZ                                                OUTELE.......23500
         VVAR(5) = VECTRX                                                OUTELE.......23600
         VVAR(6) = VECTRY                                                OUTELE.......23700
         VVAR(7) = VECTRZ                                                OUTELE.......23800
         DO 1984 M=1,NCOLS6                                              OUTELE.......23900
            VCOL(M) = VVAR(J6COL(M))                                     OUTELE.......24000
 1984    CONTINUE                                                        OUTELE.......24100
         IF (PRINTE) THEN                                                OUTELE.......24200
            WRITE(K6,1985) L,(CUTSML(VCOL(M)), M=2,NCOLS6)               OUTELE.......24300
 1985       FORMAT (I9, 2X, 19(1PE15.7))                                 OUTELE.......24400
         ELSE                                                            OUTELE.......24500
            WRITE(K6,1986) (CUTSML(VCOL(M)), M=1,NCOLS6)                 OUTELE.......24600
 1986       FORMAT (1X, 20(1PE15.7))                                     OUTELE.......24700
         END IF                                                          OUTELE.......24800
 5000 CONTINUE                                                           OUTELE.......24900
C                                                                        OUTELE.......25000
 9999 CONTINUE                                                           OUTELE.......25100
      RETURN                                                             OUTELE.......25200
C                                                                        OUTELE.......25300
      END                                                                OUTELE.......25400
C                                                                        OUTELE.......25500
C     SUBROUTINE        O  U  T  L  S  T  2        SUTRA VERSION 2.1     OUTLST2........100
C                                                                        OUTLST2........200
C *** PURPOSE :                                                          OUTLST2........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST2........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST2........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 2D PROBLEMS.                OUTLST2........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST2........700
C                                                                        OUTLST2........800
      SUBROUTINE OUTLST2(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,                 OUTLST2........900
     1   IERRU,ITRSU,ERRU,PVEC,UVEC,VMAG,VANG,SW)                        OUTLST2.......1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLST2.......1100
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),VMAG(NE),VANG(NE),SW(NN)         OUTLST2.......1200
      DIMENSION KTYPE(2)                                                 OUTLST2.......1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLST2.......1400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTLST2.......1500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLST2.......1600
     1   NSOP,NSOU,NBCN                                                  OUTLST2.......1700
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTLST2.......1800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTLST2.......1900
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTLST2.......2000
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTLST2.......2100
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTLST2.......2200
     1   KSCRN,KPAUSE                                                    OUTLST2.......2300
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLST2.......2400
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLST2.......2500
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTLST2.......2600
C                                                                        OUTLST2.......2700
C.....OUTPUT MAJOR HEADINGS FOR CURRENT TIME STEP                        OUTLST2.......2800
      IF(IT.GT.0.OR.ISSFLO.EQ.2.OR.ISSTRA.EQ.1) GOTO 100                 OUTLST2.......2900
      WRITE(K3,60)                                                       OUTLST2.......3000
   60 FORMAT('1'////11X,'I N I T I A L   C O N D I T I O N S',           OUTLST2.......3100
     1             /11X,'___________________________________')           OUTLST2.......3200
      IF(IREAD.EQ.-1) WRITE(K3,65)                                       OUTLST2.......3300
   65 FORMAT(//11X,'INITIAL CONDITIONS RETRIEVED FROM A RESTART',        OUTLST2.......3400
     1   ' FILE (WARM START).')                                          OUTLST2.......3500
      GOTO 500                                                           OUTLST2.......3600
C                                                                        OUTLST2.......3700
  100 WRITE(K3,350) IT                                                   OUTLST2.......3800
  350 FORMAT('1'//11X,'RESULTS FOR TIME STEP ',I8/                       OUTLST2.......3900
     1   11X,'_______ ___ ____ ____ ________')                           OUTLST2.......4000
C                                                                        OUTLST2.......4100
      IF(ITRMAX.GT.1) THEN                                               OUTLST2.......4200
         IF(IGOI.EQ.0) THEN                                              OUTLST2.......4300
            WRITE(K3,355) ITER                                           OUTLST2.......4400
         ELSE                                                            OUTLST2.......4500
            WRITE(K3,356) ITER                                           OUTLST2.......4600
         END IF                                                          OUTLST2.......4700
  355    FORMAT(/11X,'NON-LINEARITY ITERATIONS CONVERGED AFTER ',I5,     OUTLST2.......4800
     1      ' ITERATIONS')                                               OUTLST2.......4900
  356    FORMAT(/11X,'NON-LINEARITY ITERATIONS  N O T  CONVERGED',       OUTLST2.......5000
     1      ' AFTER ',I5,' ITERATIONS')                                  OUTLST2.......5100
         WRITE(K3,450) RPM,IPWORS,RUM,IUWORS                             OUTLST2.......5200
  450    FORMAT(11X,'MAXIMUM P CHANGE FROM PREVIOUS ITERATION ',         OUTLST2.......5300
     1      1PE14.5,' AT NODE ',I9/11X,'MAXIMUM U CHANGE FROM PREVIOUS', OUTLST2.......5400
     2      ' ITERATION ',1PE14.5,' AT NODE ',I9)                        OUTLST2.......5500
      END IF                                                             OUTLST2.......5600
C                                                                        OUTLST2.......5700
      IF ((ML.EQ.0).OR.(ML.EQ.1)) THEN                                   OUTLST2.......5800
         IF (KSOLVP.EQ.0) THEN                                           OUTLST2.......5900
            WRITE(K3,452)                                                OUTLST2.......6000
         ELSE IF (IERRP.EQ.0) THEN                                       OUTLST2.......6100
            WRITE(K3,455) ITRSP, ERRP                                    OUTLST2.......6200
         ELSE                                                            OUTLST2.......6300
            WRITE(K3,456) ITRSP, ERRP                                    OUTLST2.......6400
         END IF                                                          OUTLST2.......6500
      END IF                                                             OUTLST2.......6600
      IF ((ML.EQ.0).OR.(ML.EQ.2)) THEN                                   OUTLST2.......6700
         IF (ML.EQ.2) WRITE(K3,*) ' '                                    OUTLST2.......6800
         IF (KSOLVU.EQ.0) THEN                                           OUTLST2.......6900
            WRITE(K3,453)                                                OUTLST2.......7000
         ELSE IF (IERRU.EQ.0) THEN                                       OUTLST2.......7100
            WRITE(K3,457) ITRSU, ERRU                                    OUTLST2.......7200
         ELSE                                                            OUTLST2.......7300
            WRITE(K3,458) ITRSU, ERRU                                    OUTLST2.......7400
         END IF                                                          OUTLST2.......7500
      END IF                                                             OUTLST2.......7600
  452 FORMAT(/11X,'P-SOLUTION COMPUTED USING DIRECT SOLVER')             OUTLST2.......7700
  453 FORMAT(11X,'U-SOLUTION COMPUTED USING DIRECT SOLVER')              OUTLST2.......7800
  455 FORMAT(/11X,'P-SOLUTION CONVERGED AFTER ',I5,' MATRIX'             OUTLST2.......7900
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST2.......8000
  456 FORMAT(/11X,'P-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'         OUTLST2.......8100
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST2.......8200
  457 FORMAT(11X,'U-SOLUTION CONVERGED AFTER ',I5,' MATRIX'              OUTLST2.......8300
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST2.......8400
  458 FORMAT(11X,'U-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'          OUTLST2.......8500
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST2.......8600
C                                                                        OUTLST2.......8700
  500 IF(IT.EQ.0.AND.ISSFLO.EQ.2) GOTO 680                               OUTLST2.......8800
      IF(ISSTRA.EQ.1) GOTO 800                                           OUTLST2.......8900
      WRITE(K3,550) DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,                     OUTLST2.......9000
     1   TMONTH,TYEAR                                                    OUTLST2.......9100
  550 FORMAT(///11X,'TIME INCREMENT :',T27,1PE15.4,' SECONDS'//11X,      OUTLST2.......9200
     1   'TIME AT END',3X,T27,1PE15.4,' SECONDS',/11X,'OF STEP:',6X,T27, OUTLST2.......9300
     2   1PE15.4,' MINUTES'/T27,1PE15.4,' HOURS'/T27,1PE15.4,' DAYS'     OUTLST2.......9400
     3   /T27,1PE15.4,' WEEKS'/T27,1PE15.4,' MONTHS'/T27,1PE15.4,        OUTLST2.......9500
     4   ' YEARS')                                                       OUTLST2.......9600
C                                                                        OUTLST2.......9700
C.....OUTPUT PRESSURES FOR TRANSIENT FLOW SOLUTION (AND, POSSIBLY,       OUTLST2.......9800
C        SATURATION AND VELOCITY)                                        OUTLST2.......9900
      IF(ML.EQ.2.AND.ISTOP.GE.0) GOTO 700                                OUTLST2......10000
      IF(ISSFLO.GT.0) GOTO 700                                           OUTLST2......10100
      WRITE(K3,650) (I,CUTSML(PVEC(I)),I=1,NN)                           OUTLST2......10200
  650 FORMAT(///11X,'P  R  E  S  S  U  R  E'                             OUTLST2......10300
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST2......10400
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,CUTSML(SW(I)),I=1,NN)             OUTLST2......10500
  651 FORMAT(///11X,'S  A  T  U  R  A  T  I  O  N'                       OUTLST2......10600
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST2......10700
      IF(KVEL.EQ.1.AND.IT.GT.0) WRITE(K3,655) (L,CUTSML(VMAG(L)),L=1,NE) OUTLST2......10800
      IF(KVEL.EQ.1.AND.IT.GT.0) WRITE(K3,656) (L,CUTSML(VANG(L)),L=1,NE) OUTLST2......10900
  655 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST2......11000
     1   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST2......11100
     2   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST2......11200
  656 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST2......11300
     1   11X,'A N G L E   IN DEGREES FROM +X-AXIS TO FLOW DIRECTION ',   OUTLST2......11400
     2   'AT CENTROID OF ELEMENT'//                                      OUTLST2......11500
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST2......11600
      GOTO 700                                                           OUTLST2......11700
C                                                                        OUTLST2......11800
C.....OUTPUT PRESSURES FOR STEADY-STATE FLOW SOLUTION                    OUTLST2......11900
  680 WRITE(K3,690) (I,CUTSML(PVEC(I)),I=1,NN)                           OUTLST2......12000
  690 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     P  R  E  S', OUTLST2......12100
     1   '  S  U  R  E'//2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))   OUTLST2......12200
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,CUTSML(SW(I)),I=1,NN)             OUTLST2......12300
      GOTO 1000                                                          OUTLST2......12400
C                                                                        OUTLST2......12500
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST2......12600
C        TRANSIENT TRANSPORT SOLUTION                                    OUTLST2......12700
  700 IF(ML.EQ.1.AND.ISTOP.GE.0) GOTO 1000                               OUTLST2......12800
      IF(ME) 720,720,730                                                 OUTLST2......12900
  720 WRITE(K3,725) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST2......13000
  725 FORMAT(///11X,'C  O  N  C  E  N  T  R  A  T  I  O  N'              OUTLST2......13100
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST2......13200
      GOTO 900                                                           OUTLST2......13300
  730 WRITE(K3,735) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST2......13400
  735 FORMAT(///11X,'T  E  M  P  E  R  A  T  U  R  E'                    OUTLST2......13500
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST2......13600
      GOTO 900                                                           OUTLST2......13700
C                                                                        OUTLST2......13800
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST2......13900
C        STEADY-STATE TRANSPORT SOLUTION                                 OUTLST2......14000
  800 IF(ME) 820,820,830                                                 OUTLST2......14100
  820 WRITE(K3,825) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST2......14200
  825 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     C  O  N  C', OUTLST2......14300
     1   '  E  N  T  R  A  T  I  O  N'                                   OUTLST2......14400
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST2......14500
      GOTO 900                                                           OUTLST2......14600
  830 WRITE(K3,835) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST2......14700
  835 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     T  E  M  P', OUTLST2......14800
     1   '  E  R  A  T  U  R  E'                                         OUTLST2......14900
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST2......15000
C                                                                        OUTLST2......15100
C.....OUTPUT VELOCITIES FOR STEADY-STATE FLOW SOLUTION                   OUTLST2......15200
  900 IF(ISSFLO.NE.2.OR.IT.NE.1.OR.KVEL.NE.1) GOTO 1000                  OUTLST2......15300
      WRITE(K3,925) (L,CUTSML(VMAG(L)),L=1,NE)                           OUTLST2......15400
      WRITE(K3,950) (L,CUTSML(VANG(L)),L=1,NE)                           OUTLST2......15500
  925 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST2......15600
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST2......15700
     2   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST2......15800
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST2......15900
  950 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST2......16000
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST2......16100
     2   11X,'A N G L E   IN DEGREES FROM +X-AXIS TO FLOW DIRECTION ',   OUTLST2......16200
     3   'AT CENTROID OF ELEMENT'//                                      OUTLST2......16300
     4   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST2......16400
C                                                                        OUTLST2......16500
 1000 RETURN                                                             OUTLST2......16600
C                                                                        OUTLST2......16700
      END                                                                OUTLST2......16800
C                                                                        OUTLST2......16900
C     SUBROUTINE        O  U  T  L  S  T  3        SUTRA VERSION 2.1     OUTLST3........100
C                                                                        OUTLST3........200
C *** PURPOSE :                                                          OUTLST3........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST3........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST3........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 3D PROBLEMS.                OUTLST3........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST3........700
C                                                                        OUTLST3........800
      SUBROUTINE OUTLST3(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,                 OUTLST3........900
     1   IERRU,ITRSU,ERRU,PVEC,UVEC,VMAG,VANG1,VANG2,SW)                 OUTLST3.......1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLST3.......1100
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),VMAG(NE),VANG1(NE),VANG2(NEX),   OUTLST3.......1200
     1   SW(NN)                                                          OUTLST3.......1300
      DIMENSION KTYPE(2)                                                 OUTLST3.......1400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLST3.......1500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTLST3.......1600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLST3.......1700
     1   NSOP,NSOU,NBCN                                                  OUTLST3.......1800
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTLST3.......1900
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTLST3.......2000
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTLST3.......2100
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTLST3.......2200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTLST3.......2300
     1   KSCRN,KPAUSE                                                    OUTLST3.......2400
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLST3.......2500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLST3.......2600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTLST3.......2700
C                                                                        OUTLST3.......2800
C.....OUTPUT MAJOR HEADINGS FOR CURRENT TIME STEP                        OUTLST3.......2900
      IF(IT.GT.0.OR.ISSFLO.EQ.2.OR.ISSTRA.EQ.1) GOTO 100                 OUTLST3.......3000
      WRITE(K3,60)                                                       OUTLST3.......3100
   60 FORMAT('1'////11X,'I N I T I A L   C O N D I T I O N S',           OUTLST3.......3200
     1             /11X,'___________________________________')           OUTLST3.......3300
      IF(IREAD.EQ.-1) WRITE(K3,65)                                       OUTLST3.......3400
   65 FORMAT(//11X,'INITIAL CONDITIONS RETRIEVED FROM A RESTART',        OUTLST3.......3500
     1   ' FILE (WARM START).')                                          OUTLST3.......3600
      GOTO 500                                                           OUTLST3.......3700
C                                                                        OUTLST3.......3800
  100 WRITE(K3,350) IT                                                   OUTLST3.......3900
  350 FORMAT('1'//11X,'RESULTS FOR TIME STEP ',I8/                       OUTLST3.......4000
     1   11X,'_______ ___ ____ ____ ________')                           OUTLST3.......4100
C                                                                        OUTLST3.......4200
      IF(ITRMAX.GT.1) THEN                                               OUTLST3.......4300
         IF(IGOI.EQ.0) THEN                                              OUTLST3.......4400
            WRITE(K3,355) ITER                                           OUTLST3.......4500
         ELSE                                                            OUTLST3.......4600
            WRITE(K3,356) ITER                                           OUTLST3.......4700
         END IF                                                          OUTLST3.......4800
  355    FORMAT(/11X,'NON-LINEARITY ITERATIONS CONVERGED AFTER ',I5,     OUTLST3.......4900
     1      ' ITERATIONS')                                               OUTLST3.......5000
  356    FORMAT(/11X,'NON-LINEARITY ITERATIONS  N O T  CONVERGED',       OUTLST3.......5100
     1      ' AFTER ',I5,' ITERATIONS')                                  OUTLST3.......5200
         WRITE(K3,450) RPM,IPWORS,RUM,IUWORS                             OUTLST3.......5300
  450    FORMAT(11X,'MAXIMUM P CHANGE FROM PREVIOUS ITERATION ',         OUTLST3.......5400
     1      1PE14.5,' AT NODE ',I9/11X,'MAXIMUM U CHANGE FROM PREVIOUS', OUTLST3.......5500
     2      ' ITERATION ',1PE14.5,' AT NODE ',I9)                        OUTLST3.......5600
      END IF                                                             OUTLST3.......5700
C                                                                        OUTLST3.......5800
      IF ((ML.EQ.0).OR.(ML.EQ.1)) THEN                                   OUTLST3.......5900
         IF (KSOLVP.EQ.0) THEN                                           OUTLST3.......6000
            WRITE(K3,452)                                                OUTLST3.......6100
         ELSE IF (IERRP.EQ.0) THEN                                       OUTLST3.......6200
            WRITE(K3,455) ITRSP, ERRP                                    OUTLST3.......6300
         ELSE                                                            OUTLST3.......6400
            WRITE(K3,456) ITRSP, ERRP                                    OUTLST3.......6500
         END IF                                                          OUTLST3.......6600
      END IF                                                             OUTLST3.......6700
      IF ((ML.EQ.0).OR.(ML.EQ.2)) THEN                                   OUTLST3.......6800
         IF (ML.EQ.2) WRITE(K3,*) ' '                                    OUTLST3.......6900
         IF (KSOLVU.EQ.0) THEN                                           OUTLST3.......7000
            WRITE(K3,453)                                                OUTLST3.......7100
         ELSE IF (IERRU.EQ.0) THEN                                       OUTLST3.......7200
            WRITE(K3,457) ITRSU, ERRU                                    OUTLST3.......7300
         ELSE                                                            OUTLST3.......7400
            WRITE(K3,458) ITRSU, ERRU                                    OUTLST3.......7500
         END IF                                                          OUTLST3.......7600
      END IF                                                             OUTLST3.......7700
  452 FORMAT(/11X,'P-SOLUTION COMPUTED USING DIRECT SOLVER')             OUTLST3.......7800
  453 FORMAT(11X,'U-SOLUTION COMPUTED USING DIRECT SOLVER')              OUTLST3.......7900
  455 FORMAT(/11X,'P-SOLUTION CONVERGED AFTER ',I5,' MATRIX'             OUTLST3.......8000
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST3.......8100
  456 FORMAT(/11X,'P-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'         OUTLST3.......8200
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST3.......8300
  457 FORMAT(11X,'U-SOLUTION CONVERGED AFTER ',I5,' MATRIX'              OUTLST3.......8400
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST3.......8500
  458 FORMAT(11X,'U-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'          OUTLST3.......8600
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PE14.5)                 OUTLST3.......8700
C                                                                        OUTLST3.......8800
  500 IF(IT.EQ.0.AND.ISSFLO.EQ.2) GOTO 680                               OUTLST3.......8900
      IF(ISSTRA.EQ.1) GOTO 800                                           OUTLST3.......9000
      WRITE(K3,550) DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,                     OUTLST3.......9100
     1   TMONTH,TYEAR                                                    OUTLST3.......9200
  550 FORMAT(///11X,'TIME INCREMENT :',T27,1PE15.4,' SECONDS'//11X,      OUTLST3.......9300
     1   'TIME AT END',3X,T27,1PE15.4,' SECONDS',/11X,'OF STEP:',6X,T27, OUTLST3.......9400
     2   1PE15.4,' MINUTES'/T27,1PE15.4,' HOURS'/T27,1PE15.4,' DAYS'     OUTLST3.......9500
     3   /T27,1PE15.4,' WEEKS'/T27,1PE15.4,' MONTHS'/T27,1PE15.4,        OUTLST3.......9600
     4   ' YEARS')                                                       OUTLST3.......9700
C                                                                        OUTLST3.......9800
C.....OUTPUT PRESSURES FOR TRANSIENT FLOW SOLUTION (AND, POSSIBLY,       OUTLST3.......9900
C        SATURATION AND VELOCITY)                                        OUTLST3......10000
      IF(ML.EQ.2.AND.ISTOP.GE.0) GOTO 700                                OUTLST3......10100
      IF(ISSFLO.GT.0) GOTO 700                                           OUTLST3......10200
      WRITE(K3,650) (I,CUTSML(PVEC(I)),I=1,NN)                           OUTLST3......10300
  650 FORMAT(///11X,'P  R  E  S  S  U  R  E'                             OUTLST3......10400
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST3......10500
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,CUTSML(SW(I)),I=1,NN)             OUTLST3......10600
  651 FORMAT(///11X,'S  A  T  U  R  A  T  I  O  N'                       OUTLST3......10700
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST3......10800
      IF(KVEL.EQ.1.AND.IT.GT.0) THEN                                     OUTLST3......10900
         WRITE(K3,655) (L,CUTSML(VMAG(L)),L=1,NE)                        OUTLST3......11000
         WRITE(K3,656) (L,CUTSML(VANG1(L)),L=1,NE)                       OUTLST3......11100
         WRITE(K3,657) (L,CUTSML(VANG2(L)),L=1,NE)                       OUTLST3......11200
      END IF                                                             OUTLST3......11300
  655 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST3......11400
     1   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST3......11500
     2   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST3......11600
  656 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST3......11700
     1   11X,'A N G L E 1   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......11800
     2   '+X-AXIS TO PROJECTION OF FLOW DIRECTION IN XY-PLANE'//         OUTLST3......11900
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST3......12000
  657 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST3......12100
     1   11X,'A N G L E 2   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......12200
     2   'XY-PLANE TO FLOW DIRECTION'//                                  OUTLST3......12300
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST3......12400
C     END IF                                                             OUTLST3......12500
      GOTO 700                                                           OUTLST3......12600
C                                                                        OUTLST3......12700
C.....OUTPUT PRESSURES FOR STEADY-STATE FLOW SOLUTION                    OUTLST3......12800
  680 WRITE(K3,690) (I,CUTSML(PVEC(I)),I=1,NN)                           OUTLST3......12900
  690 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     P  R  E  S', OUTLST3......13000
     1   '  S  U  R  E'//2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))   OUTLST3......13100
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,CUTSML(SW(I)),I=1,NN)             OUTLST3......13200
      GOTO 1000                                                          OUTLST3......13300
C                                                                        OUTLST3......13400
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST3......13500
C        TRANSIENT TRANSPORT SOLUTION                                    OUTLST3......13600
  700 IF(ML.EQ.1.AND.ISTOP.GE.0) GOTO 1000                               OUTLST3......13700
      IF(ME) 720,720,730                                                 OUTLST3......13800
  720 WRITE(K3,725) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST3......13900
  725 FORMAT(///11X,'C  O  N  C  E  N  T  R  A  T  I  O  N'              OUTLST3......14000
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST3......14100
      GOTO 900                                                           OUTLST3......14200
  730 WRITE(K3,735) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST3......14300
  735 FORMAT(///11X,'T  E  M  P  E  R  A  T  U  R  E'                    OUTLST3......14400
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST3......14500
      GOTO 900                                                           OUTLST3......14600
C                                                                        OUTLST3......14700
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST3......14800
C        STEADY-STATE TRANSPORT SOLUTION                                 OUTLST3......14900
  800 IF(ME) 820,820,830                                                 OUTLST3......15000
  820 WRITE(K3,825) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST3......15100
  825 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     C  O  N  C', OUTLST3......15200
     1   '  E  N  T  R  A  T  I  O  N'                                   OUTLST3......15300
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST3......15400
      GOTO 900                                                           OUTLST3......15500
  830 WRITE(K3,835) (I,CUTSML(UVEC(I)),I=1,NN)                           OUTLST3......15600
  835 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     T  E  M  P', OUTLST3......15700
     1   '  E  R  A  T  U  R  E'                                         OUTLST3......15800
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PE15.8)))                 OUTLST3......15900
C                                                                        OUTLST3......16000
C.....OUTPUT VELOCITIES FOR STEADY-STATE FLOW SOLUTION                   OUTLST3......16100
  900 IF(ISSFLO.NE.2.OR.IT.NE.1.OR.KVEL.NE.1) GOTO 1000                  OUTLST3......16200
      WRITE(K3,925) (L,CUTSML(VMAG(L)),L=1,NE)                           OUTLST3......16300
      WRITE(K3,950) (L,CUTSML(VANG1(L)),L=1,NE)                          OUTLST3......16400
      WRITE(K3,951) (L,CUTSML(VANG2(L)),L=1,NE)                          OUTLST3......16500
  925 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST3......16600
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST3......16700
     2   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST3......16800
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST3......16900
  950 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST3......17000
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST3......17100
     2   11X,'A N G L E 1   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......17200
     3   '+X-AXIS TO PROJECTION OF FLOW DIRECTION IN XY-PLANE'//         OUTLST3......17300
     4   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST3......17400
  951 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST3......17500
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST3......17600
     2   11X,'A N G L E 2   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......17700
     3   'XY-PLANE TO FLOW DIRECTION'//                                  OUTLST3......17800
     4   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PE15.8)))                OUTLST3......17900
C                                                                        OUTLST3......18000
 1000 RETURN                                                             OUTLST3......18100
C                                                                        OUTLST3......18200
      END                                                                OUTLST3......18300
C                                                                        OUTLST3......18400
C     SUBROUTINE        O  U  T  N  O  D           SUTRA VERSION 2.1     OUTNOD.........100
C                                                                        OUTNOD.........200
C *** PURPOSE :                                                          OUTNOD.........300
C ***  TO PRINT NODE COORDINATES, PRESSURES, CONCENTRATIONS OR           OUTNOD.........400
C ***  TEMPERATURES, AND SATURATIONS IN A FLEXIBLE, COLUMNWISE FORMAT.   OUTNOD.........500
C ***  OUTPUT IS TO THE NOD FILE.                                        OUTNOD.........600
C                                                                        OUTNOD.........700
      SUBROUTINE OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2)                OUTNOD.........800
      USE EXPINT                                                         OUTNOD.........900
      USE SCHDEF                                                         OUTNOD........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTNOD........1100
      PARAMETER (NCOLMX=9)                                               OUTNOD........1200
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTNOD........1300
      CHARACTER*8  HORP                                                  OUTNOD........1400
      CHARACTER*13 TORC                                                  OUTNOD........1500
      CHARACTER*15 COLTK5(7)                                             OUTNOD........1600
      CHARACTER*1 CPHORP,CPTORC,CPSATU                                   OUTNOD........1700
      CHARACTER*14 CTYPE2                                                OUTNOD........1800
      CHARACTER*80 LAYSTR                                                OUTNOD........1900
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                OUTNOD........2000
      LOGICAL PRINTN                                                     OUTNOD........2100
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),SW(NN)                           OUTNOD........2200
      DIMENSION X(NN),Y(NN),Z(NN)                                        OUTNOD........2300
      DIMENSION VCOL(NCOLMX),VVAR(7)                                     OUTNOD........2400
      DIMENSION J5COL(NCOLMX),J6COL(NCOLMX)                              OUTNOD........2500
      DIMENSION KTYPE(2)                                                 OUTNOD........2600
      ALLOCATABLE TT(:),ITT(:),ISTORC(:),ISHORP(:),ISSATU(:)             OUTNOD........2700
      TYPE (LLD), POINTER :: DENTS                                       OUTNOD........2800
      COMMON /CLAY/ LAYSTR                                               OUTNOD........2900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTNOD........3000
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTNOD........3100
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTNOD........3200
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTNOD........3300
     1   NSOP,NSOU,NBCN                                                  OUTNOD........3400
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTNOD........3500
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTNOD........3600
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTNOD........3700
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTNOD........3800
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTNOD........3900
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTNOD........4000
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTNOD........4100
     1   KSCRN,KPAUSE                                                    OUTNOD........4200
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8                          OUTNOD........4300
      COMMON /SCH/ NSCH,ISCHTS                                           OUTNOD........4400
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTNOD........4500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTNOD........4600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTNOD........4700
      DATA (COLTK5(MM), MM=1,7) /'Node',                                 OUTNOD........4800
     1   '              X', '              Y', '              Z',        OUTNOD........4900
     2   '       Pressure', '  Concentration', '     Saturation'/        OUTNOD........5000
      SAVE COLTK5                                                        OUTNOD........5100
C                                                                        OUTNOD........5200
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTNOD........5300
C                                                                        OUTNOD........5400
C                                                                        OUTNOD........5500
      IF (.NOT. ONCEK5)  THEN                                            OUTNOD........5600
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTNOD........5700
C                                                                        OUTNOD........5800
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTNOD........5900
         IF (ISSTRA.NE.1) THEN                                           OUTNOD........6000
            KT = 1                                                       OUTNOD........6100
         ELSE                                                            OUTNOD........6200
            KT = 0                                                       OUTNOD........6300
         END IF                                                          OUTNOD........6400
         DO 4 JT=1,ITMAX                                                 OUTNOD........6500
            IF (MOD(JT,NCOLPR).EQ.0 .OR.                                 OUTNOD........6600
     1         ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NCOLPR.GT.0))))         OUTNOD........6700
     2         KT = KT + 1                                               OUTNOD........6800
    4    CONTINUE                                                        OUTNOD........6900
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,NCOLPR).NE.0) KT = KT + 1         OUTNOD........7000
         KTMAX = KT                                                      OUTNOD........7100
C                                                                        OUTNOD........7200
C........ALLOCATE LOCAL ARRAYS                                           OUTNOD........7300
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTNOD........7400
         ALLOCATE(ISTORC(KTMAX),ISHORP(KTMAX),ISSATU(KTMAX))             OUTNOD........7500
C                                                                        OUTNOD........7600
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTNOD........7700
         TS=TSTART                                                       OUTNOD........7800
C........TIME STEP VALUE                                                 OUTNOD........7900
         JT=0                                                            OUTNOD........8000
C........NUMBER OF PRINTED TIME STEPS                                    OUTNOD........8100
         KT=0                                                            OUTNOD........8200
C........TIME STEP INCREMENT                                             OUTNOD........8300
         DELTK=DELT                                                      OUTNOD........8400
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTNOD........8500
         LCHORP = 0                                                      OUTNOD........8600
         LCTORC = 0                                                      OUTNOD........8700
         CPHORP = 'N'                                                    OUTNOD........8800
         CPTORC = 'N'                                                    OUTNOD........8900
         CPSATU = 'N'                                                    OUTNOD........9000
         DO 8 M=1,NCOLS5                                                 OUTNOD........9100
            IF (J5COL(M).EQ.5) CPHORP = 'Y'                              OUTNOD........9200
            IF (J5COL(M).EQ.6) CPTORC = 'Y'                              OUTNOD........9300
            IF (J5COL(M).EQ.7) CPSATU = 'Y'                              OUTNOD........9400
    8    CONTINUE                                                        OUTNOD........9500
         IF (ISSTRA.NE.1) THEN                                           OUTNOD........9600
            KT = KT + 1                                                  OUTNOD........9700
            TT(KT) = TS                                                  OUTNOD........9800
            ITT(KT) = JT                                                 OUTNOD........9900
            ISHORP(KT) = 0                                               OUTNOD.......10000
            ISTORC(KT) = 0                                               OUTNOD.......10100
            ISSATU(KT) = 0                                               OUTNOD.......10200
         END IF                                                          OUTNOD.......10300
         DENTS => SCHDLS(ISCHTS)%SLIST                                   OUTNOD.......10400
         DO 10 JT=1,ITMAX                                                OUTNOD.......10500
            DENTS => DENTS%NENT                                          OUTNOD.......10600
            TS = DENTS%DVALU1                                            OUTNOD.......10700
            IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCHORP = JT             OUTNOD.......10800
            IF (MOD(JT,NUCYC).EQ.0 .OR. JT.EQ.1) LCTORC = JT             OUTNOD.......10900
            IF (MOD(JT,NCOLPR).EQ.0 .OR.                                 OUTNOD.......11000
     1         ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NCOLPR.GT.0)))) THEN    OUTNOD.......11100
               KT = KT + 1                                               OUTNOD.......11200
               TT(KT) = TS                                               OUTNOD.......11300
               ITT(KT) = JT                                              OUTNOD.......11400
               ISHORP(KT) = LCHORP                                       OUTNOD.......11500
               ISTORC(KT) = LCTORC                                       OUTNOD.......11600
               ISSATU(KT) = LCHORP                                       OUTNOD.......11700
            ENDIF                                                        OUTNOD.......11800
   10    CONTINUE                                                        OUTNOD.......11900
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTNOD.......12000
C                                                                        OUTNOD.......12100
C                                                                        OUTNOD.......12200
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTNOD.......12300
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,NCOLPR).NE.0) THEN                OUTNOD.......12400
            KT = KT + 1                                                  OUTNOD.......12500
            TT(KT) = TS                                                  OUTNOD.......12600
            ITT(KT) = ITMAX                                              OUTNOD.......12700
            IF (MOD(ITMAX,NPCYC).EQ.0) LCHORP = ITMAX                    OUTNOD.......12800
            IF (MOD(ITMAX,NUCYC).EQ.0) LCTORC = ITMAX                    OUTNOD.......12900
            ISHORP(KT) = LCHORP                                          OUTNOD.......13000
            ISTORC(KT) = LCTORC                                          OUTNOD.......13100
            ISSATU(KT) = LCHORP                                          OUTNOD.......13200
         ENDIF                                                           OUTNOD.......13300
C                                                                        OUTNOD.......13400
C........IF STEADY-STATE FLOW, P AND S CALCULATED ON TIME STEP 0 ONLY.   OUTNOD.......13500
         IF (ISSFLO.NE.0) THEN                                           OUTNOD.......13600
            DO 14 KT=1,KTMAX                                             OUTNOD.......13700
               ISHORP(KT) = 0                                            OUTNOD.......13800
               ISSATU(KT) = 0                                            OUTNOD.......13900
   14       CONTINUE                                                     OUTNOD.......14000
         END IF                                                          OUTNOD.......14100
C                                                                        OUTNOD.......14200
C........SET TEMPERATURE OR CONCENTRATION TEXT STRING FOR HEADER         OUTNOD.......14300
         IF (ME .GT. 0) THEN                                             OUTNOD.......14400
            TORC = "Temperature  "                                       OUTNOD.......14500
            COLTK5(6) = "    Temperature"                                OUTNOD.......14600
         ELSE                                                            OUTNOD.......14700
            TORC = "Concentration"                                       OUTNOD.......14800
         ENDIF                                                           OUTNOD.......14900
C                                                                        OUTNOD.......15000
C........SET PRESSURE TEXT STRING FOR HEADER                             OUTNOD.......15100
         HORP = "Pressure"                                               OUTNOD.......15200
C                                                                        OUTNOD.......15300
C........WRITE HEADER INFORMATION                                        OUTNOD.......15400
         WRITE(K5,950) TITLE1, TITLE2                                    OUTNOD.......15500
         IF (KTYPE(2).GT.1) THEN                                         OUTNOD.......15600
            IF (KTYPE(2).EQ.3) THEN                                      OUTNOD.......15700
               CTYPE2 = "BLOCKWISE MESH"                                 OUTNOD.......15800
            ELSE                                                         OUTNOD.......15900
               CTYPE2 = "REGULAR MESH  "                                 OUTNOD.......16000
            END IF                                                       OUTNOD.......16100
            IF (KTYPE(1).EQ.3) THEN                                      OUTNOD.......16200
               WRITE(K5,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",    OUTNOD.......16300
     1            NE, " Elems"                                           OUTNOD.......16400
            ELSE                                                         OUTNOD.......16500
               WRITE(K5,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",        OUTNOD.......16600
     1            NE, " Elems"                                           OUTNOD.......16700
            END IF                                                       OUTNOD.......16800
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTNOD.......16900
            WRITE(K5,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,           OUTNOD.......17000
     1         NN, " Nodes", NE, " Elems"                                OUTNOD.......17100
         ELSE                                                            OUTNOD.......17200
            WRITE(K5,954) KTYPE(1), NN, " Nodes", NE, " Elems"           OUTNOD.......17300
         END IF                                                          OUTNOD.......17400
         WRITE(K5,960) "NODEWISE RESULTS",                               OUTNOD.......17500
     1      KTMAX, HORP, TORC, "Sat"                                     OUTNOD.......17600
         DO 920  KT=1, KTMAX                                             OUTNOD.......17700
            WRITE(K5,961) ITT(KT), TT(KT), CPHORP, ISHORP(KT),           OUTNOD.......17800
     1         CPTORC, ISTORC(KT), CPSATU, ISSATU(KT)                    OUTNOD.......17900
  920    CONTINUE                                                        OUTNOD.......18000
  950    FORMAT("## ", 80A1,                                             OUTNOD.......18100
     1         /"## ", 80A1,                                             OUTNOD.......18200
     2         /"## ")                                                   OUTNOD.......18300
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTNOD.......18400
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTNOD.......18500
     2                 "(", I9, A, ")"                                   OUTNOD.......18600
     3         /"## ")                                                   OUTNOD.......18700
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTNOD.......18800
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTNOD.......18900
     2                 "(", I9, A, ")"                                   OUTNOD.......19000
     3         /"## ")                                                   OUTNOD.......19100
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTNOD.......19200
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTNOD.......19300
     2                 "(", I9, A, ")"                                   OUTNOD.......19400
     3         /"## ")                                                   OUTNOD.......19500
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTNOD.......19600
     1                 "(", I9, A, ")"                                   OUTNOD.......19700
     2         /"## ")                                                   OUTNOD.......19800
  960    FORMAT("## ", 92("="),                                          OUTNOD.......19900
     1         /"## ", A, 48X, I9, " Time steps printed",                OUTNOD.......20000
     2         /"## ", 92("="),                                          OUTNOD.......20100
     3         /"## ",                                                   OUTNOD.......20200
     4         /"## ", 4X, "Time steps", 23X,                            OUTNOD.......20300
     5                 "[Printed? / Latest time step computed]",         OUTNOD.......20400
     6         /"## ", 3X, "in this file      Time (sec)", 9X,A5,        OUTNOD.......20500
     7                 10X,A4, 11X,A3,                                   OUTNOD.......20600
     8         /"## ", 2X, 14("-"), 3X, 13("-"), 1X, 3(3X, 12("-")) )    OUTNOD.......20700
  961    FORMAT ("## ", 7X, I8, 4X, 1PE13.6, 3(5X, A1, 1X, I8))          OUTNOD.......20800
C                                                                        OUTNOD.......20900
C........DEALLOCATE LOCAL ARRAYS.                                        OUTNOD.......21000
         DEALLOCATE(TT,ITT,ISTORC,ISHORP,ISSATU)                         OUTNOD.......21100
C                                                                        OUTNOD.......21200
         ONCEK5 = .TRUE.                                                 OUTNOD.......21300
      ENDIF                                                              OUTNOD.......21400
C                                                                        OUTNOD.......21500
C.....NODEWISE HEADER INFORMATION REPEATED BEFORE EACH TIME STEP         OUTNOD.......21600
      IF ((IT.EQ.0).OR.((IT.EQ.1).AND.(ISSTRA.EQ.1))) THEN               OUTNOD.......21700
         DURN = 0D0                                                      OUTNOD.......21800
         TOUT = TSTART                                                   OUTNOD.......21900
      ELSE                                                               OUTNOD.......22000
         DURN = DELT                                                     OUTNOD.......22100
         TOUT = TSEC                                                     OUTNOD.......22200
      END IF                                                             OUTNOD.......22300
      WRITE(K5,966) IT, DURN, TOUT                                       OUTNOD.......22400
  966 FORMAT('## ',                                                      OUTNOD.......22500
     1      /'## ', 92('='),                                             OUTNOD.......22600
     2      /'## TIME STEP ', I8, 22X, 'Duration: ', 1PE11.4, ' sec',    OUTNOD.......22700
     3                            6X, 'Time: ', 1PE11.4, ' sec',         OUTNOD.......22800
     4      /'## ', 92('='))                                             OUTNOD.......22900
      PRINTN = (J5COL(1).EQ.1)                                           OUTNOD.......23000
      IF (PRINTN) THEN                                                   OUTNOD.......23100
         WRITE(K5,968) (COLTK5(J5COL(M)), M=1,NCOLS5)                    OUTNOD.......23200
  968    FORMAT ("## ", 2X, A4, 19(A15))                                 OUTNOD.......23300
      ELSE                                                               OUTNOD.......23400
         WRITE(K5,969) COLTK5(J5COL(1))(3:15),                           OUTNOD.......23500
     1      (COLTK5(J5COL(M)), M=2,NCOLS5)                               OUTNOD.......23600
  969    FORMAT ("## ", A13, 19(A15))                                    OUTNOD.......23700
      END IF                                                             OUTNOD.......23800
C                                                                        OUTNOD.......23900
C.....NODEWISE DATA FOR THIS TIME STEP                                   OUTNOD.......24000
      DO 978 I=1,NN                                                      OUTNOD.......24100
         VVAR(1) = DBLE(I)                                               OUTNOD.......24200
         VVAR(2) = X(I)                                                  OUTNOD.......24300
         VVAR(3) = Y(I)                                                  OUTNOD.......24400
         VVAR(4) = Z(I)                                                  OUTNOD.......24500
         VVAR(5) = PVEC(I)                                               OUTNOD.......24600
         VVAR(6) = UVEC(I)                                               OUTNOD.......24700
         VVAR(7) = SW(I)                                                 OUTNOD.......24800
         DO 972 M=1,NCOLS5                                               OUTNOD.......24900
            VCOL(M) = VVAR(J5COL(M))                                     OUTNOD.......25000
  972    CONTINUE                                                        OUTNOD.......25100
         IF (PRINTN) THEN                                                OUTNOD.......25200
            WRITE(K5,975) I,(CUTSML(VCOL(M)), M=2,NCOLS5)                OUTNOD.......25300
  975       FORMAT (I9, 19(1PE15.7))                                     OUTNOD.......25400
         ELSE                                                            OUTNOD.......25500
            WRITE(K5,976) (CUTSML(VCOL(M)), M=1,NCOLS5)                  OUTNOD.......25600
  976       FORMAT (1X, 20(1PE15.7))                                     OUTNOD.......25700
         END IF                                                          OUTNOD.......25800
  978 CONTINUE                                                           OUTNOD.......25900
C                                                                        OUTNOD.......26000
      RETURN                                                             OUTNOD.......26100
C                                                                        OUTNOD.......26200
      END                                                                OUTNOD.......26300
C                                                                        OUTNOD.......26400
C     SUBROUTINE        O  U  T  O  B  C           SUTRA VERSION 2.1     OUTOBC.........100
C                                                                        OUTOBC.........200
C *** PURPOSE :                                                          OUTOBC.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION POINTS.  SPECIFICALLY,       OUTOBC.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBC.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT SIMILAR TO THAT USED IN THE    OUTOBC.........600
C ***  NODEWISE AND ELEMENTWISE OUTPUT FILES.                            OUTOBC.........700
C                                                                        OUTOBC.........800
      SUBROUTINE OUTOBC(NFLO,OBSPTS,TIME,STEP,PM1,UM1,PVEC,UVEC,         OUTOBC.........900
     1   TITLE1,TITLE2,IN,LREG)                                          OUTOBC........1000
      USE ALLARR, ONLY : OBSDAT                                          OUTOBC........1100
      USE LLDEF                                                          OUTOBC........1200
      USE EXPINT                                                         OUTOBC........1300
      USE SCHDEF                                                         OUTOBC........1400
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTOBC........1500
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTOBC........1600
      CHARACTER*8  HORP                                                  OUTOBC........1700
      CHARACTER*13 TORC1,TORC2                                           OUTOBC........1800
      CHARACTER*15 COLTK8(7)                                             OUTOBC........1900
      CHARACTER*14 CTYPE2                                                OUTOBC........2000
      CHARACTER*80 UNAME,FNAME(0:8)                                      OUTOBC........2100
      CHARACTER*40 OBSNM, BLANKS                                         OUTOBC........2200
      CHARACTER*80 LAYSTR                                                OUTOBC........2300
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                OUTOBC........2400
      LOGICAL IMPRTD                                                     OUTOBC........2500
      DIMENSION PM1(NNVEC),UM1(NNVEC),PVEC(NNVEC),UVEC(NNVEC)            OUTOBC........2600
      DIMENSION IN(NIN),LREG(NE)                                         OUTOBC........2700
      DIMENSION KTYPE(2)                                                 OUTOBC........2800
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         OUTOBC........2900
      ALLOCATABLE TT(:),DITT(:),ISTORC(:),ISHORP(:),ISSATU(:)            OUTOBC........3000
      TYPE (LLD), POINTER :: DENTS, DENOBC                               OUTOBC........3100
      COMMON /CLAY/ LAYSTR                                               OUTOBC........3200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTOBC........3300
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTOBC........3400
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTOBC........3500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTOBC........3600
     1   NSOP,NSOU,NBCN                                                  OUTOBC........3700
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTOBC........3800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTOBC........3900
      COMMON /FNAMES/ UNAME,FNAME                                        OUTOBC........4000
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTOBC........4100
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTOBC........4200
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTOBC........4300
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTOBC........4400
     1   KSCRN,KPAUSE                                                    OUTOBC........4500
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      OUTOBC........4600
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8                          OUTOBC........4700
      COMMON /SCH/ NSCH,ISCHTS                                           OUTOBC........4800
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTOBC........4900
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTOBC........5000
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTOBC........5100
      DATA (COLTK8(MM), MM=1,7) /'Name',                                 OUTOBC........5200
     1   '              X', '              Y', '              Z',        OUTOBC........5300
     2   '       Pressure', '  Concentration', '     Saturation'/        OUTOBC........5400
      DATA BLANKS /"                                        "/           OUTOBC........5500
      SAVE COLTK8                                                        OUTOBC........5600
C                                                                        OUTOBC........5700
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTOBC........5800
C                                                                        OUTOBC........5900
      NOBS=NOBSN-1                                                       OUTOBC........6000
      DITMAX = DNINT(DBLE(ITMAX))                                        OUTOBC........6100
C                                                                        OUTOBC........6200
      K8 = IUNIO(NFLO)                                                   OUTOBC........6300
      IF (.NOT. ONCK78(NFLO))  THEN                                      OUTOBC........6400
C.....FIRST CALL FOR THIS FILE -- CREATE FILE HEADER.                    OUTOBC........6500
C                                                                        OUTOBC........6600
C........IF NO OBSERVATION POINTS, WRITE MESSAGE AND RETURN              OUTOBC........6700
         IF (NOBSN-1.EQ.0) THEN                                          OUTOBC........6800
            WRITE(K8,5) TITLE1, TITLE2                                   OUTOBC........6900
    5       FORMAT("## ", 80A1,                                          OUTOBC........7000
     1         /"## ", 80A1,                                             OUTOBC........7100
     2         /"## ")                                                   OUTOBC........7200
            WRITE(K8,7)                                                  OUTOBC........7300
    7       FORMAT(/'  *** NO OBSERVATION POINTS SPECIFIED ',            OUTOBC........7400
     1         '(NOBS=0) ***')                                           OUTOBC........7500
            ONCK78(NFLO) = .TRUE.                                        OUTOBC........7600
            ONCEK8 = .TRUE.                                              OUTOBC........7700
            RETURN                                                       OUTOBC........7800
         END IF                                                          OUTOBC........7900
C                                                                        OUTOBC........8000
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTOBC........8100
         IF (ISSTRA.NE.0) THEN                                           OUTOBC........8200
            KTMAX = 1                                                    OUTOBC........8300
            IMPRTD = .TRUE.                                              OUTOBC........8400
         ELSE                                                            OUTOBC........8500
            KTMAX = 2                                                    OUTOBC........8600
            IMPRTD = .FALSE.                                             OUTOBC........8700
            LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                       OUTOBC........8800
            IF (LENSCH.NE.0) THEN                                        OUTOBC........8900
               DENOBC => SCHDLS(OFP(NFLO)%ISCHED)%SLIST                  OUTOBC........9000
               KTMAX = KTMAX + 1                                         OUTOBC........9100
               SOB = DENOBC%DVALU2                                       OUTOBC........9200
               IF (SOB.EQ.0D0) KTMAX = KTMAX - 1                         OUTOBC........9300
               DO 4 JT=2,LENSCH                                          OUTOBC........9400
                  DENOBC => DENOBC%NENT                                  OUTOBC........9500
                  KTMAX = KTMAX + 1                                      OUTOBC........9600
    4          CONTINUE                                                  OUTOBC........9700
               SOB = DENOBC%DVALU2                                       OUTOBC........9800
               IF (SOB.EQ.DITMAX) THEN                                   OUTOBC........9900
                  KTMAX = KTMAX - 1                                      OUTOBC.......10000
                  IMPRTD = .TRUE.                                        OUTOBC.......10100
               END IF                                                    OUTOBC.......10200
            END IF                                                       OUTOBC.......10300
         END IF                                                          OUTOBC.......10400
C                                                                        OUTOBC.......10500
C........ALLOCATE LOCAL ARRAYS                                           OUTOBC.......10600
         ALLOCATE(TT(KTMAX),DITT(KTMAX))                                 OUTOBC.......10700
         ALLOCATE(ISTORC(KTMAX),ISHORP(KTMAX),ISSATU(KTMAX))             OUTOBC.......10800
C                                                                        OUTOBC.......10900
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTOBC.......11000
         IF (ISSTRA.NE.0) THEN                                           OUTOBC.......11100
            KT = 1                                                       OUTOBC.......11200
            TT(KT) = TSTART                                              OUTOBC.......11300
            DITT(KT) = 1                                                 OUTOBC.......11400
            ISHORP(KT) = 0                                               OUTOBC.......11500
            ISTORC(KT) = 1                                               OUTOBC.......11600
            ISSATU(KT) = 0                                               OUTOBC.......11700
         ELSE                                                            OUTOBC.......11800
C...........NUMBER OF PRINTED TIME STEPS                                 OUTOBC.......11900
            KT=0                                                         OUTOBC.......12000
C...........TIME STEP INCREMENT                                          OUTOBC.......12100
            DELTK=DELT                                                   OUTOBC.......12200
C...........POINTERS TO TIME STEP AND OBSERVATIONS SCHEDULES             OUTOBC.......12300
            DENTS => SCHDLS(ISCHTS)%SLIST                                OUTOBC.......12400
            DENOBC => SCHDLS(OFP(NFLO)%ISCHED)%SLIST                     OUTOBC.......12500
            LCNT = 1                                                     OUTOBC.......12600
C...........OBSERVATION TIME AND STEP                                    OUTOBC.......12700
            TOB = DENOBC%DVALU1                                          OUTOBC.......12800
            SOB = DENOBC%DVALU2                                          OUTOBC.......12900
C...........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED      OUTOBC.......13000
            LCHORP = 0                                                   OUTOBC.......13100
            LCTORC = 0                                                   OUTOBC.......13200
C...........INITIAL/STARTING CONDITIONS ARE PRINTED                      OUTOBC.......13300
            KT = KT + 1                                                  OUTOBC.......13400
            TJT = DENTS%DVALU1                                           OUTOBC.......13500
            SJT = DENTS%DVALU2                                           OUTOBC.......13600
            TT(KT) = TJT                                                 OUTOBC.......13700
            DITT(KT) = SJT                                               OUTOBC.......13800
            ISHORP(KT) = 0                                               OUTOBC.......13900
            ISTORC(KT) = 0                                               OUTOBC.......14000
            ISSATU(KT) = 0                                               OUTOBC.......14100
            IF ((SOB.EQ.SJT).AND.(LCNT.LT.LENSCH)) THEN                  OUTOBC.......14200
               DENOBC => DENOBC%NENT                                     OUTOBC.......14300
               TOB = DENOBC%DVALU1                                       OUTOBC.......14400
               SOB = DENOBC%DVALU2                                       OUTOBC.......14500
               LCNT = LCNT + 1                                           OUTOBC.......14600
            END IF                                                       OUTOBC.......14700
C...........IDENTIFY AND RECORD SCHEDULED OUTPUT                         OUTOBC.......14800
            DO 10 JT=1,ITMAX                                             OUTOBC.......14900
               DJT = DNINT(DBLE(JT))                                     OUTOBC.......15000
               DENTS => DENTS%NENT                                       OUTOBC.......15100
               TJT = DENTS%DVALU1                                        OUTOBC.......15200
               SJT = DENTS%DVALU2                                        OUTOBC.......15300
               IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCHORP = JT          OUTOBC.......15400
               IF (MOD(JT,NUCYC).EQ.0 .OR. JT.EQ.1) LCTORC = JT          OUTOBC.......15500
               DO WHILE ((SOB.LE.DJT).AND.(LCNT.LE.LENSCH))              OUTOBC.......15600
                  KT = KT + 1                                            OUTOBC.......15700
                  TT(KT) = TOB                                           OUTOBC.......15800
                  DITT(KT) = SOB                                         OUTOBC.......15900
                  ISHORP(KT) = LCHORP                                    OUTOBC.......16000
                  ISTORC(KT) = LCTORC                                    OUTOBC.......16100
                  ISSATU(KT) = LCHORP                                    OUTOBC.......16200
                  IF (LCNT.LT.LENSCH) THEN                               OUTOBC.......16300
                     DENOBC => DENOBC%NENT                               OUTOBC.......16400
                     TOB = DENOBC%DVALU1                                 OUTOBC.......16500
                     SOB = DENOBC%DVALU2                                 OUTOBC.......16600
                  END IF                                                 OUTOBC.......16700
                  LCNT = LCNT + 1                                        OUTOBC.......16800
               END DO                                                    OUTOBC.......16900
   10       CONTINUE                                                     OUTOBC.......17000
         END IF                                                          OUTOBC.......17100
C                                                                        OUTOBC.......17200
C                                                                        OUTOBC.......17300
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTOBC.......17400
         IF (.NOT.IMPRTD) THEN                                           OUTOBC.......17500
            KT = KT + 1                                                  OUTOBC.......17600
            TT(KT) = TJT                                                 OUTOBC.......17700
            DITT(KT) = SJT                                               OUTOBC.......17800
            IF (MOD(JT,NPCYC).EQ.0) LCHORP = ITMAX                       OUTOBC.......17900
            IF (MOD(JT,NUCYC).EQ.0) LCTORC = ITMAX                       OUTOBC.......18000
            ISHORP(KT) = LCHORP                                          OUTOBC.......18100
            ISTORC(KT) = LCTORC                                          OUTOBC.......18200
            ISSATU(KT) = LCHORP                                          OUTOBC.......18300
         ENDIF                                                           OUTOBC.......18400
C                                                                        OUTOBC.......18500
C........IF STEADY-STATE FLOW, P AND S CALCULATED ON TIME STEP 0 ONLY.   OUTOBC.......18600
         IF (ISSFLO.NE.0) THEN                                           OUTOBC.......18700
            DO 14 KT=1,KTMAX                                             OUTOBC.......18800
               ISHORP(KT) = 0                                            OUTOBC.......18900
               ISSATU(KT) = 0                                            OUTOBC.......19000
   14       CONTINUE                                                     OUTOBC.......19100
         END IF                                                          OUTOBC.......19200
C                                                                        OUTOBC.......19300
C........SET TEMPERATURE OR CONCENTRATION TEXT STRING FOR HEADER         OUTOBC.......19400
         IF (ME .GT. 0) THEN                                             OUTOBC.......19500
            TORC1 = "Temperature  "                                      OUTOBC.......19600
            TORC2 = "  Temperature"                                      OUTOBC.......19700
         ELSE                                                            OUTOBC.......19800
            TORC1 = "Concentration"                                      OUTOBC.......19900
            TORC2 = "Concentration"                                      OUTOBC.......20000
         ENDIF                                                           OUTOBC.......20100
C                                                                        OUTOBC.......20200
C........SET PRESSURE TEXT STRING FOR HEADER                             OUTOBC.......20300
         HORP = "Pressure"                                               OUTOBC.......20400
C                                                                        OUTOBC.......20500
C........WRITE HEADER INFORMATION                                        OUTOBC.......20600
         WRITE(K8,5) TITLE1, TITLE2                                      OUTOBC.......20700
         IF (KTYPE(2).GT.1) THEN                                         OUTOBC.......20800
            IF (KTYPE(2).EQ.3) THEN                                      OUTOBC.......20900
               CTYPE2 = "BLOCKWISE MESH"                                 OUTOBC.......21000
            ELSE                                                         OUTOBC.......21100
               CTYPE2 = "REGULAR MESH  "                                 OUTOBC.......21200
            END IF                                                       OUTOBC.......21300
            IF (KTYPE(1).EQ.3) THEN                                      OUTOBC.......21400
               WRITE(K8,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",    OUTOBC.......21500
     1            NE, " Elems"                                           OUTOBC.......21600
            ELSE                                                         OUTOBC.......21700
               WRITE(K8,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",        OUTOBC.......21800
     1            NE, " Elems"                                           OUTOBC.......21900
            END IF                                                       OUTOBC.......22000
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTOBC.......22100
            WRITE(K8,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,           OUTOBC.......22200
     1         NN, " Nodes", NE, " Elems"                                OUTOBC.......22300
         ELSE                                                            OUTOBC.......22400
            WRITE(K8,954) KTYPE(1), NN, " Nodes", NE, " Elems"           OUTOBC.......22500
         END IF                                                          OUTOBC.......22600
         WRITE(K8,960) "OBSERVATION POINT RESULTS",KTMAX                 OUTOBC.......22700
         WRITE(K8,962) HORP, TORC1, "Sat"                                OUTOBC.......22800
         DO 920  KT=1, KTMAX                                             OUTOBC.......22900
            WRITE(K8,963) DITT(KT), TT(KT), ISHORP(KT),                  OUTOBC.......23000
     1         ISTORC(KT), ISSATU(KT)                                    OUTOBC.......23100
  920    CONTINUE                                                        OUTOBC.......23200
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTOBC.......23300
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTOBC.......23400
     2                 "(", I9, A, ")"                                   OUTOBC.......23500
     3         /"## ")                                                   OUTOBC.......23600
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTOBC.......23700
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTOBC.......23800
     2                 "(", I9, A, ")"                                   OUTOBC.......23900
     3         /"## ")                                                   OUTOBC.......24000
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTOBC.......24100
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTOBC.......24200
     2                 "(", I9, A, ")"                                   OUTOBC.......24300
     3         /"## ")                                                   OUTOBC.......24400
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTOBC.......24500
     1                 "(", I9, A, ")"                                   OUTOBC.......24600
     2         /"## ")                                                   OUTOBC.......24700
  960    FORMAT("## ", 92("="),                                          OUTOBC.......24800
     1         /"## ", A, 39X, I9, " Time steps printed",                OUTOBC.......24900
     2         /"## ", 92("="))                                          OUTOBC.......25000
  962    FORMAT("## ",                                                   OUTOBC.......25100
     1         /"## ", 4X, "Time steps", 29X,                            OUTOBC.......25200
     2                 "[Latest time step computed]",                    OUTOBC.......25300
     3         /"## ", 3X, "in this file      Time (sec)", 9X,A5,        OUTOBC.......25400
     4                 10X,A4, 11X,A3,                                   OUTOBC.......25500
     5         /"## ", 2X, 14("-"), 3X, 13("-"), 1X, 3(3X, 12("-")) )    OUTOBC.......25600
  963    FORMAT("## ", 1X, F15.5, 3X, 1PE13.6, 3(7X, I8))                OUTOBC.......25700
C                                                                        OUTOBC.......25800
C........DEALLOCATE LOCAL ARRAYS.                                        OUTOBC.......25900
         DEALLOCATE(TT,DITT,ISTORC,ISHORP,ISSATU)                        OUTOBC.......26000
C                                                                        OUTOBC.......26100
         ONCK78(NFLO) = .TRUE.                                           OUTOBC.......26200
         ONCEK8 = .TRUE.                                                 OUTOBC.......26300
      ENDIF                                                              OUTOBC.......26400
C                                                                        OUTOBC.......26500
C.....IF NO OBSERVATIONS, RETURN.                                        OUTOBC.......26600
      IF (NOBSN-1.EQ.0) RETURN                                           OUTOBC.......26700
C                                                                        OUTOBC.......26800
C.....HEADER INFORMATION REPEATED BEFORE EACH TIME STEP                  OUTOBC.......26900
      SFRAC = STEP - CEILING(STEP) + 1D0                                 OUTOBC.......27000
      IF ((STEP.EQ.0D0).OR.((STEP.EQ.1D0).AND.(ISSTRA.EQ.1))) THEN       OUTOBC.......27100
         TOUT = TSTART                                                   OUTOBC.......27200
      ELSE                                                               OUTOBC.......27300
         TOUT = TIME                                                     OUTOBC.......27400
      END IF                                                             OUTOBC.......27500
      IF (KTYPE(1).EQ.3) THEN                                            OUTOBC.......27600
         WRITE(K8,966) STEP,  TOUT                                       OUTOBC.......27700
      ELSE                                                               OUTOBC.......27800
         WRITE(K8,967) STEP,  TOUT                                       OUTOBC.......27900
      END IF                                                             OUTOBC.......28000
  966 FORMAT('## ',                                                      OUTOBC.......28100
     1      /'## ', 129('='),                                            OUTOBC.......28200
     2      /'## TIME STEP ', F15.5, 83X, 'Time: ', 1PE11.4, ' sec',     OUTOBC.......28300
     3      /'## ', 129('='))                                            OUTOBC.......28400
  967 FORMAT('## ',                                                      OUTOBC.......28500
     1      /'## ', 114('='),                                            OUTOBC.......28600
     2      /'## TIME STEP ', F15.5, 68X, 'Time: ', 1PE11.4, ' sec',     OUTOBC.......28700
     3      /'## ', 114('='))                                            OUTOBC.......28800
      KCONT = 7 - KTYPE(1)                                               OUTOBC.......28900
      WRITE(K8,968) (COLTK8(K),K=1,3),(COLTK8(K),K=KCONT,7)              OUTOBC.......29000
  968 FORMAT ("## ", 33X, A4, 2X, 6(A15))                                OUTOBC.......29100
C                                                                        OUTOBC.......29200
C.....WRITE OBSERVATIONS FOR THIS TIME STEP.                             OUTOBC.......29300
      IF (KTYPE(1).EQ.3) THEN                                            OUTOBC.......29400
         DO 1010 JJ=1,NOBS                                               OUTOBC.......29500
            IF ((OBSPTS(JJ)%FRMT.EQ."OBC").AND.                          OUTOBC.......29600
     1         (OBSPTS(JJ)%SCHED.EQ.SCHDLS(OFP(NFLO)%ISCHED)%NAME))      OUTOBC.......29700
     2         THEN                                                      OUTOBC.......29800
               LENNAM = LEN_TRIM(OBSPTS(JJ)%NAME)                        OUTOBC.......29900
               OBSNM = BLANKS(1:40-LENNAM) // TRIM(OBSPTS(JJ)%NAME)      OUTOBC.......30000
               WRITE(K8,1000) OBSNM, OBSPTS(JJ)%X, OBSPTS(JJ)%Y,         OUTOBC.......30100
     1            OBSPTS(JJ)%Z, DP3STR(PUSWF(OBSPTS(JJ)%L,               OUTOBC.......30200
     2            OBSPTS(JJ)%XSI,OBSPTS(JJ)%ETA,OBSPTS(JJ)%ZET,SFRAC,    OUTOBC.......30300
     3            PM1,UM1,PVEC,UVEC,IN,LREG))                            OUTOBC.......30400
 1000          FORMAT(A40,2X,3(1PE15.7),A45)                             OUTOBC.......30500
            END IF                                                       OUTOBC.......30600
 1010    CONTINUE                                                        OUTOBC.......30700
      ELSE                                                               OUTOBC.......30800
         DO 1030 JJ=1,NOBS                                               OUTOBC.......30900
            IF ((OBSPTS(JJ)%FRMT.EQ."OBC").AND.                          OUTOBC.......31000
     1         (OBSPTS(JJ)%SCHED.EQ.SCHDLS(OFP(NFLO)%ISCHED)%NAME))      OUTOBC.......31100
     2         THEN                                                      OUTOBC.......31200
               LENNAM = LEN_TRIM(OBSPTS(JJ)%NAME)                        OUTOBC.......31300
               OBSNM = BLANKS(1:40-LENNAM) // TRIM(OBSPTS(JJ)%NAME)      OUTOBC.......31400
               WRITE(K8,1020) OBSNM, OBSPTS(JJ)%X, OBSPTS(JJ)%Y,         OUTOBC.......31500
     1            DP3STR(PUSWF(OBSPTS(JJ)%L,OBSPTS(JJ)%XSI,              OUTOBC.......31600
     2            OBSPTS(JJ)%ETA,OBSPTS(JJ)%ZET,SFRAC,                   OUTOBC.......31700
     3            PM1,UM1,PVEC,UVEC,IN,LREG))                            OUTOBC.......31800
 1020          FORMAT(A40,2X,2(1PE15.7),A45)                             OUTOBC.......31900
            END IF                                                       OUTOBC.......32000
 1030    CONTINUE                                                        OUTOBC.......32100
      END IF                                                             OUTOBC.......32200
C                                                                        OUTOBC.......32300
C                                                                        OUTOBC.......32400
      RETURN                                                             OUTOBC.......32500
C                                                                        OUTOBC.......32600
      END                                                                OUTOBC.......32700
C                                                                        OUTOBC.......32800
C     SUBROUTINE        O  U  T  O  B  S           SUTRA VERSION 2.1     OUTOBS.........100
C                                                                        OUTOBS.........200
C *** PURPOSE :                                                          OUTOBS.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION POINTS.  SPECIFICALLY,       OUTOBS.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBS.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT.                               OUTOBS.........600
C                                                                        OUTOBS.........700
      SUBROUTINE OUTOBS(NFLO,OBSPTS,TIME,STEP,PM1,UM1,PVEC,UVEC,         OUTOBS.........800
     1   TITLE1,TITLE2,IN,LREG)                                          OUTOBS.........900
      USE ALLARR, ONLY : OBSDAT                                          OUTOBS........1000
      USE LLDEF                                                          OUTOBS........1100
      USE EXPINT                                                         OUTOBS........1200
      USE SCHDEF                                                         OUTOBS........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTOBS........1400
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTOBS........1500
      CHARACTER*8  HORP                                                  OUTOBS........1600
      CHARACTER*13 TORC1,TORC2                                           OUTOBS........1700
      CHARACTER*14 CTYPE2                                                OUTOBS........1800
      CHARACTER*80 UNAME,FNAME(0:8),FRMT,FRMT2                           OUTOBS........1900
      CHARACTER*40 BLANKS                                                OUTOBS........2000
      CHARACTER*80 LAYSTR                                                OUTOBS........2100
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                OUTOBS........2200
      LOGICAL IMPRTD                                                     OUTOBS........2300
      DIMENSION PM1(NNVEC),UM1(NNVEC),PVEC(NNVEC),UVEC(NNVEC)            OUTOBS........2400
      DIMENSION IN(NIN),LREG(NE)                                         OUTOBS........2500
      DIMENSION KTYPE(2)                                                 OUTOBS........2600
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         OUTOBS........2700
      ALLOCATABLE TT(:),DITT(:),ISTORC(:),ISHORP(:),ISSATU(:)            OUTOBS........2800
      ALLOCATABLE JSET(:)                                                OUTOBS........2900
      TYPE (LLD), POINTER :: DENTS, DENOBS                               OUTOBS........3000
      COMMON /CLAY/ LAYSTR                                               OUTOBS........3100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTOBS........3200
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTOBS........3300
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTOBS........3400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTOBS........3500
     1   NSOP,NSOU,NBCN                                                  OUTOBS........3600
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTOBS........3700
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTOBS........3800
      COMMON /FNAMES/ UNAME,FNAME                                        OUTOBS........3900
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTOBS........4000
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTOBS........4100
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTOBS........4200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTOBS........4300
     1   KSCRN,KPAUSE                                                    OUTOBS........4400
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      OUTOBS........4500
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8                          OUTOBS........4600
      COMMON /SCH/ NSCH,ISCHTS                                           OUTOBS........4700
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTOBS........4800
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTOBS........4900
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTOBS........5000
      DATA BLANKS /"                                        "/           OUTOBS........5100
C                                                                        OUTOBS........5200
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTOBS........5300
C                                                                        OUTOBS........5400
      NOBS=NOBSN-1                                                       OUTOBS........5500
      DITMAX = DNINT(DBLE(ITMAX))                                        OUTOBS........5600
C                                                                        OUTOBS........5700
      K7 = IUNIO(NFLO)                                                   OUTOBS........5800
      IF (.NOT. ONCK78(NFLO))  THEN                                      OUTOBS........5900
C.....FIRST CALL FOR THIS FILE -- CREATE FILE HEADER.                    OUTOBS........6000
C                                                                        OUTOBS........6100
C........IF NO OBSERVATION POINTS, WRITE MESSAGE AND RETURN              OUTOBS........6200
         IF (NOBSN-1.EQ.0) THEN                                          OUTOBS........6300
            WRITE(K7,5) TITLE1, TITLE2                                   OUTOBS........6400
    5       FORMAT("## ", 80A1,                                          OUTOBS........6500
     1         /"## ", 80A1,                                             OUTOBS........6600
     2         /"## ")                                                   OUTOBS........6700
            WRITE(K7,7)                                                  OUTOBS........6800
    7       FORMAT(/'  *** NO OBSERVATION POINTS SPECIFIED ',            OUTOBS........6900
     1         '(NOBS=0) ***')                                           OUTOBS........7000
            ONCK78(NFLO) = .TRUE.                                        OUTOBS........7100
            ONCEK7 = .TRUE.                                              OUTOBS........7200
            RETURN                                                       OUTOBS........7300
         END IF                                                          OUTOBS........7400
C                                                                        OUTOBS........7500
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTOBS........7600
         IF (ISSTRA.NE.0) THEN                                           OUTOBS........7700
            KTMAX = 1                                                    OUTOBS........7800
            IMPRTD = .TRUE.                                              OUTOBS........7900
         ELSE                                                            OUTOBS........8000
            KTMAX = 2                                                    OUTOBS........8100
            IMPRTD = .FALSE.                                             OUTOBS........8200
            LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                       OUTOBS........8300
            IF (LENSCH.NE.0) THEN                                        OUTOBS........8400
               DENOBS => SCHDLS(OFP(NFLO)%ISCHED)%SLIST                  OUTOBS........8500
               KTMAX = KTMAX + 1                                         OUTOBS........8600
               SOB = DENOBS%DVALU2                                       OUTOBS........8700
               IF (SOB.EQ.0D0) KTMAX = KTMAX - 1                         OUTOBS........8800
               DO 4 JT=2,LENSCH                                          OUTOBS........8900
                  DENOBS => DENOBS%NENT                                  OUTOBS........9000
                  KTMAX = KTMAX + 1                                      OUTOBS........9100
    4          CONTINUE                                                  OUTOBS........9200
               SOB = DENOBS%DVALU2                                       OUTOBS........9300
               IF (SOB.EQ.DITMAX) THEN                                   OUTOBS........9400
                  KTMAX = KTMAX - 1                                      OUTOBS........9500
                  IMPRTD = .TRUE.                                        OUTOBS........9600
               END IF                                                    OUTOBS........9700
            END IF                                                       OUTOBS........9800
         END IF                                                          OUTOBS........9900
C                                                                        OUTOBS.......10000
C........ALLOCATE LOCAL ARRAYS                                           OUTOBS.......10100
         ALLOCATE(TT(KTMAX),DITT(KTMAX))                                 OUTOBS.......10200
         ALLOCATE(ISTORC(KTMAX),ISHORP(KTMAX),ISSATU(KTMAX))             OUTOBS.......10300
C                                                                        OUTOBS.......10400
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTOBS.......10500
         IF (ISSTRA.NE.0) THEN                                           OUTOBS.......10600
            KT = 1                                                       OUTOBS.......10700
            TT(KT) = TSTART                                              OUTOBS.......10800
            DITT(KT) = 1                                                 OUTOBS.......10900
            ISHORP(KT) = 0                                               OUTOBS.......11000
            ISTORC(KT) = 1                                               OUTOBS.......11100
            ISSATU(KT) = 0                                               OUTOBS.......11200
         ELSE                                                            OUTOBS.......11300
C...........NUMBER OF PRINTED TIME STEPS                                 OUTOBS.......11400
            KT=0                                                         OUTOBS.......11500
C...........TIME STEP INCREMENT                                          OUTOBS.......11600
            DELTK=DELT                                                   OUTOBS.......11700
C...........POINTERS TO TIME STEP AND OBSERVATIONS SCHEDULES             OUTOBS.......11800
            DENTS => SCHDLS(ISCHTS)%SLIST                                OUTOBS.......11900
            DENOBS => SCHDLS(OFP(NFLO)%ISCHED)%SLIST                     OUTOBS.......12000
            LCNT = 1                                                     OUTOBS.......12100
C...........OBSERVATION TIME AND STEP                                    OUTOBS.......12200
            TOB = DENOBS%DVALU1                                          OUTOBS.......12300
            SOB = DENOBS%DVALU2                                          OUTOBS.......12400
C...........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED      OUTOBS.......12500
            LCHORP = 0                                                   OUTOBS.......12600
            LCTORC = 0                                                   OUTOBS.......12700
C...........INITIAL/STARTING CONDITIONS ARE PRINTED                      OUTOBS.......12800
            KT = KT + 1                                                  OUTOBS.......12900
            TJT = DENTS%DVALU1                                           OUTOBS.......13000
            SJT = DENTS%DVALU2                                           OUTOBS.......13100
            TT(KT) = TJT                                                 OUTOBS.......13200
            DITT(KT) = SJT                                               OUTOBS.......13300
            ISHORP(KT) = 0                                               OUTOBS.......13400
            ISTORC(KT) = 0                                               OUTOBS.......13500
            ISSATU(KT) = 0                                               OUTOBS.......13600
            IF ((SOB.EQ.SJT).AND.(LCNT.LT.LENSCH)) THEN                  OUTOBS.......13700
               DENOBS => DENOBS%NENT                                     OUTOBS.......13800
               TOB = DENOBS%DVALU1                                       OUTOBS.......13900
               SOB = DENOBS%DVALU2                                       OUTOBS.......14000
               LCNT = LCNT + 1                                           OUTOBS.......14100
            END IF                                                       OUTOBS.......14200
C...........IDENTIFY AND RECORD SCHEDULED OUTPUT                         OUTOBS.......14300
            DO 10 JT=1,ITMAX                                             OUTOBS.......14400
               DJT = DNINT(DBLE(JT))                                     OUTOBS.......14500
               DENTS => DENTS%NENT                                       OUTOBS.......14600
               TJT = DENTS%DVALU1                                        OUTOBS.......14700
               SJT = DENTS%DVALU2                                        OUTOBS.......14800
               IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCHORP = JT          OUTOBS.......14900
               IF (MOD(JT,NUCYC).EQ.0 .OR. JT.EQ.1) LCTORC = JT          OUTOBS.......15000
               DO WHILE ((SOB.LE.DJT).AND.(LCNT.LE.LENSCH))              OUTOBS.......15100
                  KT = KT + 1                                            OUTOBS.......15200
                  TT(KT) = TOB                                           OUTOBS.......15300
                  DITT(KT) = SOB                                         OUTOBS.......15400
                  ISHORP(KT) = LCHORP                                    OUTOBS.......15500
                  ISTORC(KT) = LCTORC                                    OUTOBS.......15600
                  ISSATU(KT) = LCHORP                                    OUTOBS.......15700
                  IF (LCNT.LT.LENSCH) THEN                               OUTOBS.......15800
                     DENOBS => DENOBS%NENT                               OUTOBS.......15900
                     TOB = DENOBS%DVALU1                                 OUTOBS.......16000
                     SOB = DENOBS%DVALU2                                 OUTOBS.......16100
                  END IF                                                 OUTOBS.......16200
                  LCNT = LCNT + 1                                        OUTOBS.......16300
               END DO                                                    OUTOBS.......16400
   10       CONTINUE                                                     OUTOBS.......16500
         END IF                                                          OUTOBS.......16600
C                                                                        OUTOBS.......16700
C                                                                        OUTOBS.......16800
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTOBS.......16900
         IF (.NOT.IMPRTD) THEN                                           OUTOBS.......17000
            KT = KT + 1                                                  OUTOBS.......17100
            TT(KT) = TJT                                                 OUTOBS.......17200
            DITT(KT) = SJT                                               OUTOBS.......17300
            IF (MOD(JT,NPCYC).EQ.0) LCHORP = ITMAX                       OUTOBS.......17400
            IF (MOD(JT,NUCYC).EQ.0) LCTORC = ITMAX                       OUTOBS.......17500
            ISHORP(KT) = LCHORP                                          OUTOBS.......17600
            ISTORC(KT) = LCTORC                                          OUTOBS.......17700
            ISSATU(KT) = LCHORP                                          OUTOBS.......17800
         ENDIF                                                           OUTOBS.......17900
C                                                                        OUTOBS.......18000
C........IF STEADY-STATE FLOW, P AND S CALCULATED ON TIME STEP 0 ONLY.   OUTOBS.......18100
         IF (ISSFLO.NE.0) THEN                                           OUTOBS.......18200
            DO 14 KT=1,KTMAX                                             OUTOBS.......18300
               ISHORP(KT) = 0                                            OUTOBS.......18400
               ISSATU(KT) = 0                                            OUTOBS.......18500
   14       CONTINUE                                                     OUTOBS.......18600
         END IF                                                          OUTOBS.......18700
C                                                                        OUTOBS.......18800
C........SET TEMPERATURE OR CONCENTRATION TEXT STRING FOR HEADER         OUTOBS.......18900
         IF (ME .GT. 0) THEN                                             OUTOBS.......19000
            TORC1 = "Temperature  "                                      OUTOBS.......19100
            TORC2 = "  Temperature"                                      OUTOBS.......19200
         ELSE                                                            OUTOBS.......19300
            TORC1 = "Concentration"                                      OUTOBS.......19400
            TORC2 = "Concentration"                                      OUTOBS.......19500
         ENDIF                                                           OUTOBS.......19600
C                                                                        OUTOBS.......19700
C........SET PRESSURE TEXT STRING FOR HEADER                             OUTOBS.......19800
         HORP = "Pressure"                                               OUTOBS.......19900
C                                                                        OUTOBS.......20000
C........WRITE HEADER INFORMATION                                        OUTOBS.......20100
         WRITE(K7,5) TITLE1, TITLE2                                      OUTOBS.......20200
         IF (KTYPE(2).GT.1) THEN                                         OUTOBS.......20300
            IF (KTYPE(2).EQ.3) THEN                                      OUTOBS.......20400
               CTYPE2 = "BLOCKWISE MESH"                                 OUTOBS.......20500
            ELSE                                                         OUTOBS.......20600
               CTYPE2 = "REGULAR MESH  "                                 OUTOBS.......20700
            END IF                                                       OUTOBS.......20800
            IF (KTYPE(1).EQ.3) THEN                                      OUTOBS.......20900
               WRITE(K7,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",    OUTOBS.......21000
     1            NE, " Elems"                                           OUTOBS.......21100
            ELSE                                                         OUTOBS.......21200
               WRITE(K7,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",        OUTOBS.......21300
     1            NE, " Elems"                                           OUTOBS.......21400
            END IF                                                       OUTOBS.......21500
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTOBS.......21600
            WRITE(K7,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,           OUTOBS.......21700
     1         NN, " Nodes", NE, " Elems"                                OUTOBS.......21800
         ELSE                                                            OUTOBS.......21900
            WRITE(K7,954) KTYPE(1), NN, " Nodes", NE, " Elems"           OUTOBS.......22000
         END IF                                                          OUTOBS.......22100
         WRITE(K7,960) "OBSERVATION POINT RESULTS",KTMAX                 OUTOBS.......22200
         WRITE(K7,962) HORP, TORC1, "Sat"                                OUTOBS.......22300
         DO 920  KT=1, KTMAX                                             OUTOBS.......22400
            WRITE(K7,963) DITT(KT), TT(KT), ISHORP(KT),                  OUTOBS.......22500
     1         ISTORC(KT), ISSATU(KT)                                    OUTOBS.......22600
  920    CONTINUE                                                        OUTOBS.......22700
         WRITE(K7,964)                                                   OUTOBS.......22800
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTOBS.......22900
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTOBS.......23000
     2                 "(", I9, A, ")"                                   OUTOBS.......23100
     3         /"## ")                                                   OUTOBS.......23200
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTOBS.......23300
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTOBS.......23400
     2                 "(", I9, A, ")"                                   OUTOBS.......23500
     3         /"## ")                                                   OUTOBS.......23600
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTOBS.......23700
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTOBS.......23800
     2                 "(", I9, A, ")"                                   OUTOBS.......23900
     3         /"## ")                                                   OUTOBS.......24000
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTOBS.......24100
     1                 "(", I9, A, ")"                                   OUTOBS.......24200
     2         /"## ")                                                   OUTOBS.......24300
  960    FORMAT("## ", 92("="),                                          OUTOBS.......24400
     1         /"## ", A, 39X, I9, " Time steps printed",                OUTOBS.......24500
     2         /"## ", 92("="))                                          OUTOBS.......24600
  962    FORMAT("## ",                                                   OUTOBS.......24700
     1         /"## ", 4X, "Time steps", 29X,                            OUTOBS.......24800
     2                 "[Latest time step computed]",                    OUTOBS.......24900
     3         /"## ", 3X, "in this file      Time (sec)", 9X,A5,        OUTOBS.......25000
     4                 10X,A4, 11X,A3,                                   OUTOBS.......25100
     5         /"## ", 2X, 14("-"), 3X, 13("-"), 1X, 3(3X, 12("-")) )    OUTOBS.......25200
  963    FORMAT("## ", 1X, F15.5, 3X, 1PE13.6, 3(7X, I8))                OUTOBS.......25300
  964    FORMAT("## "/"## ", 92("=")/"## ")                              OUTOBS.......25400
C                                                                        OUTOBS.......25500
         ALLOCATE(JSET(NOBLIN))                                          OUTOBS.......25600
         JLTOT = 0                                                       OUTOBS.......25700
         JSETS = 0                                                       OUTOBS.......25800
         JNEXT = 1                                                       OUTOBS.......25900
         DO WHILE (JNEXT.LE.NOBS)                                        OUTOBS.......26000
            CALL LODOBS(NFLO,JNEXT,OBSPTS,JSET,JLOAD)                    OUTOBS.......26100
            IF (JLOAD.EQ.0) EXIT                                         OUTOBS.......26200
            JLLAST = JLOAD                                               OUTOBS.......26300
            JLTOT = JLTOT + JLOAD                                        OUTOBS.......26400
            JSETS = JSETS + 1                                            OUTOBS.......26500
            WRITE(FRMT,"(A,I9,A)") '("## ",30X,',JLOAD,"(4X,A))"         OUTOBS.......26600
            WRITE(K7,FRMT)                                               OUTOBS.......26700
     1         (BLANKS(1:(44-LEN_TRIM(OBSPTS(JSET(JJ))%NAME))/2)         OUTOBS.......26800
     2         // TRIM(OBSPTS(JSET(JJ))%NAME) //                         OUTOBS.......26900
     3         BLANKS(1:44-LEN_TRIM(OBSPTS(JSET(JJ))%NAME)-              OUTOBS.......27000
     4         (44-LEN_TRIM(OBSPTS(JSET(JJ))%NAME))/2),                  OUTOBS.......27100
     5         JJ=1,JLOAD)                                               OUTOBS.......27200
         END DO                                                          OUTOBS.......27300
         IF (JSETS.EQ.1) THEN                                            OUTOBS.......27400
            JJMAX = JLLAST                                               OUTOBS.......27500
         ELSE                                                            OUTOBS.......27600
            JJMAX = NOBLIN                                               OUTOBS.......27700
         END IF                                                          OUTOBS.......27800
         WRITE(FRMT2,"(A,I9,A)") '("## ",30X,',NOBLIN,"(:A,44('-')))"    OUTOBS.......27900
         WRITE(K7,FRMT2) ("    ", JJ=1,JJMAX)                            OUTOBS.......28000
         JNEXT = 1                                                       OUTOBS.......28100
         DO WHILE (JNEXT.LE.NOBS)                                        OUTOBS.......28200
            CALL LODOBS(NFLO,JNEXT,OBSPTS,JSET,JLOAD)                    OUTOBS.......28300
            IF (JLOAD.EQ.0) EXIT                                         OUTOBS.......28400
            IF (KTYPE(1).EQ.2) THEN                                      OUTOBS.......28500
               WRITE(FRMT,"(A,I9,A)") '("## ",31X,',JLOAD,               OUTOBS.......28600
     1            "(2X,7X'(',1PE14.7,',',1PE14.7,')',8X))"               OUTOBS.......28700
               WRITE(K7,FRMT) (OBSPTS(JSET(JJ))%X, OBSPTS(JSET(JJ))%Y,   OUTOBS.......28800
     1            JJ=1,JLOAD)                                            OUTOBS.......28900
            ELSE                                                         OUTOBS.......29000
               WRITE(FRMT,"(A,I9,A)") '("## ",31X,',JLOAD,               OUTOBS.......29100
     1            "(2X,'(',2(1PE14.7,','),1PE14.7,')'))"                 OUTOBS.......29200
               WRITE(K7,FRMT) (OBSPTS(JSET(JJ))%X, OBSPTS(JSET(JJ))%Y,   OUTOBS.......29300
     1            OBSPTS(JSET(JJ))%Z, JJ=1,JLOAD)                        OUTOBS.......29400
            END IF                                                       OUTOBS.......29500
         END DO                                                          OUTOBS.......29600
         WRITE(K7,FRMT2) ("    ", JJ=1,JJMAX)                            OUTOBS.......29700
         WRITE(FRMT,"(A,I9,A)") '("## ",6X,"Time Step",5X,"Time (sec)",' OUTOBS.......29800
     1      ,NOBLIN,"(:10X,A,2X,A,5X,'Saturation'))"                     OUTOBS.......29900
         WRITE(K7,FRMT) (HORP, TORC2, JJ=1,JJMAX)                        OUTOBS.......30000
C                                                                        OUTOBS.......30100
C........DEALLOCATE LOCAL ARRAYS FOR HEADER.                             OUTOBS.......30200
         DEALLOCATE(TT,DITT,ISTORC,ISHORP,ISSATU,JSET)                   OUTOBS.......30300
C                                                                        OUTOBS.......30400
         ONCK78(NFLO) = .TRUE.                                           OUTOBS.......30500
         ONCEK7 = .TRUE.                                                 OUTOBS.......30600
      ENDIF                                                              OUTOBS.......30700
C                                                                        OUTOBS.......30800
C.....IF NO OBSERVATIONS, RETURN.                                        OUTOBS.......30900
      IF (NOBSN-1.EQ.0) RETURN                                           OUTOBS.......31000
C                                                                        OUTOBS.......31100
C.....WRITE OBSERVATIONS.                                                OUTOBS.......31200
      SFRAC = STEP - CEILING(STEP) + 1D0                                 OUTOBS.......31300
      IF ((STEP.EQ.0D0).OR.((STEP.EQ.1D0).AND.(ISSTRA.EQ.1))) THEN       OUTOBS.......31400
         TOUT = TSTART                                                   OUTOBS.......31500
      ELSE                                                               OUTOBS.......31600
         TOUT = TIME                                                     OUTOBS.......31700
      END IF                                                             OUTOBS.......31800
      ALLOCATE(JSET(NOBLIN))                                             OUTOBS.......31900
      NS = 0                                                             OUTOBS.......32000
      JNEXT = 1                                                          OUTOBS.......32100
      DO WHILE (JNEXT.LE.NOBS)                                           OUTOBS.......32200
         CALL LODOBS(NFLO,JNEXT,OBSPTS,JSET,JLOAD)                       OUTOBS.......32300
         IF (JLOAD.EQ.0) EXIT                                            OUTOBS.......32400
         NS = NS + 1                                                     OUTOBS.......32500
         IF (NS.EQ.1) THEN                                               OUTOBS.......32600
            WRITE(FRMT,"(A,I9,A)") "(3X,F15.5,1PE15.7,",JLOAD,"(:3X,A))" OUTOBS.......32700
            WRITE(K7,FRMT) STEP, TOUT,                                   OUTOBS.......32800
     1         (DP3STR(PUSWF(OBSPTS(JSET(JJ))%L,                         OUTOBS.......32900
     2         OBSPTS(JSET(JJ))%XSI,OBSPTS(JSET(JJ))%ETA,                OUTOBS.......33000
     3         OBSPTS(JSET(JJ))%ZET,SFRAC,PM1,UM1,                       OUTOBS.......33100
     4         PVEC,UVEC,IN,LREG)), JJ=1,JLOAD)                          OUTOBS.......33200
         ELSE                                                            OUTOBS.......33300
            WRITE(FRMT,"(A,I9,A)") "(33X,",JLOAD,"(:3X,A))"              OUTOBS.......33400
            WRITE(K7,FRMT) (DP3STR(PUSWF(OBSPTS(JSET(JJ))%L,             OUTOBS.......33500
     1         OBSPTS(JSET(JJ))%XSI,OBSPTS(JSET(JJ))%ETA,                OUTOBS.......33600
     2         OBSPTS(JSET(JJ))%ZET,SFRAC,PM1,UM1,                       OUTOBS.......33700
     3         PVEC,UVEC,IN,LREG)), JJ=1,JLOAD)                          OUTOBS.......33800
         END IF                                                          OUTOBS.......33900
      END DO                                                             OUTOBS.......34000
      DEALLOCATE(JSET)                                                   OUTOBS.......34100
C                                                                        OUTOBS.......34200
C                                                                        OUTOBS.......34300
      RETURN                                                             OUTOBS.......34400
C                                                                        OUTOBS.......34500
      END                                                                OUTOBS.......34600
C                                                                        OUTOBS.......34700
C     SUBROUTINE        O  U  T  R  S  T           SUTRA VERSION 2.1     OUTRST.........100
C                                                                        OUTRST.........200
C *** PURPOSE :                                                          OUTRST.........300
C ***  TO STORE RESULTS THAT MAY LATER BE USED TO RESTART                OUTRST.........400
C ***  THE SIMULATION.                                                   OUTRST.........500
C                                                                        OUTRST.........600
      SUBROUTINE OUTRST(PVEC,UVEC,PM1,UM1,CS1,RCIT,SW,QINITR,PBC)        OUTRST.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTRST.........800
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),PM1(NN),UM1(NN),CS1(NN),         OUTRST.........900
     1   RCIT(NN),SW(NN),PBC(NBCN),QINITR(NN)                            OUTRST........1000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTRST........1100
     1   NSOP,NSOU,NBCN                                                  OUTRST........1200
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        OUTRST........1300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     OUTRST........1400
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTRST........1500
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTRST........1600
C                                                                        OUTRST........1700
C.....REWIND RST FILE FOR WRITING RESULTS OF CURRENT TIME STEP           OUTRST........1800
      REWIND(K4)                                                         OUTRST........1900
C                                                                        OUTRST........2000
C.....STORE TIME INFORMATION                                             OUTRST........2100
      WRITE(K4,100) TSEC,DELTP,DELTU                                     OUTRST........2200
  100 FORMAT(4E20.10)                                                    OUTRST........2300
C                                                                        OUTRST........2400
C.....STORE SOLUTION                                                     OUTRST........2500
      WRITE(K4,105)                                                      OUTRST........2600
      WRITE(K4,110) (CUTSML(PVEC(I)),I=1,NN)                             OUTRST........2700
      WRITE(K4,105)                                                      OUTRST........2800
      WRITE(K4,110) (CUTSML(UVEC(I)),I=1,NN)                             OUTRST........2900
      WRITE(K4,110) (CUTSML(PM1(I)),I=1,NN)                              OUTRST........3000
      WRITE(K4,110) (CUTSML(UM1(I)),I=1,NN)                              OUTRST........3100
      WRITE(K4,110) (CUTSML(CS1(I)),I=1,NN)                              OUTRST........3200
      WRITE(K4,110) (CUTSML(RCIT(I)),I=1,NN)                             OUTRST........3300
      WRITE(K4,110) (CUTSML(SW(I)),I=1,NN)                               OUTRST........3400
      write(k4,110) (CUTSML(QINITR(I)),I=1,NN)                           OUTRST........3500
      WRITE(K4,110) (CUTSML(PBC(IP)),IP=1,NBCN)                          OUTRST........3600
  105 FORMAT("'NONUNIFORM'")                                             OUTRST........3700
  110 FORMAT(1PE20.13,1X,1PE20.13,1X,1PE20.13,1X,1PE20.13)               OUTRST........3800
C                                                                        OUTRST........3900
      ENDFILE(K4)                                                        OUTRST........4000
C                                                                        OUTRST........4100
      RETURN                                                             OUTRST........4200
      END                                                                OUTRST........4300
C                                                                        OUTRST........4400
C     SUBROUTINE        P  R  S  W  D  S           SUTRA VERSION 2.1     PRSWDS.........100
C                                                                        PRSWDS.........200
C *** PURPOSE :                                                          PRSWDS.........300
C ***  PARSE A CHARACTER STRING INTO WORDS.  WORDS ARE CONSIDERED TO BE  PRSWDS.........400
C ***  SEPARATED BY ONE OR MORE OF THE SINGLE-CHARACTER DELIMITER DELIM  PRSWDS.........500
C ***  AND/OR BLANKS.  PARSING CONTINUES UNTIL THE ENTIRE STRING HAS     PRSWDS.........600
C ***  BEEN PROCESSED OR THE NUMBER OF WORDS PARSED EQUALS NWMAX.  IF    PRSWDS.........700
C ***  NWMAX IS SET TO ZERO, PRSWDS SIMPLY COMPUTES THE NUMBER OF WORDS. PRSWDS.........800
C                                                                        PRSWDS.........900
      SUBROUTINE PRSWDS(STRING, DELIM, NWMAX, WORD, NWORDS)              PRSWDS........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PRSWDS........1100
      CHARACTER*(*) STRING, WORD(NWMAX)                                  PRSWDS........1200
      CHARACTER DELIM*1, DELIM2*2                                        PRSWDS........1300
C                                                                        PRSWDS........1400
C.....DEFINE SET OF DELIMITERS (SPACE PLUS USER-SPECIFIED CHARACTER)     PRSWDS........1500
      DELIM2 = " " // DELIM                                              PRSWDS........1600
C                                                                        PRSWDS........1700
C.....COMPUTE LENGTH OF STRING WITHOUT TRAILING BLANKS                   PRSWDS........1800
      LSTRNG = LEN_TRIM(STRING)                                          PRSWDS........1900
C                                                                        PRSWDS........2000
C.....INITIALIZE WORD LIST AND COUNTERS                                  PRSWDS........2100
      DO 50 I=1,NWMAX                                                    PRSWDS........2200
         WORD(I) = ""                                                    PRSWDS........2300
   50 CONTINUE                                                           PRSWDS........2400
      NWORDS = 0                                                         PRSWDS........2500
      M2 = 0                                                             PRSWDS........2600
C                                                                        PRSWDS........2700
  300 CONTINUE                                                           PRSWDS........2800
C.....FIND THE NEXT CHARACTER THAT IS NOT A DELIMITER                    PRSWDS........2900
      M1L = VERIFY(STRING(M2+1:LSTRNG),DELIM2)                           PRSWDS........3000
      IF (M1L.EQ.0) RETURN                                               PRSWDS........3100
      M1 = M2 + M1L                                                      PRSWDS........3200
C                                                                        PRSWDS........3300
  400 CONTINUE                                                           PRSWDS........3400
C.....FIND THE NEXT CHARACTER THAT IS A DELIMITER                        PRSWDS........3500
      M2L = SCAN(STRING(M1+1:LSTRNG),DELIM2)                             PRSWDS........3600
      IF (M2L.EQ.0) THEN                                                 PRSWDS........3700
         M2 = LSTRNG + 1                                                 PRSWDS........3800
      ELSE                                                               PRSWDS........3900
         M2 = M1 + M2L                                                   PRSWDS........4000
      END IF                                                             PRSWDS........4100
C                                                                        PRSWDS........4200
  500 CONTINUE                                                           PRSWDS........4300
C.....STORE THE LATEST WORD FOUND                                        PRSWDS........4400
      NWORDS = NWORDS + 1                                                PRSWDS........4500
      IF (NWMAX.GT.0) WORD(NWORDS) = STRING(M1:M2-1)                     PRSWDS........4600
C                                                                        PRSWDS........4700
C.....IF END OF STRING NOT REACHED AND NUMBER OF WORDS IS LESS THAN      PRSWDS........4800
C        THE MAXIMUM ALLOWED, CONTINUE PARSING                           PRSWDS........4900
      IF ((M2.LT.LSTRNG).AND.((NWORDS.LT.NWMAX).OR.(NWMAX.EQ.0)))        PRSWDS........5000
     1   GOTO 300                                                        PRSWDS........5100
C                                                                        PRSWDS........5200
      RETURN                                                             PRSWDS........5300
      END                                                                PRSWDS........5400
C                                                                        PRSWDS........5500
C     SUBROUTINE        P  T  R  S  E  T           SUTRA VERSION 2.1     PTRSET.........100
C                                                                        PTRSET.........200
C *** PURPOSE :                                                          PTRSET.........300
C ***  TO SET UP POINTER ARRAYS NEEDED TO SPECIFY THE MATRIX STRUCTURE.  PTRSET.........400
C                                                                        PTRSET.........500
      SUBROUTINE PTRSET()                                                PTRSET.........600
      USE ALLARR                                                         PTRSET.........700
      USE PTRDEF                                                         PTRSET.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PTRSET.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              PTRSET........1000
     1   NSOP,NSOU,NBCN                                                  PTRSET........1100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        PTRSET........1200
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        PTRSET........1300
C                                                                        PTRSET........1400
C.....SET UP POINTER ARRAYS IA AND JA THAT SPECIFY MATRIX STRUCTURE IN   PTRSET........1500
C        "SLAP COLUMN" FORMAT.  FOR EACH NODE, CONSTRUCT A LINKED LIST   PTRSET........1600
C        OF NEIGHBORING NODES.  HLIST(K) POINTS TO THE HEAD OF THE LIST  PTRSET........1700
C        FOR NODE K.  THEN, TRANSFER THE LISTS TO ARRAYS IA AND JA.      PTRSET........1800
C                                                                        PTRSET........1900
C.....ALLOCATE HLIST AND LLIST, AND INITIALIZE LIST LENGTHS TO ZERO.     PTRSET........2000
      ALLOCATE(LLIST(NN), HLIST(NN))                                     PTRSET........2100
      DO 490 I=1,NN                                                      PTRSET........2200
         ALLOCATE(HLIST(I)%PL)                                           PTRSET........2300
         LLIST(I) = 0                                                    PTRSET........2400
  490 CONTINUE                                                           PTRSET........2500
C.....LOOP THROUGH INCIDENCE LIST.                                       PTRSET........2600
      DO 500 L=1,NE                                                      PTRSET........2700
      DO 500 IL=1,N48                                                    PTRSET........2800
         IC = IN((L-1)*N48+IL)                                           PTRSET........2900
      DO 500 JL=1,N48                                                    PTRSET........3000
         JC = IN((L-1)*N48+JL)                                           PTRSET........3100
C........INSERT NEIGHBOR JC IN LIST FOR NODE IC IN ASCENDING ORDER.      PTRSET........3200
C           (IF DUPLICATE OR SELF-NEIGHBOR, SKIP IT.)                    PTRSET........3300
         IF (JC.EQ.IC) THEN                                              PTRSET........3400
C...........SKIP SELF-NEIGHBOR.                                          PTRSET........3500
            GOTO 500                                                     PTRSET........3600
         ELSE IF (LLIST(IC).EQ.0) THEN                                   PTRSET........3700
C...........PLACE FIRST LIST ENTRY AT HEAD.                              PTRSET........3800
            HLIST(IC)%PL%NODNUM = JC                                     PTRSET........3900
            GOTO 498                                                     PTRSET........4000
         ELSE                                                            PTRSET........4100
C...........INSERT INTO LIST, OR SKIP IF DUPLICATE.                      PTRSET........4200
            ALLOCATE(DENTPV)                                             PTRSET........4300
            DENTPI => DENTPV                                             PTRSET........4400
            DENTPV%NENT => HLIST(IC)%PL                                  PTRSET........4500
            DO 495 K=1,LLIST(IC)                                         PTRSET........4600
               DENT => DENTPV%NENT                                       PTRSET........4700
               IF (JC.EQ.DENT%NODNUM) THEN                               PTRSET........4800
                  DEALLOCATE(DENTPI)                                     PTRSET........4900
                  GOTO 500                                               PTRSET........5000
               ELSE IF (JC.LT.DENT%NODNUM) THEN                          PTRSET........5100
                  ALLOCATE(DENTNW)                                       PTRSET........5200
                  DENTNW%NODNUM = JC                                     PTRSET........5300
                  DENTNW%NENT => DENT                                    PTRSET........5400
                  IF (K.EQ.1) THEN                                       PTRSET........5500
                     HLIST(IC)%PL => DENTNW                              PTRSET........5600
                  ELSE                                                   PTRSET........5700
                     DENTPV%NENT => DENTNW                               PTRSET........5800
                  END IF                                                 PTRSET........5900
                  DEALLOCATE(DENTPI)                                     PTRSET........6000
                  GOTO 498                                               PTRSET........6100
               END IF                                                    PTRSET........6200
               DENTPV => DENT                                            PTRSET........6300
  495       CONTINUE                                                     PTRSET........6400
C...........APPEND TO TAIL.                                              PTRSET........6500
            ALLOCATE(DENTNW)                                             PTRSET........6600
            DENTNW%NODNUM = JC                                           PTRSET........6700
            DENT%NENT => DENTNW                                          PTRSET........6800
            DEALLOCATE(DENTPI)                                           PTRSET........6900
         END IF                                                          PTRSET........7000
  498    LLIST(IC) = LLIST(IC) + 1                                       PTRSET........7100
  500 CONTINUE                                                           PTRSET........7200
C.....COMPUTE THE ARRAY DIMENSION NELT AND ALLOCATE ARRAY IA.            PTRSET........7300
      NELT = 0                                                           PTRSET........7400
      DO 600 I=1,NN                                                      PTRSET........7500
  600    NELT = NELT + LLIST(I) + 1                                      PTRSET........7600
      NDIMIA = NELT                                                      PTRSET........7700
      ALLOCATE(IA(NDIMIA))                                               PTRSET........7800
C.....TRANSFER THE LINKED LISTS TO ARRAYS IA AND JA IN SLAP COLUMN       PTRSET........7900
C        FORMAT.  DEALLOCATE POINTERS AS THEY ARE TRANSFERRED.           PTRSET........8000
      JASTRT = 1                                                         PTRSET........8100
      DO 660 I=1,NN                                                      PTRSET........8200
         JA(I) = JASTRT                                                  PTRSET........8300
         IA(JASTRT) = I                                                  PTRSET........8400
         DENT => HLIST(I)%PL                                             PTRSET........8500
         DO 650 K=1,LLIST(I)                                             PTRSET........8600
            IA(JASTRT + K) = DENT%NODNUM                                 PTRSET........8700
            DENTPV => DENT                                               PTRSET........8800
            DENT => DENT%NENT                                            PTRSET........8900
            DEALLOCATE(DENTPV)                                           PTRSET........9000
  650    CONTINUE                                                        PTRSET........9100
         JASTRT = JASTRT + LLIST(I) + 1                                  PTRSET........9200
  660 CONTINUE                                                           PTRSET........9300
      JA(NN + 1) = NELT + 1                                              PTRSET........9400
      DEALLOCATE(HLIST, LLIST)                                           PTRSET........9500
C                                                                        PTRSET........9600
      RETURN                                                             PTRSET........9700
      END                                                                PTRSET........9800
C                                                                        PTRSET........9900
C                                                                        PTRSET.......10000
C     SUBROUTINE        P  U                       SUTRA VERSION 2.1     PU.............100
C                                                                        PU.............200
C *** PURPOSE :                                                          PU.............300
C ***  TO EVALUATE P AND U AT SPECIFIED LOCAL COORDINATES WITHIN A       PU.............400
C ***  2D OR 3D ELEMENT.  ADAPTED FROM SUBROUTINES BASIS2 AND BASIS3.    PU.............500
C                                                                        PU.............600
      SUBROUTINE PU(L,XLOC,YLOC,ZLOC,PVEC,UVEC,IN,P,U)                   PU.............700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PU.............800
      DIMENSION FX(8),FY(8),FZ(8),F(8)                                   PU.............900
      DIMENSION PVEC(NN),UVEC(NN)                                        PU............1000
      DIMENSION IN(NIN),KTYPE(2)                                         PU............1100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,        PU............1200
     1   ITCYC,NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE       PU............1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              PU............1400
     1   NSOP,NSOU,NBCN                                                  PU............1500
C                                                                        PU............1600
      IF (KTYPE(1).EQ.2) THEN                                            PU............1700
C.....2D MESH                                                            PU............1800
C........EVALUATE BASIS FUNCTIONS                                        PU............1900
         XF1=1.D0-XLOC                                                   PU............2000
         XF2=1.D0+XLOC                                                   PU............2100
         YF1=1.D0-YLOC                                                   PU............2200
         YF2=1.D0+YLOC                                                   PU............2300
         FX(1)=XF1                                                       PU............2400
         FX(2)=XF2                                                       PU............2500
         FX(3)=XF2                                                       PU............2600
         FX(4)=XF1                                                       PU............2700
         FY(1)=YF1                                                       PU............2800
         FY(2)=YF1                                                       PU............2900
         FY(3)=YF2                                                       PU............3000
         FY(4)=YF2                                                       PU............3100
         DO 20 I=1,4                                                     PU............3200
   20       F(I)=0.250D0*FX(I)*FY(I)                                     PU............3300
C........EVALUATE P AND U                                                PU............3400
         P=0.D0                                                          PU............3500
         U=0.D0                                                          PU............3600
         DO 25 IL=1,4                                                    PU............3700
            II=(L-1)*4 +IL                                               PU............3800
            I=IN(II)                                                     PU............3900
            P=P+PVEC(I)*F(IL)                                            PU............4000
            U=U+UVEC(I)*F(IL)                                            PU............4100
   25    CONTINUE                                                        PU............4200
      ELSE                                                               PU............4300
C.....3D MESH                                                            PU............4400
C........EVALUATE BASIS FUNCTIONS                                        PU............4500
         XF1=1.D0-XLOC                                                   PU............4600
         XF2=1.D0+XLOC                                                   PU............4700
         YF1=1.D0-YLOC                                                   PU............4800
         YF2=1.D0+YLOC                                                   PU............4900
         ZF1=1.D0-ZLOC                                                   PU............5000
         ZF2=1.D0+ZLOC                                                   PU............5100
         FX(1)=XF1                                                       PU............5200
         FX(2)=XF2                                                       PU............5300
         FX(3)=XF2                                                       PU............5400
         FX(4)=XF1                                                       PU............5500
         FX(5)=XF1                                                       PU............5600
         FX(6)=XF2                                                       PU............5700
         FX(7)=XF2                                                       PU............5800
         FX(8)=XF1                                                       PU............5900
         FY(1)=YF1                                                       PU............6000
         FY(2)=YF1                                                       PU............6100
         FY(3)=YF2                                                       PU............6200
         FY(4)=YF2                                                       PU............6300
         FY(5)=YF1                                                       PU............6400
         FY(6)=YF1                                                       PU............6500
         FY(7)=YF2                                                       PU............6600
         FY(8)=YF2                                                       PU............6700
         FZ(1)=ZF1                                                       PU............6800
         FZ(2)=ZF1                                                       PU............6900
         FZ(3)=ZF1                                                       PU............7000
         FZ(4)=ZF1                                                       PU............7100
         FZ(5)=ZF2                                                       PU............7200
         FZ(6)=ZF2                                                       PU............7300
         FZ(7)=ZF2                                                       PU............7400
         FZ(8)=ZF2                                                       PU............7500
         DO 30 I=1,8                                                     PU............7600
   30       F(I)=0.125D0*FX(I)*FY(I)*FZ(I)                               PU............7700
C........EVALUATE P AND U                                                PU............7800
         P=0.D0                                                          PU............7900
         U=0.D0                                                          PU............8000
         DO 35 IL=1,8                                                    PU............8100
            II=(L-1)*8 +IL                                               PU............8200
            I=IN(II)                                                     PU............8300
            P=P+PVEC(I)*F(IL)                                            PU............8400
            U=U+UVEC(I)*F(IL)                                            PU............8500
   35    CONTINUE                                                        PU............8600
      END IF                                                             PU............8700
      END                                                                PU............8800
C                                                                        PU............8900
C     FUNCTION          P  U  S  W  F              SUTRA VERSION 2.1     PUSWF..........100
C                                                                        PUSWF..........200
C *** PURPOSE :                                                          PUSWF..........300
C ***  TO INTERPOLATE P, U, AND SW AT A FRACTIONAL TIME STEP (BETWEEN    PUSWF..........400
C ***  THE CURRENT AND PREVIOUS TIME STEPS) AND RETURN THE VALUES IN     PUSWF..........500
C ***  AN ARRAY.                                                         PUSWF..........600
C                                                                        PUSWF..........700
      FUNCTION PUSWF(L,XLOC,YLOC,ZLOC,SFRAC,PM1,UM1,PVEC,UVEC,IN,LREG)   PUSWF..........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               PUSWF..........900
      DIMENSION PUSWF(3)                                                 PUSWF.........1000
      DIMENSION PM1(NN),UM1(NN),PVEC(NN),UVEC(NN)                        PUSWF.........1100
      DIMENSION IN(NIN),LREG(NE)                                         PUSWF.........1200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,        PUSWF.........1300
     1   ITCYC,NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE       PUSWF.........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              PUSWF.........1500
     1   NSOP,NSOU,NBCN                                                  PUSWF.........1600
C                                                                        PUSWF.........1700
C.....EVALUATE P AND U AT PREVIOUS AND CURRENT TIME STEPS                PUSWF.........1800
      CALL PU(L,XLOC,YLOC,ZLOC,PM1,UM1,IN,P1,U1)                         PUSWF.........1900
      CALL PU(L,XLOC,YLOC,ZLOC,PVEC,UVEC,IN,P2,U2)                       PUSWF.........2000
C                                                                        PUSWF.........2100
C.....INTERPOLATE P AND U AT FRACTIONAL TIME STEP                        PUSWF.........2200
      CSFRAC = 1D0 - SFRAC                                               PUSWF.........2300
      PINT = CSFRAC*P1 + SFRAC*P2                                        PUSWF.........2400
      PUSWF(1) = PINT                                                    PUSWF.........2500
      PUSWF(2) = CSFRAC*U1 + SFRAC*U2                                    PUSWF.........2600
C                                                                        PUSWF.........2700
C.....EVALUATE SW AT FRACTIONAL TIME STEP                                PUSWF.........2800
      IF ((IUNSAT.NE.0).AND.(PINT.LT.0D0)) THEN                          PUSWF.........2900
         IUNSAT = 3                                                      PUSWF.........3000
         CALL UNSAT(SWINT,DSWDPG,RELK,PINT,LREG(L))                      PUSWF.........3100
      ELSE                                                               PUSWF.........3200
         SWINT = 1D0                                                     PUSWF.........3300
      END IF                                                             PUSWF.........3400
      PUSWF(3) = SWINT                                                   PUSWF.........3500
C                                                                        PUSWF.........3600
      RETURN                                                             PUSWF.........3700
      END                                                                PUSWF.........3800
C                                                                        PUSWF.........3900
C     SUBROUTINE        R  E  A  D  I  F           SUTRA VERSION 2.1     READIF.........100
C                                                                        READIF.........200
C *** PURPOSE :                                                          READIF.........300
C ***  TO READ A LINE FROM AN INPUT FILE INTO THE CHARACTER VARIABLE     READIF.........400
C ***  INTFIL.  HANDLE OPENING AND CLOSING OF INSERTED FILES AS          READIF.........500
C ***  NECESSARY.                                                        READIF.........600
C                                                                        READIF.........700
      SUBROUTINE READIF(KUU, INTFIL, ERRCOD)                             READIF.........800
      USE SCHDEF, ONLY : IUNIO, FNAMO                                    READIF.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                READIF........1000
      PARAMETER (KINMIN=10)                                              READIF........1100
      CHARACTER INTFIL*1000                                              READIF........1200
      CHARACTER*80 ERRCOD,CHERR(10)                                      READIF........1300
      CHARACTER*80 UNAME,FNAME,FNAIN                                     READIF........1400
      CHARACTER ERRF*3, FINS*80                                          READIF........1500
      LOGICAL IS                                                         READIF........1600
      DIMENSION INERR(10),RLERR(10)                                      READIF........1700
      DIMENSION NKS(2), KLIST(2,20), IUNIT(0:8)                          READIF........1800
      DIMENSION FNAME(0:8), FNAIN(2,20)                                  READIF........1900
      COMMON /FNAINS/ FNAIN                                              READIF........2000
      COMMON /FNAMES/ UNAME,FNAME                                        READIF........2100
      COMMON /FUNINS/ NKS,KLIST                                          READIF........2200
      COMMON /FUNITA/ IUNIT                                              READIF........2300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     READIF........2400
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      READIF........2500
C                                                                        READIF........2600
C.....COPY KUU INTO KU. SHOULD AVOID CHANGING KUU, SINCE IT IS ALREADY   READIF........2700
C        LINKED TO K1 OR K2 THROUGH THE ARGUMENT LIST, AND THE LATTER    READIF........2800
C        ARE ALSO PASSED IN THROUGH COMMON BLOCK FUNITS.                 READIF........2900
      KU = KUU                                                           READIF........3000
C                                                                        READIF........3100
C.....READ A LINE OF INPUT (UP TO 1000 CHARACTERS) FROM UNIT KU          READIF........3200
C        INTO INTFIL                                                     READIF........3300
100   READ(KU,'(A)',IOSTAT=INERR(1)) INTFIL                              READIF........3400
C.....IF THE END OF AN INSERTED FILE IS REACHED, CLOSE THAT FILE AND     READIF........3500
C        CONTINUE READING FROM THE NEXT-LEVEL-UP FILE                    READIF........3600
      IF (INERR(1).LT.0) THEN                                            READIF........3700
C........SET FLAG IK TO INDICATE WHETHER THE READ WAS ATTEMPTED FROM     READIF........3800
C           AN INP DATASET (IK=1) OR AN ICS DATASET (IK=2).              READIF........3900
         IF (KU.EQ.K1) THEN                                              READIF........4000
            IK = 1                                                       READIF........4100
         ELSE                                                            READIF........4200
            IK = 2                                                       READIF........4300
         END IF                                                          READIF........4400
C........IF READING FROM AN INSERTED FILE, CLOSE THAT FILE, UPDATE       READIF........4500
C           UNIT NUMBERS, FILENAME, AND COUNTER TO INDICATE THE          READIF........4600
C           NEXT-LEVEL-UP FILE, AND CONTINUE READING                     READIF........4700
         IF (NKS(IK).GT.0) THEN                                          READIF........4800
            CLOSE(KU)                                                    READIF........4900
            IF (KU.EQ.K1) THEN                                           READIF........5000
               K1 = KLIST(IK, NKS(IK))                                   READIF........5100
            ELSE                                                         READIF........5200
               K2 = KLIST(IK, NKS(IK))                                   READIF........5300
            END IF                                                       READIF........5400
            KU = KLIST(IK, NKS(IK))                                      READIF........5500
            FNAME(IK) = FNAIN(IK, NKS(IK))                               READIF........5600
            NKS(IK) = NKS(IK) - 1                                        READIF........5700
            GOTO 100                                                     READIF........5800
         END IF                                                          READIF........5900
C.....ELSE IF THE READ RESULTS IN A DIFFERENT KIND OF ERROR, GENERATE    READIF........6000
C        ERROR MESSAGE                                                   READIF........6100
      ELSE IF (INERR(1).GT.0) THEN                                       READIF........6200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        READIF........6300
      END IF                                                             READIF........6400
C                                                                        READIF........6500
C.....IF BLANK OR COMMENT LINE, SKIP IT.                                 READIF........6600
      IF ((INTFIL(1:1).EQ.'#').OR.(INTFIL.EQ.'')) GOTO 100               READIF........6700
C                                                                        READIF........6800
C.....IF INSERT STATEMENT, OPEN THE FILE AND CONTINUE READING            READIF........6900
      IF (INTFIL(1:7).EQ.'@INSERT') THEN                                 READIF........7000
C........SET FLAG IK TO INDICATE WHETHER THE READ WAS DONE FROM          READIF........7100
C           AN INP DATASET (IK=1) OR AN ICS DATASET (IK=2).              READIF........7200
C           SET ERRF TO THE FILE TYPE ('INP' OR 'ICS').                  READIF........7300
         IF (KU.EQ.K1) THEN                                              READIF........7400
            IK = 1                                                       READIF........7500
            ERRF = 'INP'                                                 READIF........7600
         ELSE                                                            READIF........7700
            IK = 2                                                       READIF........7800
            ERRF = 'ICS'                                                 READIF........7900
         END IF                                                          READIF........8000
C........READ THE FILE SPECIFICATION FOR THE INSERTED FILE               READIF........8100
         READ(INTFIL(8:),*,IOSTAT=INERR(1)) KINS, FINS                   READIF........8200
         IF (INERR(1).NE.0) THEN                                         READIF........8300
            CHERR(1) = ERRCOD                                            READIF........8400
            ERRCOD = 'REA-' // ERRF // '-INS'                            READIF........8500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     READIF........8600
         END IF                                                          READIF........8700
C........CHECK FOR DUPLICATE FILENAME AMONG INSERTED FILES               READIF........8800
         DO 550 I=1,2                                                    READIF........8900
         DO 550 K=1,NKS(I)                                               READIF........9000
            IF (FINS.EQ.FNAIN(I, K)) THEN                                READIF........9100
               ERRCOD = 'FIL-4'                                          READIF........9200
               INERR(1) = KINS                                           READIF........9300
               CHERR(1) = FNAME(IK)                                      READIF........9400
               CHERR(2) = FINS                                           READIF........9500
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  READIF........9600
            END IF                                                       READIF........9700
  550    CONTINUE                                                        READIF........9800
C........CHECK FOR DUPLICATE FILENAME AMONG NON-INSERTED,                READIF........9900
C           NON-OBSERVATION FILES                                        READIF.......10000
         DO 560 NFF=0,6                                                  READIF.......10100
            IF (FINS.EQ.FNAME(NFF)) THEN                                 READIF.......10200
               ERRCOD = 'FIL-4'                                          READIF.......10300
               INERR(1) = KINS                                           READIF.......10400
               CHERR(1) = FNAME(IK)                                      READIF.......10500
               CHERR(2) = FINS                                           READIF.......10600
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  READIF.......10700
            END IF                                                       READIF.......10800
  560    CONTINUE                                                        READIF.......10900
C........CHECK FOR DUPLICATE FILENAME AMONG OBSERVATION FILES            READIF.......11000
         DO 570 NJ=1,NFLOMX                                              READIF.......11100
            IF (FINS.EQ.FNAMO(NJ)) THEN                                  READIF.......11200
               ERRCOD = 'FIL-4'                                          READIF.......11300
               INERR(1) = KINS                                           READIF.......11400
               CHERR(1) = FNAME(IK)                                      READIF.......11500
               CHERR(2) = FINS                                           READIF.......11600
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  READIF.......11700
            END IF                                                       READIF.......11800
  570    CONTINUE                                                        READIF.......11900
C........IF THE SPECIFIED UNIT NUMBER IS LESS THAN KINMIN,               READIF.......12000
C           SET IT TO KINMIN                                             READIF.......12100
         KINS = MAX(KINS, KINMIN)                                        READIF.......12200
C........IF THE FILE TO BE INSERTED EXISTS, ASSIGN IT A UNIT NUMBER      READIF.......12300
C           AND OPEN IT                                                  READIF.......12400
         INQUIRE(FILE=FINS,EXIST=IS)                                     READIF.......12500
         IF (IS) THEN                                                    READIF.......12600
            CALL NAFU(KINS,NFLOMX,FINS)                                  READIF.......12700
            OPEN(UNIT=KINS,FILE=FINS,STATUS='OLD',FORM='FORMATTED',      READIF.......12800
     1         IOSTAT=KERR)                                              READIF.......12900
            IF (KERR.GT.0) THEN                                          READIF.......13000
               CHERR(1) = FNAME(IK)                                      READIF.......13100
               CHERR(2) = FINS                                           READIF.......13200
               INERR(1) = KINS                                           READIF.......13300
               ERRCOD = 'FIL-2'                                          READIF.......13400
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  READIF.......13500
            END IF                                                       READIF.......13600
         ELSE                                                            READIF.......13700
            CHERR(1) = FNAME(IK)                                         READIF.......13800
            CHERR(2) = FINS                                              READIF.......13900
            ERRCOD = 'FIL-1'                                             READIF.......14000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     READIF.......14100
         END IF                                                          READIF.......14200
C........UPDATE THE INSERTION COUNTER.  IF THE COUNT EXCEEDS 20,         READIF.......14300
C           GENERATE AN ERROR                                            READIF.......14400
         NKS(IK) = NKS(IK) + 1                                           READIF.......14500
         IF (NKS(IK).GT.20) THEN                                         READIF.......14600
            CHERR(1) = FNAME(IK)                                         READIF.......14700
            CHERR(2) = FINS                                              READIF.......14800
            ERRCOD = 'FIL-8'                                             READIF.......14900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     READIF.......15000
         END IF                                                          READIF.......15100
C........UPDATE UNIT NUMBERS AND FILENAMES TO INDICATE THE NEWLY         READIF.......15200
C           INSERTED FILE, AND CONTINUE READING                          READIF.......15300
         IF (KU.EQ.K1) THEN                                              READIF.......15400
            K1 = KINS                                                    READIF.......15500
         ELSE                                                            READIF.......15600
            K2 = KINS                                                    READIF.......15700
         END IF                                                          READIF.......15800
         KLIST(IK, NKS(IK)) = KU                                         READIF.......15900
         FNAIN(IK, NKS(IK)) = FNAME(IK)                                  READIF.......16000
         KU = KINS                                                       READIF.......16100
         FNAME(IK) = FINS                                                READIF.......16200
         GOTO 100                                                        READIF.......16300
      END IF                                                             READIF.......16400
C                                                                        READIF.......16500
      RETURN                                                             READIF.......16600
      END                                                                READIF.......16700
C                                                                        READIF.......16800
C     SUBROUTINE        R  O  T  A  T  E           SUTRA VERSION 2.1     ROTATE.........100
C                                                                        ROTATE.........200
C *** PURPOSE :                                                          ROTATE.........300
C ***  TO TRANSFORM THE COORDINATES OF A VECTOR, {x}, BY APPLYING THE    ROTATE.........400
C ***  ROTATION MATRIX, [G]:  {xp}=[G]{x}.                               ROTATE.........500
C                                                                        ROTATE.........600
      SUBROUTINE ROTATE(G11,G12,G13,G21,G22,G23,G31,G32,G33,X,Y,Z,       ROTATE.........700
     1   XP,YP,ZP)                                                       ROTATE.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ROTATE.........900
C                                                                        ROTATE........1000
C.....COMPUTE VECTOR {xp} AS THE PRODUCT OF MATRIX [G] AND VECTOR {x}    ROTATE........1100
      XP= G11*X + G12*Y + G13*Z                                          ROTATE........1200
      YP= G21*X + G22*Y + G23*Z                                          ROTATE........1300
      ZP= G31*X + G32*Y + G33*Z                                          ROTATE........1400
C                                                                        ROTATE........1500
      RETURN                                                             ROTATE........1600
      END                                                                ROTATE........1700
C                                                                        ROTATE........1800
C     SUBROUTINE        R  O  T  M  A  T           SUTRA VERSION 2.1     ROTMAT.........100
C                                                                        ROTMAT.........200
C *** PURPOSE :                                                          ROTMAT.........300
C ***  TO COMPUTE A TRANSFORMATION MATRIX, [G], THAT CONVERTS            ROTMAT.........400
C ***  COORDINATES OF A VECTOR, {v}, FROM A COORDINATE SYSTEM (X, Y, Z)  ROTMAT.........500
C ***  TO A NEW COORDINATE SYSTEM (X', Y', Z'):  {v'} = [G]{v}.          ROTMAT.........600
C ***  THE OVERALL TRANSFORMATION IS THE RESULT OF THREE ROTATIONS       ROTMAT.........700
C ***  APPLIED CONSECUTIVELY:                                            ROTMAT.........800
C ***  A1 = ROTATION IN THE XY-PLANE, COUNTER-CLOCKWISE FROM THE         ROTMAT.........900
C ***     +X-AXIS (LOOKING DOWN THE +Z-AXIS TOWARD THE ORIGIN),          ROTMAT........1000
C ***  A2 = ROTATION IN THE NEW XZ-PLANE, COUNTER-CLOCKWISE FROM THE     ROTMAT........1100
C ***     NEW +X-AXIS (LOOKING DOWN THE NEW +Y-AXIS TOWARD THE ORIGIN),  ROTMAT........1200
C ***  A3 = ROTATION IN THE NEW YZ-PLANE, COUNTER-CLOCKWISE FROM THE     ROTMAT........1300
C ***     NEW +Y-AXIS (LOOKING DOWN THE NEW +X-AXIS TOWARD THE ORIGIN).  ROTMAT........1400
C                                                                        ROTMAT........1500
      SUBROUTINE ROTMAT(A1,A2,A3,G11,G12,G13,G21,G22,G23,G31,G32,G33)    ROTMAT........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ROTMAT........1700
C                                                                        ROTMAT........1800
C.....COMPUTE SINES AND COSINES OF ANGLES.                               ROTMAT........1900
      S1= DSIN(A1)                                                       ROTMAT........2000
      C1= DCOS(A1)                                                       ROTMAT........2100
      S2= DSIN(A2)                                                       ROTMAT........2200
      C2= DCOS(A2)                                                       ROTMAT........2300
      S3= DSIN(A3)                                                       ROTMAT........2400
      C3= DCOS(A3)                                                       ROTMAT........2500
C                                                                        ROTMAT........2600
C.....COMPUTE ROTATION MATRIX.                                           ROTMAT........2700
      G11 =  C1*C2                                                       ROTMAT........2800
      G12 =  -C1*S2*S3 - S1*C3                                           ROTMAT........2900
      G13 =  -C1*S2*C3 + S1*S3                                           ROTMAT........3000
      G21 =  S1*C2                                                       ROTMAT........3100
      G22 =  -S1*S2*S3 + C1*C3                                           ROTMAT........3200
      G23 =  -S1*S2*C3 - C1*S3                                           ROTMAT........3300
      G31 =  S2                                                          ROTMAT........3400
      G32 =  C2*S3                                                       ROTMAT........3500
      G33 =  C2*C3                                                       ROTMAT........3600
      RETURN                                                             ROTMAT........3700
      END                                                                ROTMAT........3800
C                                                                        ROTMAT........3900
C     SUBROUTINE        S  O  L  V  E  B           SUTRA VERSION 2.1     SOLVEB.........100
C                                                                        SOLVEB.........200
C *** PURPOSE :                                                          SOLVEB.........300
C ***  TO SOLVE THE MATRIX EQUATION BY:                                  SOLVEB.........400
C ***   (1) DECOMPOSING THE MATRIX                                       SOLVEB.........500
C ***   (2) MODIFYING THE RIGHT-HAND SIDE                                SOLVEB.........600
C ***   (3) BACK-SUBSTITUTING FOR THE SOLUTION                           SOLVEB.........700
C                                                                        SOLVEB.........800
      SUBROUTINE SOLVEB(KMT,C,R,NNP,IHALFB,MAXNP,MAXBW)                  SOLVEB.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOLVEB........1000
      DIMENSION C(MAXNP,MAXBW),R(MAXNP)                                  SOLVEB........1100
C                                                                        SOLVEB........1200
      IHBP=IHALFB+1                                                      SOLVEB........1300
C                                                                        SOLVEB........1400
C.....DECOMPOSE MATRIX C BY BANDED GAUSSIAN ELIMINATION FOR              SOLVEB........1500
C        NON-SYMMETRIC MATRIX                                            SOLVEB........1600
      IF(KMT-1) 5,5,50                                                   SOLVEB........1700
    5 NU=NNP-IHALFB                                                      SOLVEB........1800
      DO 20 NI=1,NU                                                      SOLVEB........1900
      PIVOTI=1.D0/C(NI,IHBP)                                             SOLVEB........2000
      NJ=NI+1                                                            SOLVEB........2100
      IB=IHBP                                                            SOLVEB........2200
      NK=NI+IHALFB                                                       SOLVEB........2300
      DO 10 NL=NJ,NK                                                     SOLVEB........2400
      IB=IB-1                                                            SOLVEB........2500
      A=-C(NL,IB)*PIVOTI                                                 SOLVEB........2600
      C(NL,IB)=A                                                         SOLVEB........2700
      JB=IB+1                                                            SOLVEB........2800
      KB=IB+IHALFB                                                       SOLVEB........2900
      LB=IHBP-IB                                                         SOLVEB........3000
      DO10 MB=JB,KB                                                      SOLVEB........3100
      NB=LB+MB                                                           SOLVEB........3200
   10 C(NL,MB)=C(NL,MB)+A*C(NI,NB)                                       SOLVEB........3300
   20 CONTINUE                                                           SOLVEB........3400
      NR=NU+1                                                            SOLVEB........3500
      NU=NNP-1                                                           SOLVEB........3600
      NK=NNP                                                             SOLVEB........3700
      DO 40 NI=NR,NU                                                     SOLVEB........3800
      PIVOTI=1.D0/(C(NI,IHBP))                                           SOLVEB........3900
      NJ=NI+1                                                            SOLVEB........4000
      IB=IHBP                                                            SOLVEB........4100
      DO 30 NL=NJ,NK                                                     SOLVEB........4200
      IB=IB-1                                                            SOLVEB........4300
      A=-C(NL,IB)*PIVOTI                                                 SOLVEB........4400
      C(NL,IB)=A                                                         SOLVEB........4500
      JB=IB+1                                                            SOLVEB........4600
      KB=IB+IHALFB                                                       SOLVEB........4700
      LB=IHBP-IB                                                         SOLVEB........4800
      DO 30 MB=JB,KB                                                     SOLVEB........4900
      NB=LB+MB                                                           SOLVEB........5000
   30 C(NL,MB)=C(NL,MB)+A*C(NI,NB)                                       SOLVEB........5100
   40 CONTINUE                                                           SOLVEB........5200
      IF(KMT-1) 50,44,50                                                 SOLVEB........5300
   44 RETURN                                                             SOLVEB........5400
C                                                                        SOLVEB........5500
C.....UPDATE RIGHT-HAND SIDE VECTOR, R                                   SOLVEB........5600
   50 NU=NNP+1                                                           SOLVEB........5700
      IBAND=2*IHALFB+1                                                   SOLVEB........5800
      DO 70 NI=2,IHBP                                                    SOLVEB........5900
      IB=IHBP-NI+1                                                       SOLVEB........6000
      NJ=1                                                               SOLVEB........6100
      SUM=0.0D0                                                          SOLVEB........6200
      DO 60 JB=IB,IHALFB                                                 SOLVEB........6300
      SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........6400
   60 NJ=NJ+1                                                            SOLVEB........6500
   70 R(NI)=R(NI)+SUM                                                    SOLVEB........6600
      IB=1                                                               SOLVEB........6700
      NL=IHBP+1                                                          SOLVEB........6800
      DO 90 NI=NL,NNP                                                    SOLVEB........6900
      NJ=NI-IHBP+1                                                       SOLVEB........7000
      SUM=0.D0                                                           SOLVEB........7100
      DO 80 JB=IB,IHALFB                                                 SOLVEB........7200
      SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........7300
   80 NJ=NJ+1                                                            SOLVEB........7400
   90 R(NI)=R(NI)+SUM                                                    SOLVEB........7500
C                                                                        SOLVEB........7600
C.....BACK SOLVE                                                         SOLVEB........7700
      R(NNP)=R(NNP)/C(NNP,IHBP)                                          SOLVEB........7800
      DO 110 IB=2,IHBP                                                   SOLVEB........7900
      NI=NU-IB                                                           SOLVEB........8000
      NJ=NI                                                              SOLVEB........8100
      MB=IHALFB+IB                                                       SOLVEB........8200
      SUM=0.D0                                                           SOLVEB........8300
      DO 100 JB=NL,MB                                                    SOLVEB........8400
      NJ=NJ+1                                                            SOLVEB........8500
  100 SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........8600
  110 R(NI)=(R(NI)-SUM)/C(NI,IHBP)                                       SOLVEB........8700
      MB=IBAND                                                           SOLVEB........8800
      DO 130 IB=NL,NNP                                                   SOLVEB........8900
      NI=NU-IB                                                           SOLVEB........9000
      NJ=NI                                                              SOLVEB........9100
      SUM=0.D0                                                           SOLVEB........9200
      DO 120 JB=NL,MB                                                    SOLVEB........9300
      NJ=NJ+1                                                            SOLVEB........9400
  120 SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........9500
  130 R(NI)=(R(NI)-SUM)/C(NI,IHBP)                                       SOLVEB........9600
C                                                                        SOLVEB........9700
C                                                                        SOLVEB........9800
      RETURN                                                             SOLVEB........9900
      END                                                                SOLVEB.......10000
C                                                                        SOLVEB.......10100
C     SUBROUTINE        S  O  L  V  E  R           SUTRA VERSION 2.1     SOLVER.........100
C                                                                        SOLVER.........200
C *** PURPOSE :                                                          SOLVER.........300
C ***  TO CALL THE APPROPRIATE MATRIX EQUATION SOLVER.                   SOLVER.........400
C                                                                        SOLVER.........500
      SUBROUTINE SOLVER(KMT,KPU,KSOLVR,C,R,XITER,B,NNP,IHALFB,MAXNP,     SOLVER.........600
     1                  MAXBW,IWK,FWK,IA,JA,IERR,ITRS,ERR)               SOLVER.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOLVER.........800
      CHARACTER SOLNAM(0:10)*40,SOLWRD(0:10)*10,KPUTXT(2)*1              SOLVER.........900
      DIMENSION C(MAXNP,MAXBW),R(NNVEC),XITER(NNP),B(NNNX)               SOLVER........1000
      DIMENSION IWK(NWI),FWK(NWF)                                        SOLVER........1100
      DIMENSION IA(NDIMIA),JA(NDIMJA)                                    SOLVER........1200
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SOLVER........1300
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SOLVER........1400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SOLVER........1500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SOLVER........1600
     1   KSCRN,KPAUSE                                                    SOLVER........1700
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SOLVER........1800
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           SOLVER........1900
      DATA (KPUTXT(K), K=1,2) /'P', 'U'/                                 SOLVER........2000
      SAVE KPUTXT                                                        SOLVER........2100
C                                                                        SOLVER........2200
C.....IF THE RIGHT-HAND-SIDE VECTOR OF THE MATRIX EQUATION IS ZERO,      SOLVER........2300
C        RETURN A ZERO SOLUTION VECTOR.                                  SOLVER........2400
      LENR = NNVEC                                                       SOLVER........2500
      RHSNRM = DNRM2(LENR, R, 1)                                         SOLVER........2600
      IF (RHSNRM.EQ.0D0) THEN                                            SOLVER........2700
         IERR = 0                                                        SOLVER........2800
         DO 44 I=1,NNVEC                                                 SOLVER........2900
   44       R(I) = 0D0                                                   SOLVER........3000
         IF (KSCRN.EQ.1) WRITE(*,55) KPUTXT(KPU),KPUTXT(KPU),KPUTXT(KPU) SOLVER........3100
         WRITE (K00,55) KPUTXT(KPU), KPUTXT(KPU), KPUTXT(KPU)            SOLVER........3200
   55    FORMAT (1X, 6X, A1, '-solution (', A1, '=0)'                    SOLVER........3300
     1      ' inferred from matrix equation A*', A1, '=0;'               SOLVER........3400
     1      ' solver not called.')                                       SOLVER........3500
         RETURN                                                          SOLVER........3600
      END IF                                                             SOLVER........3700
C                                                                        SOLVER........3800
C.....SIGNAL THE START OF A SOLUTION                                     SOLVER........3900
  101 IF (KSCRN.EQ.1) WRITE(*,133) KPUTXT(KPU),TRIM(SOLWRD(KSOLVR))      SOLVER........4000
      WRITE (K00,133) KPUTXT(KPU), TRIM(SOLWRD(KSOLVR))                  SOLVER........4100
  133 FORMAT (1X, 6X, "Starting ", A1, "-solution using ", A,            SOLVER........4200
     1   " solver ...")                                                  SOLVER........4300
C                                                                        SOLVER........4400
C.....IF KSOLVR=0, CALL BANDED GAUSSIAN (DIRECT) SOLVER.                 SOLVER........4500
C        OTHERWISE, CALL ITERATIVE SOLVER.                               SOLVER........4600
      IF (KSOLVR.EQ.0) THEN                                              SOLVER........4700
         CALL SOLVEB(KMT, C, R, NNP, IHALFB, MAXNP, MAXBW)               SOLVER........4800
         IERR = 0                                                        SOLVER........4900
         ITRS = 0                                                        SOLVER........5000
         ERR = 0D0                                                       SOLVER........5100
      ELSE                                                               SOLVER........5200
         CALL SOLWRP(KPU, KSOLVR, C, R, XITER, B, NNP,                   SOLVER........5300
     1         IWK, FWK, IA, JA, IERR, ITRS, ERR)                        SOLVER........5400
      END IF                                                             SOLVER........5500
C                                                                        SOLVER........5600
      RETURN                                                             SOLVER........5700
      END                                                                SOLVER........5800
C                                                                        SOLVER........5900
C     SUBROUTINE        S  O  L  W  R  P           SUTRA VERSION 2.1     SOLWRP.........100
C                                                                        SOLWRP.........200
C *** PURPOSE :                                                          SOLWRP.........300
C ***  TO SERVE AS A WRAPPER FOR THE ITERATIVE SOLVERS, PERFORMING       SOLWRP.........400
C ***  SOME PRELIMINARIES ON VECTORS BEFORE CALLING A SOLVER.            SOLWRP.........500
C                                                                        SOLWRP.........600
      SUBROUTINE SOLWRP(KPU, KSOLVR, A, R, XITER, B, NNP,                SOLWRP.........700
     1                  IWK, FWK, IA, JA, IERR, ITRS, ERR)               SOLWRP.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOLWRP.........900
      CHARACTER*80 UNAME,FNAME(0:8)                                      SOLWRP........1000
      CHARACTER*1 KPUTXT(2)                                              SOLWRP........1100
      CHARACTER*40 SOLNAM(0:10)                                          SOLWRP........1200
      CHARACTER*10 SOLWRD(0:10)                                          SOLWRP........1300
      DIMENSION A(NELT)                                                  SOLWRP........1400
      DIMENSION IA(NDIMIA),JA(NDIMJA)                                    SOLWRP........1500
      DIMENSION IWK(NWI),FWK(NWF)                                        SOLWRP........1600
      DIMENSION XITER(NNP),R(NNP),B(NNNX)                                SOLWRP........1700
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SOLWRP........1800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           SOLWRP........1900
      COMMON /FNAMES/ UNAME,FNAME                                        SOLWRP........2000
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SOLWRP........2100
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            SOLWRP........2200
      COMMON /ITSOLR/ TOLP,TOLU                                          SOLWRP........2300
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SOLWRP........2400
     1   KSCRN,KPAUSE                                                    SOLWRP........2500
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SOLWRP........2600
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SOLWRP........2700
      COMMON /SOLVN/ NSLVRS                                              SOLWRP........2800
      DATA (KPUTXT(K), K=1,2) /'P', 'U'/                                 SOLWRP........2900
      SAVE KPUTXT                                                        SOLWRP........3000
      EXTERNAL DSLUGM,DSLUOM                                             SOLWRP........3100
C                                                                        SOLWRP........3200
C.....COPY THE RHS VECTOR R INTO VECTOR B, THEN USE R AS THE             SOLWRP........3300
C        SOLUTION VECTOR.  INITIALIZE IT FROM THE LATEST SUTRA           SOLWRP........3400
C        SOLUTION.  XITER IS NOT USED AS THE SOLUTION VECTOR BECAUSE     SOLWRP........3500
C        DOING SO MIGHT INTERFERE WITH SUBSEQUENT CALCULATIONS.          SOLWRP........3600
      DO 150 N=1,NNP                                                     SOLWRP........3700
         B(N) = R(N)                                                     SOLWRP........3800
         R(N) = XITER(N)                                                 SOLWRP........3900
  150 CONTINUE                                                           SOLWRP........4000
C                                                                        SOLWRP........4100
C.....SET ITERATIVE SOLVER PARAMETERS.                                   SOLWRP........4200
C        IUNITE --> UNIT ON WHICH TO WRITE SOLVER ERROR (0 = NONE)       SOLWRP........4300
C        ISYM  --> 0 = FULL STORAGE; 1 = SYMMETRIC STORAGE               SOLWRP........4400
C        ITRMX --> MAXIMUM NUMBER OF SOLVER ITERATIONS                   SOLWRP........4500
C        ITOL  --> TYPE OF CONVERGENCE CRITERION                         SOLWRP........4600
C        TOL   --> CONVERGENCE TOLERANCE                                 SOLWRP........4700
C        NSAVE --> NUMBER OF DIRECTION VECTORS                           SOLWRP........4800
      IF (KPU.EQ.1) THEN                                                 SOLWRP........4900
C........SET PARAMETERS FOR ITERATIVE P SOLUTION.                        SOLWRP........5000
         ISYM = 0                                                        SOLWRP........5100
         ITRMX = MAX(ITRMXP, 1)                                          SOLWRP........5200
         ITOL = ITOLP                                                    SOLWRP........5300
         TOL = TOLP                                                      SOLWRP........5400
         NSAVE = NSAVEP                                                  SOLWRP........5500
      ELSE                                                               SOLWRP........5600
C........SET PARAMETERS FOR ITERATIVE U SOLUTION.                        SOLWRP........5700
         ISYM = 0                                                        SOLWRP........5800
         ITRMX = MAX(ITRMXU, 1)                                          SOLWRP........5900
         ITOL = ITOLU                                                    SOLWRP........6000
         TOL = TOLU                                                      SOLWRP........6100
         NSAVE = NSAVEU                                                  SOLWRP........6200
      END IF                                                             SOLWRP........6300
      IUNITE = K00                                                       SOLWRP........6400
C                                                                        SOLWRP........6500
C.....CALL AN ITERATIVE SOLVER:                                          SOLWRP........6600
C        DSICCG = CG WITH IC PRECONDITIONING,                            SOLWRP........6700
C        DSLUGM = GMRES WITH ILU PRECONDITIONING,                        SOLWRP........6800
C        DSLUOM = ORTHOMIN WITH ILU PRECONDITIONING.                     SOLWRP........6900
      IF (KSOLVR.EQ.1) THEN                                              SOLWRP........7000
         CALL DSICCG(NNP, B, R, NELT, IA, JA, A, ISYM, ITOL, TOL,        SOLWRP........7100
     1            ITRMX, ITRS, ERR, IERR, IUNITE, FWK, NWF, IWK, NWI)    SOLWRP........7200
      ELSE IF (KSOLVR.EQ.2) THEN                                         SOLWRP........7300
         CALL DSLUGM(NNP, B, R, NELT, IA, JA, A, ISYM, NSAVE, ITOL, TOL, SOLWRP........7400
     1            ITRMX, ITRS, ERR, IERR, IUNITE, FWK, NWF, IWK, NWI)    SOLWRP........7500
      ELSE                                                               SOLWRP........7600
         CALL DSLUOM(NNP, B, R, NELT, IA, JA, A, ISYM, NSAVE, ITOL, TOL, SOLWRP........7700
     1            ITRMX, ITRS, ERR, IERR, IUNITE, FWK, NWF, IWK, NWI)    SOLWRP........7800
      END IF                                                             SOLWRP........7900
C                                                                        SOLWRP........8000
C.....WRITE CONVERGENCE INFORMATION.                                     SOLWRP........8100
      IF (IERR.EQ.0) THEN                                                SOLWRP........8200
         IF (KSCRN.EQ.1) WRITE (*,555) KPUTXT(KPU), ITRS, ERR            SOLWRP........8300
         WRITE (K00,555) KPUTXT(KPU), ITRS, ERR                          SOLWRP........8400
  555    FORMAT (1X, 6X, A1, '-solution converged in ', I5,              SOLWRP........8500
     1      ' solver iterations  (Error ~ ', 1PE8.1, ')')                SOLWRP........8600
      ELSE                                                               SOLWRP........8700
         IF (KSCRN.EQ.1) WRITE (*,557) KPUTXT(KPU), ITRS, ERR            SOLWRP........8800
         WRITE (K00,557) KPUTXT(KPU), ITRS, ERR                          SOLWRP........8900
  557    FORMAT (1X, 6X, A1, '-solution FAILED after ', I5,              SOLWRP........9000
     1      ' solver iterations  (Error ~ ', 1PE8.1, ')')                SOLWRP........9100
      END IF                                                             SOLWRP........9200
C                                                                        SOLWRP........9300
      RETURN                                                             SOLWRP........9400
      END                                                                SOLWRP........9500
C                                                                        SOLWRP........9600
C     SUBROUTINE        S  O  U  R  C  E           SUTRA VERSION 2.1     SOURCE.........100
C                                                                        SOURCE.........200
C *** PURPOSE :                                                          SOURCE.........300
C ***  TO READ AND ORGANIZE FLUID MASS SOURCE DATA AND ENERGY OR         SOURCE.........400
C ***  SOLUTE MASS SOURCE DATA.                                          SOURCE.........500
C                                                                        SOURCE.........600
      SUBROUTINE SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT)          SOURCE.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOURCE.........800
      CHARACTER INTFIL*1000                                              SOURCE.........900
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8)                     SOURCE........1000
      DIMENSION KTYPE(2)                                                 SOURCE........1100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SOURCE........1200
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SOURCE........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SOURCE........1400
     1   NSOP,NSOU,NBCN                                                  SOURCE........1500
      COMMON /FNAMES/ UNAME,FNAME                                        SOURCE........1600
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SOURCE........1700
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),IQSOU(NSOU)         SOURCE........1800
      DIMENSION INERR(10),RLERR(10)                                      SOURCE........1900
C                                                                        SOURCE........2000
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES.                      SOURCE........2100
C.....NSOUI IS ACTUAL NUMBER OF SOLUTE MASS OR ENERGY SOURCE NODES.      SOURCE........2200
      NSOPI=NSOP-1                                                       SOURCE........2300
      NSOUI=NSOU-1                                                       SOURCE........2400
      IQSOPT=1                                                           SOURCE........2500
      IQSOUT=1                                                           SOURCE........2600
      NIQP=0                                                             SOURCE........2700
      NIQU=0                                                             SOURCE........2800
      IF(NSOPI.EQ.0) GOTO 1000                                           SOURCE........2900
      IF(ME) 50,50,150                                                   SOURCE........3000
   50 WRITE(K3,100)                                                      SOURCE........3100
  100 FORMAT('1'////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........3200
     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........3300
     2   'SPECIFIED ****'//11X,'NODE NUMBER',10X,                        SOURCE........3400
     3   'FLUID INFLOW(+)/OUTFLOW(-)',5X,'SOLUTE CONCENTRATION OF'       SOURCE........3500
     4   /11X,'(MINUS INDICATES',5X,'(FLUID MASS/SECOND)',               SOURCE........3600
     5   12X,'INFLOWING FLUID'/12X,'TIME-VARYING',39X,                   SOURCE........3700
     6   '(MASS SOLUTE/MASS WATER)'/12X,'FLOW RATE OR'/12X,              SOURCE........3800
     7   'CONCENTRATION)'//)                                             SOURCE........3900
      GOTO 300                                                           SOURCE........4000
  150 WRITE(K3,200)                                                      SOURCE........4100
  200 FORMAT('1'////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........4200
     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........4300
     2   'SPECIFIED ****'//11X,'NODE NUMBER',10X,                        SOURCE........4400
     3   'FLUID INFLOW(+)/OUTFLOW(-)',5X,'TEMPERATURE {DEGREES CELSIUS}' SOURCE........4500
     4   /11X,'(MINUS INDICATES',5X,'(FLUID MASS/SECOND)',12X,           SOURCE........4600
     5   'OF INFLOWING FLUID'/12X,'TIME-VARYING'/12X,'FLOW OR'/12X,      SOURCE........4700
     6   'TEMPERATURE)'//)                                               SOURCE........4800
C                                                                        SOURCE........4900
C.....INPUT DATASET 17:  DATA FOR FLUID SOURCES AND SINKS                SOURCE........5000
  300 CONTINUE                                                           SOURCE........5100
  305 NIQP=NIQP+1                                                        SOURCE........5200
      ERRCOD = 'REA-INP-17'                                              SOURCE........5300
      CALL READIF(K1, INTFIL, ERRCOD)                                    SOURCE........5400
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCP                                SOURCE........5500
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SOURCE........5600
      IQCPA = IABS(IQCP)                                                 SOURCE........5700
      IF (IQCP.EQ.0) THEN                                                SOURCE........5800
         GOTO 700                                                        SOURCE........5900
      ELSE IF (IQCPA.GT.NN) THEN                                         SOURCE........6000
         ERRCOD = 'INP-17-1'                                             SOURCE........6100
         INERR(1) = IQCPA                                                SOURCE........6200
         INERR(2) = NN                                                   SOURCE........6300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE........6400
      ELSE IF (NIQP.GT.NSOPI) THEN                                       SOURCE........6500
         GOTO 305                                                        SOURCE........6600
      END IF                                                             SOURCE........6700
      ERRCOD = 'REA-INP-17'                                              SOURCE........6800
      IF (IQCP.GT.0) THEN                                                SOURCE........6900
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC                        SOURCE........7000
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     SOURCE........7100
         IF (QINC.GT.0D0) THEN                                           SOURCE........7200
            READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC,UINC                SOURCE........7300
            IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)  SOURCE........7400
         END IF                                                          SOURCE........7500
      END IF                                                             SOURCE........7600
      IQSOP(NIQP)=IQCP                                                   SOURCE........7700
      IF(IQCP.LT.0) IQSOPT=-1                                            SOURCE........7800
      IQP=IABS(IQCP)                                                     SOURCE........7900
      QIN(IQP)=QINC                                                      SOURCE........8000
      UIN(IQP)=UINC                                                      SOURCE........8100
      IF(IQCP.GT.0) GOTO 450                                             SOURCE........8200
      WRITE(K3,500) IQCP                                                 SOURCE........8300
      GOTO 600                                                           SOURCE........8400
  450 IF(QINC.GT.0) GOTO 460                                             SOURCE........8500
      WRITE(K3,500) IQCP,QINC                                            SOURCE........8600
      GOTO 600                                                           SOURCE........8700
  460 WRITE(K3,500) IQCP,QINC,UINC                                       SOURCE........8800
  500 FORMAT(11X,I10,13X,1PE14.7,16X,1PE14.7)                            SOURCE........8900
  600 GOTO 305                                                           SOURCE........9000
  700 NIQP = NIQP - 1                                                    SOURCE........9100
      IF(NIQP.EQ.NSOPI) GOTO 890                                         SOURCE........9200
         ERRCOD = 'INP-3,17-1'                                           SOURCE........9300
         INERR(1) = NIQP                                                 SOURCE........9400
         INERR(2) = NSOPI                                                SOURCE........9500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE........9600
  890 IF(IQSOPT.EQ.-1) WRITE(K3,900)                                     SOURCE........9700
  900 FORMAT(////11X,'THE SPECIFIED TIME VARIATIONS ARE ',               SOURCE........9800
     1   'USER-PROGRAMMED IN SUBROUTINE  B C T I M E .')                 SOURCE........9900
C                                                                        SOURCE.......10000
C                                                                        SOURCE.......10100
 1000 IF(NSOUI.EQ.0) GOTO 9000                                           SOURCE.......10200
      IF(ME) 1050,1050,1150                                              SOURCE.......10300
 1050 WRITE(K3,1100)                                                     SOURCE.......10400
 1100 FORMAT(////////11X,'S O L U T E   S O U R C E   D A T A'           SOURCE.......10500
     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF SOLUTE ',      SOURCE.......10600
     2   'MASS ARE SPECIFIED ****'//11X,'NODE NUMBER',10X,               SOURCE.......10700
     3   'SOLUTE SOURCE(+)/SINK(-)'/11X,'(MINUS INDICATES',5X,           SOURCE.......10800
     4   '(SOLUTE MASS/SECOND)'/12X,'TIME-VARYING'/12X,                  SOURCE.......10900
     5   'SOURCE OR SINK)'//)                                            SOURCE.......11000
      GOTO 1305                                                          SOURCE.......11100
 1150 WRITE(K3,1200)                                                     SOURCE.......11200
 1200 FORMAT(////////11X,'E N E R G Y   S O U R C E   D A T A'           SOURCE.......11300
     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF ',             SOURCE.......11400
     2   'ENERGY ARE SPECIFIED ****'//11X,'NODE NUMBER',10X,             SOURCE.......11500
     3   'ENERGY SOURCE(+)/SINK(-)'/11X,'(MINUS INDICATES',5X,           SOURCE.......11600
     4   '(ENERGY/SECOND)'/12X,'TIME-VARYING'/12X,                       SOURCE.......11700
     5   'SOURCE OR SINK)'//)                                            SOURCE.......11800
C                                                                        SOURCE.......11900
C.....INPUT DATASET 18:  DATA FOR ENERGY OR SOLUTE MASS SOURCES OR SINKS SOURCE.......12000
 1305 NIQU=NIQU+1                                                        SOURCE.......12100
      ERRCOD = 'REA-INP-18'                                              SOURCE.......12200
      CALL READIF(K1, INTFIL, ERRCOD)                                    SOURCE.......12300
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCU                                SOURCE.......12400
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        SOURCE.......12500
      IQCUA = IABS(IQCU)                                                 SOURCE.......12600
      IF (IQCU.EQ.0) THEN                                                SOURCE.......12700
         GOTO 1700                                                       SOURCE.......12800
      ELSE IF (IQCUA.GT.NN) THEN                                         SOURCE.......12900
         ERRCOD = 'INP-18-1'                                             SOURCE.......13000
         INERR(1) = IQCUA                                                SOURCE.......13100
         INERR(2) = NN                                                   SOURCE.......13200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE.......13300
      ELSE IF (NIQU.GT.NSOUI) THEN                                       SOURCE.......13400
         GOTO 1305                                                       SOURCE.......13500
      END IF                                                             SOURCE.......13600
      IF (IQCU.GT.0) THEN                                                SOURCE.......13700
         ERRCOD = 'REA-INP-18'                                           SOURCE.......13800
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCU,QUINC                       SOURCE.......13900
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     SOURCE.......14000
      END IF                                                             SOURCE.......14100
      IQSOU(NIQU)=IQCU                                                   SOURCE.......14200
      IF(IQCU.LT.0) IQSOUT=-1                                            SOURCE.......14300
      IQU=IABS(IQCU)                                                     SOURCE.......14400
      QUIN(IQU)=QUINC                                                    SOURCE.......14500
      IF(IQCU.GT.0) GOTO 1450                                            SOURCE.......14600
      WRITE(K3,1500) IQCU                                                SOURCE.......14700
      GOTO 1600                                                          SOURCE.......14800
 1450 WRITE(K3,1500) IQCU,QUINC                                          SOURCE.......14900
 1500 FORMAT(11X,I10,13X,1PE14.7)                                        SOURCE.......15000
 1600 GOTO 1305                                                          SOURCE.......15100
 1700 NIQU = NIQU - 1                                                    SOURCE.......15200
      IF(NIQU.EQ.NSOUI) GOTO 1890                                        SOURCE.......15300
         ERRCOD = 'INP-3,18-1'                                           SOURCE.......15400
         IF (ME.EQ.1) THEN                                               SOURCE.......15500
            CHERR(1) = 'energy'                                          SOURCE.......15600
         ELSE                                                            SOURCE.......15700
            CHERR(1) = 'solute'                                          SOURCE.......15800
         END IF                                                          SOURCE.......15900
         INERR(1) = NIQU                                                 SOURCE.......16000
         INERR(2) = NSOUI                                                SOURCE.......16100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE.......16200
 1890 IF(IQSOUT.EQ.-1) WRITE(K3,900)                                     SOURCE.......16300
C                                                                        SOURCE.......16400
 9000 RETURN                                                             SOURCE.......16500
C                                                                        SOURCE.......16600
      END                                                                SOURCE.......16700
C                                                                        SOURCE.......16800
C     SUBROUTINE        S  U  T  E  R  R           SUTRA VERSION 2.1     SUTERR.........100
C                                                                        SUTERR.........200
C *** PURPOSE :                                                          SUTERR.........300
C ***  TO HANDLE SUTRA AND FORTRAN ERRORS.                               SUTERR.........400
C                                                                        SUTERR.........500
      SUBROUTINE SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTERR.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTERR.........700
      CHARACTER*80 ERRCOD,CHERR(10),CODE(3),CODUM(3),UNAME,FNAME(0:8)    SUTERR.........800
      CHARACTER*70 DS(50),EX(50)                                         SUTERR.........900
      CHARACTER CDUM80*80                                                SUTERR........1000
      CHARACTER CINERR(10)*9,CRLERR(10)*15                               SUTERR........1100
      CHARACTER SOLNAM(0:10)*40,SOLWRD(0:10)*10                          SUTERR........1200
      CHARACTER*8 VERNUM, VERNIN                                         SUTERR........1300
      DIMENSION INERR(10), RLERR(10)                                     SUTERR........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTERR........1500
     1   NSOP,NSOU,NBCN                                                  SUTERR........1600
      COMMON /FNAMES/ UNAME,FNAME                                        SUTERR........1700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SUTERR........1800
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTERR........1900
     1   KSCRN,KPAUSE                                                    SUTERR........2000
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTERR........2100
      COMMON /SOLVN/ NSLVRS                                              SUTERR........2200
      COMMON /VER/ VERNUM, VERNIN                                        SUTERR........2300
C                                                                        SUTERR........2400
C.....PARSE THE ERROR CODE                                               SUTERR........2500
      CALL PRSWDS(ERRCOD, '-', 3, CODE, NWORDS)                          SUTERR........2600
C                                                                        SUTERR........2700
C.....IF AN ERROR OTHER THAN A MATRIX SOLVER OR NONLINEAR CONVERGENCE    SUTERR........2800
C        ERROR HAS OCCURRED, OVERRIDE THE SCREEN OUTPUT CONTROLS SO      SUTERR........2900
C        THAT THE ERROR IS PRINTED TO THE SCREEN AND SUTRA PAUSES FOR    SUTERR........3000
C        A USER RESPONSE.                                                SUTERR........3100
      IF ((CODE(1).NE.'SOL').AND.(CODE(1).NE.'CON')) THEN                SUTERR........3200
         KSCRN = +1                                                      SUTERR........3300
         KPAUSE = +1                                                     SUTERR........3400
      END IF                                                             SUTERR........3500
C                                                                        SUTERR........3600
C.....COPY INTEGER AND REAL ERROR PARAMETERS INTO CHARACTER STRINGS      SUTERR........3700
      DO 150 I=1,10                                                      SUTERR........3800
         WRITE(UNIT=CINERR(I), FMT='(I9)') INERR(I)                      SUTERR........3900
         WRITE(UNIT=CRLERR(I), FMT='(1PE15.7)') RLERR(I)                 SUTERR........4000
  150 CONTINUE                                                           SUTERR........4100
C                                                                        SUTERR........4200
C.....INITIALIZE THE ERROR OUTPUT STRINGS                                SUTERR........4300
      DO 200 I=1,50                                                      SUTERR........4400
         DS(I) = "null_line"                                             SUTERR........4500
         EX(I) = "null_line"                                             SUTERR........4600
  200 CONTINUE                                                           SUTERR........4700
C                                                                        SUTERR........4800
C.....SET THE ERROR OUTPUT STRINGS ACCORDING TO THE TYPE OF ERROR        SUTERR........4900
      IF (ERRCOD.EQ.'INP-2A-1') THEN                                     SUTERR........5000
        DS(1)="The first word of SIMULA is not 'SUTRA'."                 SUTERR........5100
        EX(1)="In dataset 2A of the main input file, the first word"     SUTERR........5200
        EX(2)="of the variable SIMULA must be 'SUTRA'."                  SUTERR........5300
        EX(3)=" "                                                        SUTERR........5400
        EX(4)="Example of a valid dataset 2A:"                           SUTERR........5500
        EX(5)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........5600
      ELSE IF (ERRCOD.EQ.'INP-2A-2') THEN                                SUTERR........5700
        DS(1)="The second word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........5800
        EX(1)="In dataset 2A of the main input file, when the second"    SUTERR........5900
        EX(2)="word is not 'VERSION', the version 2.0 input format is"   SUTERR........6000
        EX(3)="assumed, and the second word must be 'SOLUTE' or"         SUTERR........6100
        EX(4)="'ENERGY'."                                                SUTERR........6200
        EX(5)=" "                                                        SUTERR........6300
        EX(6)="Example of a valid (version 2.0) dataset 2A:"             SUTERR........6400
        EX(7)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........6500
      ELSE IF (ERRCOD.EQ.'INP-2A-3') THEN                                SUTERR........6600
        DS(1)="The fourth word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........6700
        EX(1)="In dataset 2A of the main input file, the fourth word"    SUTERR........6800
        EX(2)="must be 'SOLUTE' or 'ENERGY' (unless the version 2.0"     SUTERR........6900
        EX(3)="input format is being used)."                             SUTERR........7000
        EX(4)=" "                                                        SUTERR........7100
        EX(5)="Example of a valid (version 2.0) dataset 2A:"             SUTERR........7200
        EX(6)="'SUTRA VERSION " // TRIM(VERNUM) // " SOLUTE TRANSPORT'"  SUTERR........7300
      ELSE IF (ERRCOD.EQ.'INP-2A-4') THEN                                SUTERR........7400
        DS(1)="Unsupported SUTRA version: " // CHERR(1)                  SUTERR........7500
        EX(1)="Input files from SUTRA version " // TRIM(CHERR(1))        SUTERR........7600
        EX(2)="are not supported by version " // TRIM(VERNUM) // "."     SUTERR........7700
      ELSE IF (ERRCOD.EQ.'INP-2B-1') THEN                                SUTERR........7800
        DS(1)="The first word of MSHSTR is not '2D' or '3D'."            SUTERR........7900
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR........8000
        EX(2)="of the variable MSHSTR must be '2D' or '3D'."             SUTERR........8100
        EX(3)=" "                                                        SUTERR........8200
        EX(4)="Example of a valid dataset 2B:"                           SUTERR........8300
        EX(5)="'3D REGULAR MESH'  10  20  30"                            SUTERR........8400
C.....ERROR CODE 'INP-2B-2' IS NO LONGER USED.                           SUTERR........8500
      ELSE IF (ERRCOD.EQ.'INP-2B-3') THEN                                SUTERR........8600
        DS(1)="At least one of the rectangular dimensions NN1, NN2,"     SUTERR........8700
        DS(2)="and NN3 is set improperly."                               SUTERR........8800
        EX(1)="In dataset 2B of the main input file, the rectangular"    SUTERR........8900
        EX(2)="dimensions NN1, NN2, and (for 3D problems) NN3 must"      SUTERR........9000
        EX(3)="each be greater than 1."                                  SUTERR........9100
        EX(4)=" "                                                        SUTERR........9200
        EX(5)="Example of a valid dataset 2B:"                           SUTERR........9300
        EX(6)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR........9400
      ELSE IF (ERRCOD.EQ.'INP-2B-4') THEN                                SUTERR........9500
        DS(1)="The second word of MSHSTR is not 'IRREGULAR', 'REGULAR'," SUTERR........9600
        DS(2)="'BLOCKWISE', or 'LAYERED'."                               SUTERR........9700
        EX(1)="In dataset 2B of the main input file, the second word"    SUTERR........9800
        EX(2)="of the variable MSHSTR must be 'IRREGULAR', 'REGULAR',"   SUTERR........9900
        EX(3)="'BLOCKWISE' or 'LAYERED'.  By definition, only 3D meshes" SUTERR.......10000
        EX(4)="can be LAYERED."                                          SUTERR.......10100
        EX(5)=" "                                                        SUTERR.......10200
        EX(6)="Example of a valid dataset 2B:"                           SUTERR.......10300
        EX(7)="'3D REGULAR MESH'  10  20  30"                            SUTERR.......10400
      ELSE IF (ERRCOD.EQ.'INP-2B-5') THEN                                SUTERR.......10500
        DS(1)="A 2D LAYERED mesh has been specified."                    SUTERR.......10600
        EX(1)="By definition, only 3D meshes can be LAYERED."            SUTERR.......10700
        EX(2)=" "                                                        SUTERR.......10800
        EX(3)="Example of a valid dataset 2B:"                           SUTERR.......10900
        EX(4)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......11000
      ELSE IF (ERRCOD.EQ.'INP-2B-6') THEN                                SUTERR.......11100
        DS(1)="The first word of LAYSTR is not 'ACROSS' or 'WITHIN'."    SUTERR.......11200
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR.......11300
        EX(2)="of the variable LAYSTR must be 'ACROSS' or 'WITHIN'."     SUTERR.......11400
        EX(3)=" "                                                        SUTERR.......11500
        EX(4)="Example of a valid dataset 2B:"                           SUTERR.......11600
        EX(5)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......11700
      ELSE IF (ERRCOD.EQ.'INP-2B-7') THEN                                SUTERR.......11800
        DS(1)="At least one of the layer dimensions NLAYS, NNLAY,"       SUTERR.......11900
        DS(2)="and NELAY is set improperly."                             SUTERR.......12000
        EX(1)="In dataset 2B of the main input file, the layer"          SUTERR.......12100
        EX(2)="dimensions are subject to the following constraints:"     SUTERR.......12200
        EX(3)="NLAYS>1, NNLAY>3, and NELAY>0."                           SUTERR.......12300
        EX(4)=" "                                                        SUTERR.......12400
        EX(5)="Example of a valid dataset 2B:"                           SUTERR.......12500
        EX(6)="'3D LAYERED MESH'  10  2560  2210  'ACROSS'"              SUTERR.......12600
      ELSE IF (ERRCOD.EQ.'INP-2B,3-1') THEN                              SUTERR.......12700
        DS(1)="The number of nodes, NN, does not match the rectangular"  SUTERR.......12800
        DS(2)="dimensions, NN1*NN2(*NN3)."                               SUTERR.......12900
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......13000
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......13100
        EX(3)="rectangular dimensions, NN1*NN2 in 2D, or NN1*NN2*NN3"    SUTERR.......13200
        EX(4)="in 3D."                                                   SUTERR.......13300
        EX(5)=" "                                                        SUTERR.......13400
        EX(6)="Example:"                                                 SUTERR.......13500
        EX(7)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......13600
        EX(8)="NN=10*20*30=6000 (dataset 3)."                            SUTERR.......13700
      ELSE IF (ERRCOD.EQ.'INP-2B,3-2') THEN                              SUTERR.......13800
        DS(1)="The number of elements, NE, does not match the"           SUTERR.......13900
        DS(2)="rectangular dimensions, (NN1-1)*(NN2-1)[*(NN3-1)]."       SUTERR.......14000
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......14100
        EX(2)="number of elements, NE, must equal the product of the"    SUTERR.......14200
        EX(3)="rectangular dimensions, (NN1-1)*(NN2-1) in 2D, or"        SUTERR.......14300
        EX(4)="(NN1-1)*(NN2-1)*(NN3-1) in 3D."                           SUTERR.......14400
        EX(5)=" "                                                        SUTERR.......14500
        EX(6)="Example:"                                                 SUTERR.......14600
        EX(7)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......14700
        EX(8)="NE=9*19*29=4959 (dataset 3)."                             SUTERR.......14800
      ELSE IF (ERRCOD.EQ.'INP-2B,3-3') THEN                              SUTERR.......14900
        DS(1)="The number of nodes, NN, does not match the layered"      SUTERR.......15000
        DS(2)="dimensions, NLAYS*NNLAY."                                 SUTERR.......15100
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......15200
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......15300
        EX(3)="layered dimensions, NLAYS*NNLAY."                         SUTERR.......15400
        EX(4)=" "                                                        SUTERR.......15500
        EX(5)="Example:"                                                 SUTERR.......15600
        EX(6)="If NLAYS=10 and NNLAY=2560 (dataset 2B), then"            SUTERR.......15700
        EX(7)="NN=10*2560=25600 (dataset 3)."                            SUTERR.......15800
      ELSE IF (ERRCOD.EQ.'INP-2B,3-4') THEN                              SUTERR.......15900
        DS(1)="The number of nodes, NE, does not match the layered"      SUTERR.......16000
        DS(2)="dimensions, (NLAYS-1)*NELAY."                             SUTERR.......16100
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......16200
        EX(2)="number of nodes, NE, must equal the product of the"       SUTERR.......16300
        EX(3)="layered dimensions, (NLAYS-1)*NELAY."                     SUTERR.......16400
        EX(4)=" "                                                        SUTERR.......16500
        EX(5)="Example:"                                                 SUTERR.......16600
        EX(6)="If NLAYS=10 and NELAY=2210 (dataset 2B), then"            SUTERR.......16700
        EX(7)="NN=9*2210=19890 (dataset 3)."                             SUTERR.......16800
      ELSE IF (ERRCOD.EQ.'INP-4-1') THEN                                 SUTERR.......16900
        DS(1)="The first word of CUNSAT is not 'SATURATED' or"           SUTERR.......17000
        DS(2)="'UNSATURATED'."                                           SUTERR.......17100
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......17200
        EX(2)="of the variable CUNSAT must be 'SATURATED' or"            SUTERR.......17300
        EX(3)="'UNSATURATED'."                                           SUTERR.......17400
        EX(4)=" "                                                        SUTERR.......17500
        EX(5)="Example of a valid dataset 4:"                            SUTERR.......17600
        EX(6)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......17700
     1        " 'COLD' 10"                                               SUTERR.......17800
      ELSE IF (ERRCOD.EQ.'INP-4-2') THEN                                 SUTERR.......17900
        DS(1)="The first word of CSSFLO is not 'STEADY' or 'TRANSIENT'." SUTERR.......18000
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......18100
        EX(2)="of the variable CSSFLO must be 'STEADY' or 'TRANSIENT'."  SUTERR.......18200
        EX(3)=" "                                                        SUTERR.......18300
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......18400
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......18500
     1        " 'COLD' 10"                                               SUTERR.......18600
      ELSE IF (ERRCOD.EQ.'INP-4-3') THEN                                 SUTERR.......18700
        DS(1)="The first word of CSSTRA is not 'STEADY' or 'TRANSIENT'." SUTERR.......18800
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......18900
        EX(2)="of the variable CSSTRA must be 'STEADY' or 'TRANSIENT'."  SUTERR.......19000
        EX(3)=" "                                                        SUTERR.......19100
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......19200
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......19300
     1        " 'COLD' 10"                                               SUTERR.......19400
      ELSE IF (ERRCOD.EQ.'INP-4-4') THEN                                 SUTERR.......19500
        DS(1)="The first word of CREAD is not 'COLD' or 'WARM'."         SUTERR.......19600
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......19700
        EX(2)="of the variable CREAD must be 'COLD' or 'WARM'."          SUTERR.......19800
        EX(3)=" "                                                        SUTERR.......19900
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......20000
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......20100
     1        " 'COLD' 10"                                               SUTERR.......20200
      ELSE IF (ERRCOD.EQ.'INP-4-5') THEN                                 SUTERR.......20300
        DS(1)="Specified TRANSIENT flow with STEADY transport."          SUTERR.......20400
        EX(1)="In dataset 4 of the main input file, TRANSIENT flow"      SUTERR.......20500
        EX(2)="requires TRANSIENT transport.  Likewise, STEADY"          SUTERR.......20600
        EX(3)="transport requires STEADY flow.  The following are"       SUTERR.......20700
        EX(4)="valid combinations:"                                      SUTERR.......20800
        EX(5)=" "                                                        SUTERR.......20900
        EX(6)="     CSSFLO      CSSTRA"                                  SUTERR.......21000
        EX(7)="   ----------- -----------"                               SUTERR.......21100
        EX(8)="    'STEADY'    'STEADY'"                                 SUTERR.......21200
        EX(9)="    'STEADY'   'TRANSIENT'"                               SUTERR.......21300
        EX(10)="   'TRANSIENT' 'TRANSIENT'"                              SUTERR.......21400
        EX(11)=" "                                                       SUTERR.......21500
        EX(12)="Example of a valid dataset 4:"                           SUTERR.......21600
        EX(13)="'SATURATED FLOW' 'STEADY FLOW' 'STEADY TRANSPORT'" //    SUTERR.......21700
     1        " 'COLD' 10"                                               SUTERR.......21800
      ELSE IF (ERRCOD.EQ.'INP-7B&C-1') THEN                              SUTERR.......21900
        DS(1)="Unrecognized solver name."                                SUTERR.......22000
        EX(1)="In datasets 7B&C, valid solver selections are:"           SUTERR.......22100
        EX(2)=" "                                                        SUTERR.......22200
        DO 400 M=0,NSLVRS-1                                              SUTERR.......22300
           EX(M+3)=SOLWRD(M) // " --> " // SOLNAM(M)                     SUTERR.......22400
  400   CONTINUE                                                         SUTERR.......22500
        EX(NSLVRS+3)=" "                                                 SUTERR.......22600
        EX(NSLVRS+4)="Note that solver selections for P and U must be"   SUTERR.......22700
        EX(NSLVRS+5)="both DIRECT or both iterative."                    SUTERR.......22800
      ELSE IF (ERRCOD.EQ.'INP-7B&C-2') THEN                              SUTERR.......22900
        DS(1)="Solver selections for P and U are not both DIRECT or"     SUTERR.......23000
        DS(2)="both iterative."                                          SUTERR.......23100
        EX(1)="The solver selections for P and U must be both"           SUTERR.......23200
        EX(2)="DIRECT or both iterative."                                SUTERR.......23300
      ELSE IF (ERRCOD.EQ.'INP-7B&C-3') THEN                              SUTERR.......23400
        DS(1)="Invalid selection of the CG solver."                      SUTERR.......23500
        EX(1)="The CG solver may be used only for the flow (P) equation" SUTERR.......23600
        EX(2)="with no upstream weighting (UP=0.0).  It may not be used" SUTERR.......23700
        EX(3)="for the transport (U) equation."                          SUTERR.......23800
C.....ERROR CODE 'INP-7B&C-4' IS NO LONGER USED.                         SUTERR.......23900
      ELSE IF (ERRCOD.EQ.'INP-3,19-1') THEN                              SUTERR.......24000
        DS(1)="The actual number of specified pressure nodes, "          SUTERR.......24100
     1        // CINERR(1) // ","                                        SUTERR.......24200
        DS(2)="does not equal the input value,                "          SUTERR.......24300
     1        // CINERR(2) // "."                                        SUTERR.......24400
        EX(1)="In dataset 3 of the main input file, the variable NPBC"   SUTERR.......24500
        EX(2)="must specify the exact number of specified pressure"      SUTERR.......24600
        EX(3)="nodes listed in dataset 19."                              SUTERR.......24700
      ELSE IF (ERRCOD.EQ.'INP-3,20-1') THEN                              SUTERR.......24800
        DS(1)="The actual number of specified conc. nodes, "             SUTERR.......24900
     1        // CINERR(1) // ","                                        SUTERR.......25000
        DS(2)="does not equal the input value,             "             SUTERR.......25100
     1        // CINERR(2) // "."                                        SUTERR.......25200
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......25300
        EX(2)="must specify the exact number of specified concentration" SUTERR.......25400
        EX(3)="nodes listed in dataset 20."                              SUTERR.......25500
      ELSE IF (ERRCOD.EQ.'INP-3,20-2') THEN                              SUTERR.......25600
        DS(1)="The actual number of specified temp. nodes, "             SUTERR.......25700
     1        // CINERR(1) // ","                                        SUTERR.......25800
        DS(2)="does not equal the input value,             "             SUTERR.......25900
     1        // CINERR(2) // "."                                        SUTERR.......26000
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......26100
        EX(2)="must specify the exact number of specified temperature"   SUTERR.......26200
        EX(3)="nodes listed in dataset 20."                              SUTERR.......26300
      ELSE IF (ERRCOD.EQ.'INP-22-1') THEN                                SUTERR.......26400
        DS(1)="Line 1 of the element incidence data does not begin with" SUTERR.......26500
        DS(2)="the word 'INCIDENCE'."                                    SUTERR.......26600
        EX(1)="In dataset 22 of the main input file, the first line"     SUTERR.......26700
        EX(2)="must begin with the word 'INCIDENCE'."                    SUTERR.......26800
      ELSE IF (ERRCOD.EQ.'INP-22-2') THEN                                SUTERR.......26900
        DS(1)="The incidence data for element " // CINERR(1)             SUTERR.......27000
        DS(2)="are not in numerical order in the dataset."               SUTERR.......27100
        EX(1)="In dataset 22 of the main input file, incidence data"     SUTERR.......27200
        EX(2)="must be listed in order of increasing element number."    SUTERR.......27300
        EX(3)="Note that the numbering of elements must begin at 1"      SUTERR.......27400
        EX(4)="and be continuous; element numbers may not be skipped."   SUTERR.......27500
      ELSE IF (ERRCOD.EQ.'INP-14B,22-1') THEN                            SUTERR.......27600
        DS(1)="At least one element has incorrect geometry."             SUTERR.......27700
        EX(1)="Incorrect element geometry can result from improper"      SUTERR.......27800
        EX(2)="specification of node coordinates in dataset 14B of the"  SUTERR.......27900
        EX(3)="main input file, or from improper ordering of nodes in"   SUTERR.......28000
        EX(4)="a node incidence list in dataset 22 of the same file."    SUTERR.......28100
      ELSE IF (ERRCOD.EQ.'FIL-1') THEN                                   SUTERR.......28200
        DS(1)="The file " // CHERR(2)                                    SUTERR.......28300
        DS(2)="does not exist."                                          SUTERR.......28400
        EX(1)="One of the files required by SUTRA does not exist."       SUTERR.......28500
        EX(2)="Check the filename and the directory path."               SUTERR.......28600
      ELSE IF (ERRCOD.EQ.'FIL-2') THEN                                   SUTERR.......28700
        DS(1)="The file " // CHERR(2)                                    SUTERR.......28800
        DS(2)="could not be opened on FORTRAN unit " // CINERR(1) // "." SUTERR.......28900
        EX(1)="One of the files required by SUTRA could not be opened."  SUTERR.......29000
        EX(2)="Check to make sure the file is not protected or in use"   SUTERR.......29100
        EX(3)="by another application, and that the FORTRAN unit number" SUTERR.......29200
        EX(4)="is valid."                                                SUTERR.......29300
C.....ERROR CODE 'FIL-3' IS NO LONGER USED.                              SUTERR.......29400
      ELSE IF (ERRCOD.EQ.'FIL-4') THEN                                   SUTERR.......29500
        DS(1)="An attempt was made to use the file"                      SUTERR.......29600
        DS(2)=CHERR(2)                                                   SUTERR.......29700
        DS(3)="for more than one purpose simultaneously."                SUTERR.......29800
        EX(1)='Each filename listed in "SUTRA.FIL" must be unique'       SUTERR.......29900
        EX(2)='and may not be reused in an "@INSERT" statement.'         SUTERR.......30000
        EX(3)='Also, if you have nested "@INSERT" statements'            SUTERR.......30100
        EX(4)='(i.e., a file inserted into a file, which is itself'      SUTERR.......30200
        EX(5)='inserted into a file, etc.), a given file may be'         SUTERR.......30300
        EX(6)='used only once in the nested sequence.'                   SUTERR.......30400
      ELSE IF (ERRCOD.EQ.'FIL-5') THEN                                   SUTERR.......30500
        DS(1)="Invalid file type: " // CHERR(2)                          SUTERR.......30600
        EX(1)="Valid file types are:"                                    SUTERR.......30700
        EX(2)='   INP (".inp" input file)'                               SUTERR.......30800
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......30900
        EX(4)='   SMY (".smy" output file)'                              SUTERR.......31000
        EX(5)='   LST (".lst" output file)'                              SUTERR.......31100
        EX(6)='   RST (".rst" output file)'                              SUTERR.......31200
        EX(7)='   NOD (".nod" output file)'                              SUTERR.......31300
        EX(8)='   ELE (".ele" output file)'                              SUTERR.......31400
        EX(9)='   OBS (".obs" output file)'                              SUTERR.......31500
        EX(10)='   OBC (".obc" output file)'                             SUTERR.......31600
      ELSE IF (ERRCOD.EQ.'FIL-6') THEN                                   SUTERR.......31700
        DS(1)="File type " // CHERR(2)                                   SUTERR.......31800
        DS(2)="has been assigned more than once."                        SUTERR.......31900
        EX(1)="The following file types must be assigned:"               SUTERR.......32000
        EX(2)='   INP (".inp" input file)'                               SUTERR.......32100
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......32200
        EX(4)='   LST (".lst" output file)'                              SUTERR.......32300
        EX(5)='   RST (".rst" output file)'                              SUTERR.......32400
        EX(6)="The following file types are optional:"                   SUTERR.......32500
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......32600
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......32700
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......32800
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......32900
        EX(11)='   OBC (".obc" output file; defaults to "SUTRA.OBC")'    SUTERR.......33000
        EX(12)="No file type may be assigned more than once."            SUTERR.......33100
      ELSE IF (ERRCOD.EQ.'FIL-7') THEN                                   SUTERR.......33200
        DS(1)="Required file type " // CHERR(2)                          SUTERR.......33300
        DS(2)="has not been assigned."                                   SUTERR.......33400
        EX(1)="The following file types must be assigned:"               SUTERR.......33500
        EX(2)='   INP (".inp" input file)'                               SUTERR.......33600
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......33700
        EX(4)='   LST (".lst" output file)'                              SUTERR.......33800
        EX(5)='   RST (".rst" output file)'                              SUTERR.......33900
        EX(6)="The following file types are optional:"                   SUTERR.......34000
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......34100
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......34200
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......34300
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......34400
        EX(11)='   OBC (".obc" output file; defaults to "SUTRA.OBC")'    SUTERR.......34500
        EX(12)="No file type may be assigned more than once."            SUTERR.......34600
      ELSE IF (ERRCOD.EQ.'FIL-8') THEN                                   SUTERR.......34700
        DS(1)="The file " // CHERR(2)                                    SUTERR.......34800
        DS(2)="could not be inserted."                                   SUTERR.......34900
        EX(1)="Inserts cannot be nested more than 20 levels deep."       SUTERR.......35000
      ELSE IF (ERRCOD.EQ.'FIL-9') THEN                                   SUTERR.......35100
        DS(1)="A file listed in 'SUTRA.FIL' is named 'SUTRA.FIL'."       SUTERR.......35200
        EX(1)="The filename 'SUTRA.FIL' is reserved by SUTRA."           SUTERR.......35300
        EX(2)="Files listed in 'SUTRA.FIL' may not be named"             SUTERR.......35400
        EX(3)="'SUTRA.FIL'."                                             SUTERR.......35500
      ELSE IF (ERRCOD.EQ.'FIL-10') THEN                                  SUTERR.......35600
        DS(1)="SUTRA was unable to automatically"                        SUTERR.......35700
        DS(2)="assign unit number " // CINERR(1)                         SUTERR.......35800
        DS(3)="to file " // CHERR(2)                                     SUTERR.......35900
        EX(1)="SUTRA attempted to automatically assign to one of the "   SUTERR.......36000
        EX(2)="files listed in 'SUTRA.FIL' a unit number that is not"    SUTERR.......36100
        EX(3)="allowed on this computer.  Please check the unit"         SUTERR.......36200
        EX(4)="number assignments in 'SUTRA.FIL'.  It may be possible"   SUTERR.......36300
        EX(5)="to avoid this problem by explicitly assigning a"          SUTERR.......36400
        EX(6)="different unit number to the file in question or by"      SUTERR.......36500
        EX(7)="reducing the number of optional files listed in"          SUTERR.......36600
        EX(8)="'SUTRA.FIL'."                                             SUTERR.......36700
      ELSE IF (ERRCOD.EQ.'INP-6-1') THEN                                 SUTERR.......36800
        DS(1)="NPCYC<1 and/or NUCYC<1."                                  SUTERR.......36900
        EX(1)="In dataset 6 of the main input file, both NPCYC and"      SUTERR.......37000
        EX(2)="NUCYC must be set greater than or equal to 1."            SUTERR.......37100
      ELSE IF (ERRCOD.EQ.'INP-6-2') THEN                                 SUTERR.......37200
        DS(1)="Neither NPCYC nor NUCYC is set to 1."                     SUTERR.......37300
        EX(1)="In dataset 6 of the main input file, either NPCYC or"     SUTERR.......37400
        EX(2)="NUCYC (or both) must be set to 1."                        SUTERR.......37500
      ELSE IF (ERRCOD.EQ.'INP-6-3') THEN                                 SUTERR.......37600
        DS(1)="DELT is greater than DTMAX."                              SUTERR.......37700
        EX(1)="In dataset 6 of the main input file, DELT must be set"    SUTERR.......37800
        EX(2)="less than or equal to DTMAX."                             SUTERR.......37900
      ELSE IF (ERRCOD.EQ.'INP-6-4') THEN                                 SUTERR.......38000
        DS(1)="The actual number of schedules listed does not equal"     SUTERR.......38100
        DS(2)="the input value, or the schedule list does not end"       SUTERR.......38200
        DS(3)="with '-'."                                                SUTERR.......38300
        EX(1)="In dataset 6 of the main input file, the number of"       SUTERR.......38400
        EX(2)="schedules listed must equal the number, NSCH, specified"  SUTERR.......38500
        EX(3)="in dataset 6 of the same file, and the final entry in"    SUTERR.......38600
        EX(4)="the list must be '-'."                                    SUTERR.......38700
        EX(5)=" "                                                        SUTERR.......38800
        EX(6)="Example of a valid dataset 6 with two schedules:"         SUTERR.......38900
        EX(7)="2   1   1"                                                SUTERR.......39000
        EX(8)="'TIME_STEPS' 'TIME CYCLE' 'ELAPSED'  1. 100   0. " //     SUTERR.......39100
     1     "3.e+9 3.e+7 999 1. 0. 1.e+99"                                SUTERR.......39200
        EX(9)="'SCHED_A'    'STEP LIST'        4   20  40  60  80"       SUTERR.......39300
        EX(10)="'-'"                                                     SUTERR.......39400
      ELSE IF (ERRCOD.EQ.'INP-6-5') THEN                                 SUTERR.......39500
        DS(1)="Multiple definitions of schedule " // CHERR(1)            SUTERR.......39600
        EX(1)="A given schedule name may not be defined more than once"  SUTERR.......39700
        EX(2)="in dataset 6 of the main input file."                     SUTERR.......39800
      ELSE IF (ERRCOD.EQ.'INP-6-6') THEN                                 SUTERR.......39900
        DS(1)="Invalid time descriptor " // CHERR(1)                     SUTERR.......40000
        EX(1)="Time-based schedules must be defined in terms of either"  SUTERR.......40100
        EX(2)="ABSOLUTE or ELAPSED times."                               SUTERR.......40200
      ELSE IF (ERRCOD.EQ.'INP-6-7') THEN                                 SUTERR.......40300
        DS(1)="ELAPSED times in TIME_STEPS schedule,"                    SUTERR.......40400
        DS(2)="but initial elapsed time is not zero."                    SUTERR.......40500
        EX(1)="When the TIME_STEPS schedule is defined in terms of"      SUTERR.......40600
        EX(2)="ELAPSED times, the first (initial) elapsed time in the"   SUTERR.......40700
        EX(3)="schedule must be set to zero."                            SUTERR.......40800
      ELSE IF (ERRCOD.EQ.'INP-6-8') THEN                                 SUTERR.......40900
        DS(1)="Invalid number of schedules (NSCH<0)."                    SUTERR.......41000
        EX(1)="The number of schedules, NSCH, must be non-negative."     SUTERR.......41100
        EX(2)="NSCH=0 is allowed only if flow and transport are both"    SUTERR.......41200
        EX(3)="steady-state."                                            SUTERR.......41300
      ELSE IF (ERRCOD.EQ.'INP-6-9') THEN                                 SUTERR.......41400
        DS(1)="Invalid schedule type " // CHERR(1)                       SUTERR.......41500
        EX(1)="An invalid schedule type has been specified."             SUTERR.......41600
        EX(2)="Valid schedule types are:"                                SUTERR.......41700
        EX(3)="   'TIME CYCLE'"                                          SUTERR.......41800
        EX(4)="   'TIME LIST'"                                           SUTERR.......41900
        EX(5)="   'STEP CYCLE'"                                          SUTERR.......42000
        EX(6)="   'STEP LIST'"                                           SUTERR.......42100
      ELSE IF (ERRCOD.EQ.'INP-6-10') THEN                                SUTERR.......42200
        DS(1)="Incomplete TIME_STEPS schedule."                          SUTERR.......42300
        EX(1)="The TIME_STEPS schedule must contain at least two"        SUTERR.......42400
        EX(2)="distinct times, including the starting time."             SUTERR.......42500
C.....ERROR CODE INP-6-11 HAS NEVER BEEN USED IN AN OFFICIAL RELEASE OF  SUTERR.......42600
C        SUTRA AND IS THEREFORE STILL AVAILABLE.                         SUTERR.......42700
      ELSE IF (ERRCOD.EQ.'INP-6-12') THEN                                SUTERR.......42800
        DS(1)="Repeated " // TRIM(CHERR(1)) // " " // CRLERR(1)          SUTERR.......42900
        DS(2)="in schedule " // CHERR(2)                                 SUTERR.......43000
        EX(1)="A time or time step value may not appear more than once"  SUTERR.......43100
        EX(2)="in a given schedule."                                     SUTERR.......43200
      ELSE IF (ERRCOD.EQ.'INP-6-13') THEN                                SUTERR.......43300
        DS(1)="Invalid number of schedules (NSCH=0)."                    SUTERR.......43400
        EX(1)="NSCH=0 is allowed only if flow and transport are both"    SUTERR.......43500
        EX(2)="steady-state."                                            SUTERR.......43600
      ELSE IF (ERRCOD.EQ.'INP-6-14') THEN                                SUTERR.......43700
        DS(1)="Missing TIME_STEPS schedule."                             SUTERR.......43800
        EX(1)="When transport is transient, a TIME_STEPS schedule must"  SUTERR.......43900
        EX(2)="be defined by the user in dataset 6."                     SUTERR.......44000
      ELSE IF ((ERRCOD.EQ.'INP-8A-1').OR.(ERRCOD.EQ.'INP-8A-2')          SUTERR.......44100
     1     .OR.(ERRCOD.EQ.'INP-8A-3').OR.(ERRCOD.EQ.'INP-8A-4')          SUTERR.......44200
     1     .OR.(ERRCOD.EQ.'INP-8A-5').OR.(ERRCOD.EQ.'INP-8A-6')          SUTERR.......44300
     1     .OR.(ERRCOD.EQ.'INP-8A-7')) THEN                              SUTERR.......44400
        DS(1)=CHERR(1)(1:6) // " is not 'Y' or 'N'."                     SUTERR.......44500
        EX(1)="In dataset 8A of the main input file, " // CHERR(1)(1:6)  SUTERR.......44600
        EX(2)="must be set to either 'Y' or 'N'."                        SUTERR.......44700
        EX(3)=" "                                                        SUTERR.......44800
        EX(4)="Example of a valid dataset 8A:"                           SUTERR.......44900
        EX(5)="10   'N'   'N'   'N'   'Y'   'Y'   'Y'   'Y'"             SUTERR.......45000
      ELSE IF (ERRCOD.EQ.'INP-8B-1') THEN                                SUTERR.......45100
        DS(1)="Node number listed in column other than column 1."        SUTERR.......45200
        EX(1)="In dataset 8B of the main input file, if the node number" SUTERR.......45300
        EX(2)="is to appear, it must appear only in column 1, i.e.,"     SUTERR.......45400
        EX(3)="only NCOL(1) can be set to 'N'."                          SUTERR.......45500
      ELSE IF (ERRCOD.EQ.'INP-8B-2') THEN                                SUTERR.......45600
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......45700
        EX(1)="In dataset 8B of the main input file, 'Z' can be listed"  SUTERR.......45800
        EX(2)="only if the problem is 3D."                               SUTERR.......45900
      ELSE IF (ERRCOD.EQ.'INP-8B-3') THEN                                SUTERR.......46000
        DS(1)="Unrecognized value for NCOL."                             SUTERR.......46100
        EX(1)="In dataset 8B of the main input file, the following"      SUTERR.......46200
        EX(2)="variables may be listed:"                                 SUTERR.......46300
        EX(3)=" "                                                        SUTERR.......46400
        EX(4)="'N'  =  node number (if used, it must appear first)"      SUTERR.......46500
        EX(5)="'X'  =  X-coordinate of node"                             SUTERR.......46600
        EX(6)="'Y'  =  Y-coordinate of node"                             SUTERR.......46700
        EX(7)="'Z'  =  Z-coordinate of node (3D only)"                   SUTERR.......46800
        EX(8)="'P'  =  pressure"                                         SUTERR.......46900
        EX(9)="'U'  =  concentration or temperature"                     SUTERR.......47000
        EX(10)="'S'  =  saturation"                                      SUTERR.......47100
        EX(11)=" "                                                       SUTERR.......47200
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......47300
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......47400
        EX(14)=" "                                                       SUTERR.......47500
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......47600
        EX(16)="10  'N'  'X'  'Y'  'Z'  'S'  'U'  '-'"                   SUTERR.......47700
      ELSE IF (ERRCOD.EQ.'INP-8C-1') THEN                                SUTERR.......47800
        DS(1)="Element number listed in column other than column 1."     SUTERR.......47900
        EX(1)="In dataset 8C of the main input file, if the element"     SUTERR.......48000
        EX(2)="number is to appear, it must appear only in column 1,"    SUTERR.......48100
        EX(3)="i.e., only LCOL(1) can be set to 'E'."                    SUTERR.......48200
      ELSE IF (ERRCOD.EQ.'INP-8C-2') THEN                                SUTERR.......48300
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......48400
        EX(1)="In dataset 8C of the main input file, 'Z' can be listed"  SUTERR.......48500
        EX(2)="only if the problem is 3D."                               SUTERR.......48600
      ELSE IF (ERRCOD.EQ.'INP-8C-3') THEN                                SUTERR.......48700
        DS(1)="Unrecognized value for LCOL."                             SUTERR.......48800
        EX(1)="In dataset 8C of the main input file, the following"      SUTERR.......48900
        EX(2)="variables may be listed:"                                 SUTERR.......49000
        EX(3)=" "                                                        SUTERR.......49100
        EX(4)="'E'  =  element number (if used, it must appear first)"   SUTERR.......49200
        EX(5)="'X'  =  X-coordinate of element centroid"                 SUTERR.......49300
        EX(6)="'Y'  =  Y-coordinate of element centroid"                 SUTERR.......49400
        EX(7)="'Z'  =  Z-coordinate of element centroid (3D only)"       SUTERR.......49500
        EX(8)="'VX'  =  X-component of fluid velocity"                   SUTERR.......49600
        EX(9)="'VY'  =  Y-component of fluid velocity"                   SUTERR.......49700
        EX(10)="'VZ'  =  Z-component of fluid velocity (3D only)"        SUTERR.......49800
        EX(11)=" "                                                       SUTERR.......49900
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......50000
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......50100
        EX(14)=" "                                                       SUTERR.......50200
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......50300
        EX(16)="10  'E'  'X'  'Y'  'Z'  'VX'  'VY'  'VZ'  '-'"           SUTERR.......50400
      ELSE IF (ERRCOD.EQ.'INP-8C-4') THEN                                SUTERR.......50500
        DS(1)="Specified that 'VZ' be output for a 2D problem."          SUTERR.......50600
        EX(1)="In dataset 8C of the main input file, 'VZ' can be listed" SUTERR.......50700
        EX(2)="only if the problem is 3D."                               SUTERR.......50800
      ELSE IF (ERRCOD.EQ.'INP-8D-1') THEN                                SUTERR.......50900
        DS(1)="The actual number of observation points listed does not"  SUTERR.......51000
        DS(2)="equal the input value, or the observation point list"     SUTERR.......51100
        DS(3)="does not end with a zero."                                SUTERR.......51200
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......51300
        EX(2)="points listed must equal the number, NOBS, specified in"  SUTERR.......51400
        EX(3)="dataset 3 of the same file, and a zero must appear after" SUTERR.......51500
        EX(4)="the last point in the list when the old format is used."  SUTERR.......51600
        EX(5)="Any information appearing after the zero is ignored."     SUTERR.......51700
        EX(6)=" "                                                        SUTERR.......51800
        EX(7)="Example of a valid old-format dataset 8D with three"      SUTERR.......51900
        EX(8)="observation points (nodes 45, 46, and 7347),"             SUTERR.......52000
        EX(9)="assuming NN>=7347:"                                       SUTERR.......52100
        EX(10)="10   45   46   7347   0"                                 SUTERR.......52200
      ELSE IF (ERRCOD.EQ.'INP-8D-2') THEN                                SUTERR.......52300
        DS(1)="The observation node list contains an invalid node"       SUTERR.......52400
        DS(2)="number."                                                  SUTERR.......52500
        EX(1)="In dataset 8D of the main input file, all node numbers"   SUTERR.......52600
        EX(2)="must be greater than or equal to 1, and less than or"     SUTERR.......52700
        EX(3)="equal to NN, the total number of nodes.  The last entry"  SUTERR.......52800
        EX(4)="must be a zero, which signals the end of the list."       SUTERR.......52900
        EX(5)=" "                                                        SUTERR.......53000
        EX(6)="Example of a valid old-format dataset 8D with three"      SUTERR.......53100
        EX(7)="observation nodes (45, 46, and 7347),"                    SUTERR.......53200
        EX(8)="assuming NN>=7347:"                                       SUTERR.......53300
        EX(9)="10   45   46   7347   0"                                  SUTERR.......53400
      ELSE IF (ERRCOD.EQ.'INP-8D-3') THEN                                SUTERR.......53500
        DS(1)="Element not found for the following observation point:"   SUTERR.......53600
        DS(2)="   " // CHERR(1)                                          SUTERR.......53700
        DS(3)="   " // CHERR(2)                                          SUTERR.......53800
        EX(1)="SUTRA was unable to find an element that contains"        SUTERR.......53900
        EX(2)="the observation point named above.  Please check"         SUTERR.......54000
        EX(3)="to make sure the coordinates specified for that"          SUTERR.......54100
        EX(4)="observation point are within the model domain."           SUTERR.......54200
      ELSE IF (ERRCOD.EQ.'INP-8D-4') THEN                                SUTERR.......54300
        DS(1)="The actual number of observation points listed does not"  SUTERR.......54400
        DS(2)="equal the input value, or the observation point list"     SUTERR.......54500
        DS(3)="does not end with '-'."                                   SUTERR.......54600
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......54700
        EX(2)="points listed must equal the number, NOBS, specified in"  SUTERR.......54800
        EX(3)="dataset 3 of the same file, and the final entry in the"   SUTERR.......54900
        EX(4)="list must be '-'."                                        SUTERR.......55000
        EX(5)=" "                                                        SUTERR.......55100
        EX(6)="Example of a valid dataset 8D with two 3D observation"    SUTERR.......55200
        EX(7)="points, assuming schedules A and B have been defined:"    SUTERR.......55300
        EX(8)="'POINT_1'     0.   100.   500.   'A'   'OBS'"             SUTERR.......55400
        EX(9)="'POINT_2'   100.   200.   800.   'B'   'OBC'"             SUTERR.......55500
        EX(10)="'-'"                                                     SUTERR.......55600
      ELSE IF (ERRCOD.EQ.'INP-8D-5') THEN                                SUTERR.......55700
        DS(1)="Undefined schedule " // CHERR(1)                          SUTERR.......55800
        DS(2)="specified for observation " // CHERR(2)                   SUTERR.......55900
        EX(1)="The output schedule specified for one of the"             SUTERR.......56000
        EX(2)="observation points has not been defined in dataset 6"     SUTERR.......56100
        EX(3)="of the main input file."                                  SUTERR.......56200
      ELSE IF (ERRCOD.EQ.'INP-11-1') THEN                                SUTERR.......56300
        DS(1)="Unrecognized sorption model."                             SUTERR.......56400
        EX(1)="In dataset 11 of the main input file, the sorption model" SUTERR.......56500
        EX(2)="may be chosen from the following:"                        SUTERR.......56600
        EX(3)=" "                                                        SUTERR.......56700
        EX(4)="'NONE'       =  No sorption"                              SUTERR.......56800
        EX(5)="'LINEAR'     =  Linear sorption model"                    SUTERR.......56900
        EX(6)="'FREUNDLICH' =  Freundlich sorption model"                SUTERR.......57000
        EX(7)="'LANGMUIR'   =  Langmuir sorption model"                  SUTERR.......57100
      ELSE IF (ERRCOD.EQ.'INP-11-2') THEN                                SUTERR.......57200
        DS(1)="The second Freundlich sorption coefficient is less than"  SUTERR.......57300
        DS(2)="or equal to zero."                                        SUTERR.......57400
        EX(1)="In dataset 11 of the main input file, the second"         SUTERR.......57500
        EX(2)="coefficient, CHI2, must be positive if Freundlich"        SUTERR.......57600
        EX(3)="sorption is chosen."                                      SUTERR.......57700
      ELSE IF (ERRCOD.EQ.'INP-14A-1') THEN                               SUTERR.......57800
        DS(1)="Dataset 14A does not begin with the word 'NODE'."         SUTERR.......57900
        EX(1)="Dataset 14A of the main input file must begin with the"   SUTERR.......58000
        EX(2)="word 'NODE'."                                             SUTERR.......58100
        EX(3)=" "                                                        SUTERR.......58200
        EX(4)="Example of a valid dataset 14A:"                          SUTERR.......58300
        EX(5)="'NODE'  1000.  1000.  1.  0.1"                            SUTERR.......58400
      ELSE IF (ERRCOD.EQ.'INP-15A-1') THEN                               SUTERR.......58500
        DS(1)="Dataset 15A does not begin with the word 'ELEMENT'."      SUTERR.......58600
        EX(1)="Dataset 15A of the main input file must begin with the"   SUTERR.......58700
        EX(2)="word 'ELEMENT'."                                          SUTERR.......58800
        EX(3)=" "                                                        SUTERR.......58900
        EX(4)="Example of a valid dataset 15A for a " // CHERR(1)(1:2)   SUTERR.......59000
     1         // " problem:"                                            SUTERR.......59100
        IF (CHERR(1).EQ."3D") THEN                                       SUTERR.......59200
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1." SUTERR.......59300
        ELSE                                                             SUTERR.......59400
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1."                         SUTERR.......59500
        END IF                                                           SUTERR.......59600
      ELSE IF (ERRCOD.EQ.'ICS-2-1') THEN                                 SUTERR.......59700
        DS(1)="Unrecognized initialization type."                        SUTERR.......59800
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......59900
        EX(2)="the valid types of initializations for P are UNIFORM"     SUTERR.......60000
        EX(3)="and NONUNIFORM."                                          SUTERR.......60100
      ELSE IF (ERRCOD.EQ.'ICS-2-2') THEN                                 SUTERR.......60200
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......60300
        DS(2)="start."                                                   SUTERR.......60400
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......60500
        EX(2)="initial values for P must be specified as NONUNIFORM"     SUTERR.......60600
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......60700
        EX(4)="of the main input file)."                                 SUTERR.......60800
      ELSE IF (ERRCOD.EQ.'ICS-3-1') THEN                                 SUTERR.......60900
        DS(1)="Unrecognized initialization type."                        SUTERR.......61000
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......61100
        EX(2)="the valid types of initializations for U are UNIFORM"     SUTERR.......61200
        EX(3)="and NONUNIFORM."                                          SUTERR.......61300
      ELSE IF (ERRCOD.EQ.'ICS-3-2') THEN                                 SUTERR.......61400
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......61500
        DS(2)="start."                                                   SUTERR.......61600
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......61700
        EX(2)="initial values for U must be specified as NONUNIFORM"     SUTERR.......61800
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......61900
        EX(4)="of the main input file)."                                 SUTERR.......62000
      ELSE IF (ERRCOD.EQ.'SOL-1') THEN                                   SUTERR.......62100
        DS(1)="Error returned by the " // CHERR(2)(1:10)                 SUTERR.......62200
        DS(2)="solver while solving for " // CHERR(1)(1:1) // "."        SUTERR.......62300
        EX(1)="The iterative solver has stopped because of an error."    SUTERR.......62400
        EX(2)="Error flag values are interpreted as follows:"            SUTERR.......62500
        EX(3)="  "                                                       SUTERR.......62600
        EX(4)="IERR = 2  =>  Method stalled or failed to converge in"    SUTERR.......62700
        EX(5)="              the maximum number of iterations allowed."  SUTERR.......62800
        EX(6)="IERR = 4  =>  Convergence tolerance set too tight for"    SUTERR.......62900
        EX(7)="              machine precision."                         SUTERR.......63000
        EX(8)="IERR = 5  =>  Method broke down because preconditioning"  SUTERR.......63100
        EX(9)="              matrix is non-positive-definite."           SUTERR.......63200
        EX(10)="IERR = 6  =>  Method broke down because matrix is non-"  SUTERR.......63300
        EX(11)="              positive-definite or nearly so."           SUTERR.......63400
        EX(12)=" "                                                       SUTERR.......63500
        EX(13)="If the P-solution resulted in a solver error, an"        SUTERR.......63600
        EX(14)="attempt was still made to obtain a U-solution."          SUTERR.......63700
        EX(15)="The last P and U solutions were written to the"          SUTERR.......63800
        EX(16)="appropriate output files (except the restart file)"      SUTERR.......63900
        EX(17)="whether or not they resulted in solver errors."          SUTERR.......64000
      ELSE IF (ERRCOD.EQ.'INP-3,17-1') THEN                              SUTERR.......64100
        DS(1)="The actual number of"                                     SUTERR.......64200
        DS(2)="specified fluid source nodes,   " // CINERR(1) // ","     SUTERR.......64300
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......64400
        EX(1)="In dataset 3 of the main input file, the variable NSOP"   SUTERR.......64500
        EX(2)="must specify the exact number of specified fluid source"  SUTERR.......64600
        EX(3)="nodes listed in dataset 17."                              SUTERR.......64700
      ELSE IF (ERRCOD.EQ.'INP-3,18-1') THEN                              SUTERR.......64800
        DS(1)="The actual number of"                                     SUTERR.......64900
        DS(2)="specified " // CHERR(1)(1:6) // " source nodes,  "        SUTERR.......65000
     1         // CINERR(1) // ","                                       SUTERR.......65100
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......65200
        EX(1)="In dataset 3 of the main input file, the variable NSOU"   SUTERR.......65300
        EX(2)="must specify the exact number of specified "              SUTERR.......65400
     1         // CHERR(1)(1:6) // " source"                             SUTERR.......65500
        EX(3)="nodes listed in dataset 18."                              SUTERR.......65600
      ELSE IF (ERRCOD.EQ.'INP-17-1') THEN                                SUTERR.......65700
        DS(1)="Invalid node number referenced in dataset 17: "           SUTERR.......65800
     1         // CINERR(1)                                              SUTERR.......65900
        EX(1)="Dataset 17 of the main input file contains a reference"   SUTERR.......66000
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......66100
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......66200
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......66300
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......66400
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......66500
      ELSE IF (ERRCOD.EQ.'INP-18-1') THEN                                SUTERR.......66600
        DS(1)="Invalid node number referenced in dataset 18: "           SUTERR.......66700
     1         // CINERR(1)                                              SUTERR.......66800
        EX(1)="Dataset 18 of the main input file contains a reference"   SUTERR.......66900
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......67000
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......67100
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......67200
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......67300
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......67400
      ELSE IF (ERRCOD.EQ.'INP-19-1') THEN                                SUTERR.......67500
        DS(1)="Invalid node number referenced in dataset 19: "           SUTERR.......67600
     1         // CINERR(1)                                              SUTERR.......67700
        EX(1)="Dataset 19 of the main input file contains a reference"   SUTERR.......67800
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......67900
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......68000
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......68100
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......68200
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......68300
      ELSE IF (ERRCOD.EQ.'INP-20-1') THEN                                SUTERR.......68400
        DS(1)="Invalid node number referenced in dataset 20: "           SUTERR.......68500
     1         // CINERR(1)                                              SUTERR.......68600
        EX(1)="Dataset 20 of the main input file contains a reference"   SUTERR.......68700
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......68800
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......68900
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......69000
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......69100
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......69200
      ELSE IF (ERRCOD.EQ.'CON-1') THEN                                   SUTERR.......69300
        CDUM80 = 's'                                                     SUTERR.......69400
        IF (INERR(4).GT.13) THEN                                         SUTERR.......69500
           LDUM = 1                                                      SUTERR.......69600
        ELSE                                                             SUTERR.......69700
           LDUM = 0                                                      SUTERR.......69800
        END IF                                                           SUTERR.......69900
        DS(1)="Simulation terminated due to unconverged non-linearity"   SUTERR.......70000
        DS(2)="iterations.  Tolerance" // CDUM80(1:LDUM)                 SUTERR.......70100
     1         // " for " // CHERR(1)(1:INERR(4))                        SUTERR.......70200
        DS(3)="not reached."                                             SUTERR.......70300
        EX(1)="The " // CHERR(1)(1:INERR(4)) // " solution"              SUTERR.......70400
     1         // CDUM80(1:LDUM) // " failed"                            SUTERR.......70500
        EX(2)="to converge to the specified tolerance"                   SUTERR.......70600
     1         // CDUM80(1:LDUM) // " within"                            SUTERR.......70700
        EX(3)="the maximum number of iterations allowed to resolve"      SUTERR.......70800
        EX(4)="non-linearities.  The parameters that control these"      SUTERR.......70900
        EX(5)="iterations are set in dataset 7A of the main input file." SUTERR.......71000
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......71100
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS'))) THEN          SUTERR.......71200
        IF (CODE(2).EQ.'INP') THEN                                       SUTERR.......71300
           CDUM80 = 'main input'                                         SUTERR.......71400
           LDUM = 10                                                     SUTERR.......71500
        ELSE                                                             SUTERR.......71600
           CDUM80 = 'initial conditions'                                 SUTERR.......71700
           LDUM = 18                                                     SUTERR.......71800
        END IF                                                           SUTERR.......71900
        IF ((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')) THEN                SUTERR.......72000
          DS(1)="FORTRAN returned an error while reading the restart"    SUTERR.......72100
          DS(2)="information following dataset 3 of the initial"         SUTERR.......72200
          DS(3)="conditions."                                            SUTERR.......72300
        ELSE IF (CODE(3).EQ.'INS') THEN                                  SUTERR.......72400
          CALL PRSWDS(CHERR(1), '-', 3, CODUM, NWORDS)                   SUTERR.......72500
          DS(1)="FORTRAN returned an error while reading an '@INSERT'"   SUTERR.......72600
          DS(2)="statement in the vicinity of dataset " // CODUM(3)(1:3) SUTERR.......72700
          DS(3)="of the " // CDUM80(1:LDUM) // "."                       SUTERR.......72800
        ELSE                                                             SUTERR.......72900
          DS(1)="FORTRAN returned an error while reading"                SUTERR.......73000
          DS(2)="dataset " // CODE(3)(1:3)                               SUTERR.......73100
     1           // " of the " // CDUM80(1:LDUM) // "."                  SUTERR.......73200
        END IF                                                           SUTERR.......73300
        EX(1)="A FORTRAN error has occurred while reading input data."   SUTERR.......73400
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......73500
        EX(3)=" "                                                        SUTERR.......73600
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......73700
        EX(5)="                all the required data were read from"     SUTERR.......73800
        EX(6)="                that line.  Check the specified dataset"  SUTERR.......73900
        EX(7)="                for missing data or lines of data that"   SUTERR.......74000
        EX(8)="                exceed 1000 characters."                  SUTERR.......74100
        EX(9)="IOSTAT > 0  =>  An error occurred while the specified"    SUTERR.......74200
        EX(10)="                dataset was being read.  Usually, this"  SUTERR.......74300
        EX(11)="                indicates that the READ statement"       SUTERR.......74400
        EX(12)="                encountered data of a type that is"      SUTERR.......74500
        EX(13)="                incompatible with the type it expected." SUTERR.......74600
        EX(14)="                Check the dataset for typographical"     SUTERR.......74700
        EX(15)="                errors and missing or extraneous data."  SUTERR.......74800
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......74900
        DS(1)='FORTRAN returned an error while reading "SUTRA.FIL".'     SUTERR.......75000
        EX(1)='A FORTRAN error has occurred while reading "SUTRA.FIL".'  SUTERR.......75100
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......75200
        EX(3)=" "                                                        SUTERR.......75300
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......75400
        EX(5)="                all the required data were read from"     SUTERR.......75500
        EX(6)='                that line.  Check "SUTRA.FIL" for'        SUTERR.......75600
        EX(7)="                missing data."                            SUTERR.......75700
        EX(8)="IOSTAT > 0  =>  An error occurred while the input"        SUTERR.......75800
        EX(9)="                file was being read.  Usually, this"      SUTERR.......75900
        EX(10)="                indicates that the READ statement"       SUTERR.......76000
        EX(11)="                encountered data of a type that is"      SUTERR.......76100
        EX(12)="                incompatible with the type it expected." SUTERR.......76200
        EX(13)='                Check "SUTRA.FIL" for typographical'     SUTERR.......76300
        EX(14)="                errors and missing or extraneous data."  SUTERR.......76400
      END IF                                                             SUTERR.......76500
C                                                                        SUTERR.......76600
C.....WRITE ERROR MESSAGE.  FORMAT DEPENDS ON THE TYPE OF ERROR.         SUTERR.......76700
      IF ((CODE(1).EQ.'INP').OR.(CODE(1).EQ.'ICS')) THEN                 SUTERR.......76800
C........ERROR TYPES 'INP' AND 'ICS' (INPUT DATA ERROR)                  SUTERR.......76900
         IF (KSCRN.EQ.1)                                                 SUTERR.......77000
     1      WRITE (*,1888) '           INPUT DATA ERROR           '      SUTERR.......77100
         WRITE (K00,1888) '           INPUT DATA ERROR           '       SUTERR.......77200
         IF (KSCRN.EQ.1) WRITE (*,1011)                                  SUTERR.......77300
         WRITE (K00,1011)                                                SUTERR.......77400
 1011    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......77500
         IF (CODE(1).EQ.'INP') THEN                                      SUTERR.......77600
            CDUM80 = FNAME(1)                                            SUTERR.......77700
         ELSE                                                            SUTERR.......77800
            CDUM80 = FNAME(2)                                            SUTERR.......77900
         END IF                                                          SUTERR.......78000
         IF (KSCRN.EQ.1) WRITE (*,1013) ERRCOD, CDUM80, CODE(2)          SUTERR.......78100
         WRITE (K00,1013) ERRCOD, CDUM80, CODE(2)                        SUTERR.......78200
 1013    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......78300
     1           /4X,'File:      ',2X,A40                                SUTERR.......78400
     1           /4X,'Dataset(s):',2X,A40/)                              SUTERR.......78500
         DO 1015 I=1,50                                                  SUTERR.......78600
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......78700
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......78800
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......78900
 1015    CONTINUE                                                        SUTERR.......79000
         IF (KSCRN.EQ.1) WRITE (*,1021)                                  SUTERR.......79100
         WRITE (K00,1021)                                                SUTERR.......79200
 1021    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......79300
         DO 1025 I=1,50                                                  SUTERR.......79400
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......79500
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......79600
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......79700
 1025    CONTINUE                                                        SUTERR.......79800
         IF (KSCRN.EQ.1) WRITE (*,1081)                                  SUTERR.......79900
         WRITE (K00,1081)                                                SUTERR.......80000
 1081    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR.......80100
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR.......80200
     1     /4X,'appears to be correct, check the preceding lines'        SUTERR.......80300
     1     /4X,'for missing data or extraneous characters.')             SUTERR.......80400
      ELSE IF (CODE(1).EQ.'FIL') THEN                                    SUTERR.......80500
C........ERROR TYPE 'FIL' (FILE ERROR)                                   SUTERR.......80600
         IF (KSCRN.EQ.1)                                                 SUTERR.......80700
     1      WRITE (*,1888)'              FILE ERROR              '       SUTERR.......80800
         WRITE (K00,1888) '              FILE ERROR              '       SUTERR.......80900
         IF (KSCRN.EQ.1) WRITE (*,1211)                                  SUTERR.......81000
         WRITE (K00,1211)                                                SUTERR.......81100
 1211    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......81200
         IF (KSCRN.EQ.1) WRITE (*,1213) ERRCOD, CHERR(1)                 SUTERR.......81300
         WRITE (K00,1213) ERRCOD, CHERR(1)                               SUTERR.......81400
 1213    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......81500
     1           /4X,'File:      ',2X,A40/)                              SUTERR.......81600
         DO 1215 I=1,50                                                  SUTERR.......81700
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......81800
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......81900
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......82000
 1215    CONTINUE                                                        SUTERR.......82100
         IF (KSCRN.EQ.1) WRITE (*,1221)                                  SUTERR.......82200
         WRITE (K00,1221)                                                SUTERR.......82300
 1221    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......82400
         DO 1225 I=1,50                                                  SUTERR.......82500
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......82600
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......82700
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......82800
 1225    CONTINUE                                                        SUTERR.......82900
      ELSE IF (CODE(1).EQ.'SOL') THEN                                    SUTERR.......83000
C........ERROR TYPE 'SOL' (MATRIX SOLVER ERROR)                          SUTERR.......83100
         IF (KSCRN.EQ.1)                                                 SUTERR.......83200
     1      WRITE (*,1888) '         MATRIX SOLVER ERROR          '      SUTERR.......83300
         WRITE (K00,1888) '         MATRIX SOLVER ERROR          '       SUTERR.......83400
         IF (KSCRN.EQ.1) WRITE (*,1311)                                  SUTERR.......83500
         WRITE (K00,1311)                                                SUTERR.......83600
 1311    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......83700
         IF (KSCRN.EQ.1) WRITE (*,1313) ERRCOD, CHERR(2),                SUTERR.......83800
     1      INERR(1), INERR(2), RLERR(1), RLERR(2)                       SUTERR.......83900
         WRITE (K00,1313) ERRCOD, CHERR(2), INERR(1), INERR(2),          SUTERR.......84000
     1      RLERR(1), RLERR(2)                                           SUTERR.......84100
 1313    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......84200
     1           /4X,'Solver:    ',2X,A40                                SUTERR.......84300
     1          //4X,'Error flag..........IERR = ',I3                    SUTERR.......84400
     1           /4X,'# of solver iters...ITRS = ',I5                    SUTERR.......84500
     1           /4X,'Error estimate.......ERR = ',1PE8.1                SUTERR.......84600
     1           /4X,'Error tolerance......TOL = ',1PE8.1/)              SUTERR.......84700
         DO 1315 I=1,50                                                  SUTERR.......84800
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......84900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......85000
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......85100
 1315    CONTINUE                                                        SUTERR.......85200
         IF (KSCRN.EQ.1) WRITE (*,1321)                                  SUTERR.......85300
         WRITE (K00,1321)                                                SUTERR.......85400
 1321    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......85500
         DO 1325 I=1,50                                                  SUTERR.......85600
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......85700
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......85800
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......85900
 1325    CONTINUE                                                        SUTERR.......86000
      ELSE IF (CODE(1).EQ.'CON') THEN                                    SUTERR.......86100
C........ERROR TYPE 'CON' (CONVERGENCE ERROR)                            SUTERR.......86200
         IF (KSCRN.EQ.1)                                                 SUTERR.......86300
     1      WRITE (*,1888) '          CONVERGENCE ERROR           '      SUTERR.......86400
         WRITE (K00,1888) '         CONVERGENCE ERROR          '         SUTERR.......86500
         IF (KSCRN.EQ.1) WRITE (*,1411)                                  SUTERR.......86600
         WRITE (K00,1411)                                                SUTERR.......86700
 1411    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......86800
         IF (KSCRN.EQ.1) WRITE (*,1413) ERRCOD, CHERR(1), INERR(3),      SUTERR.......86900
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR.......87000
         WRITE (K00,1413) ERRCOD, CHERR(1), INERR(3),                    SUTERR.......87100
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR.......87200
 1413    FORMAT (/4X,'Error code: ',2X,A40                               SUTERR.......87300
     1           /4X,'Unconverged:',2X,A40                               SUTERR.......87400
     1      //4X,'# of iterations.....ITER = ',I5                        SUTERR.......87500
     1       /4X,'Maximum P change.....RPM = ',1PE14.5,' (node ',I9,')'  SUTERR.......87600
     1       /4X,'Tolerance for P....RPMAX = ',1PE14.5                   SUTERR.......87700
     1       /4X,'Maximum U change.....RUM = ',1PE14.5,' (node ',I9,')'  SUTERR.......87800
     1       /4X,'Tolerance for U....RUMAX = ',1PE14.5/)                 SUTERR.......87900
         DO 1415 I=1,50                                                  SUTERR.......88000
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......88100
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......88200
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......88300
 1415    CONTINUE                                                        SUTERR.......88400
         IF (KSCRN.EQ.1) WRITE (*,1421)                                  SUTERR.......88500
         WRITE (K00,1421)                                                SUTERR.......88600
 1421    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......88700
         DO 1425 I=1,50                                                  SUTERR.......88800
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......88900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......89000
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......89100
 1425    CONTINUE                                                        SUTERR.......89200
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......89300
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS'))) THEN          SUTERR.......89400
C........ERROR TYPE 'REA-INP' OR 'REA-ICS' (FORTRAN READ ERROR)          SUTERR.......89500
         IF (KSCRN.EQ.1)                                                 SUTERR.......89600
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR.......89700
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR.......89800
         IF (KSCRN.EQ.1) WRITE (*,1511)                                  SUTERR.......89900
         WRITE (K00,1511)                                                SUTERR.......90000
 1511    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......90100
         IF (CODE(2).EQ.'INP') THEN                                      SUTERR.......90200
            CDUM80 = FNAME(1)                                            SUTERR.......90300
         ELSE                                                            SUTERR.......90400
            CDUM80 = FNAME(2)                                            SUTERR.......90500
         END IF                                                          SUTERR.......90600
         IF (((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')).OR.               SUTERR.......90700
     1       (CODE(3).EQ.'INS')) THEN                                    SUTERR.......90800
           IF (KSCRN.EQ.1) WRITE (*,1513) ERRCOD, CDUM80, INERR(1)       SUTERR.......90900
           WRITE (K00,1513) ERRCOD, CDUM80, INERR(1)                     SUTERR.......91000
 1513      FORMAT (/4X,'Error code:',2X,A40                              SUTERR.......91100
     1             /4X,'File:      ',2X,A40                              SUTERR.......91200
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR.......91300
         ELSE                                                            SUTERR.......91400
           IF (KSCRN.EQ.1) WRITE (*,1514) ERRCOD, CDUM80, CODE(3)(1:3),  SUTERR.......91500
     1        INERR(1)                                                   SUTERR.......91600
           WRITE (K00,1514) ERRCOD, CDUM80, CODE(3)(1:3), INERR(1)       SUTERR.......91700
 1514      FORMAT (/4X,'Error code:',2X,A40                              SUTERR.......91800
     1             /4X,'File:      ',2X,A40                              SUTERR.......91900
     1             /4X,'Dataset:   ',2X,A3                               SUTERR.......92000
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR.......92100
         END IF                                                          SUTERR.......92200
         DO 1515 I=1,50                                                  SUTERR.......92300
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......92400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......92500
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......92600
 1515    CONTINUE                                                        SUTERR.......92700
         IF (KSCRN.EQ.1) WRITE (*,1521)                                  SUTERR.......92800
         WRITE (K00,1521)                                                SUTERR.......92900
 1521    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......93000
         DO 1525 I=1,50                                                  SUTERR.......93100
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......93200
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......93300
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......93400
 1525    CONTINUE                                                        SUTERR.......93500
         IF (KSCRN.EQ.1) WRITE (*,1581)                                  SUTERR.......93600
         WRITE (K00,1581)                                                SUTERR.......93700
 1581    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR.......93800
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR.......93900
     1     /4X,'appears to be correct, check the preceding lines'        SUTERR.......94000
     1     /4X,'for missing data or extraneous characters.')             SUTERR.......94100
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......94200
C........ERROR TYPE 'REA-FIL' (FORTRAN READ ERROR)                       SUTERR.......94300
         IF (KSCRN.EQ.1)                                                 SUTERR.......94400
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR.......94500
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR.......94600
         IF (KSCRN.EQ.1) WRITE (*,1611)                                  SUTERR.......94700
         WRITE (K00,1611)                                                SUTERR.......94800
 1611    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......94900
         IF (KSCRN.EQ.1) WRITE (*,1613) ERRCOD, INERR(1)                 SUTERR.......95000
         WRITE (K00,1613) ERRCOD, INERR(1)                               SUTERR.......95100
 1613    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......95200
     1           /4X,'File:      ',2X,'SUTRA.FIL'                        SUTERR.......95300
     1          //4X,'Error status flag.....IOSTAT = ',I5/)              SUTERR.......95400
         DO 1615 I=1,50                                                  SUTERR.......95500
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......95600
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......95700
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......95800
 1615    CONTINUE                                                        SUTERR.......95900
         IF (KSCRN.EQ.1) WRITE (*,1621)                                  SUTERR.......96000
         WRITE (K00,1621)                                                SUTERR.......96100
 1621    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......96200
         DO 1625 I=1,50                                                  SUTERR.......96300
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......96400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......96500
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......96600
 1625    CONTINUE                                                        SUTERR.......96700
      END IF                                                             SUTERR.......96800
 1888 FORMAT (                                                           SUTERR.......96900
     1   /1X,'+--------+',38('-'),'+--------+'                           SUTERR.......97000
     1   /1X,'| \\  // |',38('-'),'| \\  // |'                           SUTERR.......97100
     1   /1X,'|  \\//  |',38(' '),'|  \\//  |'                           SUTERR.......97200
     1   /1X,'|   //   |',A38,    '|   //   |'                           SUTERR.......97300
     1   /1X,'|  //\\  |',38(' '),'|  //\\  |'                           SUTERR.......97400
     1   /1X,'| //  \\ |',38('-'),'| //  \\ |'                           SUTERR.......97500
     1   /1X,'+--------+',38('-'),'+--------+')                          SUTERR.......97600
C                                                                        SUTERR.......97700
C.....WRITE RUN TERMINATION MESSAGES AND CALL TERMINATION SEQUENCE       SUTERR.......97800
      IF (KSCRN.EQ.1) WRITE (*,8888)                                     SUTERR.......97900
      WRITE (K00,8888)                                                   SUTERR.......98000
      IF (K3.NE.-1) WRITE (K3,8889)                                      SUTERR.......98100
      IF (K5.NE.-1) WRITE (K5,8889)                                      SUTERR.......98200
      IF (K6.NE.-1) WRITE (K6,8889)                                      SUTERR.......98300
 8888 FORMAT (/1X,'+',56('-'),'+'/1X,'| ',54X,' |'/1X,'|',3X,            SUTERR.......98400
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR.......98500
     1   3X,'|'/1X,'| ',54X,' |'/1X,'+',56('-'),'+')                     SUTERR.......98600
 8889 FORMAT (//13X,'+',56('-'),'+'/13X,'| ',54X,' |'/13X,'|',3X,        SUTERR.......98700
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR.......98800
     1   3X,'|'/13X,'| ',54X,' |'/13X,'+',56('-'),'+')                   SUTERR.......98900
      IF (KSCRN.EQ.1) WRITE (*,8890)                                     SUTERR.......99000
 8890 FORMAT (/' The above error message also appears in the SMY file,'  SUTERR.......99100
     1        /' which may contain additional error information.')       SUTERR.......99200
      CALL TERSEQ()                                                      SUTERR.......99300
C                                                                        SUTERR.......99400
      RETURN                                                             SUTERR.......99500
      END                                                                SUTERR.......99600
C                                                                        SUTERR.......99700
C     SUBROUTINE        S  U  T  R  A              SUTRA VERSION 2.1     SUTRA..........100
C                                                                        SUTRA..........200
C *** PURPOSE :                                                          SUTRA..........300
C ***  MAIN CONTROL ROUTINE FOR SUTRA SIMULATION.  ORGANIZES             SUTRA..........400
C ***  INITIALIZATION, CALCULATIONS FOR EACH TIME STEP AND ITERATION,    SUTRA..........500
C ***  AND VARIOUS OUTPUTS.                                              SUTRA..........600
C                                                                        SUTRA..........700
      SUBROUTINE SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,  SUTRA..........800
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA..........900
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA.........1000
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA.........1100
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA.........1200
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA.........1300
     6   IN,IQSOP,IQSOU,IPBC,IUBC,OBSPTS,NREG,LREG,IWK,IA,JA,            SUTRA.........1400
     7   IQSOPT,IQSOUT,IPBCT,IUBCT)                                      SUTRA.........1500
      USE ALLARR, ONLY : OBSDAT                                          SUTRA.........1600
      USE LLDEF                                                          SUTRA.........1700
      USE EXPINT                                                         SUTRA.........1800
      USE SCHDEF                                                         SUTRA.........1900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA.........2000
      PARAMETER (NCOLMX=9)                                               SUTRA.........2100
      CHARACTER*8 VERNUM, VERNIN                                         SUTRA.........2200
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA.........2300
      CHARACTER*10 ADSMOD                                                SUTRA.........2400
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:8),CDUM80              SUTRA.........2500
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA.........2600
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA.........2700
      LOGICAL PRNALL,PRN0,PRNDEF,PRNK3,PRNK5,PRNK6,PRNK78                SUTRA.........2800
      LOGICAL SCHSTP, TSPRTD                                             SUTRA.........2900
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8                                SUTRA.........3000
      DIMENSION INERR(10),RLERR(10)                                      SUTRA.........3100
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             SUTRA.........3200
      DIMENSION PMAT(NELT,NCBI),UMAT(NELT,NCBI)                          SUTRA.........3300
      DIMENSION PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN), SUTRA.........3400
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA.........3500
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA.........3600
     3   QIN(NN),QINITR(NN),UIN(NN),QUIN(NN),RCIT(NN),RCITM1(NN)         SUTRA.........3700
      DIMENSION PVEC(NNVEC),UVEC(NNVEC)                                  SUTRA.........3800
      DIMENSION ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),        SUTRA.........3900
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA.........4000
     2   PANGL1(NE)                                                      SUTRA.........4100
      DIMENSION ALMID(NEX),ATMID(NEX),                                   SUTRA.........4200
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA.........4300
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX)                 SUTRA.........4400
      DIMENSION PBC(NBCN),UBC(NBCN),QPLITR(NBCN)                         SUTRA.........4500
      DIMENSION GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48)                  SUTRA.........4600
      DIMENSION FWK(NWF),B(NNNX)                                         SUTRA.........4700
      DIMENSION IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),   SUTRA.........4800
     1   NREG(NN),LREG(NE),IWK(NWI),IA(NDIMIA),JA(NDIMJA)                SUTRA.........4900
      TYPE (OBSDAT), DIMENSION (NOBSN) :: OBSPTS                         SUTRA.........5000
      DIMENSION KTYPE(2)                                                 SUTRA.........5100
      TYPE (LLD), POINTER :: DENTS                                       SUTRA.........5200
      TYPE (LLD), ALLOCATABLE :: DENOB(:)                                SUTRA.........5300
      DIMENSION LCNT(NFLOMX)                                             SUTRA.........5400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA.........5500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SUTRA.........5600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA.........5700
     1   NSOP,NSOU,NBCN                                                  SUTRA.........5800
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        SUTRA.........5900
      COMMON /DIMX2/ NELTA, NNVEC, NDIMIA, NDIMJA                        SUTRA.........6000
      COMMON /FNAMES/ UNAME,FNAME                                        SUTRA.........6100
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     SUTRA.........6200
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA.........6300
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA.........6400
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             SUTRA.........6500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTRA.........6600
     1   KSCRN,KPAUSE                                                    SUTRA.........6700
      COMMON /MODSOR/ ADSMOD                                             SUTRA.........6800
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      SUTRA.........6900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA.........7000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA.........7100
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7, ONCEK8                       SUTRA.........7200
      COMMON /SCH/ NSCH,ISCHTS                                           SUTRA.........7300
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTRA.........7400
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           SUTRA.........7500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA.........7600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  SUTRA.........7700
      COMMON /VER/ VERNUM, VERNIN                                        SUTRA.........7800
C                                                                        SUTRA.........7900
C.....WRITE TITLE TO CONSOLE                                             SUTRA.........8000
      DO 100 I=80,1,-1                                                   SUTRA.........8100
         IF (TITLE1(I).NE.' ') THEN                                      SUTRA.........8200
            LENT1 = I                                                    SUTRA.........8300
            GOTO 101                                                     SUTRA.........8400
         END IF                                                          SUTRA.........8500
  100 CONTINUE                                                           SUTRA.........8600
      LENT1 = 1                                                          SUTRA.........8700
  101 DO 105 I=80,1,-1                                                   SUTRA.........8800
         IF (TITLE2(I).NE.' ') THEN                                      SUTRA.........8900
            LENT2 = I                                                    SUTRA.........9000
            GOTO 106                                                     SUTRA.........9100
         END IF                                                          SUTRA.........9200
  105 CONTINUE                                                           SUTRA.........9300
      LENT2 = 1                                                          SUTRA.........9400
  106 CONTINUE                                                           SUTRA.........9500
      IF (KSCRN.EQ.1) WRITE (*,121) VERNUM                               SUTRA.........9600
      WRITE (K00,121) VERNUM                                             SUTRA.........9700
  121 FORMAT (/1X,9X,53("=")//1X,25X,"S    U    T    R    A",//          SUTRA.........9800
     1   31X,"Version ",A8//1X,9X,53("=")/)                              SUTRA.........9900
      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE1(I),I=1,LENT1)                SUTRA........10000
      WRITE (K00,122) (TITLE1(I),I=1,LENT1)                              SUTRA........10100
      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE2(I),I=1,LENT2)                SUTRA........10200
      WRITE (K00,122) (TITLE2(I),I=1,LENT2)                              SUTRA........10300
  122 FORMAT (1X,80A1)                                                   SUTRA........10400
      IF (KSCRN.EQ.1) WRITE (*,*)                                        SUTRA........10500
      WRITE (K00,*)                                                      SUTRA........10600
C                                                                        SUTRA........10700
C.....DETERMINE ACTUAL NUMBER OF TIME STEPS AND DURATION FROM TIME       SUTRA........10800
C        STEP SCHEDULE.                                                  SUTRA........10900
      IF (ISSTRA.EQ.0) THEN                                              SUTRA........11000
         DENTS => SCHDLS(ISCHTS)%SLIST                                   SUTRA........11100
         DO 310 K=0,ITMAX                                                SUTRA........11200
            TMAX = DENTS%DVALU1                                          SUTRA........11300
            DENTS => DENTS%NENT                                          SUTRA........11400
  310    CONTINUE                                                        SUTRA........11500
      ELSE                                                               SUTRA........11600
         TMAX = TSTART                                                   SUTRA........11700
      END IF                                                             SUTRA........11800
      TEMAX = TMAX - TSTART                                              SUTRA........11900
C                                                                        SUTRA........12000
C.....INITIALIZE TIME STEP NUMBER AND SCHEDULE POINTERS                  SUTRA........12100
      IT=0                                                               SUTRA........12200
      DELTLC = DELT                                                      SUTRA........12300
      DENTS => SCHDLS(ISCHTS)%SLIST                                      SUTRA........12400
      ALLOCATE(DENOB(NFLOMX))                                            SUTRA........12500
      DO 400 NFLO=1,NFLOMX                                               SUTRA........12600
         DENOB(NFLO)%NENT => SCHDLS(OFP(NFLO)%ISCHED)%SLIST              SUTRA........12700
         LCNT(NFLO) = 1                                                  SUTRA........12800
  400 CONTINUE                                                           SUTRA........12900
C                                                                        SUTRA........13000
C.....SET FLAG FOR TIME-DEPENDENT SOURCES OR BOUNDARY CONDITIONS.        SUTRA........13100
C        WHEN IBCT=+4, THERE ARE NO TIME-DEPENDENT SPECIFICATIONS.       SUTRA........13200
      IBCT=IQSOPT+IQSOUT+IPBCT+IUBCT                                     SUTRA........13300
C                                                                        SUTRA........13400
C.....SET STARTING TIME OF SIMULATION CLOCK                              SUTRA........13500
C     TSEC=TSTART                                                        SUTRA........13600
      TSECP0=TSEC                                                        SUTRA........13700
      TSECU0=TSEC                                                        SUTRA........13800
      TMIN=TSEC/60.D0                                                    SUTRA........13900
      THOUR=TMIN/60.D0                                                   SUTRA........14000
      TDAY=THOUR/24.D0                                                   SUTRA........14100
      TWEEK=TDAY/7.D0                                                    SUTRA........14200
      TMONTH=TDAY/30.4375D0                                              SUTRA........14300
      TYEAR=TDAY/365.25D0                                                SUTRA........14400
C                                                                        SUTRA........14500
C.....OUTPUT INITIAL/STARTING CONDITIONS FOR TRANSIENT TRANSPORT         SUTRA........14600
      IF(ISSTRA.NE.1) THEN                                               SUTRA........14700
C........PRINT TO LST OUTPUT FILE                                        SUTRA........14800
         IF (KTYPE(1).EQ.3) THEN                                         SUTRA........14900
            CALL OUTLST3(0,0,0,0,0,0D0,0,0,0D0,PVEC,UVEC,VMAG,VANG1,     SUTRA........15000
     1         VANG2,SW)                                                 SUTRA........15100
         ELSE                                                            SUTRA........15200
            CALL OUTLST2(0,0,0,0,0,0D0,0,0,0D0,PVEC,UVEC,VMAG,VANG1,SW)  SUTRA........15300
         END IF                                                          SUTRA........15400
C........IF TRANSIENT FLOW, PRINT TO NODEWISE AND OBSERVATION OUTPUT     SUTRA........15500
C           FILES NOW.  (OTHERWISE, WAIT UNTIL STEADY-STATE FLOW         SUTRA........15600
C           SOLUTION IS COMPUTED.)                                       SUTRA........15700
         IF (ISSFLO.EQ.0) THEN                                           SUTRA........15800
            IF (K5.NE.-1)                                                SUTRA........15900
     1         CALL OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2)             SUTRA........16000
            DO 650 NFLO=1,NFLOMX                                         SUTRA........16100
               IF (IUNIO(NFLO).NE.-1) THEN                               SUTRA........16200
                  IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                      SUTRA........16300
                     CALL OUTOBS(NFLO,OBSPTS,TSTART,0D0,PM1,UM1,         SUTRA........16400
     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........16500
                  ELSE                                                   SUTRA........16600
                     CALL OUTOBC(NFLO,OBSPTS,TSTART,0D0,PM1,UM1,         SUTRA........16700
     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........16800
                  END IF                                                 SUTRA........16900
                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........17000
                  LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                 SUTRA........17100
                  IF ((STEP.EQ.0D0).AND.(LCNT(NFLO).LT.LENSCH)) THEN     SUTRA........17200
                     DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT           SUTRA........17300
                     LCNT(NFLO) = LCNT(NFLO) + 1                         SUTRA........17400
                  END IF                                                 SUTRA........17500
               END IF                                                    SUTRA........17600
  650       CONTINUE                                                     SUTRA........17700
         END IF                                                          SUTRA........17800
      END IF                                                             SUTRA........17900
C                                                                        SUTRA........18000
C.....SET SWITCHES AND PARAMETERS FOR SOLUTION WITH STEADY-STATE FLOW    SUTRA........18100
      IF(ISSFLO.NE.1) GOTO 1000                                          SUTRA........18200
      ML=1                                                               SUTRA........18300
      NOUMAT=0                                                           SUTRA........18400
      ISSFLO=2                                                           SUTRA........18500
      ITER=0                                                             SUTRA........18600
      DLTPM1=DELTP                                                       SUTRA........18700
      DLTUM1=DELTU                                                       SUTRA........18800
      BDELP1 = 1D0                                                       SUTRA........18900
      BDELP=0.0D0                                                        SUTRA........19000
      BDELU=0.0D0                                                        SUTRA........19100
      IF (ISSTRA.NE.0) THEN                                              SUTRA........19200
         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAX                         SUTRA........19300
         WRITE (K00,902) IT, ITMAX                                       SUTRA........19400
      ELSE                                                               SUTRA........19500
         TELAPS = TSEC - TSTART                                          SUTRA........19600
         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAX, TELAPS, TEMAX          SUTRA........19700
         WRITE (K00,903) IT, ITMAX, TSEC, TEMAX                          SUTRA........19800
      END IF                                                             SUTRA........19900
  902 FORMAT (1X, 'TIME STEP ', I8, ' OF ', I8)                          SUTRA........20000
  903 FORMAT (1X, 'TIME STEP ', I8, ' OF ', I8, ';',                     SUTRA........20100
     1        3X, 'ELAPSED TIME: ', 1PE11.4, ' OF ', 1PE11.4, ' [s]')    SUTRA........20200
      GOTO 1100                                                          SUTRA........20300
C                                                                        SUTRA........20400
C                                                                        SUTRA........20500
C ********************************************************************** SUTRA........20600
C.....BEGIN TIME STEP ************************************************** SUTRA........20700
C ********************************************************************** SUTRA........20800
C.....INCREMENT TIME STEP NUMBER                                         SUTRA........20900
 1000 IT=IT+1                                                            SUTRA........21000
      DIT = DNINT(DBLE(IT))                                              SUTRA........21100
      DENTS => DENTS%NENT                                                SUTRA........21200
      ITER=0                                                             SUTRA........21300
      ML=0                                                               SUTRA........21400
      NOUMAT=0                                                           SUTRA........21500
C.....SET NOUMAT TO OBTAIN U SOLUTION BY SIMPLE BACK SUBSTITUTION        SUTRA........21600
C        BEGINNING ON SECOND TIME STEP AFTER A PRESSURE SOLUTION         SUTRA........21700
C        IF THE SOLUTION IS NON-ITERATIVE (ITRMAX=1)                     SUTRA........21800
      IF(MOD(IT-1,NPCYC).NE.0.AND.MOD(IT,NPCYC).NE.0.AND.IT.GT.2         SUTRA........21900
     1   .AND.ITRMAX.EQ.1) NOUMAT=1                                      SUTRA........22000
C.....CHOOSE SOLUTION VARIABLE ON THIS TIME STEP:                        SUTRA........22100
C        ML=0 FOR P AND U, ML=1 FOR P ONLY, AND ML=2 FOR U ONLY.         SUTRA........22200
      IF(IT.EQ.1.AND.ISSFLO.NE.2) GOTO 1005                              SUTRA........22300
      IF(MOD(IT,NPCYC).NE.0) ML=2                                        SUTRA........22400
      IF(MOD(IT,NUCYC).NE.0) ML=1                                        SUTRA........22500
C.....INCREMENT SIMULATION CLOCK, TSEC, TO END OF NEW TIME STEP          SUTRA........22600
 1005 TSECM1 = TSEC                                                      SUTRA........22700
      TSEC = DENTS%DVALU1                                                SUTRA........22800
      TMIN=TSEC/60.D0                                                    SUTRA........22900
      THOUR=TMIN/60.D0                                                   SUTRA........23000
      TDAY=THOUR/24.D0                                                   SUTRA........23100
      TWEEK=TDAY/7.D0                                                    SUTRA........23200
      TMONTH=TDAY/30.4375D0                                              SUTRA........23300
      TYEAR=TDAY/365.25D0                                                SUTRA........23400
C.....UPDATE TIME STEP SIZE                                              SUTRA........23500
      DELTM1 = DELT                                                      SUTRA........23600
      DELT = TSEC - TSECM1                                               SUTRA........23700
C.....NO SIMPLE BACK SUBSTITUTION FOR U IF TIME STEP HAS CHANGED         SUTRA........23800
C        BY MORE THAN A VERY SMALL TOLERANCE                             SUTRA........23900
      RELCHG = DABS((DELT - DELTLC)/DELTLC)                              SUTRA........24000
      IF (RELCHG.GT.1D-14) THEN                                          SUTRA........24100
         DELTLC = DELT                                                   SUTRA........24200
         NOUMAT = 0                                                      SUTRA........24300
      END IF                                                             SUTRA........24400
C                                                                        SUTRA........24500
C.....WRITE TIME STEP NUMBER AND ELAPSED TIME                            SUTRA........24600
      IF (ISSTRA.NE.0) THEN                                              SUTRA........24700
         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAX                         SUTRA........24800
         WRITE (K00,902) IT, ITMAX                                       SUTRA........24900
      ELSE                                                               SUTRA........25000
         TELAPS = TSEC - TSTART                                          SUTRA........25100
         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAX, TELAPS, TEMAX          SUTRA........25200
         WRITE (K00,903) IT, ITMAX, TSEC, TEMAX                          SUTRA........25300
      END IF                                                             SUTRA........25400
C                                                                        SUTRA........25500
C.....SET TIME STEP (DELTP AND/OR DELTU) AND INCREMENT CLOCK             SUTRA........25600
C        FOR WHICHEVER OF P AND/OR U ARE SOLVED FOR ON THIS TIME STEP    SUTRA........25700
      IF(ML-1) 1010,1020,1030                                            SUTRA........25800
 1010 DLTPM1=DELTP                                                       SUTRA........25900
      DLTUM1=DELTU                                                       SUTRA........26000
      DELTP=TSEC-TSECP0                                                  SUTRA........26100
      DELTU=TSEC-TSECU0                                                  SUTRA........26200
      TSECP0=TSEC                                                        SUTRA........26300
      TSECU0=TSEC                                                        SUTRA........26400
      GOTO 1040                                                          SUTRA........26500
 1020 DLTPM1=DELTP                                                       SUTRA........26600
      DELTP=TSEC-TSECP0                                                  SUTRA........26700
      TSECP0=TSEC                                                        SUTRA........26800
      GOTO 1040                                                          SUTRA........26900
 1030 DLTUM1=DELTU                                                       SUTRA........27000
      DELTU=TSEC-TSECU0                                                  SUTRA........27100
      TSECU0=TSEC                                                        SUTRA........27200
 1040 CONTINUE                                                           SUTRA........27300
C.....SET PROJECTION FACTORS USED ON FIRST ITERATION TO EXTRAPOLATE      SUTRA........27400
C        AHEAD ONE-HALF TIME STEP                                        SUTRA........27500
      BDELP=(DELTP/DLTPM1)*0.50D0                                        SUTRA........27600
      BDELU=(DELTU/DLTUM1)*0.50D0                                        SUTRA........27700
      BDELP1=BDELP+1.0D0                                                 SUTRA........27800
      BDELU1=BDELU+1.0D0                                                 SUTRA........27900
C                                                                        SUTRA........28000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........28100
C.....BEGIN ITERATION - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........28200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........28300
C.....INCREMENT ITERATION NUMBER                                         SUTRA........28400
 1100 ITER=ITER+1                                                        SUTRA........28500
C.....IF ITERATIVE SOLUTION, WRITE ITERATION NUMBER                      SUTRA........28600
      IF (ITRMAX.NE.1) THEN                                              SUTRA........28700
         IF (KSCRN.EQ.1) WRITE (*,1104) ITER                             SUTRA........28800
         WRITE (K00,1104) ITER                                           SUTRA........28900
      END IF                                                             SUTRA........29000
 1104 FORMAT (1X, 3X, 'NON-LINEARITY ITERATION ', I5)                    SUTRA........29100
C                                                                        SUTRA........29200
      IF(ML-1) 2000,2200,2400                                            SUTRA........29300
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH BOTH P AND U SOLUTIONS    SUTRA........29400
 2000 DO 2025 I=1,NN                                                     SUTRA........29500
C.....SET DPDT-ITERATE TO VALUE FROM PREVIOUS ITERATION FOR PRESSURE     SUTRA........29600
C      COMING FROM THIS TIME STEP                                        SUTRA........29700
C       (THIS IS OVERWRITTEN ON THE FIRST ITERATION JUST BELOW)          SUTRA........29800
C     NOTE: DPDTITR IS USED ONLY IN THE BUDGET                           SUTRA........29900
      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........30000
      PITER(I)=PVEC(I)                                                   SUTRA........30100
      PVEL(I)=PVEC(I)                                                    SUTRA........30200
      UITER(I)=UVEC(I)                                                   SUTRA........30300
      RCITM1(I)=RCIT(I)                                                  SUTRA........30400
 2025 RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........30500
      DO 2050 IP=1,NPBC                                                  SUTRA........30600
      I=IABS(IPBC(IP))                                                   SUTRA........30700
      QPLITR(IP)=GNUP*(PBC(IP)-PITER(I))                                 SUTRA........30800
 2050 CONTINUE                                                           SUTRA........30900
C.....QINITR VALUE DIFFERS FROM QIN ONLY IF BCTIME CHANGED QIN           SUTRA........31000
      IF (ITER.LE.2) THEN                                                SUTRA........31100
         DO 2060 I=1,NN                                                  SUTRA........31200
 2060    QINITR(I)=QIN(I)                                                SUTRA........31300
      END IF                                                             SUTRA........31400
      IF(ITER.GT.1) GOTO 2600                                            SUTRA........31500
      DO 2075 I=1,NN                                                     SUTRA........31600
      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........31700
      UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                               SUTRA........31800
C.....RESETS DPDT-ITERATE TO VALUE FROM MOST RECENT PRESSURE TIME STEP   SUTRA........31900
C      ON THE FIRST ITERATION FOR THIS TIME STEP                         SUTRA........32000
      DPDTITR(I)=(PVEC(I)-PM1(I))/DLTPM1                                 SUTRA........32100
      PM1(I)=PVEC(I)                                                     SUTRA........32200
      UM2(I)=UM1(I)                                                      SUTRA........32300
 2075 UM1(I)=UVEC(I)                                                     SUTRA........32400
      GOTO 2600                                                          SUTRA........32500
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH P SOLUTION ONLY           SUTRA........32600
 2200 DO 2225 I=1,NN                                                     SUTRA........32700
      PVEL(I)=PVEC(I)                                                    SUTRA........32800
 2225 PITER(I)=PVEC(I)                                                   SUTRA........32900
      IF(ITER.GT.1) GOTO 2600                                            SUTRA........33000
      DO 2250 I=1,NN                                                     SUTRA........33100
      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........33200
      UITER(I)=UVEC(I)                                                   SUTRA........33300
      RCITM1(I)=RCIT(I)                                                  SUTRA........33400
      RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........33500
 2250 PM1(I)=PVEC(I)                                                     SUTRA........33600
      GOTO 2600                                                          SUTRA........33700
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH U SOLUTION ONLY           SUTRA........33800
 2400 IF (ITER.EQ.1) THEN                                                SUTRA........33900
         DO 2405 I=1,NN                                                  SUTRA........34000
 2405       UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                         SUTRA........34100
      ELSE                                                               SUTRA........34200
         DO 2410 I=1,NN                                                  SUTRA........34300
 2410       UITER(I)=UVEC(I)                                             SUTRA........34400
      END IF                                                             SUTRA........34500
      IF(NOUMAT.EQ.1) GOTO 2480                                          SUTRA........34600
C.....SET PARAMETERS FROM MOST RECENT PRESSURE TIME STEP                 SUTRA........34700
      IF(ITER.GT.1) GOTO 2600                                            SUTRA........34800
      DO 2450 I=1,NN                                                     SUTRA........34900
      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........35000
      QINITR(I)=QIN(I)                                                   SUTRA........35100
      PITER(I)=PVEC(I)                                                   SUTRA........35200
      PVEL(I)=PVEC(I)                                                    SUTRA........35300
 2450 RCITM1(I)=RCIT(I)                                                  SUTRA........35400
      DO 2475 IP=1,NPBC                                                  SUTRA........35500
      I=IABS(IPBC(IP))                                                   SUTRA........35600
      QPLITR(IP)=GNUP*(PBC(IP)-PITER(I))                                 SUTRA........35700
 2475 CONTINUE                                                           SUTRA........35800
 2480 DO 2500 I=1,NN                                                     SUTRA........35900
      UM2(I)=UM1(I)                                                      SUTRA........36000
 2500 UM1(I)=UVEC(I)                                                     SUTRA........36100
 2600 CONTINUE                                                           SUTRA........36200
C                                                                        SUTRA........36300
C.....INITIALIZE ARRAYS WITH VALUE OF ZERO                               SUTRA........36400
      MATDIM=NELT*NCBI                                                   SUTRA........36500
      IF(ML-1) 3000,3000,3300                                            SUTRA........36600
 3000 CALL ZERO(PMAT,MATDIM,0.0D0)                                       SUTRA........36700
      CALL ZERO(PVEC,NNVEC,0.0D0)                                        SUTRA........36800
      CALL ZERO(VOL,NN,0.0D0)                                            SUTRA........36900
      IF(ML-1) 3300,3400,3300                                            SUTRA........37000
 3300 IF(NOUMAT) 3350,3350,3375                                          SUTRA........37100
 3350 CALL ZERO(UMAT,MATDIM,0.0D0)                                       SUTRA........37200
 3375 CALL ZERO(UVEC,NNVEC,0.0D0)                                        SUTRA........37300
 3400 CONTINUE                                                           SUTRA........37400
C                                                                        SUTRA........37500
C.....SET TIME-DEPENDENT BOUNDARY CONDITIONS, SOURCES AND SINKS          SUTRA........37600
C        FOR THIS TIME STEP                                              SUTRA........37700
      IF(ITER.EQ.1.AND.IBCT.NE.4)                                        SUTRA........37800
     1   CALL BCTIME(IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,         SUTRA........37900
     2   IPBCT,IUBCT,IQSOPT,IQSOUT,X,Y,Z)                                SUTRA........38000
C                                                                        SUTRA........38100
C.....SET SORPTION PARAMETERS FOR THIS TIME STEP                         SUTRA........38200
      IF(ML.NE.1.AND.ME.EQ.-1.AND.NOUMAT.EQ.0.AND.                       SUTRA........38300
     1   ADSMOD.NE.'NONE      ') CALL ADSORB(CS1,CS2,CS3,SL,SR,UITER)    SUTRA........38400
C                                                                        SUTRA........38500
C.....DO ELEMENTWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U      SUTRA........38600
      IF (NOUMAT.EQ.0) THEN                                              SUTRA........38700
       IF (KTYPE(1).EQ.3) THEN                                           SUTRA........38800
C..... 3D PROBLEM                                                        SUTRA........38900
       CALL ELEMN3(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........39000
     2   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,                            SUTRA........39100
     3   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA........39200
     4   PANGL1,PANGL2,PANGL3,VMAG,VANG1,VANG2,VOL,PMAT,PVEC,            SUTRA........39300
     5   UMAT,UVEC,GXSI,GETA,GZET,PVEL,LREG,IA,JA)                       SUTRA........39400
       ELSE                                                              SUTRA........39500
C..... 2D PROBLEM                                                        SUTRA........39600
       CALL ELEMN2(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........39700
     2   ALMAX,ALMIN,ATMAX,ATMIN,PERMXX,PERMXY,PERMYX,PERMYY,PANGL1,     SUTRA........39800
     3   VMAG,VANG1,VOL,PMAT,PVEC,UMAT,UVEC,GXSI,GETA,PVEL,LREG,IA,JA)   SUTRA........39900
       END IF                                                            SUTRA........40000
      END IF                                                             SUTRA........40100
C                                                                        SUTRA........40200
C.....DO NODEWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U         SUTRA........40300
      CALL NODAL(ML,VOL,PMAT,PVEC,UMAT,UVEC,PITER,UITER,PM1,UM1,UM2,     SUTRA........40400
     1   POR,QIN,UIN,QUIN,QINITR,CS1,CS2,CS3,SL,SR,SW,DSWDP,RHO,SOP,     SUTRA........40500
     2   NREG,JA)                                                        SUTRA........40600
C                                                                        SUTRA........40700
C.....SET SPECIFIED P AND U CONDITIONS IN MATRIX EQUATION FOR P AND/OR U SUTRA........40800
      CALL BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,JA)        SUTRA........40900
C                                                                        SUTRA........41000
C.....MATRIX EQUATION FOR P AND/OR U COMPLETE.  SOLVE EQUATIONS:         SUTRA........41100
C        WITH DIRECT SOLVER,                                             SUTRA........41200
C           WHEN KMT=0, DECOMPOSE AND BACK-SUBSTITUTE,                   SUTRA........41300
C           WHEN KMT=2, BACK-SUBSTITUTE ONLY.                            SUTRA........41400
C        KPU=1 WHEN SOLVING FOR P,                                       SUTRA........41500
C        KPU=2 WHEN SOLVING FOR U.                                       SUTRA........41600
      IHALFB=NBHALF-1                                                    SUTRA........41700
      IERRP = 0                                                          SUTRA........41800
      IERRU = 0                                                          SUTRA........41900
      IF(ML-1) 5000,5000,5500                                            SUTRA........42000
C                                                                        SUTRA........42100
C.....SOLVE FOR P                                                        SUTRA........42200
 5000 KMT=000000                                                         SUTRA........42300
      KPU=1                                                              SUTRA........42400
      KSOLVR = KSOLVP                                                    SUTRA........42500
      CALL SOLVER(KMT,KPU,KSOLVR,PMAT,PVEC,PITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........42600
     1            IWK,FWK,IA,JA,IERRP,ITRSP,ERRP)                        SUTRA........42700
C.....P SOLUTION NOW IN PVEC                                             SUTRA........42800
C                                                                        SUTRA........42900
C.....IF STEADY FLOW, SET PM1=PVEC SO THAT INTERPOLATION AT FRACTIONAL   SUTRA........43000
C        TIME STEPS YIELDS THE STEADY-STATE PRESSURE.                    SUTRA........43100
      IF (ISSFLO.NE.0) THEN                                              SUTRA........43200
         DO 5200 I=1,NN                                                  SUTRA........43300
            PM1(I) = PVEC(I)                                             SUTRA........43400
 5200    CONTINUE                                                        SUTRA........43500
      END IF                                                             SUTRA........43600
C                                                                        SUTRA........43700
      IF(ML-1) 5500,6000,5500                                            SUTRA........43800
C                                                                        SUTRA........43900
C.....SOLVE FOR U                                                        SUTRA........44000
 5500 KMT=000000                                                         SUTRA........44100
      KPU=2                                                              SUTRA........44200
      IF(NOUMAT) 5700,5700,5600                                          SUTRA........44300
 5600 KMT=2                                                              SUTRA........44400
 5700 KSOLVR = KSOLVU                                                    SUTRA........44500
      CALL SOLVER(KMT,KPU,KSOLVR,UMAT,UVEC,UITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........44600
     1            IWK,FWK,IA,JA,IERRU,ITRSU,ERRU)                        SUTRA........44700
 6000 CONTINUE                                                           SUTRA........44800
C.....U SOLUTION NOW IN UVEC                                             SUTRA........44900
C                                                                        SUTRA........45000
      IERR = IABS(IERRP) + IABS(IERRU)                                   SUTRA........45100
C                                                                        SUTRA........45200
C.....CHECK PROGRESS AND CONVERGENCE OF NON-LINEARITY ITERATIONS         SUTRA........45300
C        AND SET STOP AND GO FLAGS:                                      SUTRA........45400
C           ISTOP = -1   NOT CONVERGED - STOP SIMULATION                 SUTRA........45500
C           ISTOP =  0   ITERATIONS LEFT OR CONVERGED - KEEP SIMULATING  SUTRA........45600
C           ISTOP =  1   LAST TIME STEP REACHED - STOP SIMULATION        SUTRA........45700
C           IGOI = 0   P AND U CONVERGED, OR NO ITERATIONS DONE          SUTRA........45800
C           IGOI = 1   ONLY P HAS NOT YET CONVERGED TO CRITERION         SUTRA........45900
C           IGOI = 2   ONLY U HAS NOT YET CONVERGED TO CRITERION         SUTRA........46000
C           IGOI = 3   BOTH P AND U HAVE NOT YET CONVERGED TO CRITERIA   SUTRA........46100
      ISTOP=0                                                            SUTRA........46200
      IGOI=0                                                             SUTRA........46300
      IF(ITRMAX-1) 7500,7500,7000                                        SUTRA........46400
 7000 RPM=0.D0                                                           SUTRA........46500
      RUM=0.D0                                                           SUTRA........46600
      IPWORS=0                                                           SUTRA........46700
      IUWORS=0                                                           SUTRA........46800
      IF(ML-1) 7050,7050,7150                                            SUTRA........46900
 7050 DO 7100 I=1,NN                                                     SUTRA........47000
      RP=DABS(PVEC(I)-PITER(I))                                          SUTRA........47100
      IF(RP-RPM) 7100,7060,7060                                          SUTRA........47200
 7060 RPM=RP                                                             SUTRA........47300
      IPWORS=I                                                           SUTRA........47400
 7100 CONTINUE                                                           SUTRA........47500
      IF(RPM.GT.RPMAX) IGOI=IGOI+1                                       SUTRA........47600
 7150 IF(ML-1) 7200,7350,7200                                            SUTRA........47700
 7200 DO 7300 I=1,NN                                                     SUTRA........47800
      RU=DABS(UVEC(I)-UITER(I))                                          SUTRA........47900
      IF(RU-RUM) 7300,7260,7260                                          SUTRA........48000
 7260 RUM=RU                                                             SUTRA........48100
      IUWORS=I                                                           SUTRA........48200
 7300 CONTINUE                                                           SUTRA........48300
      IF(RUM.GT.RUMAX) IGOI=IGOI+2                                       SUTRA........48400
 7350 CONTINUE                                                           SUTRA........48500
      IF (KSCRN.EQ.1) WRITE (*,7377) RPM, RUM                            SUTRA........48600
      WRITE (K00,7377) RPM, RUM                                          SUTRA........48700
 7377 FORMAT (1X, 6X, 'Maximum changes in P, U: ',1PE8.1,", ",1PE8.1)    SUTRA........48800
      IF(IGOI.GT.0.AND.ITER.EQ.ITRMAX) ISTOP=-1                          SUTRA........48900
      IF(IGOI.GT.0.AND.ISTOP.EQ.0.AND.IERR.EQ.0) GOTO 1100               SUTRA........49000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........49100
C.....END ITERATION - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........49200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........49300
C                                                                        SUTRA........49400
 7500 CONTINUE                                                           SUTRA........49500
      IF(ISTOP.NE.-1.AND.IT.EQ.ITMAX) ISTOP=1                            SUTRA........49600
C                                                                        SUTRA........49700
C.....OUTPUT RESULTS FOR TIME STEP IN ACCORDANCE WITH PRINT CYCLES       SUTRA........49800
C                                                                        SUTRA........49900
C.....COMPUTE SOME LOGICAL CONDITIONS.  PRNALL=.TRUE. INDICATES THAT     SUTRA........50000
C        ALL RESULTS SHOULD BE PRINTED BECAUSE THIS IS THE LAST TIME     SUTRA........50100
C        STEP (EITHER BY DESIGN OR BECAUSE OF AN ERROR).  PRN0=.TRUE.    SUTRA........50200
C        INDICATES THAT INITIAL CONDITIONS ARE TO BE PRINTED FOR A       SUTRA........50300
C        STEADY-FLOW, TRANSIENT-TRANSPORT RUN.  PRNDEF=.TRUE. IF         SUTRA........50400
C        EITHER OF THE TWO PRECEDING CONDITIONS IS TRUE.                 SUTRA........50500
      PRNALL = ((ISTOP.NE.0).OR.(IERR.NE.0))                             SUTRA........50600
      PRN0 = ((IT.EQ.0).AND.(ISSFLO.NE.0).AND.(ISSTRA.NE.1))             SUTRA........50700
      PRNDEF = (PRNALL.OR.PRN0)                                          SUTRA........50800
C.....PRINT RESULTS TO THE LST OUTPUT FILE                               SUTRA........50900
      PRNK3 = (PRNDEF.OR.(MOD(IT,NPRINT).EQ.0)                           SUTRA........51000
     1         .OR.((IT.EQ.1).AND.(NPRINT.GT.0)))                        SUTRA........51100
      IF (PRNK3) THEN                                                    SUTRA........51200
      IF (KTYPE(1).EQ.3) THEN                                            SUTRA........51300
         CALL OUTLST3(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........51400
     1      PVEC,UVEC,VMAG,VANG1,VANG2,SW)                               SUTRA........51500
      ELSE                                                               SUTRA........51600
         CALL OUTLST2(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........51700
     1      PVEC,UVEC,VMAG,VANG1,SW)                                     SUTRA........51800
      END IF                                                             SUTRA........51900
C.....CALCULATE AND PRINT FLUID MASS AND/OR ENERGY OR SOLUTE MASS BUDGET SUTRA........52000
      IF(KBUDG.EQ.1)                                                     SUTRA........52100
     1   CALL BUDGET(ML,IBCT,VOL,SW,DSWDP,RHO,SOP,QIN,PVEC,PM1,DPDTITR,  SUTRA........52200
     2      PBC,QPLITR,IPBC,IQSOP,POR,UVEC,UM1,UM2,UIN,QUIN,QINITR,      SUTRA........52300
     3      IQSOU,UBC,IUBC,CS1,CS2,CS3,SL,SR,NREG)                       SUTRA........52400
      END IF                                                             SUTRA........52500
C.....PRINT NODEWISE AND ELEMENTWISE RESULTS TO OUTPUT FILES             SUTRA........52600
      PRNK5 = ((PRNDEF.OR.((IT.NE.0).AND.(MOD(IT,NCOLPR).EQ.0))          SUTRA........52700
     1         .OR.((IT.EQ.1).AND.(NCOLPR.GT.0))).AND.(K5.NE.-1))        SUTRA........52800
      IF (PRNK5) CALL OUTNOD(PVEC,UVEC,SW,X,Y,Z,TITLE1,TITLE2)           SUTRA........52900
      PRNK6 = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,LCOLPR).EQ.0))          SUTRA........53000
     1         .OR.(IT.EQ.1)).AND.(K6.NE.-1))                            SUTRA........53100
      IF (PRNK6) CALL OUTELE(VMAG,VANG1,VANG2,IN,X,Y,Z,TITLE1,TITLE2)    SUTRA........53200
C.....PRINT RESULTS TO OBSERVATION OUTPUT FILES.  CHECK FOR OUTPUT       SUTRA........53300
C       SCHEDULED WITHIN THE CURRENT TIME STEP AND PRINT IT.  IF THIS    SUTRA........53400
C       IS THE INITIAL CONDITION (IT=0) OR THE FINAL TIME STEP, PRINT    SUTRA........53500
C       RESULTS IF THEY HAVE NOT ALREADY BEEN PRINTED.                   SUTRA........53600
C.....LOOP OVER OBSERVATION OUTPUT SCHEDULES.                            SUTRA........53700
      DO 7650 NFLO=1,NFLOMX                                              SUTRA........53800
C........IF NO FILE FOR THIS OUTPUT, SKIP IT                             SUTRA........53900
         IF (IUNIO(NFLO).EQ.-1) CYCLE                                    SUTRA........54000
C........SET FLAG INDICATING THAT OUTPUT HAS NOT (YET) BEEN PRINTED      SUTRA........54100
C           FOR THE END OF THE CURRENT TIME STEP                         SUTRA........54200
         TSPRTD = .FALSE.                                                SUTRA........54300
C........IF TRANSPORT IS TRANSIENT AND THIS IS NOT THE INITIAL           SUTRA........54400
C            CONDITION, CHECK FOR SCHEDULED OUTPUT AND PRINT IT.         SUTRA........54500
         IF ((ISSTRA.EQ.0).AND.(IT.NE.0)) THEN                           SUTRA........54600
C...........GET THE LENGTH OF THE SCHEDULE AND THE NEXT TIME/STEP        SUTRA........54700
C              SCHEDULED TO BE OUTPUT.                                   SUTRA........54800
            LENSCH = SCHDLS(OFP(NFLO)%ISCHED)%LLEN                       SUTRA........54900
            TIME = DENOB(NFLO)%NENT%DVALU1                               SUTRA........55000
            STEP = DENOB(NFLO)%NENT%DVALU2                               SUTRA........55100
C...........LOOP THROUGH THE SCHEDULE, PRINTING ANY OUTPUT SCHEDULED     SUTRA........55200
C              WITHIN THE CURRENT TIME STEP.  (LOOP AS LONG AS THE       SUTRA........55300
C              SCHEDULED OUTPUT IS WITHIN THE CURRENT TIME STEP          SUTRA........55400
C              AND THE SCHEDULE HAS NOT BEEN EXHAUSTED.)                 SUTRA........55500
            DO WHILE ((DIT.GE.STEP).AND.(LCNT(NFLO).LE.LENSCH))          SUTRA........55600
C..............IF THE SCHEDULED STEP IS NOT ZERO, PRINT RESULTS.         SUTRA........55700
               IF (STEP.NE.0D0) THEN                                     SUTRA........55800
                  IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                      SUTRA........55900
                     CALL OUTOBS(NFLO,OBSPTS,TIME,STEP,PM1,UM1,          SUTRA........56000
     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........56100
                  ELSE                                                   SUTRA........56200
                     CALL OUTOBC(NFLO,OBSPTS,TIME,STEP,PM1,UM1,          SUTRA........56300
     1                  PVEC,UVEC,TITLE1,TITLE2,IN,LREG)                 SUTRA........56400
                  END IF                                                 SUTRA........56500
C.................IF END OF TIME STEP HAS JUST BEEN PRINTED, SET FLAG.   SUTRA........56600
                  IF (DIT.EQ.STEP) TSPRTD = .TRUE.                       SUTRA........56700
               END IF                                                    SUTRA........56800
C..............GO TO THE NEXT ENTRY IN THE SCHEDULE, IF THERE IS ONE.    SUTRA........56900
               IF (LCNT(NFLO).LT.LENSCH) THEN                            SUTRA........57000
                  DENOB(NFLO)%NENT => DENOB(NFLO)%NENT%NENT              SUTRA........57100
                  TIME = DENOB(NFLO)%NENT%DVALU1                         SUTRA........57200
                  STEP = DENOB(NFLO)%NENT%DVALU2                         SUTRA........57300
               END IF                                                    SUTRA........57400
C..............INCREMENT THE COUNTER.                                    SUTRA........57500
               LCNT(NFLO) = LCNT(NFLO) + 1                               SUTRA........57600
            END DO                                                       SUTRA........57700
         END IF                                                          SUTRA........57800
C........IF THIS IS THE INITIAL OR FINAL CONDITION, PRINT IT IF IT       SUTRA........57900
C           HAS NOT ALREADY BEEN PRINTED.                                SUTRA........58000
         IF (PRNDEF.AND.(.NOT.TSPRTD)) THEN                              SUTRA........58100
            IF (OFP(NFLO)%FRMT.EQ."OBS") THEN                            SUTRA........58200
               CALL OUTOBS(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,PVEC,UVEC,       SUTRA........58300
     1            TITLE1,TITLE2,IN,LREG)                                 SUTRA........58400
            ELSE                                                         SUTRA........58500
               CALL OUTOBC(NFLO,OBSPTS,TSEC,DIT,PM1,UM1,PVEC,UVEC,       SUTRA........58600
     1            TITLE1,TITLE2,IN,LREG)                                 SUTRA........58700
            END IF                                                       SUTRA........58800
         END IF                                                          SUTRA........58900
 7650 CONTINUE                                                           SUTRA........59000
C                                                                        SUTRA........59100
C.....STORE RESULTS FOR POSSIBLE RESTART OF SIMULATION EACH              SUTRA........59200
C        ISTORE TIME STEPS AND AFTER LAST TIME STEP, THEN GO             SUTRA........59300
C        TO NEXT TIME STEP                                               SUTRA........59400
      IF (IERR.EQ.0) THEN                                                SUTRA........59500
         IF ((ISTORE.NE.0).AND.((ISTOP.NE.0).OR.(MOD(IT,ISTORE).EQ.0)))  SUTRA........59600
     1      CALL OUTRST(PVEC,UVEC,PM1,UM1,CS1,RCIT,SW,QINITR,PBC)        SUTRA........59700
         IF (ISTOP.EQ.0) GOTO 1000                                       SUTRA........59800
      END IF                                                             SUTRA........59900
C                                                                        SUTRA........60000
C ********************************************************************** SUTRA........60100
C.....END TIME STEP **************************************************** SUTRA........60200
C ********************************************************************** SUTRA........60300
C                                                                        SUTRA........60400
C                                                                        SUTRA........60500
C.....DEALLOCATE ARRAY DENOB                                             SUTRA........60600
      DEALLOCATE (DENOB)                                                 SUTRA........60700
C                                                                        SUTRA........60800
C.....COMPLETE OUTPUT AND TERMINATE SIMULATION                           SUTRA........60900
      IF (IERRP.NE.0) THEN                                               SUTRA........61000
         ERRCOD = 'SOL-1'                                                SUTRA........61100
         CHERR(1) = 'P'                                                  SUTRA........61200
         CHERR(2) = SOLWRD(KSOLVP)                                       SUTRA........61300
         INERR(1) = IERRP                                                SUTRA........61400
         INERR(2) = ITRSP                                                SUTRA........61500
         RLERR(1) = ERRP                                                 SUTRA........61600
         RLERR(2) = TOLP                                                 SUTRA........61700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........61800
      ELSE IF (IERRU.NE.0) THEN                                          SUTRA........61900
         ERRCOD = 'SOL-1'                                                SUTRA........62000
         CHERR(1) = 'U'                                                  SUTRA........62100
         CHERR(2) = SOLWRD(KSOLVU)                                       SUTRA........62200
         INERR(1) = IERRU                                                SUTRA........62300
         INERR(2) = ITRSU                                                SUTRA........62400
         RLERR(1) = ERRU                                                 SUTRA........62500
         RLERR(2) = TOLU                                                 SUTRA........62600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........62700
      END IF                                                             SUTRA........62800
C                                                                        SUTRA........62900
      IF(ISTORE.GT.0) WRITE(K3,8100)                                     SUTRA........63000
 8100 FORMAT(//////11X,'*** LAST SOLUTION HAS BEEN STORED ',             SUTRA........63100
     1   'IN THE RESTART DATA FILE ***')                                 SUTRA........63200
C                                                                        SUTRA........63300
C.....OUTPUT END OF SIMULATION MESSAGE AND RETURN TO MAIN FOR STOP       SUTRA........63400
      IF(ISTOP.EQ.-1) THEN                                               SUTRA........63500
         ERRCOD = 'CON-1'                                                SUTRA........63600
         IF (ME.EQ.1) THEN                                               SUTRA........63700
            CDUM80 = 'temperature'                                       SUTRA........63800
            LENC = 11                                                    SUTRA........63900
         ELSE                                                            SUTRA........64000
            CDUM80 = 'concentration'                                     SUTRA........64100
            LENC = 13                                                    SUTRA........64200
         END IF                                                          SUTRA........64300
         IF (IGOI.EQ.1) THEN                                             SUTRA........64400
            CHERR(1) = 'pressure'                                        SUTRA........64500
            LENC = 8                                                     SUTRA........64600
         ELSE IF (IGOI.EQ.2) THEN                                        SUTRA........64700
            CHERR(1) = CDUM80                                            SUTRA........64800
         ELSE IF (IGOI.EQ.3) THEN                                        SUTRA........64900
            CHERR(1) = 'pressure and ' // CDUM80(1:LENC)                 SUTRA........65000
            LENC = 13 + LENC                                             SUTRA........65100
         END IF                                                          SUTRA........65200
         INERR(1) = IPWORS                                               SUTRA........65300
         INERR(2) = IUWORS                                               SUTRA........65400
         INERR(3) = ITER                                                 SUTRA........65500
         INERR(4) = LENC                                                 SUTRA........65600
         RLERR(1) = RPM                                                  SUTRA........65700
         RLERR(2) = RPMAX                                                SUTRA........65800
         RLERR(3) = RUM                                                  SUTRA........65900
         RLERR(4) = RUMAX                                                SUTRA........66000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........66100
      ELSE IF (ISTOP.EQ.2) THEN                                          SUTRA........66200
         WRITE(K3,8450)                                                  SUTRA........66300
 8450    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........66400
     1      ' COMPLETION OF TIME PERIOD'/                                SUTRA........66500
     2                  11X,'***** ********** ********** **',            SUTRA........66600
     3      ' ********** ** **** ******')                                SUTRA........66700
      ELSE                                                               SUTRA........66800
         WRITE(K3,8550)                                                  SUTRA........66900
 8550    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........67000
     1      ' COMPLETION OF TIME STEPS'/                                 SUTRA........67100
     2                  11X,'***** ********** ********** **',            SUTRA........67200
     3      ' ********** ** **** *****')                                 SUTRA........67300
      END IF                                                             SUTRA........67400
C                                                                        SUTRA........67500
      IF (KSCRN.EQ.1) WRITE(*,8590)                                      SUTRA........67600
      WRITE(K00,8590)                                                    SUTRA........67700
 8590 FORMAT(/1X,'S I M U L A T I O N   E N D E D'/)                     SUTRA........67800
      RETURN                                                             SUTRA........67900
C                                                                        SUTRA........68000
      END                                                                SUTRA........68100
C                                                                        SUTRA........68200
C     SUBROUTINE        T  E  N  S  Y  M           SUTRA VERSION 2.1     TENSYM.........100
C                                                                        TENSYM.........200
C *** PURPOSE :                                                          TENSYM.........300
C ***  TO TRANSFORM A DIAGONAL MATRIX TO A NEW COORDINATE SYSTEM.        TENSYM.........400
C ***  [T] IS THE DIAGONAL MATRIX EXPRESSED IN THE FIRST (INPUT)         TENSYM.........500
C ***  COORDINATE SYSTEM; [P] IS THE (SYMMETRIC) MATRIX EXPRESSED        TENSYM.........600
C ***  IN THE SECOND (OUTPUT) COORDINATE SYSTEM; AND [Q] IS THE          TENSYM.........700
C ***  THE TRANSFORMATION MATRIX.                                        TENSYM.........800
C                                                                        TENSYM.........900
      SUBROUTINE TENSYM(T11,T22,T33,Q11,Q12,Q13,Q21,Q22,Q23,             TENSYM........1000
     1   Q31,Q32,Q33,P11,P12,P13,P21,P22,P23,P31,P32,P33)                TENSYM........1100
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TENSYM........1200
C                                                                        TENSYM........1300
C.....COMPUTE TRANSFORMED MATRIX.                                        TENSYM........1400
      P11= T11*Q11*Q11 + T22*Q12*Q12 + T33*Q13*Q13                       TENSYM........1500
      P12= T11*Q11*Q21 + T22*Q12*Q22 + T33*Q13*Q23                       TENSYM........1600
      P13= T11*Q11*Q31 + T22*Q12*Q32 + T33*Q13*Q33                       TENSYM........1700
      P22= T11*Q21*Q21 + T22*Q22*Q22 + T33*Q23*Q23                       TENSYM........1800
      P23= T11*Q21*Q31 + T22*Q22*Q32 + T33*Q23*Q33                       TENSYM........1900
      P33= T11*Q31*Q31 + T22*Q32*Q32 + T33*Q33*Q33                       TENSYM........2000
      P21= P12                                                           TENSYM........2100
      P31= P13                                                           TENSYM........2200
      P32= P23                                                           TENSYM........2300
C                                                                        TENSYM........2400
      RETURN                                                             TENSYM........2500
      END                                                                TENSYM........2600
C                                                                        TENSYM........2700
C     SUBROUTINE        T  E  R  S  E  Q           SUTRA VERSION 2.1     TERSEQ.........100
C                                                                        TERSEQ.........200
C *** PURPOSE :                                                          TERSEQ.........300
C ***  TO GRACEFULLY TERMINATE A SUTRA RUN BY DEALLOCATING THE MAIN      TERSEQ.........400
C ***  ALLOCATABLE ARRAYS AND CLOSING ALL FILES.                         TERSEQ.........500
C                                                                        TERSEQ.........600
      SUBROUTINE TERSEQ()                                                TERSEQ.........700
      USE ALLARR                                                         TERSEQ.........800
      USE SCHDEF                                                         TERSEQ.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TERSEQ........1000
      CHARACTER CDUM*1                                                   TERSEQ........1100
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8                     TERSEQ........1200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     TERSEQ........1300
     1   KSCRN,KPAUSE                                                    TERSEQ........1400
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                      TERSEQ........1500
C                                                                        TERSEQ........1600
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND STOP     TERSEQ........1700
      IF (ALLO1) THEN                                                    TERSEQ........1800
         DEALLOCATE(PITER,UITER,PM1,DPDTITR,UM1,UM2,PVEL,SL,SR,X,Y,Z,    TERSEQ........1900
     1      VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,QIN,UIN,QUIN,QINITR,    TERSEQ........2000
     2      RCIT,RCITM1)                                                 TERSEQ........2100
         DEALLOCATE(PVEC,UVEC)                                           TERSEQ........2200
         DEALLOCATE(ALMAX,ALMIN,ATMAX,ATMIN,VMAG,VANG1,PERMXX,PERMXY,    TERSEQ........2300
     1      PERMYX,PERMYY,PANGL1)                                        TERSEQ........2400
         DEALLOCATE(ALMID,ATMID,VANG2,PERMXZ,PERMYZ,PERMZX,PERMZY,       TERSEQ........2500
     1      PERMZZ,PANGL2,PANGL3)                                        TERSEQ........2600
         DEALLOCATE(PBC,UBC,QPLITR)                                      TERSEQ........2700
         DEALLOCATE(GXSI,GETA,GZET)                                      TERSEQ........2800
         DEALLOCATE(B)                                                   TERSEQ........2900
         DEALLOCATE(IN,IQSOP,IQSOU,IPBC,IUBC,NREG,LREG,JA)               TERSEQ........3000
         DEALLOCATE(OBSPTS)                                              TERSEQ........3100
      END IF                                                             TERSEQ........3200
      IF (ALLO2) THEN                                                    TERSEQ........3300
         DEALLOCATE(PMAT,UMAT,FWK)                                       TERSEQ........3400
         DEALLOCATE(IWK)                                                 TERSEQ........3500
      END IF                                                             TERSEQ........3600
      IF (ALLO3) THEN                                                    TERSEQ........3700
         DEALLOCATE(IA)                                                  TERSEQ........3800
      END IF                                                             TERSEQ........3900
      IF (ALLOCATED(SCHDLS)) DEALLOCATE(SCHDLS)                          TERSEQ........4000
      IF (ALLOCATED(OFP)) DEALLOCATE(OFP)                                TERSEQ........4100
      IF (ALLOCATED(FNAMO)) DEALLOCATE(FNAMO)                            TERSEQ........4200
      IF (ALLOCATED(ONCK78)) DEALLOCATE(ONCK78)                          TERSEQ........4300
C.....ARRAY IUNIO WILL BE DEALLOCATED AFTER THE OBSERVATION OUTPUT       TERSEQ........4400
C        FILES ARE CLOSED                                                TERSEQ........4500
      CLOSE(K00)                                                         TERSEQ........4600
      CLOSE(K0)                                                          TERSEQ........4700
      CLOSE(K1)                                                          TERSEQ........4800
      CLOSE(K2)                                                          TERSEQ........4900
      CLOSE(K3)                                                          TERSEQ........5000
      CLOSE(K4)                                                          TERSEQ........5100
      CLOSE(K5)                                                          TERSEQ........5200
      CLOSE(K6)                                                          TERSEQ........5300
      CLOSE(K7)                                                          TERSEQ........5400
      CLOSE(K8)                                                          TERSEQ........5500
      DO 8000 NFO=1,NFLOMX                                               TERSEQ........5600
         CLOSE(IUNIO(NFO))                                               TERSEQ........5700
 8000 CONTINUE                                                           TERSEQ........5800
      IF (ALLOCATED(IUNIO)) DEALLOCATE(IUNIO)                            TERSEQ........5900
      IF ((KSCRN.EQ.1).AND.(KPAUSE.EQ.1)) THEN                           TERSEQ........6000
         WRITE(*,9990)                                                   TERSEQ........6100
 9990    FORMAT(/' Press ENTER to exit ...')                             TERSEQ........6200
         READ(*,'(A1)') CDUM                                             TERSEQ........6300
      END IF                                                             TERSEQ........6400
      STOP ' '                                                           TERSEQ........6500
C                                                                        TERSEQ........6600
      RETURN                                                             TERSEQ........6700
      END                                                                TERSEQ........6800
C                                                                        TERSEQ........6900
C     FUNCTION          T  I  M  E  T  S           SUTRA VERSION 2.1     TIMETS.........100
C                                                                        TIMETS.........200
C *** PURPOSE :                                                          TIMETS.........300
C ***  TO RETURN THE TIME ASSOCIATED WITH A GIVEN TIME STEP.  IF THE     TIMETS.........400
C ***  SPECIFIED TIME STEP IS GREATER THAN THE MAXIMUM, A VALUE OF       TIMETS.........500
C ***  +HUGE(1D0) (THE LARGEST NUMBER THAT CAN BE REPRESENTED IN DOUBLE  TIMETS.........600
C ***  PRECISION) IS RETURNED.  IF THE SPECIFIED TIME STEP IS LESS THAN  TIMETS.........700
C ***  ZERO, A VALUE OF -HUGE(1D0) IS RETURNED.  IF THE TIME STEP        TIMETS.........800
C ***  SCHEDULE HAS NOT YET BEEN DEFINED, A VALUE OF ZERO IS RETURNED.   TIMETS.........900
C                                                                        TIMETS........1000
      DOUBLE PRECISION FUNCTION TIMETS(NSTEP)                            TIMETS........1100
      USE LLDEF                                                          TIMETS........1200
      USE SCHDEF                                                         TIMETS........1300
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TIMETS........1400
      TYPE (LLD), POINTER :: DEN                                         TIMETS........1500
      COMMON /SCH/ NSCH,ISCHTS                                           TIMETS........1600
C                                                                        TIMETS........1700
      IF (ISCHTS.EQ.0) THEN                                              TIMETS........1800
         TIMETS = 0D0                                                    TIMETS........1900
         RETURN                                                          TIMETS........2000
      END IF                                                             TIMETS........2100
C                                                                        TIMETS........2200
      NSMAX = SCHDLS(ISCHTS)%LLEN - 1                                    TIMETS........2300
C                                                                        TIMETS........2400
      IF (NSTEP.LT.0) THEN                                               TIMETS........2500
         TIMETS = -HUGE(1D0)                                             TIMETS........2600
         RETURN                                                          TIMETS........2700
      ELSE IF (NSTEP.GT.NSMAX) THEN                                      TIMETS........2800
         TIMETS = +HUGE(1D0)                                             TIMETS........2900
         RETURN                                                          TIMETS........3000
      END IF                                                             TIMETS........3100
C                                                                        TIMETS........3200
      DEN => SCHDLS(ISCHTS)%SLIST                                        TIMETS........3300
      DO 100 NS=1,NSTEP                                                  TIMETS........3400
         DEN => DEN%NENT                                                 TIMETS........3500
  100 CONTINUE                                                           TIMETS........3600
      TIMETS = DEN%DVALU1                                                TIMETS........3700
C                                                                        TIMETS........3800
      RETURN                                                             TIMETS........3900
      END                                                                TIMETS........4000
C                                                                        TIMETS........4100
C     SUBROUTINE        Z  E  R  O                 SUTRA VERSION 2.1     ZERO...........100
C                                                                        ZERO...........200
C *** PURPOSE :                                                          ZERO...........300
C ***  TO FILL AN ARRAY WITH A CONSTANT VALUE (USUALLY ZERO).            ZERO...........400
C                                                                        ZERO...........500
      SUBROUTINE ZERO(A,IADIM,FILL)                                      ZERO...........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ZERO...........700
      DIMENSION A(IADIM)                                                 ZERO...........800
C                                                                        ZERO...........900
C.....FILL ARRAY A WITH VALUE IN VARIABLE 'FILL'                         ZERO..........1000
      DO 10 I=1,IADIM                                                    ZERO..........1100
   10 A(I)=FILL                                                          ZERO..........1200
C                                                                        ZERO..........1300
C                                                                        ZERO..........1400
      RETURN                                                             ZERO..........1500
      END                                                                ZERO..........1600

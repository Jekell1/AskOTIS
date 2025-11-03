      ******************************************************************
      * DESCRIPTION: JAY'S TEST LAYOUT FOR SQL-COL-BUILD.SH
      *=================================================================
      * REV:
      * JKC 2022-0322 ***NEW***
      ******************************************************************
       78  KC-SIZE                     VALUE 2100.
       01  KC-REC.
           03  KC1-KEY.
               05  KC-OWNBR        PIC 9(4).
               05  KC-ACCTNO       PIC 9(8).
           03  KC-CLASS            PIC 9(3).
           03  KC-SUBCLASS         PIC 99.
           03  KC-LAWCODE          PIC 99.
      *-----------------------------------------------------------------
           03  KC-BWTABLEX.
               05  KC-SSNO1        PIC 9(9).
               05  KC-MAKERCD1     PIC X.
               05  KC-SSNO2        PIC 9(9).
               05  KC-MAKERCD2     PIC X.
               05  KC-SSNO3        PIC 9(9).
               05  KC-MAKERCD3     PIC X.
               05  KC-SSNO4        PIC 9(9).
               05  KC-MAKERCD4     PIC X.
           03  KC-BWTABLE          REDEFINES KC-BWTABLEX.
               05  KC-BORRTBL      OCCURS 4 TIMES.
                   07  KC-SSNO     PIC 9(9).
                   07  KC-MAKERCD  PIC X.
      *-----------------------------------------------------------------
           03  KC-CONVERCD         PIC X.
               88 BULK-PURCHASE    VALUE "B".
               88 CONVERSION-NORMAL VALUE "Y".
      *-----------------------------------------------------------------
           03  KC-STATFLAGS        PIC X(20).
           03  KC-STATUSFLAGS      REDEFINES KC-STATFLAGS
                                   OCCURS 20 TIMES
                                   INDEXED BY STFG.
               05  KC-STATFG       PIC X.
      *-----------------------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  KC-ENTDATE          PIC 9(8)      COMP-3.
      *-----------------------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  KC-1STPYDATE        PIC 9(8).
           03  KC-1STPYDATEX       REDEFINES KC-1STPYDATE.
               05  KC-1STPY-YYYYMM PIC 9(6).
               05  KC-1STPYDA      PIC 99.
      *-----------------------------------------------------------------
           03  KC-FEETBL           OCCURS 20 TIMES.
               05  KC-FEECODE      PIC XX.
               05  KC-FEEAMT       PIC 9(5)V99   COMP-3.
               05  KC-FEEOURS      PIC X.
               05  KC-FEEFINCHG    PIC X.
      *-----------------------------------------------------------------
           03  KC-INSCODES.
               05  KC-INSURED      PIC X.
                   88  KC-INS-NO-ONE       VALUE "N".
                   88  KC-INS-SINGLE       VALUE "M" "S".
                   88  KC-INS-JOINT        VALUE "J".
               05  KC-INSTYPES     PIC X.
                   88  KC-INSTYPES-VALID   VALUE "A" THRU "Z"
                                                 "0" "1" "2" "3" "4"
                                                 "5" "6" "7" "8" "9".
                   88  KC-INSTYPES-DL      VALUE "C"
                                                 "G" "I" "J" "M" "P"
                                                 "T" "X" "Y" "5" "6"
                                                 "7".
               05  KC-INSELIGIBLE  PIC X.
                   88  KC-INSELIGIBLE-VALID  VALUE "A" THRU "Z"
                                                   "0" "1" "2" "3" "4"
                                                   "5" "6" "7" "8" "9".
                   88  KC-INSELIGIBLE-DL     VALUE "C"
                                                   "G" "I" "J" "M" "P"
                                                   "T" "X" "Y" "5" "6"
                                                   "7".
      *-----------------------------------------------------------------
           03  KC-INSTBL              OCCURS 10 TIMES.
               05  KC-INSCOMP         PIC 99.
               05  KC-INSOURS         PIC X.
      $XFD DATE=YYYYMMDD
               05  KC-INSEFF-DATE     PIC 9(8)      COMP-3.
               05  KC-INSTRM          REDEFINES KC-INSEFF-DATE
                                      PIC 9(8)      COMP-3.
      $XFD DATE=YYYYMMDD
               05  KC-INSEXP-DATE     PIC 9(8)      COMP-3.
               05  KC-INSPREM         PIC 9(5)V99   COMP-3.
               05  KC-INSCOVR         PIC 9(7)V99   COMP-3.
               05  KC-INSCOMM         PIC 9(5)V99   COMP-3.
               05  KC-INSANTICERN     PIC S9(7)V99  COMP-3.
               05  KC-INSANTICADJ     PIC S9(7)V99  COMP-3.
               05  KC-INSUFERN        PIC S9(7)V99  COMP-3.
               05  KC-INSACCERN       PIC S9(7)V99  COMP-3.
               05  KC-INSEARN-ORDER   PIC 99.
               05  KC-INSFINCHG       PIC X.
      *-----------------------------------------------------------------
           03  KC-DLRVPRIN         OCCURS 3
                                   PIC 9(7)V99   COMP-3.
           03  KC-DLRVCHGS         OCCURS 3
                                   PIC 9(7)V99   COMP-3.
      *-----------------------------------------------------------------
           03  KC-REBATE-TAB.
               05  KC-CL-REBATE    PIC X.
               05  KC-AH-REBATE    PIC X.
               05  KC-PP-REBATE    PIC X.
               05  KC-INT-REBATE   PIC X.
               05  KC-SC-REBATE    PIC X.
               05  KC-MF-REBATE    PIC X.
               05  KC-DF-REBATE    PIC X.
               05  KC-EX-REBATE    PIC X.
               05  KC-O1-REBATE    PIC X.
               05  KC-O2-REBATE    PIC X.
               05  KC-O3-REBATE    PIC X.
               05  KC-O4-REBATE    PIC X.
               05  KC-O5-REBATE    PIC X.
               05  KC-O6-REBATE    PIC X.
               05  KC-O7-REBATE    PIC X.
               05  KC-UN-O7-REB    PIC X.
               05  KC-UN-O8-REB    PIC X.
               05  KC-UN-O9-REB    PIC X.
               05  KC-UN-O10-REB   PIC X.
           03  KC-REBATE-TABX     REDEFINES KC-REBATE-TAB.
               05  KC-REBATE       PIC X OCCURS 19.
      *-----------------------------------------------------------------
           03  KC-UNITPER-INFO.
               05  KC-UNITPER-CD    PIC X.
                   88  KC-UNITPER-CD-DAYS    VALUE "D" "S" "W" "B".
                   88  KC-UNITPER-CD-MONTHS  VALUE "M" "Y".
               05  KC-UNITPER-FREQ  PIC 999        COMP-3.
      *-----------------------------------------------------------------
           03  KC-PERIOD-LEVEL.
               05  KC-PERIOD-TBL              OCCURS 3.
                   07  KC-ACTIVITY-TBL        OCCURS 2.
                       09  KC-TYPE-TBL        OCCURS 11.
                           11  KC-NO          PIC S9(7)    COMP-3.
                           11  KC-AMT         PIC S9(9)V99 COMP-3.
                           11  KC-O2          PIC S9(9)V99 COMP-3.
                           11  KC-INT         PIC S9(9)V99 COMP-3.
      *-----------------------------------------------------------------
           03  KC-CODE-LEVEL.
               05  KC-CODE                   PIC X(8).
               05  KC-CODE-CT        REDEFINES KC-CODE.
                   07  KC-CT-STATE           PIC X(2).
                   07  KC-CT-COUNTY          PIC X(3).
                   07  KC-CT-FILLER          PIC X(3).
               05  KC-CODE-CY        REDEFINES KC-CODE.
                   07  KC-CY-STATE           PIC X(2).
                   07  KC-CY-CITY            PIC X(3).
                   07  KC-CY-FILLER          PIC X(3).
               05  KC-CODE-EL        REDEFINES KC-CODE.
                   07  KC-EL-CODE            PIC 9(2).
                   07  KC-EL-FILLER          PIC X(6).
               05  KC-CODE-LV        REDEFINES KC-CODE.
                   07  KC-LV-LEVEL           PIC 9(1).
                   07  KC-LV-FILLER          PIC X(7).
               05  KC-CODE-PL        REDEFINES KC-CODE.
                   07  KC-PL-POOLID          PIC 9(5).
                   07  KC-PL-FILLER          PIC X(3).

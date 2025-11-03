      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%% TEST FOR /USR/LOCAL/BIN/SQL-PGM-COL.SH
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      * COPYMEMBER: LIBLP/LP01ST
      ******************************************************************
      *
      * IDENTIFICATION:  LIBUT/UT01UT.CPY
      *
      * DESCRIPTION   :  UTILITY TEST FILE LAYOUT
      *
      *=================================================================
      * REV:
      * JKC 2023-0317 ***NEW***
      ******************************************************************
       78  UT-SIZE                                VALUE 45.
       01  UT-REC.
           03  UT1-KEY.
               05  UT-BRNO                PIC 9(04).
               05  UT-CODE                PIC X(02).
      $XFD DATE=YYYYMMDD
           03  UT-LTOUCH-DATE             PIC 9(08)   COMP-3.
           03  UT-DESC                    PIC X(16).
           03  UT-REINSTATE-WINDOW        PIC X(01).
           03  UT-REPO-RENOTICE           PIC X(01).
           03  UT-TITLE-RELEASE-TYPE      PIC X(01).
           03  UT-AUCTION-DATE-REQ        PIC X(01).
KCTEST     03  UT-GEOGRAPHIC-CODE         PIC 9(04).
           03  UT-MAX-LOAN-AVAIL          PIC 9(6)V99  COMP-3.
KCTEST     03  UT-MAX-TAX-LOAN-AMT        PIC S9(7)V99 COMP-3.
           03  UT-NSF-FEE-AMT             PIC S9(5)V99 COMP-3.
           03  UT-ARRAY1                  PIC X(1)   OCCURS 11.
           03  UT-TEST1                   PIC X(1).
           03  UT-ARRAY2                  PIC X(2)   OCCURS 22.
           03  UT-TEST2                   PIC X(2).
           03  UT-ARRAY3                             OCCURS 3.
               05  UT-ARRAY4                         OCCURS 4.
                   07  UT-TEST-34A        PIC X(4).
                   07  UT-ARRAY5                     OCCURS 5.
                       09  UT-TEST-345A-ABCDEFGHIJK
                                          PIC X(5).
                   07  UT-TEST-34B        PIC X(4).
           03  UT-TEST3                   PIC X(3).
               88  UT-TEST3-BAD           VALUE "BAD".
               88  UT-TEST3-GOOD          VALUE "GOO".
               88  UT-TEST3-ALL           VALUE "BAD" "GOO".
      *----------------------------------------------------
           03  UT-BWTABLEX.
               05  UT-SSNO1        PIC 9(9).
               05  UT-MAKERCD1     PIC X.
               05  UT-SSNO2        PIC 9(9).
               05  UT-MAKERCD2     PIC X.
               05  UT-SSNO3        PIC 9(9).
               05  UT-MAKERCD3     PIC X.
               05  UT-SSNO4        PIC 9(9).
               05  UT-MAKERCD4     PIC X.
           03  UT-BWTABLE          REDEFINES UT-BWTABLEX.
               05  UT-BORRTBL      OCCURS 4 TIMES.
                   07  UT-SSNO     PIC 9(9).
                   07  UT-MAKERCD  PIC X.
           03  UT-STATFLAGS        PIC X(20).
           03  UT-STATUSFLAGS      REDEFINES UT-STATFLAGS
                                   OCCURS 20 TIMES
                                   INDEXED BY STFG.
               05  UT-STATFG       PIC X.
           03  UT-1STPYDATE        PIC 9(8).
           03  UT-1STPYDATEX       REDEFINES UT-1STPYDATE.
               05  UT-1STPY-YYYYMM PIC 9(6).
               05  UT-1STPYDA      PIC 99.

           03  UT-INSEFF-DATEX.
               05  UT-INSEFF-DATE     PIC 9(8)      COMP-3.
               05  UT-INSTRM          REDEFINES UT-INSEFF-DATE
                                      PIC 9(8)      COMP-3.
           03  UT-REBATE-TAB.
               05  UT-CL-REBATE    PIC X.
               05  UT-AH-REBATE    PIC X.
               05  UT-PP-REBATE    PIC X.
               05  UT-INT-REBATE   PIC X.
               05  UT-SC-REBATE    PIC X.
               05  UT-MF-REBATE    PIC X.
               05  UT-DF-REBATE    PIC X.
               05  UT-EX-REBATE    PIC X.
               05  UT-O1-REBATE    PIC X.
               05  UT-O2-REBATE    PIC X.
               05  UT-O3-REBATE    PIC X.
               05  UT-O4-REBATE    PIC X.
               05  UT-O5-REBATE    PIC X.
               05  UT-O6-REBATE    PIC X.
               05  UT-O7-REBATE    PIC X.
               05  UT-UN-O7-REB    PIC X.
               05  UT-UN-O8-REB    PIC X.
               05  UT-UN-O9-REB    PIC X.
               05  UT-UN-O10-REB   PIC X.
           03  UT-REBATE-TABX     REDEFINES UT-REBATE-TAB.
               05  UT-REBATE       PIC X OCCURS 19.
           03  UT-TEST-ALPHA       PIC X(9).
           03  UT-TEST-NUMERIC     REDEFINES UT-TEST-ALPHA
                                   PIC X(9).

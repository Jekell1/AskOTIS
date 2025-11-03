      * COPYMEMBER: LIBGL/GL01GT
      ******************************************************************
      *
      *   GENERAL LEDGER TRANSACTION FILE                        RSZ=95
      *
      *   PRIMARY KEY      : GT-SOURCE
      *                      GT-DATE2
      *                      GT-BRANCH
      *                      GT-ACCTNO 
      *                      GT-DATE3
      *                      GT-DRCRFG
      *                      GT-LINE
      *
      *   SECONDARY KEY (2): GT-DATE1
      *
      *   SECONDARY KEY (3): GT-BRANCH
      *                      GT-ACCTNO 
      *                      GT-DATE3
      *                      GT-DRCRFG
      *                      GT-LINE
      *
      *=================================================================
      *
      *   ****NOTE**** GT-DATE1 GT-DATE2 GT-DATE3
      *                ARE IN YYMMDD FORMAT.
      *
      *=================================================================
      * REV:
      * BLV 080797 ADDED GT-SMITH-SENT (WORLD)
      * MJD 090597 EXPANDED GT-AMT TO BE S9(9)V99
      * MJD 990126 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLF 070119 ADDED GT-POST-USERID, GT-POST-DATE, GT-POST-TIME, 
      *            SUPPORT PR# 2780
      ******************************************************************
       78  GT-SIZE                VALUE 95.
       01  GT-REC.
           03  GT-FLAG            PIC X.
           03  GT3-KEY.
               05  GT-REF         PIC X(14).
               05  GT2-KEY.
                   07  GT-DATE1   PIC 9(6)     COMP-3.
           03  GT1-KEY.
               05  GT-SOURCE      PIC 9.
               05  GT-DATE2       PIC 9(6)     COMP-3.
               05  GT4-KEY.
                   07  GT-BRANCH  PIC 9(4)     COMP-3.
                   07  GT-ACCTNO  PIC 9(8)     COMP-3.
                   07  GT-DATE3   PIC 9(6)     COMP-3.
                   07  GT-DRCRFG  PIC X.
                   07  GT-LINE    PIC 9(6)     COMP-3.
           03  GT-AMT             PIC S9(9)V99 COMP-3.
           03  GT-EXPL            PIC X(32).
           03  GT-PERIOD          PIC 99       COMP-3.
           03  GT-SMITH-SENT      PIC X.
           03  GT-POST-USERID     PIC X(8).
           03  GT-POST-DATE       PIC 9(6)     COMP-6.
           03  GT-POST-TIME       PIC 9(4)     COMP-6.

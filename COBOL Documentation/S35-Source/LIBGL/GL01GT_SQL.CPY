      * COPYMEMBER: LIBGL/GL01GT_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - G/L TRANSACTION FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      * JKC 2023-1019 COMMENTED OUT START-GT1-FILE-GREATER; ONE PROGRAM
      *          (GL/DELDUP) ONLY USES IT; DUE TO THE COMPLEX GT1-KEY I
      *          DO NOT BELIEVE IT IS PROPERLY SETUP FOR IT TO WORK
      *          CORRECTLY; AFTER DISCUSSING WITH KEVIN MULLEN, DELDUP
      *          WAS ONLY USED ONCE BUT WOULD BE NICE TO HAVE IN CASE
      *          NEEDED FOR AN EMERGENCY; RON ZAMISKA WAS ABLE TO
      *          ACCOMPLISH WHAT DELDUP DOES WITHIN A SQL CODE; WE
      *          ARCHIVED DELDUP FROM USE SO THE USE OF
      *          START-GT1-FILE-GREATER IS NO LONGER NEEDED.
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  GT1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GT1-CURSOR-STAT-GOOD              VALUE "00".
           88  GT1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *01  GT1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
      *    88  GT1-CURSOR-GR-STAT-GOOD           VALUE "00".
      *    88  GT1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
      *                                                "01" THRU "~~".
       01  GT1-CURSOR-NG-STAT           PIC X(2) VALUE SPACES.
           88  GT1-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  GT1-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  GT2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GT2-CURSOR-STAT-GOOD              VALUE "00".
           88  GT2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  GT3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  GT3-CURSOR-STAT-GOOD              VALUE "00".
           88  GT3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  GT1-RETURN-CODE           PIC S9(4)   COMP-5.
       01  GT2-RETURN-CODE           PIC S9(4)   COMP-5.
       01  GT3-RETURN-CODE           PIC S9(4)   COMP-5.

       01  QGT1-WHERE-BEG-KEY.
           03  QGT1-WBEG-SOURCE            PIC 9.
           03  QGT1-WBEG-DATE2             PIC X(10).
           03  QGT1-WBEG-BRANCH            PIC 9(4).
           03  QGT1-WBEG-ACCTNO            PIC 9(8).
           03  QGT1-WBEG-DATE3             PIC X(10).
           03  QGT1-WBEG-DRCRFG            PIC X.
           03  QGT1-WBEG-LINE              PIC 9(6).
 
       01  QGT1-WHERE-END-KEY.
           03  QGT1-WEND-SOURCE            PIC 9.
           03  QGT1-WEND-DATE2             PIC X(10).
           03  QGT1-WEND-BRANCH            PIC 9(4).
           03  QGT1-WEND-ACCTNO            PIC 9(8).
           03  QGT1-WEND-DATE3             PIC X(10).
           03  QGT1-WEND-DRCRFG            PIC X.
           03  QGT1-WEND-LINE              PIC 9(6).

       01  QGT1-WHERE-SCAN-BEG-KEY.
           03  QGT1-WSBEG-SOURCE           PIC 9.
           03  QGT1-WSBEG-DATE2            PIC X(10).
           03  QGT1-WSBEG-BRANCH           PIC 9(4).
           03  QGT1-WSBEG-ACCTNO           PIC 9(8).
           03  QGT1-WSBEG-DATE3            PIC X(10).
           03  QGT1-WSBEG-DRCRFG           PIC X.
 
       01  QGT2-WHERE-BEG-KEY.
           03  QGT2-WBEG-DATE1             PIC X(10).
 
       01  QGT2-WHERE-END-KEY.
           03  QGT2-WEND-DATE1             PIC X(10).

       01  QGT3-WHERE-BEG-KEY.
           03  QGT3-WBEG-BRANCH            PIC 9(4).
           03  QGT3-WBEG-ACCTNO            PIC 9(8).
           03  QGT3-WBEG-DATE3             PIC X(10).
           03  QGT3-WBEG-DRCRFG            PIC X.
           03  QGT3-WBEG-LINE              PIC 9(6).
 
       01  QGT3-WHERE-END-KEY.
           03  QGT3-WEND-BRANCH            PIC 9(4).
           03  QGT3-WEND-ACCTNO            PIC 9(8).
           03  QGT3-WEND-DATE3             PIC X(10).
           03  QGT3-WEND-DRCRFG            PIC X.
           03  QGT3-WEND-LINE              PIC 9(6).
 
       01  QGT3-WHERE-SCAN-BEG-KEY.
           03  QGT3-WSBEG-BRANCH           PIC 9(4).
           03  QGT3-WSBEG-ACCTNO           PIC 9(8).
           03  QGT3-WSBEG-DATE3            PIC X(10).
           03  QGT3-WSBEG-DRCRFG           PIC X.
 
       01  QGT-REC.
           03  QGT-FLAG                         PIC X.
           03  QGT-REF                          PIC X(14).
           03  QGT-DATE1                        PIC X(10).
           03  QGT-SOURCE                       PIC 9.
           03  QGT-DATE2                        PIC X(10).
           03  QGT-BRANCH                       PIC 9(4).
           03  QGT-ACCTNO                       PIC 9(8).
           03  QGT-DATE3                        PIC X(10).
           03  QGT-DRCRFG                       PIC X.
           03  QGT-LINE                         PIC 9(6).
           03  QGT-LTOUCH-DATE                  PIC X(10).
           03  QGT-AMT                          PIC S9(9)V99.
           03  QGT-EXPL                         PIC X(32).
           03  QGT-PERIOD                       PIC 99.
           03  QGT-SMITH-SENT                   PIC X.
           03  QGT-POST-USERID                  PIC X(10).
           03  QGT-POST-DATE                    PIC X(10).
           03  QGT-POST-TIME                    PIC 9(4).

       EXEC SQL END DECLARE SECTION END-EXEC.

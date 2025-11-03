       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNCRE2.
       AUTHOR.          MIKE.DONIGAN.
       DATE-WRITTEN.    29-FEB-2016.
      **************************************************************************
      *  DESC:    VALIDATE AND UPDATE DIRECT LOANS FOR WORLD'S LIVE CHECK FEED
      *
      *           READ IN A LIST OF SSNO'S, DATES, LOAN ID'S AND CHECK NUMBERS
      *           AND AUTOMATICALLY UPDATE THEM IN TO A NEW DIRECT LOAN USING
      *           THE LOAN DEFINED IN THE CAN LOAN FILE AS WELL AS THE BORROWER
      *           INFORMATION ALREADY IN THE SYSTEM FOR THE SSNO INDICATED.
      *
      *           THESE CAN LOANS WILL BE ZERO PROCEEDS LOANS WITH THE LOAN.
      *           AMOUNT INCLUDED AS A FEE CODE
      *
      *           THIS WILL BE RUN DAILY FROM HOME OFFICE AND THE LOANS WILL
      *           UPDATE IN TO THE APPROPRIATE BRANCH AS INDICATED BY THE
      *           BRANCH NUMBER IN THE INPUT FILE.
      *
      *  LOCATION:  MAIN MENU (LP/LPMENU)
      *             > SPECIAL PROCEDURES MENU (SP/SPMENU)
      *               > DIRECT LOAN BATCH PROCESSING (SP/SPDLMU)
      *                 > LIVE CHECK BATCH UPDATE (LP/LNCRE2)
      *
      *  REQUIREMENTS:
      *           THE FOLDER /USR/DATA/<MACHINE>/FTP/LIVECHECK MUST EXIST
      *           THE FOLDER /USR/DATA/<MACHINE>/FTP/LIVECHECK/DONE MUST EXIST
      *
      *           NOTE: "/USR/DATA" MAY BE VARY BUT WILL BE BASED ON DATA PATH
      *                 DEFINED IN THE USERS FIL
      *           THE INPUT FILE MUST BE PRESENT.
      *
      *           EXCEPTIONS THAT WILL STOP PROCESSING COMPLETELY:
      *              - STATUS FILE ALREADY EXISTS FOR THE DAY INDICATING
      *                THE PROCESS HAS ALREADY BEEN RUN.
      *              - INPUT FILE IS MISSING
      *              - CANNOT FIND THE USERS BRANCH RECORD
      *                (IE THE PERSON RUNNING THE REPORT)
      *           EXCEPTIONS THAT WILL PREVENT A LOAN FROM UPDATING BUT
      *                       WILL NOT STOP PROCESSING OF OTHER LOANS:
      *              - DAY HAS NOT BEEN OPENED FOR THE BRANCH
      *              - FEE CODES NOT FOUND IN THE BRANCH
      *              - STATE PARAMETER FILE (SPFILE) HAS EXPIRED OR MISSING
      *              - CAN LOAN RECORD IS NOT FOUND
      *              - GL INTERFACE IS MISSING (GIFILE)
      *
      *  INPUT FILE: ASCII TEXT FILE
      *                    /USR/DATA/<MACHINE>/FTP/LIVECHECK/LIVECHECK.IN
      *  INPUT FILE FORMAT:
      *                    BBBBSSSSSSSSSDDDDDDDDLLCCCCCCCCCC
      *
      *                    WHERE BBBB = BRANCH NUMBER (4 NUMERIC)
      *                    SSSSSSSSS = BORROWER SSNO (9 NUMERIC)
      *                    DDDDDDDD = LOAN DATE (8 NUMERIC) YYYYMMDD
      *                    LL = CAN LOAN NUMBER ASSOCIATED WITH LIVE CHECK
      *                                                   (2 ALPHA)
      *                    CCCCCCCCCC = CHECK NUMBER (10 NUMERIC)
      *
      *  STATUS FILE: ASCII FILE
      *            /USR/DATA/<MACHINE>/FTP/LIVECHECK/LCSTATUS
      *
      *  STATUS FILE EXAMPLE:
      *
      *            0001 XXX-XX-8888           ** DAY NOT OPEN FOR BRANCH: 0102
      *            0001 XXX-XX-8888           ** DAY NOT OPEN FOR BRANCH: 0102
      *            0001 XXX-XX-8888           ** DAY NOT OPEN FOR BRANCH: 0102
      *            0001 XXX-XX-8888           ** DAY NOT OPEN FOR BRANCH: 0102
      *
      *            TOTAL LOANS REJECTED         :      4
      *            TOTAL LOANS UPDATED          :      0
      *            TOTAL LOANS ON OTHER MACHINES:      0
      *
      *             NOTE: NO EXCEPTION FOR BRANCHS ON OTHER MACHINES
      *                   INCL IN TOTALS FOR BALANCING PURPOSES.
      *
      *  DONE FILE: ASCII FILE
      *                    /USR/DATA/<MACHINE>/FTP/LIVECHECK/DONE/LCDONE.<DATE>
      *
      *  DONE FILE FORMAT:
      *                    BBBBSSSSSSSSSDDDDDDDDLLCCCCCCCCCC
      *
      *                    WHERE BBBB = BRANCH NUNBER (4 NUMERIC)
      *                    SSSSSSSSS = BORROWES SSNO (9 NUMERIC)
      *                    DDDDDDDD = LOAN DATE (8 NUMERIC) YYYYMMDD
      *                    LL = CAN LOAN NUMBER ASSOCIATED WITH LIVE CHECK
      *                                                   (2 ALPHA/NUMERIC)
      *                    CCCCCCCCCC = CHECK NUMBER (10 NUMERIC)
      *
      *                    NOTE: NUMERIC FIELDS MUST LAVE LEADING ZEROS,
      *                          LEADING SPACES WILL GENERATE EXEPTION
      *
      *                    NOTE: THIS IS A COPY OF THE INPUT FILE
      *                          MOVED AND RENAMED.
      *
      *   RUN OPTIONS:
      *                    "Y" UPDATE WILL NOT UPDATE THE LOANS, IT WILL ONLY
      *                        CREATE THE STATUS FILE INDICATING TOTALS AND
      *                        CAUGHT EXCEPTIONS IN THE STATUS FILE
      *                    "N" UPDATE WILL UPDATE THE LOANS AND CREATE THE
      *                        STATUS FILE INDICATING TOTALS AND CAUGHT
      *                        EXCEPTIONS IN THE STATUS FILE
      *                    "A" UPDATE WILL NOT UPDATE THE LOANS, IT WILL ONLY
      *                        CREATE THE STATUS FILE INDICATING TOTALS, CAUGHT
      *                        EXCEPTIONS AND WILL INDICATE WHETHER OR NOT THE
      *                        LOAN PASSED THE LOAN VERIFICATION PROCESS IN THE
      *                        STATUS FILE
      *                    "L" UPDATE WILL UPDATE THE LOANS AND CREATE THE
      *                        STATUS FILE INDICATING TOTALS, CAUGHT EXCEPTIONS
      *                        AND LOAN NUMBERS IN THE STATUS FILE
      *
      *  NOTE: THIS PROGRAM USES BOTH PROG-BUF AND EOBUF.   THESE TWO WORKERS 
      *        HAVE COMMON NAMED FIELDS.  PARTICULAR FIELDS REFERENCED IN  
      *        THIS PROGRAM HAVE BEEN ALTERRED USING COPY REPLACING STATEMENTS.
      *        WHEN MAKING CHANGES BE AWARE AND MAKE SURE YOU ARE LOOKING 
      *        AT/SETTING THE CORRECT FIELD.
      *        THE CONAME FIELD IN THESE TWO WORKERS ARE DIFFERENT SIZES.   
      *        IT IS 40 CHARACTERS FOR EOBUF AND ONLY 20 FOR PROG-BUF. THE
      *        FIELD IN THE FORM IS 40 SO EOBUF WILL WORK CORRECTLY IN BATCH.
      *        WHEN USING PROG-BUF THE EXTRA FIELDS IN THE LAYOUT BELOW CONAME
      *        ARE OVERLAYED IN THE REMAINING 20 CHARACTERS OF CONAME IN THE 
      *        FORM. WHILE IT WORKS WHEN CALLING LNVERI AND LONPC2, AND  
      *        REDISPLAYS OF THE FORM WILL CAUSE CONAME, SYSDATE, ETC  
      *        TO BECOME FILLED WITH JUNKON THE FORM.  LOGIC HAS BEEN ADDED 
      *        TO LNVERI AND LONPC0 TO PREVENT THEM FROM PERFORMING 
      *        SEND-LEGEND (WRFORM) AND THIS PROGRAM WILL REFRESH CONAME AND 
      *        SYSDATE (SYSINFO-BUF) VIA AN XFER IMMEDIATELY AFTER RETURNING 
      *        FROM CALLED PROGRAMS IN ORDER TO PREVENT ANY FURTHUR FORM 
      *        DISPLAYS FROM DISPLAYING BAD DATA.
      *
      * REV:
      *  MJD 160229 NEW PROGRAM                                   61000 PR#1133
      *                                                                 PR#1133A
      *  MJD 160407 BUG FIX: UPON F7 EXIT INPUT FILE WAS MOVED TO DONE
      *             PREVENTING THE UPDATE FROM BEING RUN FOR REAL  61000 PR#1148
      *  MJD 160601 CONVERTING TO A30.
      *  MJD 160719 ADDED BATCH & PRINT LOGIC, REMOVED MASK ON SSNO,
      *             ADDED DATE RANGE FOR LOAN DATE.  PRINT LOGIC HAS BEEN ADDED 
      *             BUT THE LCSTATUS FILE WILL STILL BE CREATED IN THE 
      *             FTP/LIVECHACK FOLDER                           61000 PR#1167
      * MJD 160719 ADDENDUM TO PR 1167, SET LN-LBOX TO "N" AS PER K.MULLEN
      *           "ONE MORE THING FOR EITHER NOW OR AN ADDENDUM. WE HAD AN ISSUE
      *            THIS WEEK WHERE A USER GOT A MESSAGE ON AN NSF REVERSAL FOR A
      *            LIVE CHECK LOAN THAT IT WAS A LOCKBOX TYPE NSF BECAUSE THE 
      *            LOCKBOX FLAG WAS SET TO Y ON THE ACCOUNT. THE LIVE CHECK 
      *            UPDATE NEEDS TO BE CHANGED TO NOT SET THE LOCKBOX 
      *            FLAG TO Y."                                     61000 PR#1167
      * MJD 160815 ADDING BATCH LOGIC.
      * MJD 160922 ADDED TEST FOR DUAL LOANS.  IF FOUND WRITE EXCEPTION AND
      *            PROCESS NEXT LIVE CHECK.   ADDED EXT-CALLED-FROM AND 
      *            EXT-RETURN-ERR-BUF SO BISCAN KNOWS WHERE IT WAS CALLED FROM
      *                                                            61000 PR#1191
      * MJD 160929 CHANGED ERR MSG "BORROWER &&&&&&&&& HAS LOANS IN THE SYSTEM"
      *            TO "** BORROWER &&&&&&&&& HAS LOANS IN THE SYSTEM"
      *            CHANGED UNDER ORIG PR, HAS NOT BEEN COPIED TO A99  61000#1191
      * BAH 170608 ACTIVATE 8 DIGIT DATES, PD0006
      * MJD 170606 FIXED A BIG IN READING THE CL FILE.   WE WERE NOT TESTING IF
      *            THE GROUP ID CHANGED.   WHEN WE CHANGE BRANCHES AND THE CAN
      *             LOAN NUMBER REMAINES THE SAME WE ARE NOT TRIGGERING A 
      *            REREAD OF THE CL FILE TO GET THE CORRECT CAN LOAN INFORMATION
      *             FOR THE BRANCH/STATE.                          61000 PR#1251
      * BAH 181004 GLOBAL BWFILE
      * BAH 190111 GLOBAL PDFILE
      * BAH 190131 SPLIT CDFILE
      * BAH 2019.0919 REMOVED ACCESS-CALL ON BWFILE
      *  CS 2021.0112 FIXED TO RUN IN BATCH AND EOCRON PR#1481
      *              SEE BETH'S NOTES IN CL/BICREA
      *  CS 2021.0121 FIXED TO RUN IN EOCRON, ADDED ADDITIONAL ERROR TO LCSTATUS
      *              IF ERRORS ON INPUT.  MUST RUN IN EOCRON PR#1481
      *  CS 2021.0121 ADDED ADDITIONAL TO LCSTATUS IF ERRORS ON INPUT FILE
      * BAH 2024.0315 REMOVED BISCAN-LIASMLN AND BISCAN-PRLOANS
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBLP/LPFSOP.CPY".
       COPY "LIBLP/LPFSWK.CPY".

      * FILE SELECT FOR LIVE CHECK RELATIVE FILE
           SELECT LCR-FILE ASSIGN TO LCR-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE EXCLUSIVE
                  FILE STATUS FILE-STAT.

      * FILE SELECT FOR LIVE CHECK INDEXED FILE SORTED BY BRANCH
           SELECT LCI-FILE ASSIGN LCI-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LCI-KEY
                  FILE STATUS FILE-STAT.

      * FILE SELECT FOR LIVE CHECK TOTALS BY LOAN TYPE
           SELECT LCE-FILE ASSIGN LCE-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE EXCLUSIVE
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDOP.CPY".

      * FILE DEFINITION FOR LIVE CHECK RELATIVE FILE
       FD LCR-FILE
           LABEL RECORD IS STANDARD.
       01 LCR-REC.
           03 LCR-BRNO                  PIC 9(4).
           03 LCR-SSNO                  PIC 9(9).
           03 LCR-LOAN-DATE             PIC 9(8).
           03 LCR-CAN-LOAN              PIC X(2).
           03 LCR-CHECK-NO              PIC 9(10).

      * FILE DEFINITION FOR LIVE CHECK INDEXED FILE BY BRANCH
       FD LCI-FILE
           LABEL RECORD IS STANDARD.
       01 LCI-REC.
          03 LCI-KEY.
             05 LCI-BRNO                  PIC 9(4).
             05 LCI-SSNO                  PIC 9(9).
             05 LCI-LOAN-DATE             PIC 9(8).
             05 LCI-CAN-LOAN              PIC X(2).
             05 LCI-CHECK-NO              PIC 9(10).

      * EXCEPTION LISTING
       FD LCE-FILE
           LABEL RECORD IS STANDARD.
       01 LCE-REC                      PIC X(132).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LNCRE2.CBL S35 03/16/23-12:49:23 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      * INPUT PATH FOR LIVE CHECK FILE:
      *             /USR/DATA/<MACHINE>/FTP/LIVECHECK/LIVECHECK.IN
       01  LCR-PATH.
           03  LCR-PATH-BASE.
               05  LCR-PATH-GLOBAL  PIC X(13).
               05  LCR-PATH-FTP     PIC X(4) VALUE "FTP/".
               05  FILLER           PIC X(10) VALUE "LIVECHECK/".
           03  FILLER               PIC X(12) VALUE "LIVECHECK.IN".

      * FILE PATH FOR LIVE CHECK DONE:
      *              /USR/DATA/<MACHINE>/FTP/LIVECHECK/DONE/LCDONE.<DATE>
       01  LCR-DONE-PATH.
           03  LCR-DONE-GLOBAL       PIC X(13).
           03  LCR-DONE-PATH-FTP     PIC X(4) VALUE "FTP/".
           03  FILLER                PIC X(22)
                                     VALUE "LIVECHECK/DONE/LCDONE.".
           03  LCR-DONE-DATE         PIC 9(8).

      * INPUT PATH FOR LIVE CHECK DONE:
      *              /USR/DATA/<MACHINE>/FTP/LIVECHECK/LCSTATUS
       01  LCE-PATH.
           03  LCE-PATH-GLOBAL  PIC X(13).
           03  LCE-PATH-FTP     PIC X(4) VALUE "FTP/".
           03  FILLER           PIC X(18) VALUE "LIVECHECK/LCSTATUS".

      * INPUT PATH FOR LIVE CHECK INDEXED FILE (SORT WORK FILE)
       01  LCI-PATH            PIC X(35) VALUE "LCI".
       01  WK-IFN              PIC X(2) VALUE "WK".
       01  LCR-IFN             PIC X(3) VALUE "LCR".
       01  LCI-IFN             PIC X(3) VALUE "LCI".
       01  LCE-IFN             PIC X(3) VALUE "LCE".
       01  LCE-KEY             PIC 9(4) VALUE 0.
       01  WK-KEY              PIC 9(4) VALUE 0.
       01  ERRORS-EXIST        PIC X VALUE "N".

      * TOTALS:
       01  T-NOLOANS           PIC S9(7) COMP.
       01  T-NOERRORS          PIC S9(7) COMP.
       01  T-NO-OTHER          PIC S9(7) COMP.

      * SCREEN FIELD(S)
       COPY "LIBGB/EOBUF_EXT.CPY".
            03  REPORT-ONLY-FG      PIC X.

      *   MISC WORKERS
       01  SUB                  PIC 9(6) COMP VALUE 0.
       01  RECORDS-FOUND        PIC X VALUE " ".
           88  NO-RECORDS-FOUND       VALUE "N".
       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  EXIT-STATUS          PIC 9(6).
       01  RUN-DISPLAY-MESS.
           03  PROGRESS-MESS    PIC X(32) VALUE
           "REPORT IN PROGRESS......".
       01  RETURN-ERR-BUF       PIC X(55) VALUE SPACES.
       01  HOLD-ERR-DESC        PIC X(60) VALUE SPACES.
       01  DAY-OPEN-FG          PIC X VALUE " ".
           88  DAY-IS-OPEN            VALUE "Y".
           88  DAY-NOT-OPEN           VALUE "N".

       01  HOLD-BRANCH          PIC 9(4) VALUE 0.
       01  HOLD-MACHINE         PIC X(2).
       01  HOLD-CAN-LOAN        PIC X(2) VALUE SPACES.
       01  HOLD-CAN-GROUP-ID    PIC X(4) VALUE SPACES.
       01  DISP-FILE-PATH-LIST  PIC X(14) VALUE "FILE01 FILE02.".
       01  DISP-FILE-PATH-BUF.
           03 DISP-FILE-PATH-FILE01  PIC X(58).
           03 DISP-FILE-PATH-FILE02  PIC X(58).

       01  LN-FEED          PIC 99    VALUE 0.
       01  LN-COUNT         PIC 999   VALUE 99.
       01  PAGE-NUMBER      PIC 9(4)  VALUE 0.
       01  SUMMARY-FG       PIC X     VALUE "N".

       01  EXT-CALLED-FROM     PIC X(6) EXTERNAL.
       01  EXT-RETURN-ERR-BUF  PIC X(55) EXTERNAL.
       01  DUAL-LOANS-FOUND-FG PIC 9.
       88  DUAL-LOANS-FOUND    VALUE 9.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  ERROR-LINE REDEFINES DETAIL-LINE.
           03  FILLER          PIC X.
           03  D-ERR-BRNO    PIC 9999.
           03  FILLER          PIC X.
           03  D-ERR-SSNO      PIC 999/99/9999.
           03  FILLER           PIC X(12).
           03  D-ERR-DESC      PIC X(45).
           03  FILLER          PIC X(2).
           03  D-ERR-DESC2     PIC X(10).
           03  D-ERR-LOAN-DATE  PIC 99/99/99.
           03  FILLER          PIC X(2).
           03  D-ERR-DESC3     PIC X(5).
           03  D-ERR-CAN       PIC X(2).
           03  FILLER          PIC X(2).
           03  D-ERR-DESC4     PIC X(9).
           03  D-ERR-CHECKNO   PIC X(10). 
       01  ERROR-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-DESC    PIC X(30).
           03  FILLER          PIC X.
           03  D-TOTAL-NO      PIC ZZZZZ9.
           03  FILLER          PIC X.
       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(17)
               VALUE "LIVE CHECK UPLOAD".

       01  HEAD3.
           03  FILLER      PIC X(01)    VALUE " ".
           03  FILLER      PIC X(34)    VALUE
               "BRNO SSNO        PMT DATE   STATUS".

       01  VERSION-CONTROL        PIC X  VALUE "A".

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LNCRE2_DEF.CPY".
       COPY "LIBLP/LNCRE2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBCL/BISCANW.CPY".
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/LNCRE2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE OP-FILE PR-FILE LCR-FILE LCI-FILE LCE-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE OP-FILE PR-FILE LCR-FILE LCI-FILE LCE-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNCRE2" TO FORM-PROG VDU-FORM-NAME.

            PERFORM INITIALIZATION.
            IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      * NEED TO GET BRANCH INFOR FOR THE USER RUNNING THE UPDATE.
      * THIS IS USED TO SETUP PATHS FOR INPUT/STATUS FILES AND USED
      * TO DETERMINE THE USERS HOME MACHINE (R1, R2, ....).
      *     MOVE USER-BRANCH TO BR-NO BRANCH BR-NO HOLD-BRANCH.
            MOVE EXT-CONAME-USER-LOC TO BR-NO
                                        BRANCH
                                        BR-NO
                                        HOLD-BRANCH.
            PERFORM READ-BR-FILE
            IF IO-BAD
               MOVE "** USER BRANCH RECORD NOT FOUND: &&&&"
                        TO HOLD-ERR-DESC
               INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&"
                                     BY BR-NO
               PERFORM PRINT-ERROR-LINE
               GO TO END-PROGRAM
            END-IF
            MOVE BR-MACHINE TO HOLD-MACHINE.
            PERFORM SETUP-LCR-FILE.

            PERFORM OPEN-LCI-FILE.
            MOVE SPACES TO LCI-KEY.
            MOVE 0      TO LCI-BRNO LCI-SSNO LCI-LOAN-DATE LCI-CHECK-NO.

            PERFORM START-LCI-FILE.

       NEXT-LCI-REC.
            PERFORM READ-LCI-FILE-NEXT.
            IF IO-BAD
               IF RECORDS-FOUND = "Y"
                  PERFORM PRINT-GRAND-TOTALS
               END-IF
               GO TO END-PROGRAM.

            MOVE "Y" TO RECORDS-FOUND.
            IF LCI-BRNO NOT = HOLD-BRANCH
               MOVE LCI-BRNO TO BR-NO BRANCH BR-NO
               PERFORM READ-BR-FILE
               IF IO-BAD
                  MOVE "** BRANCH RECORD NOT FOUND: &&&&"
                           TO HOLD-ERR-DESC
                  INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&"
                                        BY BR-NO
                  PERFORM PRINT-ERROR-LINE
                  GO TO NEXT-LCI-REC
               END-IF
               IF BR-MACHINE NOT = HOLD-MACHINE
                  ADD 1 TO T-NO-OTHER
                  GO TO NEXT-LCI-REC
               END-IF
               MOVE BR-NO TO HOLD-BRANCH
               PERFORM CLOSE-OP-FILE
               PERFORM CLOSE-BW1-FILE
               MOVE EXT-FILPATH-BASE TO FILPATH-BASE
               MOVE BR-MACHINE       TO FILPATH-MACHINE
               MOVE BR-DATA-PATH     TO FILPATH-DATA
               MOVE "B" TO BW-PATH-OVERRIDE
                           OPEN-PATH-OVERRIDE.

            MOVE BR-NO TO OPEN-PATH-BRNO.
            MOVE TRANS-DATE TO DATE-YYYYMMDD.
            PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
            MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.

            IF (LCI-CAN-LOAN NOT = HOLD-CAN-LOAN) 
                  OR  (BR-CAN-GROUP-ID NOT = HOLD-CAN-GROUP-ID)
               PERFORM OPEN-CL1-FILE
               MOVE BR-CAN-GROUP-ID TO CL-GROUP-ID
               MOVE LCI-CAN-LOAN TO CL-CAN-NO
               PERFORM READ-CL1-FILE
               PERFORM CLOSE-CL1-FILE
               IF IO-BAD
                  MOVE "** CAN LOAN RECORD NOT FOUND: &&"
                       TO HOLD-ERR-DESC
                  INSPECT HOLD-ERR-DESC REPLACING FIRST "&&"
                                        BY CL-CAN-NO
                  PERFORM PRINT-ERROR-LINE
                  GO TO NEXT-LCI-REC
               END-IF
               MOVE CL-CAN-NO TO HOLD-CAN-LOAN
               MOVE CL-GROUP-ID TO HOLD-CAN-GROUP-ID.

      * SETUP PD-REC FIELDS
            MOVE CL-PD-REC TO PD-REC.
            MOVE LCI-SSNO TO PD-SSNO(1).
            MOVE LCI-CHECK-NO TO PD-COLLID.
            MOVE LCI-LOAN-DATE TO PD-LOANDATE
                                  PD-INTDATE
                                  PD-ENTDATE
                                  NDTE-DATE.

            MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
            PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.

      * VERIFY DATES DO NOT ALLOW LOANS IN THE FUTURE AND DO NOT ALLOWS LOANS
      * WITH LOAN DATES MORE THAN 10 DAYS IN THE PAST (INCLUDING TODAYS DATE).
      * EXAMPLE FROM WORLD, IF TODAYS DATE IS 05/25/2016, ALLOW LOANS FROM
      * 05/15/2016 THROUGH 05/24/2015.  DO NOT ALLOW LOAN DATE OF 05/26/2016
      * OR GREATER
            IF LCI-LOAN-DATE > DATE-YYYYMMDD
               MOVE "LOANDATE > SYSTEM DATE: &&&&&&&&"
                    TO HOLD-ERR-DESC
               INSPECT HOLD-ERR-DESC
                       REPLACING FIRST "&&&&&&&&"
                       BY LCI-LOAN-DATE
               PERFORM PRINT-ERROR-LINE
               GO TO NEXT-LCI-REC
            ELSE
               IF LCI-LOAN-DATE < DATE-YYYYMMDD
                  MOVE PD-UNITPER-CD TO DATER-UNITPER-CD
                  MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ
                  MOVE "365" TO ELAPSED-YRTYPE
                  PERFORM TIMALL
                  IF ELAPSED-DAYS <= -10
                    MOVE "LOANDATE TOO OLD: &&&&&&&&" TO HOLD-ERR-DESC
                    INSPECT HOLD-ERR-DESC
                            REPLACING FIRST "&&&&&&&&"
                            BY LCI-LOAN-DATE
                   PERFORM PRINT-ERROR-LINE
                   GO TO NEXT-LCI-REC.

            MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
            MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
            MOVE 1 TO NDTE-HOLD.
            PERFORM INCREMENT-UNITPER.
            MOVE NDTE-DATE TO PD-1STPYDATE
                              PD-ORIG-1STPYDATE.

            MOVE SPACES TO PD2-KEY.

            MOVE BRANCH     TO PD2-BRANCH
                               PD-BRANCH
                               PD-POSTING-BR
                               PD-OWNBR
                               PD-ORGBR.

            MOVE "Q"          TO PD2-ID.
            MOVE SPACES       TO PD2-BWNAME.
            MOVE LCI-CAN-LOAN TO PD-CAN-LOAN-NO.
            MOVE PD-PROCEEDS  TO PD-CAN-LOAN-MAX.
            MOVE "B"          TO PD-DLPAID.
            MOVE "N"          TO PD-LBOX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * VALIDATE SPR, LNVERI WOULD SEND TO IO-ERROR:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE TO SP-LAWCODE.
           IF SP1-KEY NOT = HOLD-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 MOVE "** BAD SPR, KEY = &&&&&&&&&" TO HOLD-ERR-DESC
                 INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&&&&&&"
                                                 BY SP1-KEY
                 PERFORM PRINT-ERROR-LINE
                 MOVE SPACES TO HOLD-SP1-KEY
                 GO TO NEXT-LCI-REC
              ELSE
                 MOVE SP1-KEY TO HOLD-SP1-KEY.

      *    VERIFY THAT ALL THE FEECODES WITHIN THE CAN LOAN
      *    EXIST FOR THE POSTING BRANCH
            MOVE 0 TO IO-FG.
            PERFORM VARYING SUB FROM 1 BY 1
                    UNTIL (SUB > 10 OR IO-FG = 9)
               IF PD-FEECODE(SUB) NOT = " "
                  MOVE PD-FEECODE(SUB) TO CDB-CODE
                  PERFORM GET-CODE
               END-IF
            END-PERFORM.
            IF IO-BAD
               MOVE "** BRANCH &&&& MISSING THE PROPER FEE CODE: "
                     TO MESS
               INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
               MOVE SPACES TO HOLD-ERR-DESC
               STRING MESS," ",CDB-CODE DELIMITED BY "  "
                     INTO HOLD-ERR-DESC
               PERFORM PRINT-ERROR-LINE
               GO TO NEXT-LCI-REC.

      * CHECK SPR (STATE PARAMETER FILE) EXPIRATION DATE
            IF SP-EFFDATE NOT > PD-LOANDATE
               IF SP-EXPDATE = 0
                  CONTINUE
               ELSE
                  IF SP-EXPDATE NOT > PD-LOANDATE
                     MOVE "** SPR HAS EXPIRED: &&&&&&&&&"
                           TO HOLD-ERR-DESC
                     INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&&&&&&"
                                                  BY SP1-KEY
                     PERFORM PRINT-ERROR-LINE
                     GO TO NEXT-LCI-REC.


      * TEST FOR DUAL LOANS, WE ONLY HAVE ONE BORROWER SO
      * ONLY PERFORMING FOR BORROWER(1) ONLY.
            MOVE 0 TO SUB BISCAN-CALL-CODE DUAL-LOANS-FOUND-FG.
      *     PERFORM DUAL-CHECK 4 TIMES.
            PERFORM DUAL-CHECK.
            IF DUAL-LOANS-FOUND
               MOVE "** BORROWER &&&&&&&&& HAS LOANS IN THE SYSTEM"
                     TO HOLD-ERR-DESC
               INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&&&&&&"
                  BY PD-SSNO(1)
               PERFORM PRINT-ERROR-LINE
               GO TO NEXT-LCI-REC
            END-IF.

      * TEST RC-STATUS FOR OPEN DAY
           MOVE "A1"       TO RC-STATUS.
           MOVE BR-NO      TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.
           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-BAD
              MOVE "N" TO DAY-OPEN-FG
           ELSE
              MOVE "Y" TO DAY-OPEN-FG.

      *  DAY IS OPEN, RESERVE OP FILE SO THE DAY CANNOT BE CLOSED.
           PERFORM LOAD-OP-FILE.
           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "N" TO DAY-OPEN-FG
           ELSE
              MOVE "Y" TO DAY-OPEN-FG
              PERFORM OPEN-OP-FILE.

      * IF DAY NOT OPEN FOR BRANCH, PRINT DETAIL & SKIP UPDATE
           IF DAY-NOT-OPEN
              MOVE "** DAY NOT OPEN FOR BRANCH: &&&&" TO HOLD-ERR-DESC
              INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&" BY BR-NO
              PERFORM PRINT-ERROR-LINE
              GO TO NEXT-LCI-REC.

      * VALIDATE GL INTERFACE
           MOVE BRANCH   TO GI-BRANCH.
           MOVE PD-CLASS TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-BAD
              MOVE "** INVALID GI INTERFACE CLASS &&&" TO HOLD-ERR-DESC
              INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&" BY PD-CLASS
              PERFORM PRINT-ERROR-LINE
              GO TO NEXT-LCI-REC.

      * VALIDATE BORROWER CREDIT SCORE, CHECKS PRIMARY SSNO, CAUSE
      * WE NEED BW-CREDIT-SCORE, OTHER SSNO'S CHECKED IN SFVERI
           PERFORM OPEN-BW1-FILE.
           MOVE PD-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "** INVALID PRIMARY BORROWER" TO HOLD-ERR-DESC
              PERFORM PRINT-ERROR-LINE
              GO TO NEXT-LCI-REC.

      * CALL DIRECT LOAN VERIFY:
           PERFORM SAVE-SCREEN.
           MOVE "LNCRE2" TO LPLOAN-CALLED-FROM.
           MOVE SPACES TO LPLOAN-RETURN-ERR-BUF.
           MOVE "CL/LNVERI" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRORS-EXIST
                                 PROG-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ERRORS-EXIST = "Y"
              MOVE SPACES TO HOLD-ERR-DESC
              STRING "** " RETURN-ERR-BUF
                     DELIMITED BY SIZE
                     INTO HOLD-ERR-DESC
              PERFORM PRINT-ERROR-LINE
              GO TO NEXT-LCI-REC.

           IF REPORT-ONLY-FG = "Y"
              MOVE 0 TO LOAN-KEY
              GO TO NEXT-LCI-REC.

           IF REPORT-ONLY-FG = "A"
              MOVE "LOAN READY FOR UPDATE" TO HOLD-ERR-DESC
              PERFORM PRINT-ERROR-LINE
              SUBTRACT 1 FROM T-NOERRORS
              GO TO NEXT-LCI-REC.


      * CALL DIRECT LOAN UPDATE:
           MOVE "LNCRE2" TO LPLOAN-CALLED-FROM.
           MOVE SPACES TO LPLOAN-RETURN-ERR-BUF.
           MOVE SPACES TO PRLOAN-RESTRICTED.
           MOVE "LP/LONPC2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF
                                 UPDATE-BUF.
           CANCEL FORM-PROGX.

           ADD 1 TO T-NOLOANS.

            IF REPORT-ONLY-FG = "L"
               MOVE "LOAN UPDATED : &&&&&&&&&" TO HOLD-ERR-DESC
               INSPECT HOLD-ERR-DESC REPLACING FIRST "&&&&&&&&&"
                                               BY LOAN-KEY
               PERFORM PRINT-ERROR-LINE
               SUBTRACT 1 FROM T-NOERRORS.

           GO TO NEXT-LCI-REC.

      *****************************************
       INITIALIZATION SECTION.
      *****************************************
           MOVE LNCRE2-QUEUE-POS TO Q-POS.
           MOVE LNCRE2-COPIES-POS TO C-POS.
           MOVE LNCRE2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           MOVE " " TO RECORDS-FOUND.
           MOVE 0 TO T-NOLOANS.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM VERIFY-LIVECHECK-FILES.
           IF REPORT-ONLY-FG = "Y"
              MOVE "REPORT IN PROGRESS......" TO PROGRESS-MESS
           ELSE
              MOVE "REPORT/UPDATE IN PROGRESS....." TO PROGRESS-MESS.
           MOVE " " TO ERRCD.
           MOVE 0 TO T-NOLOANS
                     T-NOERRORS
                     T-NO-OTHER
                     HOLD-BRANCH.
           MOVE SPACES TO HOLD-CAN-LOAN
                          HOLD-CAN-GROUP-ID
                          HOLD-SP1-KEY.
           MOVE "N" TO RECORDS-FOUND.
           MOVE EXT-CONAME-CONAME   TO H-NAME.
           MOVE EXT-CONAME-SYSDATE  TO H-DATE.
           MOVE EXT-CONAME-USER-ID   TO H-USERID.

           MOVE "LNCRE2" TO BICREA-CALLED-FROM.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-LCE-FILE-OUTPUT.
      
           IF EXT-ROUTE-BUF = "00"
               MOVE "REPORT/UPDATE IN PROGRESS............." TO WR-BUF 
               MOVE MESS-LIST TO WR-LIST.
               PERFORM CALL-WRFORM.

      * NEED TO USE MKTEMP HERE FIRST BECAUSE WE NEED THE WORK
      * FILE PATH IN IT LAST FOR THE REMOVE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           GO TO EXIT-INITIALIZE.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************
       ENTER-PARAMETERS SECTION.
      *****************************************

      * NEED TO PUT HERE ELSE WILL NOT DISPLAY PROPERLY DUE TO PFS BATCH LOGIC
           PERFORM SETUP-LIVECHECK-PATHS.
           PERFORM DISPLAY-LIVECHECK-PATHS.

       REPORT-ONLY-YN.
      *   "Y" UPDATE WILL NOT UPDATE THE LOANS, IT WILL ONLY CREATE THE
      *       STATUS FILE INDICATING TOTALS AND CAUGHT EXCEPTIONS
      *       IN THE STATUS FILE
      *   "N" UPDATE WILL UPDATE THE LOANS AND CREATE THE
      *       STATUS FILE INDICATING TOTALS AND CAUGHT EXCEPTIONS
      *       IN THE STATUS FILE
      *   "A" UPDATE WILL NOT UPDATE THE LOANS, IT WILL ONLY CREATE THE
      *       STATUS FILE INDICATING TOTALS, CAUGHT EXCEPTIONS AND WILL
      *       INDICATE WHETHER OR NOT THE LOAN PASSED THE LOAN VERIFICATION
      *       PROCESS IN THE STATUS FILE
      *   "L" UPDATE WILL UPDATE THE LOANS AND CREATE THE
      *       STATUS FILE INDICATING TOTALS, CAUGHT EXCEPTIONS, AND
      *       LOAN NUMBERS IN THE STATUS FILE
           MOVE "E  A010REPORTONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM
           IF VDUSTATUS = "1U" GO TO REPORT-ONLY-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
               GO TO REPORT-ONLY-YN.
           IF NOT (VDUBUF = "Y" OR "N" OR "A" OR "L")
              MOVE "ENTER Y, N, A OR L ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO REPORT-ONLY-YN
           ELSE
             MOVE VDUBUF TO REPORT-ONLY-FG.
       EXIT-PARM.
           EXIT.

      *****************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************
           PERFORM SETUP-LIVECHECK-PATHS.
           PERFORM DISPLAY-LIVECHECK-PATHS.

           MOVE "S  A010REPORTONLY." TO VDU.
           MOVE REPORT-ONLY-FG TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************
       SAVE-SCREEN SECTION.
      *****************************************

      * SAVE PD-REC TO WORK FILE - USED IN LOAN UPDATE

           MOVE 2 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE PD-RECORD TO WK-SAVE-RECORD.
           IF IO-BAD
              PERFORM WRITE-WK-FILE
           ELSE
              PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      *****************************************
       PRINT-ERROR-LINE SECTION.
      *****************************************

      * WRITE EXCEPTIONS TO STATUS FILE

           MOVE LCI-BRNO TO D-ERR-BRNO.
           MOVE LCI-SSNO TO D-ERR-SSNO.
           INSPECT D-ERR-SSNO
                   REPLACING ALL "/" BY "-".
           MOVE HOLD-ERR-DESC TO D-ERR-DESC.
           MOVE "LOAN DATE:" TO D-ERR-DESC2.
           MOVE LCI-LOAN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-ERR-LOAN-DATE.
           MOVE "CAN#:" TO D-ERR-DESC3.
           MOVE LCI-CAN-LOAN TO D-ERR-CAN.
           MOVE "CHECK NO:" TO D-ERR-DESC4.
           MOVE LCI-CHECK-NO TO D-ERR-CHECKNO.
           MOVE DETAIL-LINE TO LCE-REC.
           PERFORM WRITE-LCE-REC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO DETAIL-LINE.
           ADD 1 TO T-NOERRORS.


      *****************************************
       PRINT-INPUT-ERRORS SECTION.
      *****************************************
           MOVE LCR-BRNO TO D-ERR-BRNO.
           MOVE LCR-SSNO TO D-ERR-SSNO.
           INSPECT D-ERR-SSNO
                   REPLACING ALL "/" BY "-".
           MOVE HOLD-ERR-DESC TO D-ERR-DESC.
           MOVE "LOAN DATE:"  TO D-ERR-DESC2.
           MOVE LCR-LOAN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-ERR-LOAN-DATE.
           MOVE "CAN#:"      TO D-ERR-DESC3.
           MOVE LCR-CAN-LOAN TO D-ERR-CAN.
           MOVE "CHECK NO:"  TO D-ERR-DESC4.
           MOVE LCR-CHECK-NO TO D-ERR-CHECKNO.
           MOVE DETAIL-LINE TO LCE-REC.
           PERFORM WRITE-LCE-REC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************
       PRINT-GRAND-TOTALS SECTION.
      *****************************************
           MOVE SPACES TO LCE-REC.
           PERFORM WRITE-LCE-REC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL LOANS REJECTED         :" TO D-TOTAL-DESC.
           MOVE T-NOERRORS TO D-TOTAL-NO.
           MOVE DETAIL-LINE TO LCE-REC.
           PERFORM WRITE-LCE-REC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL LOANS UPDATED          :" TO D-TOTAL-DESC.
           MOVE T-NOLOANS TO D-TOTAL-NO.
           MOVE DETAIL-LINE TO LCE-REC.
           PERFORM WRITE-LCE-REC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL LOANS ON OTHER MACHINES:" TO D-TOTAL-DESC.
           MOVE T-NO-OTHER TO D-TOTAL-NO.
           MOVE DETAIL-LINE TO LCE-REC.
           PERFORM WRITE-LCE-REC.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************
       PRINT-RANGE-LINES SECTION.
      *****************************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 4 TO LN-FEED.
           MOVE "REPORT TYPE:" TO RANGE-LINE.
           MOVE  REPORT-ONLY-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************
       HEAD-PAGE SECTION.
      *****************************************
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF SUMMARY-FG = "N"
              MOVE HEAD3  TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 4 TO LN-COUNT
              MOVE 2 TO LN-FEED
            ELSE
              MOVE 3 TO LN-COUNT
              MOVE 2 TO LN-FEED.

      *****************************************
       SETUP-LCR-FILE SECTION.
      *****************************************

      * READ RELATIVE CHECK FILE IN AND WRITE TO INDEXED FILE
      * SORTED BY BRANCH. THE INDEXED FILE WILL BE THE DRIVER
      * FOR THIS UPDATE
           PERFORM OPEN-LCR-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO LCI-PATH.
           PERFORM OPEN-LCI-FILE-OUTPUT.
           PERFORM CLOSE-LCI-FILE.
           PERFORM OPEN-LCI-FILE.

           PERFORM OPEN-LCE-FILE-OUTPUT.

       SETUP-LCR-FILE-NEXT.
           PERFORM READ-LCR-FILE-NEXT.
              IF IO-BAD
                 PERFORM CLOSE-LCR-FILE
                 GO TO SETUP-LCR-FILE-EXIT.
           IF LCR-BRNO NOT NUMERIC
              MOVE "ERROR IN INPUT FILE BRNO = '&&&&'" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY LCR-BRNO
              IF EXT-ROUTE-BUF = "00"
                 PERFORM SEND-MESS
              ELSE
                 MOVE MESS TO HOLD-ERR-DESC
                 PERFORM PRINT-INPUT-ERRORS
              END-IF
              GO TO END-PROGRAM.
           IF LCR-LOAN-DATE NOT NUMERIC
              MOVE "ERROR IN INPUT FILE DATE = '&&&&&&&&'" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&" BY LCR-LOAN-DATE
              IF EXT-ROUTE-BUF = "00"
                 PERFORM SEND-MESS
              ELSE
                 MOVE MESS TO HOLD-ERR-DESC
                 PERFORM PRINT-INPUT-ERRORS
              GO TO END-PROGRAM.
           IF LCR-CHECK-NO NOT NUMERIC
              MOVE "ERROR IN INPUT FILE CHECK = '&&&&&&&&&&'" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&" BY LCR-CHECK-NO
              IF EXT-ROUTE-BUF = "00"
                  PERFORM SEND-MESS
              ELSE
                 MOVE MESS TO HOLD-ERR-DESC
                 PERFORM PRINT-INPUT-ERRORS
              GO TO END-PROGRAM.
           IF LCR-CAN-LOAN = "  "
              MOVE "ERROR IN INPUT FILE CAN LOAN = '&&'" TO MESS
              INSPECT MESS REPLACING FIRST "&&" BY LCR-CAN-LOAN
              IF EXT-ROUTE-BUF = "00"
                  PERFORM SEND-MESS
              ELSE
                 MOVE MESS TO HOLD-ERR-DESC
                 PERFORM PRINT-INPUT-ERRORS
              GO TO END-PROGRAM.
           IF LCR-SSNO NOT NUMERIC
              MOVE "ERROR IN INPUT FILE SSNO = '&&&&&&&&&'" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&" BY LCR-SSNO
              IF EXT-ROUTE-BUF = "00"
                  PERFORM SEND-MESS
              ELSE
                 MOVE MESS TO HOLD-ERR-DESC
                 PERFORM PRINT-INPUT-ERRORS
              GO TO END-PROGRAM.
           MOVE LCR-REC TO LCI-REC.
           PERFORM WRITE-LCI-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           GO TO SETUP-LCR-FILE-NEXT.
       SETUP-LCR-FILE-EXIT.
           EXIT.

      *****************************************
       SETUP-LIVECHECK-PATHS SECTION.
      *****************************************

            MOVE EXT-FILPATH-GLOBAL TO LCR-PATH-GLOBAL
                                       LCR-DONE-GLOBAL
                                       LCE-PATH-GLOBAL.
      *  CONVERT LOWER CASE "FTP" IN THE PATHS TO LOWER CASE.
      *  THIS WILL BE NEEDED WHEN CONVERTING THE PROGRAM TO A30
      *  OR IF THE PROGRAM MISTAKINGLY GETS CONVERTED TO UPPER CASE.
            CALL "C$TOLOWER" USING LCR-PATH-FTP VALUE 4.
            CALL "C$TOLOWER" USING LCE-PATH-FTP VALUE 4.
            CALL "C$TOLOWER" USING LCR-DONE-PATH-FTP VALUE 4.

      * USE TODAYS DATE AS TRANS-DATE TO TEST FOR OPEN DAY.
      * THIS MAY DIFFER FROM LOAN DATE IN INPUT FILE
      * SET DATE FOR DATED BACKUP OF INPUT (LCR) FILE
            MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
            PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
            MOVE DATE-YYYYMMDD TO TRANS-DATE LCR-DONE-DATE.

      *****************************************
       DISPLAY-LIVECHECK-PATHS SECTION.
      *****************************************

      * DISPLAY FILE PATH INFO ON THE SCREEN FOR INPUT FILE
      * AND STATUS FILE
           MOVE LCR-PATH TO DISP-FILE-PATH-FILE01.
           MOVE LCE-PATH TO DISP-FILE-PATH-FILE02.
           MOVE DISP-FILE-PATH-LIST TO WR-LIST.
           MOVE DISP-FILE-PATH-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      *****************************************
       VERIFY-LIVECHECK-FILES SECTION.
      *****************************************

      * CHECK TO SEE IF STATUS FILE
      *     USR/DATA/<MACHINE>/FTP/LIVECHECK/DONE/LCDONE.YYYYMMDD
      * ALREADY EXISTS, IF IT DOES THEN THE UPDATE HAS ALREADY BEEN RUN FOR
      * TODAY, ABORT
            PERFORM OPEN-LCE-FILE-OUTPUT.
            MOVE LCR-DONE-PATH TO ACCESS-BUF.
            PERFORM ACCESS-CALL.
            IF STAT = "00"
               MOVE "** CHECKS ALREADY UPDATED FOR &&&&&&&&" TO MESS
               INSPECT MESS REPLACING FIRST "&&&&&&&&" BY LCR-DONE-DATE
               MOVE MESS TO LCE-REC
               PERFORM WRITE-LCE-REC
               PERFORM SEND-MESS
               GO TO END-PROGRAM.

      * CHECK TO SEE INPUT FILE
      *     USR/DATA/<MACHINE>/FTP/LIVECHECK/LIVECHECK.IN
      * ALREADY EXISTS, IF F IT DOES THEN THE UPDATE HAS ALREADY BEEN RUN FOR
      * TODAY, ABORT
            MOVE LCR-PATH TO ACCESS-BUF.
            PERFORM ACCESS-CALL.
            IF STAT NOT = "00"
               MOVE "** CANNOT FIND LIVE CHECK INPUT FILE" TO MESS
               MOVE MESS TO LCE-REC
               PERFORM WRITE-LCE-REC
               PERFORM SEND-MESS
               GO TO END-PROGRAM.

      *******************************************
       GET-CODE SECTION.
      *******************************************
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-BAD
              MOVE " * NOT ON FILE * " TO CDB-DESC.

           IF PD-GIFTNET = "Y"
              IF CDB-GC-FEE-GIFTNET = "Y"
                 NEXT SENTENCE
              ELSE
                 MOVE 9 TO IO-FG
           ELSE
              IF CDB-GC-FEE-GIFTNET = "Y"
                 MOVE 9 TO IO-FG.

      ******************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************
           PERFORM SETUP-LIVECHECK-PATHS.
           PERFORM VERIFY-LIVECHECK-FILES.
       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           MOVE EXT-CONAME-CONAME TO LNCRE2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNCRE2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LNCRE2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNCRE2-FORM-FIELDS TO WR-BUF.
           MOVE LNCRE2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE LNCRE2-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNCRE2-HELP--DIR  TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LNCRE2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       DUAL-CHECK SECTION.
      ******************************************

      * CHECK FOR DUAL LOANS.

           ADD 1 TO SUB.
           IF NOT (( PD-SSNO(SUB)     = ZEROES ) OR
                   ( BISCAN-CALL-CODE = 9      ))
              MOVE PD-SSNO(SUB) TO BISCAN-SSNO
              MOVE PD-SRCD      TO BISCAN-SRCD
              MOVE PD-PRLNNO    TO BISCAN-PRLNNO
              IF (SP-DUAL-IN-STATE  NOT = "Y" OR
                  SP-DUAL-IN-BRANCH NOT = "Y" )
                    MOVE 2 TO BISCAN-CALL-CODE
                    PERFORM CALL-SCAN
              END-IF
              IF BISCAN-CALL-CODE = 9
                 MOVE 9 TO DUAL-LOANS-FOUND-FG
              END-IF
           END-IF.

      ******************************************
       CALL-SCAN SECTION.
      ******************************************

      * CALL BISCAN FOR DUAL LOANS VERIFICATION.

           MOVE "LNCRE2" TO EXT-CALLED-FROM.
           MOVE SPACES TO EXT-RETURN-ERR-BUF.
           MOVE SP-DUAL-IN-STATE  TO BISCAN-STATE-FG.
           MOVE SP-DUAL-IN-BRANCH TO BISCAN-BRANCH-FG.
           MOVE PD-OWNBR          TO BISCAN-BRANCH.
           MOVE PD-ORGST          TO BISCAN-STATE.
           MOVE TRANS-DATE        TO BISCAN-TRANS-DATE.
           MOVE "BISCAN"          TO FORM-PROGX.
           CALL FORM-PROGX USING FORM-PATH
                                 BISCAN-WORKERS
                                 EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE SPACES TO EXT-CALLED-FROM
                          EXT-RETURN-ERR-BUF.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************
       MISC-PARA SECTION.
      ******************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************
       IO-ROUTINE SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPOPIN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPCL1RN.CPY".
       COPY "LIBLP/LPWKIN.CPY".

       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-FILE.

       OPEN-LCR-FILE.
           PERFORM OPEN-IT.
           MOVE LCR-PATH TO E-FILE.
           OPEN INPUT LCR-FILE.
           IF IO-FG = 8
              GO TO OPEN-LCR-FILE.
           IF IO-FG = 7
              CLOSE LCR-FILE
              GO TO OPEN-LCR-FILE.
       READ-LCR-FILE.
           PERFORM READ-IT.
           MOVE LCR-PATH TO E-FILE.
           READ LCR-FILE.
           IF IO-FG = 8
              GO TO READ-LCR-FILE.
       READ-LCR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LCR-PATH TO E-FILE.
           READ LCR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-LCR-FILE-NEXT.
       CLOSE-LCR-FILE.
           PERFORM CLOSE-IT.
           CLOSE LCR-FILE.

       OPEN-LCE-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE LCE-PATH TO E-FILE.
           OPEN OUTPUT LCE-FILE.
           IF IO-FG = 8
              GO TO OPEN-LCE-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE LCE-FILE
              GO TO OPEN-LCE-FILE-OUTPUT.
       WRITE-LCE-REC.
           PERFORM WRITE-IT.
           MOVE LCE-PATH TO E-FILE.
           WRITE LCE-REC.
           IF IO-FG = 8
              GO TO WRITE-LCE-REC.
       CLOSE-LCE-FILE.
           PERFORM CLOSE-IT.
           CLOSE LCE-FILE.

       OPEN-LCI-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE LCI-IFN TO E-FILE.
           OPEN OUTPUT LCI-FILE.
           IF IO-LOCKED
             GO TO OPEN-LCI-FILE-OUTPUT.
           IF IO-ALREADY-OPEN
             CLOSE LCI-FILE
             GO TO OPEN-LCI-FILE-OUTPUT.
           UNLOCK LCI-FILE.
       OPEN-LCI-FILE.
           PERFORM OPEN-IT.
           MOVE LCI-IFN TO E-FILE.
           OPEN I-O LCI-FILE.
           IF IO-LOCKED GO TO OPEN-LCI-FILE.
           IF IO-ALREADY-OPEN
              CLOSE LCI-FILE
              GO TO OPEN-LCI-FILE.
           UNLOCK LCI-FILE.
       READ-LCI-FILE.
           PERFORM READ-IT.
           MOVE LCI-IFN TO E-FILE.
           MOVE LCI-KEY TO E-KEYX.
           READ LCI-FILE.
           IF IO-LOCKED GO TO READ-LCI-FILE.
       READ-LCI-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LCI-IFN TO E-FILE.
           MOVE LCI-KEY TO E-KEYX.
           READ LCI-FILE NEXT.
           IF IO-LOCKED GO TO READ-LCI-FILE-NEXT.
       START-LCI-FILE.
           PERFORM START-IT.
           MOVE LCI-IFN TO E-FILE.
           MOVE LCI-KEY TO E-KEYX.
           START LCI-FILE KEY NOT < LCI-KEY.
           UNLOCK LCI-FILE.
       WRITE-LCI-FILE.
           PERFORM WRITE-IT.
           MOVE LCI-IFN TO E-FILE.
           MOVE LCI-KEY TO E-KEYX.
           WRITE LCI-REC.
           IF IO-LOCKED GO TO WRITE-LCI-FILE.
           UNLOCK LCI-FILE.
       CLOSE-LCI-FILE.
           PERFORM CLOSE-IT.
           CLOSE LCI-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************
       END-PROGRAM SECTION.
      ***********************************
       END-RUN.
           IF NOT (EXT-ROUTE-BUF = "40" OR "50" OR "60")
              PERFORM PRINT-RANGE-LINES.

           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "40" OR "50" OR "60"
              GO TO EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *  IF RUNNING THE PROCEDURE WITH UPDATE THEN MOVE THE INPUT
      *  FILE TO DONE/LCDONE.YYYYMMDD
      *  DO NOT MOVE FILE IF NONE OF THE LOANS UPDATED.
           IF (REPORT-ONLY-FG = "N" OR "L") AND
                       (T-NOLOANS > 0)
              MOVE SPACES TO SYSTEM-BUF
              STRING MV-CMD " " LCR-PATH " " LCR-DONE-PATH
                     DELIMITED BY SIZE
                     INTO SYSTEM-BUF
      *  USE CALL "SYSTEM" HERE, SYSTEM-CALL DID NOT CORRECTLY
      *  PERFORM THE OPERATION, INSTEAD OF PUTTING THE FILE
      *  IN THE DONE FOLDER IT PLACED IT UNDER THE FTP FOLDER
      *  AND CALLED IT LIVECHEC2
              CALL "SYSTEM" USING SYSTEM-BUF GIVING EXIT-STATUS
              END-CALL
              IF EXIT-STATUS NOT = 0
                 MOVE "ERROR MOVING INPUT FILE TO LIVECHECK.&&&&&&&&"
                           TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&&&&&"
                               BY LCR-DONE-DATE
                 PERFORM SEND-MESS.
       EXIT-PROG.
      * BIXCCREA-CALLED-FROM IS EXTERNAL SO NEEDS TO BE RESET UPON LEAVING
      * HERE ELSE SUBSEQUENT CALLES TO BICREA WILL NOT WORK AS INTENDED.
           MOVE SPACES TO BICREA-CALLED-FROM.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LNCRE2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

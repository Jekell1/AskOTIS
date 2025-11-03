       IDENTIFICATION DIVISION.
       PROGRAM-ID.       AUTOC2.
       DATE-WRITTEN.     12/6/05.
      ******************************************************************
      *   DIR :  SP
      *   DESC:  FORCE EOD CLOSE & RUN EOD UPDATE & BATCH FROM CRON
      *
      ***************************************************************
      ****** COPIED FROM AUTOCL ON 03/21/2020. WORLD DID NOT WANT TO CLOSE
      ****** THE DAY IF UNCOMPLETED RENEWALS, CASH DRAWER OPEN, CREATE CHECKS
      ****** IF CX EXISTED. ALSO NEED TO ADD CHECK FOR GL 1497000 IN BALANCE
      *****************************************************************
      *          STARTED WITH SP/EOCRON.C.   ADDED LOGIC FROM LONPR0
      *          TO CALL LPEOD5.  SO THIS RUNS FROM A SHELL, SIMILAR TO
      *          EOCRON, BUT 1ST ARGUMENT IS THE GROUP ID INSTEAD OF 
      *          THE BATCH ID.  EXAMPLE OF A SHELL TO RUN THIS PROGRAM
      *          (THE LAST 3 LINES ARE ALL 1 LINE):
      *        
      *          SH/FORCECLOSE.CRON
      *        EXPORT POSTNAME=A9990PFS
      *        EXPORT LOGNAME=A9990PFS
      *        EXPORT FIL=US2/DA34/R1/SC9990
      *        EXPORT REL=USR/S35
      *        EXPORT TERM=VT220
      *        CD /$FIL
      *        PATH=/USR/LOCAL/BIN:/USR/BIN:/$REL/SH:/USR/LOCAL/SBIN:
      *              /USR/SBIN:/OPT/ACUCORP/104/BIN:/ETC
      *        EXPORT PATH
      *        EXPORT LD_LIBRARY_PATH=/OPT/ACUCORP/104/LIB
      *        RUNCBL -C /$REL/SH/XCBLCONFIG +E /$FIL/../LG/ACU_TCLP
      *                  /$REL/CO/AUTOC2 ALL -00001 +00000 0001 9999
      *
      *           THIS SHELL HAS OFFSET OF -1, SHOULD FORCE CLOSE DAYS FROM
      *           THE NIGHT BEFORE.  EVERYTHING SHOULD WORK PRETTY MUCH
      *           LIKE A NORMAL END-OF-DAY.  THE CASH DRAWERS ARE FORCED
      *           CLOSED IF NECESSARY.  UNPRINTED CHECKS ARE UPDATED
      *           TO CK, TY & TR.  ERRORS & LOGS FOR THIS PROCEDURE ARE
      *           UPDATED TO GLOBAL SP/AUTOCLOSE (AUTOCLOSE IS IN CAPS).
      *           THERE YOU WILL SEE LOGS & ERRORS, FOR EXAMPLE:
      *
      *   /USR/DATA/R1/SP/AUTOCLOSE
      *   LINUX A30$ CAT LOG.141106
      *   09:34 AM BR #   1  11/05/14  *** BEGINNING END-OF-DAY PROCESSING ***
      *   09:34 AM BR #   1  11/05/14  FORCED CLOSE OF CASH DRAWER # 1
      *   09:34 AM BR #   1  11/05/14  END OF DAY UPDATE SUCCESSFUL
      *   09:34 AM BR #   2  11/05/14  *** BEGINNING END-OF-DAY PROCESSING ***
      *   09:34 AM BR #   2  11/05/14  FORCED CLOSE OF CASH DRAWER # 1
      *   09:34 AM BR #   2  11/05/14  END OF DAY UPDATE SUCCESSFUL
      *
      *
      * REV:
      * BLF 060309 FIX PROBLEM WITH STATUS 93 ON OPFILE (SOMEONE STILL IN
      *            DAILY PROCESSING) CAUSING LOCKUP DUE TO DECLARATIVES
      *            TRYING TO DO ERRMSG & LOCKING UP CRON PROCESS.  MOVED
      *            AUTOCL TO ERRLOGW-PROGRAM (THIS IS IN ERRLOGW COPIED BY
      *            FILEWK) & CHANGED DECLARP TO CHECK FOR PROGRAM
      *  BAH 061107 ADDED CHECK ON CX-RESCISSION = "U" IF THE CX-DATE DOES
      *             NOT MATCH THE TRANS-DATE, IT STILL NEEDS TO STOP YOU
      *             FROM CLOSING. THESE ARE CHECKS THAT CAME FROM A LOAN THAT
      *             WAS JUST UPDATED OUT OF RESCISSION AND THEY MUST PRINT
      *             THESE CHECKS. THEY WERE FALLING THRU CAUSE THE CX-DATE
      *             IS BACK WHEN THE LOAN WAS BOOKED, SO WE NOW FLAG THEM
      *             WITH CX-RESCISSION = "U" SO WE CAN FIND THEM, REGWAN PL#556
      *  BLM 100706 ADDED CHMOD ON TRFILE, IF DAY IS OPEN BUT NO TRANSACTIONS,
      *             TRFILE WAS GETTING CREATED WITH RW-X-X. THIS MUST BE
      *             IF BRANCH WAS AUTO-OPENED THRU RCOPEN & NO ONE POSTED.
      *             REGACC
      *  BAH 140516 REMOVED REL FROM BPEOD-CMD, C$TOLOWER ON BPEOD-PATH
      *   CS 140528 REPLACED MKDIR-CMD WITH C$MAKEDIR
      *  BLM 140509 FIXED ALL CHMODS TO BE 770 INSTEAD OF 777 IF I-AM-REGACC,
      *             GOT RID OF AUTOCL.CMD & EMBEDDED 4 CHMOD ROUTINES IN
      *             COBOL, CHANGED MKDIR SYSTEM CALLS TO USE C$MAKEDIR,
      *             ADD CHMOD OF UP FLAG, REGACC PR# 353
      *   CS 150529 REMOVED CLOSE-PATH/UPDATE-PATH AND NOW USE THE 'RC' FILE.
      *             STILL NEED THE OP-FG FOR LOCKING
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BLM 160311 FIXED FOR NEW TY-DATE FORMAT, PD0003
      * BLM 160513 FIXED FOR NEW DS-DATE FORMAT, PD0003
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * KEC 2017.0209 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED SETTING HOLD-DT-BRNO IN THE 
      *         CREATE-DT-HISTORY SECTION.
      * BAH 2017.0313 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0912 PR#1246 WHEN ADDING THIS PR# TO A30, I NOTICED THAT A 
      *               COPY MEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *               - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *               - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *               - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      * BAH 2018.1010 GLOBAL CXFILE
      * BAH 2018.1029 GLOBAL DTFILE
      * BAH 2022.0816 COMMENTED OUT LOGUID
      * JKC 2024.0131 ADDED A 'PERFORM CLOSE-RC2-FILE' BEFORE THE
      *              'GO TO END-PROGRAM' AFTER THE
      *              'PERFORM READ-RC2-FILE-NEXT' IN THE MAIN-PROGRAM SECTION
      *              JUST AS A SAFEGUARD TO PREVENT A POTENTIAL 
      *              'INVALID CURSOR STATE' ERROR.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSOP_LCK.CPY".
       COPY "LIBLP/LPFSED.CPY".
       COPY "LIBLP/LPFSER.CPY".
       COPY "LIBGB/GBFSER.CPY".
           SELECT CLOSELOG-FILE ASSIGN CLOSELOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDOP.CPY".
       COPY "LIBLP/LPFDED.CPY".
       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDER.CPY".
      **************************************************
      *  AUTO CLOSE LOG FILE                        *
      **************************************************
       FD  CLOSELOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  CLOSELOG-REC.
           03  CLOSELOG-TIME     PIC X(8).
           03  FILLER            PIC X.
           03  CLOSELOG-BRHEAD   PIC X(4) VALUE "BR# ".
           03  CLOSELOG-BRNO     PIC Z(4).
           03  FILLER            PIC XX.
           03  CLOSELOG-DATE     PIC 99/99/99 BLANK ZERO.
           03  FILLER            PIC XX.
           03  CLOSELOG-MSG      PIC X(51).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/AUTOC2.CBL S35 02/22/24-08:16:01 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LPWSER.CPY".
       COPY "LIBGB/GBWSER.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       01  ER-FEED                     PIC 9     VALUE 1.
       01  CLOSING-ERRCD               PIC X VALUE SPACES.

       01  HOLD-EXT-FILPATH            PIC X(19).

       01 CLOSELOG-PATH.
          03 CLOSELOG-PATH-DIRECTORY.
             05 CLOSELOG-PATH-BASE    PIC X(09).
             05 FILLER                PIC X(01) VALUE "/".
             05 CLOSELOG-PATH-MACHINE PIC X(02).
             05 FILLER                PIC X(14) VALUE "/SP/AUTOCLOSE/".
          03 CLOSELOG-PATH-FILENAME.
             05 CLOSELOG-PATH-TYPE    PIC X(03).
             05 FILLER                PIC X(01) VALUE ".".
             05 CLOSELOG-PATH-YY      PIC 9(02).
             05 CLOSELOG-PATH-MM      PIC 9(02).
             05 CLOSELOG-PATH-DD      PIC 9(02).

       01  ERR-NUM                     PIC 9(9)  COMP-4.

       01  ERRCD            PIC X VALUE SPACES.
       01  HOLD-USERID.
           03  FILLER       PIC X.
           03  USERID-BR    PIC 9(4).
           03  FILLER       PIC XXX.

      * MUST MATCH LPEOD5
       01  LPEOD5-BUF.
           03  LPEOD5-BRNAME     PIC X(20).
           03  LPEOD5-PCODE      PIC XXX.
           03  LPEOD5-CONAME     PIC X(20).
           03  LPEOD5-BRANCH     PIC 9(4).
           03  LPEOD5-TRANS-DATE PIC 9(8).
           03  FILLER REDEFINES LPEOD5-TRANS-DATE.
               05  LPEOD5-TRANS-CC    PIC 99.
               05  LPEOD5-TRANS-YEAR  PIC 99.
               05  LPEOD5-TRANS-MONTH PIC 99.
               05  LPEOD5-TRANS-DAY   PIC 99.
           03  LPEOD5-SYSDATE    PIC X(8).
           03  LPEOD5-SEL        PIC 99.
           03  LPEOD5-USERID     PIC X(10).
           03  LPEOD5-ERRCD      PIC X.

       COPY "LIBGB/EOBUF_EXT.CPY".

       01  GROUP-FLAG                  PIC X    VALUE "Y".
       01  FIRST-STEP                  PIC 9999 VALUE 1.
       01  LAST-STEP                   PIC 9999 VALUE 9999.
       01  CURRENT-STEP                PIC 9999.
       01  DISPLAY-STEP                PIC 9999.
       01  LOAD-STAT                   PIC X(6).
       01  FIRST-TIME                  PIC X(6).
       01  PCODE                       PIC XXX.
       01  DATE1                       PIC 9(8).
       01  FILLER REDEFINES DATE1.
           03  DATE1-CC                PIC 99.
           03  DATE1-YY                PIC 99.
           03  DATE1-MM                PIC 99.
           03  DATE1-DD                PIC 99.
       01  DATE2                       PIC 9(8).
       01  FILLER REDEFINES DATE2.
           03  DATE2-CC                PIC 99.
           03  DATE2-YY                PIC 99.
           03  DATE2-MM                PIC 99.
           03  DATE2-DD                PIC 99.
       01  BEG-RC-TRANS-DATE           PIC 9(8).
       01  END-RC-TRANS-DATE           PIC 9(8).
       01  HOUR-BUF                    PIC 99.
       01  MIN-BUF                     PIC 99.

       01  AUDIT-ERROR                 PIC 9  VALUE 0.
       01  SECURITY-ERROR              PIC 9  VALUE 0.
       01  STEP-COUNT                  PIC 9999.
       01  SKIP-SWITCH                 PIC 9  VALUE 0.
       01  SKIP-STEP                   PIC 9999 VALUE 0.
       01  SUB                         PIC 9999 VALUE 1.

       01  WS-PROG-PATHNAME.
           03  WS-PGM-DIR              PIC XX.
           03  WS-PGM-SL               PIC X    VALUE "/".
           03  WS-PGM-NAME             PIC X(6).

       01  ACC-TIME                PIC 9(8).
       01  THIS REDEFINES ACC-TIME.
           03  ACC-HR              PIC 99.
           03  ACC-MN              PIC 99.
           03  ACC-SC              PIC 99.
           03  ACC-MS              PIC 99.
       01  CUR-DATE                PIC 9(8).
       01  CUR-DATEX REDEFINES CUR-DATE.
           03  CUR-CC              PIC 99.
           03  CUR-YY              PIC 99.
           03  CUR-MO              PIC 99.
           03  CUR-DD              PIC 99.
       01  CUR-TIME                PIC 9(6).
       01  THIS REDEFINES CUR-TIME.
           03  CUR-HR              PIC 99.
           03  CUR-MN              PIC 99.
           03  CUR-SC              PIC 99.

       01  HOLD-RC-OVER-SHORT          PIC S9(9)V99.
       01  HOLD-RC-DEPOSIT             PIC S9(9)V99.
       01  HOLD-RC-ERRCD               PIC X.

       01  HOLD-RC2-KEY.
           03  HOLD-RC-STATUS                  PIC X(02).
           03  HOLD-RC-BRNO                    PIC 9(04).
           03  HOLD-RC-TRANS-DATE              PIC 9(08).
           03  FILLER                REDEFINES HOLD-RC-TRANS-DATE.
               05  HOLD-RC-TRANS-DATE-CC       PIC 9(02).
               05  HOLD-RC-TRANS-DATE-YY       PIC 9(02).
               05  HOLD-RC-TRANS-DATE-MM       PIC 9(02).
               05  HOLD-RC-TRANS-DATE-DD       PIC 9(02).

       01  WK-BEGBAL                   PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-RUNNING                  PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-NET-CASH-NOTES           PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-ENDBAL                   PIC S9(7)V99 COMP-3 VALUE +0.
       01  WK-DEPOSIT                  PIC S9(7)V99 COMP-3 VALUE +0.

       01  TODAYS-CKWRITTEN-TAB.
           03  CKWRITTEN      OCCURS 8 PIC S9(7)V99 COMP-3.
       01  BATCH-ERROR                 PIC X.

       01  EORUN-ERROR.
            03  EORUN-ERR-PROG   PIC X(10).
            03  EORUN-ERR-MSG    PIC X(10).

       01  EDIT-LN-ACCTNO              PIC 999999/99.

       01  CHMOD-REPORTS-COMMAND.
           03  CHMOD-REPORTS-CMD         PIC X(6) VALUE "CHMOD ".
           03  CHMOD-REPORTS-ARGS        PIC 9(3) VALUE 777.
           03  FILLER                    PIC X    VALUE " ".
           03  CHMOD-REPORTS-BASE        PIC X(9).
           03  FILLER                    PIC X    VALUE "/".
           03  CHMOD-REPORTS-MACHINE     PIC XX.
           03  FILLER                    PIC X     VALUE "/".
           03  CHMOD-REPORTS-DATA        PIC X(6).
           03  FILLER                    PIC X(4)  VALUE "/EO/".
           03  CHMOD-REPORTS-PCODE       PIC X(3).
           03  CHMOD-REPORTS-DATE-MMDDYY PIC 9(6).
           03  CHMOD-REPORTS-STAR        PIC X(2)  VALUE "/*".

       01  CHMOD-CLOSELOG-COMMAND.
           03  CHMOD-CLOSELOG-CMD      PIC X(6) VALUE "CHMOD ".
           03  CHMOD-CLOSELOG-ARGS     PIC 9(3) VALUE 777.
           03  FILLER                  PIC X    VALUE " ".
           03  CHMOD-CLOSELOG-BASE     PIC X(9).
           03  FILLER                  PIC X    VALUE "/".
           03  CHMOD-CLOSELOG-MACHINE  PIC XX.
           03  FILLER                  PIC X(13) VALUE "/SP/AUTOCLOSE".
           03  CHMOD-CLOSELOG-STAR     PIC X(02) VALUE "/*".

       01  CHMOD-ERRLOG-COMMAND.
           03  CHMOD-ERRLOG-CMD          PIC X(6) VALUE "CHMOD ".
           03  CHMOD-ERRLOG-ARGS         PIC 9(3) VALUE 777.
           03  FILLER                    PIC X    VALUE " ".
           03  CHMOD-ERRLOG-BASE         PIC X(9).
           03  FILLER                    PIC X     VALUE "/".
           03  CHMOD-ERRLOG-MACHINE      PIC XX.
           03  FILLER                    PIC X(08) VALUE "/LG/ERR.".
           03  CHMOD-ERRLOG-DATE-YYMMDD  PIC 9(06).


       01  CHMOD-TRFILE-COMMAND.
           03  CHMOD-TRFILE-CMD        PIC X(6) VALUE "CHMOD ".
           03  CHMOD-TRFILE-ARGS       PIC 9(3) VALUE 777.
           03  FILLER                  PIC X    VALUE " ".
           03  CHMOD-TRFILE-BASE       PIC X(9).
           03  FILLER                  PIC X    VALUE "/".
           03  CHMOD-TRFILE-MACHINE    PIC XX.
           03  FILLER                  PIC X    VALUE "/".
           03  CHMOD-TRFILE-DATA       PIC X(6).
           03  FILLER                  PIC X(6)  VALUE "/ED/TR".
           03  CHMOD-TRFILE-MONTH      PIC 9(2).
           03  CHMOD-TRFILE-YEAR       PIC 9(2).


      *********************************
      * BULK XFER EOD CHECK  - COMMANDS
      *********************************
      * "/$REL/SH/EC /$REL/SH/BULKEOD.SH ".
       01  BULKEOD-CMD.
           03  FILLER              PIC X    VALUE "/".
           03  BULKEOD-REL         PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  BULKEOD-EC          PIC X(3) VALUE "EC ".
           03  FILLER              PIC X    VALUE "/".
           03  BULKEOD-REL2        PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  BULKEOD-SHELL       PIC X(11) VALUE "BULKEOD.SH ".
           03  BULKEOD-BR          PIC 9(4).
           03  FILLER              PIC X     VALUE " ".
           03  BULKEOD-DATA        PIC X(18).

       01  BULKEOD-PATH.
           03  FILLER          PIC X(16) VALUE
               "/USR/TMP/BULKEOD".
           03  BULKEOD-PATH-BR PIC 9999.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".

       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBCL/CK1497W.CPY".

       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).

       01  GROUP-ID            PIC X(4).
       01  BEG-OFFSET.
           03  BEG-OFFSET-SIGN PIC X.
           03  BEG-OFFSET-DAYS PIC 9(5).
       01  BEG-DATE            REDEFINES BEG-OFFSET
                               PIC 9(6).
       01  END-OFFSET.
           03  END-OFFSET-SIGN PIC X.
           03  END-OFFSET-DAYS PIC 9(5).
       01  END-DATE            REDEFINES END-OFFSET
                               PIC 9(6).

       01  BEG-STEPX.
           05  BEG-STEP        PIC 9(4).
       01  END-STEPX.
           05  END-STEP        PIC 9(4).

       PROCEDURE DIVISION CHAINING GROUP-ID
                                   BEG-OFFSET
                                   END-OFFSET
                                   BEG-STEPX
                                   END-STEPX.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               OP-FILE ED-FILE ER-FILE CLOSELOG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-EO1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               OP-FILE ED-FILE ER-FILE CLOSELOG-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ****************************************
       MAIN-PROGRAM SECTION.
      ****************************************

      * SQL-CONNECT MUST TAKE PLACE AFTER SETENV IS CALLED FURTHER DOWN

       MAIN-MODULE.
      D    STOP MESS.

           MOVE "/   /   /  /" TO FORM-PATH.
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-BAD
              MOVE "ENV ERROR: REL" TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.

           MOVE GETENV-BUF TO FORM-REL.

           MOVE "CO" TO FORM-OBJ.
           MOVE "SP/AUTOC2" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH.
           CANCEL FORM-PROGX.

      * THIS MUST STAY HERE! SETENV MUST BE CALLED FIRST
           PERFORM SQL-CONNECT.

           MOVE "SP/AUTOC2" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SP" TO ER-DIR.
           MOVE "AUTOC2" TO ER-PROGRAM.

      * GET SHELLS IN PROPER CASE
           CALL "C$TOLOWER" USING CHMOD-REPORTS-COMMAND, VALUE 5.
           CALL "C$TOLOWER" USING CHMOD-CLOSELOG-COMMAND, VALUE 5.
           CALL "C$TOLOWER" USING CHMOD-ERRLOG-COMMAND, VALUE 5.
           CALL "C$TOLOWER" USING CHMOD-TRFILE-COMMAND, VALUE 5.

           MOVE EXT-FILPATH-BASE TO CLOSELOG-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO CLOSELOG-PATH-MACHINE.
           MOVE EXT-FILPATH TO HOLD-EXT-FILPATH.

      * CHECK AUTOCLOSE DIR

           MOVE CLOSELOG-PATH-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 STRING
                 "CANNOT CREATE AUTOCLOSE DIRECTORY: "
                 DIR-ACCESS-BUF
                          DELIMITED BY SIZE INTO ERRLOG-KEY
                 PERFORM UPDATE-LG-LOG
              ELSE
                 CALL "C$TOLOWER" USING CHMOD-CLOSELOG-CMD, VALUE 5
                 MOVE 777     TO CHMOD-CLOSELOG-ARGS
                 MOVE SPACES TO CHMOD-CLOSELOG-STAR
                 MOVE EXT-FILPATH-BASE TO CHMOD-CLOSELOG-BASE
                 MOVE EXT-FILPATH-MACHINE TO CHMOD-CLOSELOG-MACHINE
                 MOVE CHMOD-CLOSELOG-COMMAND TO SYSTEM-BUF
                 PERFORM SYSTEM-CALL.

           PERFORM INITIALIZATION.

       GET-NEXT-RC.
           MOVE SPACES TO CLOSING-ERRCD.

           PERFORM READ-RC2-FILE-NEXT.
           IF IO-BAD OR RC-STATUS > "A1"
              PERFORM START-RC2-FILE
              GO TO END-PROGRAM.

      *    UNLOCK RC-FILE.

           MOVE RC2-KEY TO HOLD-RC2-KEY.

           IF RC-TRANS-DATE < BEG-RC-TRANS-DATE
              GO TO GET-NEXT-RC.

           IF RC-TRANS-DATE > END-RC-TRANS-DATE
              GO TO GET-NEXT-RC.

           MOVE RC-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO GET-NEXT-RC.

           MOVE RC-BRNO TO BR-NO LPEOD5-BRANCH.
           PERFORM READ-BR-FILE.
      *    UNLOCK BR-FILE.
           IF IO-BAD
              GO TO GET-NEXT-RC.

           IF BR-ALLOW-FORCE-OPEN-FG NOT = "Y"
              GO TO GET-NEXT-RC.

           MOVE RC-TRANS-DATE TO LPEOD5-TRANS-DATE.

           MOVE "A0001AUTO" TO HOLD-USERID.
           MOVE RC-BRNO TO USERID-BR.
           MOVE HOLD-USERID TO LPEOD5-USERID.

           IF BR-EOD-PROCEDURE = " "
              MOVE "EOD" TO PCODE EO-SPOOL-PCODE EO-PCODE
           ELSE
              MOVE BR-EOD-PROCEDURE TO PCODE EO-SPOOL-PCODE EO-PCODE.

           MOVE 0 TO EO-STEP.
           PERFORM OPEN-EO1-FILE.
           PERFORM READ-EO1-FILE.
           PERFORM CLOSE-EO1-FILE.
           IF IO-BAD
              MOVE "EOD" TO PCODE EO-SPOOL-PCODE EO-PCODE.

           MOVE 99 TO MIN-BUF HOUR-BUF.

           IF BR-DATA-PATH = SPACES OR BR-MACHINE = SPACES
              MOVE "BRANCH HAS UNDEFINED DATA PATH" TO CLOSELOG-MSG
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO GET-NEXT-RC.

      * CHANGE THE EXTERNAL FILPATH TO THAT OF THE CURRENT BRANCH.

           MOVE BR-MACHINE            TO EXT-FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.

           MOVE LPEOD5-BRANCH     TO OPEN-PATH-BRNO.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO OPEN-PATH-TRDATE.

           PERFORM LOAD-OPEN-FILE.

      * CHECK ALL PD RENEWAL 

           PERFORM OPEN-PD1-FILE.

           MOVE "R"           TO PD-ID.
           MOVE LPEOD5-BRANCH TO PD-BRANCH.
           MOVE 0             TO PD-SOC-SEC.

           PERFORM START-PD1-FILE.

       NEXT-PD1.
           PERFORM READ-PD1-FILE-NEXT.
           IF IO-FG = 9 
              GO TO END-PD-RENEWAL.

           IF (PD-BRANCH NOT = LPEOD5-BRANCH) OR (PD-ID NOT = "R")
      *       UNLOCK PD-FILE
              GO TO END-PD-RENEWAL.

           MOVE PD-SOC-SEC TO EDIT-LN-ACCTNO.
           INSPECT EDIT-LN-ACCTNO REPLACING ALL "/" BY "-".

           STRING "RENEWAL LOAN FOR ACCOUNT NO: "
                     EDIT-LN-ACCTNO
                       " NOT COMPLETED!"
                  DELIMITED BY SIZE INTO CLOSELOG-MSG.
           MOVE "ERR" TO CLOSELOG-PATH-TYPE.
           MOVE "BR #" TO CLOSELOG-BRHEAD.
           MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO.
           MOVE "X" TO CLOSING-ERRCD
           PERFORM CREATE-CLOSELOG.

           GO NEXT-PD1.

       END-PD-RENEWAL.
           PERFORM CLOSE-PD1-FILE.

      * CHECK OPEN BATCHES 

      * "REL" GETENV IS DONE UP ABOVE
           MOVE FORM-REL TO BPEOD-REL BPEOD-REL2.
           CALL "C$TOLOWER" USING BPEOD-EC, VALUE 3.
           CALL "C$TOLOWER" USING BPEOD-SHELL, VALUE 9.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO BPEOD-MO.
           MOVE DATE-YYYYMMDD-DD TO BPEOD-DA.
           MOVE LPEOD5-BRANCH TO BPEOD-BR.
           MOVE BPEOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           CALL "C$TOLOWER" USING BPEOD-PATH, VALUE 14.
           MOVE LPEOD5-BRANCH TO BPEOD-PATH-BR.
           MOVE BPEOD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "WARNING: BATCH PAYS NOT COMPLETED!!" TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              PERFORM CREATE-CLOSELOG.

      * CHECK BULK XFERS IN PROGRESS

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO BULKEOD-REL BULKEOD-REL2.
           CALL "C$TOLOWER" USING BULKEOD-EC, VALUE 3.
           CALL "C$TOLOWER" USING BULKEOD-SHELL, VALUE 11.
           MOVE LPEOD5-BRANCH TO BULKEOD-BR BULKEOD-PATH-BR.
           MOVE EXT-FILPATH-GLOBAL TO BULKEOD-DATA.
           MOVE BULKEOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           CALL "C$TOLOWER" USING BULKEOD-PATH, VALUE 16.
           MOVE BULKEOD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "WARNING: BULK TRANSFER UPDATE IN PROGRESS!"
                 TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "X" TO CLOSING-ERRCD
              PERFORM CREATE-CLOSELOG.

      * CREATE SPOOL DIRECTORY

           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE-NAME.
           MOVE SPACES      TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 STRING
                 "CANNOT CREATE SPOOL DIRECTORY: "
                 DIR-ACCESS-BUF
                  DELIMITED BY SIZE INTO CLOSELOG-MSG
                 MOVE "BR #" TO CLOSELOG-BRHEAD
                 MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
                 MOVE "ERR" TO CLOSELOG-PATH-TYPE
                 PERFORM CREATE-CLOSELOG
                 GO TO GET-NEXT-RC.

           CALL "C$TOLOWER" USING CHMOD-REPORTS-CMD, VALUE 5.
           MOVE 777     TO CHMOD-REPORTS-ARGS.
           MOVE PCODE TO CHMOD-REPORTS-PCODE.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO CHMOD-REPORTS-DATE-MMDDYY.
           MOVE SPACES TO CHMOD-REPORTS-STAR.
           MOVE CHMOD-REPORTS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE "BEGINNING END-OF-DAY PROCESSING" TO ER-MESSAGE.
           MOVE "SP" TO EO-DIR.
           MOVE "AUTOC2" TO EO-PROGRAM.
           PERFORM UPDATE-SPOOL-LOG.
           MOVE "*** BEGINNING END-OF-DAY PROCESSING ***"
              TO CLOSELOG-MSG.
           MOVE "BR #" TO CLOSELOG-BRHEAD.
           MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO.
           MOVE "LOG" TO CLOSELOG-PATH-TYPE.
           PERFORM CREATE-CLOSELOG.

      * IF FILE CAN NOT BE OPENED EXCLUSIVELY, USERS ARE STILL IN.
      * NEED TO SET ERRCD IN CASE DECLARE EXITS THE PROGRAM FROM
      * EXCLUSIVE RESERVATION
      
      * NEED TO CLOSE BEFORE OPEN AGAIN 
           CLOSE OP-FILE.

           MOVE "X" TO ERRCD.
           PERFORM OPEN-IT.
           MOVE "OP" TO E-FILE.
           MOVE "AUTOC2" TO ERRLOGW-PROGRAM.

           OPEN OUTPUT OP-FILE.
           IF FILE-STAT = "93"
              MOVE "CAN'T RESERVE OP-FILE" TO ER-MESSAGE CLOSELOG-MSG
              PERFORM UPDATE-SPOOL-LOG
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO GET-NEXT-RC.

           MOVE " " TO ERRCD.

           PERFORM CHECK-1497-GL.

           PERFORM CHECK-CX-REQUESTS.       

           PERFORM CHECK-CASH-DRAWERS.

           IF CLOSING-ERRCD NOT = SPACES
               MOVE 0  TO  WK-BEGBAL
                           WK-RUNNING
                           WK-NET-CASH-NOTES
                           WK-ENDBAL
                           WK-DEPOSIT
               GO TO GET-NEXT-RC.

      * IF YOU GET HERE, THERE SHOULD BE NO UNFINISHED RENEWALS, NO UNWRITTEN
      * CHECKS, NO OPEN CASH DRAWERS, NO BULKS IN PROGRESS, AND GL 1497000
      * SHOULD NOT TO ZERO.

      * OVER/SHORT

           COMPUTE HOLD-RC-OVER-SHORT =
               WK-ENDBAL - (WK-BEGBAL + WK-RUNNING + WK-NET-CASH-NOTES).

           MOVE 0 TO CKWRITTEN(1) CKWRITTEN(2) CKWRITTEN(3)
                     CKWRITTEN(4) CKWRITTEN(5) CKWRITTEN(6)
                     CKWRITTEN(7) CKWRITTEN(8).

           PERFORM GET-TY-CKWRITTEN.


      * CREATE DEPOSIT RECORD

           PERFORM OPEN-TRD1-FILE.

           PERFORM CLEAR-TRD-FILE.

           MOVE LPEOD5-BRANCH     TO TD-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TD-DATE.

           COMPUTE TD-OVERSHORT =
              (WK-BEGBAL + WK-RUNNING + WK-NET-CASH-NOTES) - WK-ENDBAL.
           MOVE BR-GLONHAND       TO TD-CASH.
           MOVE BR-GLBANK-DEPOSIT TO TD-BANK.
           MOVE BR-GLOVERSHORT    TO TD-GLOVERSHORT.

           IF BR-AUTO-OPEN-BAL = "Y"
              MOVE WK-DEPOSIT TO TD-DEPOSIT
                                 HOLD-RC-DEPOSIT
           ELSE
              COMPUTE TD-DEPOSIT
                      HOLD-RC-DEPOSIT = WK-ENDBAL - WK-BEGBAL.


           PERFORM WRITE-TR-DEPOSIT.

           PERFORM CLOSE-TRD1-FILE.

      * NEED TO CHMOD TRFILE, IF IT DID NOT EXIST, MAY HAVE BAD ACCESS

           CALL "C$TOLOWER" USING CHMOD-TRFILE-CMD, VALUE 5.
           MOVE 777     TO CHMOD-TRFILE-ARGS.
           MOVE LPEOD5-TRANS-MONTH TO CHMOD-TRFILE-MONTH.
           MOVE LPEOD5-TRANS-YEAR  TO CHMOD-TRFILE-YEAR.
           MOVE CHMOD-TRFILE-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *  IN PROCESS
           MOVE HOLD-RC-BRNO       TO RC-BRNO.
           MOVE HOLD-RC-TRANS-DATE TO RC-TRANS-DATE.
           PERFORM READ-RC1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE "B1"     TO RC-STATUS.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO RC-CLOSED-DATE.
           MOVE CUR-TIME TO RC-CLOSED-TIME.

      *  WITHDRAWALS(6), ADD INT DEP(7)
      *  WORLD DOESN'T SUBTRACT NSF'S(5) FROM DEPOSIT
      * (THIS IS DEPOSIT AMT TRANSMITTED IN CASH CONCENTRATION!)

           COMPUTE RC-DEPOSIT = HOLD-RC-DEPOSIT - CKWRITTEN(6)
                                                - CKWRITTEN(7).

           MOVE HOLD-RC-OVER-SHORT TO RC-OVER-SHORT.

           ADD CKWRITTEN(1) CKWRITTEN(2) CKWRITTEN(3)
               CKWRITTEN(4) CKWRITTEN(8) GIVING RC-CHECKS.

           MOVE EXT-CONAME-USER-ID TO RC-CL-USERID.

           PERFORM REWRITE-RC1-FILE.

      * JOURNAL WORK FILE

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO ED-PATH.
           OPEN OUTPUT ED-FILE.
           IF FILE-STAT NOT = "00"
              MOVE "CAN'T CREATE JOURNAL WORK FILE" TO ER-MESSAGE
                                                     CLOSELOG-MSG
              PERFORM UPDATE-SPOOL-LOG
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO RESTART-RC.

           CLOSE ED-FILE.

      * PERFORM LPEOD5 TO UPDATE GL AND PRINT RECAP:

           MOVE BR-NAME             TO LPEOD5-BRNAME.
           IF BR-EOD-PROCEDURE = " "
              MOVE "EOD" TO LPEOD5-PCODE
           ELSE
              MOVE BR-EOD-PROCEDURE TO LPEOD5-PCODE.
           MOVE EXT-CONAME-CONAME   TO LPEOD5-CONAME.
           MOVE LPEOD5-BRANCH       TO LPEOD5-BRANCH.
           MOVE EXT-CONAME-SYSDATE  TO LPEOD5-SYSDATE.
           MOVE 2 TO LPEOD5-SEL.

           CALL "C$TOLOWER" USING CHMOD-REPORTS-CMD, VALUE 5.
           MOVE 777     TO CHMOD-REPORTS-ARGS.
           MOVE PCODE             TO CHMOD-REPORTS-PCODE.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO CHMOD-REPORTS-DATE-MMDDYY.
           MOVE SPACES            TO CHMOD-REPORTS-STAR.
           MOVE CHMOD-REPORTS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE "LP/LPEOD5" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LPEOD5-BUF ED-PATH.
           CANCEL FORM-PROGX.

           MOVE LPEOD5-ERRCD TO HOLD-RC-ERRCD ERRCD.
           MOVE "BR #" TO CLOSELOG-BRHEAD.
           MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO.
           IF ERRCD = "Y"
              MOVE "END OF DAY UPDATE UNSUCCESSFUL" TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
           ELSE IF ERRCD = "I"
              MOVE "END OF DAY UPDATE NOT RUN, MISSING GL INTERFACE"
                    TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
           ELSE IF ERRCD = "G"
            MOVE "END OF DAY UPDATE SUCCESSFUL, BUT MISSING GL MASTER!"
                    TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
           ELSE
              MOVE "END OF DAY UPDATE SUCCESSFUL" TO CLOSELOG-MSG.
           MOVE "LOG" TO CLOSELOG-PATH-TYPE.
           PERFORM CREATE-CLOSELOG.

      * IF NO ERROR OR MISSING GL MASTER, MOVE CLOSE TO UPDATE
           IF (ERRCD = " ") OR (ERRCD = "G")
              MOVE " " TO ERRCD.

      * SET RC-RECAP-FG TO "Y" OR "ERRCD"
           PERFORM UPDATE-RC1-RECAP.
      
           IF ERRCD NOT = "I"
              CALL "CBL_DELETE_FILE" USING OPEN-PATH  
                                GIVING STAT-99.


           MOVE BEG-STEP TO FIRST-STEP.
           MOVE END-STEP TO LAST-STEP.

      **************************
       BEGUPD.
      **************************
           MOVE "Y" TO STAT.
           MOVE SPACES TO EORUN-ERROR.

           MOVE 0 TO AUDIT-ERROR STEP-COUNT SECURITY-ERROR
                     DISPLAY-STEP.

           PERFORM CLEAR-EO-FILE.

           MOVE " " TO BATCH-ERROR.

           MOVE "60" TO EXT-ROUTE-BUF.
           MOVE SPACES TO LASTNEXT-BUF.
           MOVE FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.
           MOVE SPACES TO EXT-EOBUF-ERRCD.
       NEXT-AUDIT.
           PERFORM GET-NEXT-REPORT-ENTRY.
           IF IO-FG = 9
              GO TO END-AUDIT.

           ADD 1 TO STEP-COUNT.

           PERFORM PROGRAM-LOAD.

           PERFORM UPDATE-REPORT-ENTRY.
           IF EO-COMPLETE-STAT = "READY"
              GO TO NEXT-AUDIT.

           MOVE WS-PROG-PATHNAME TO EORUN-ERR-PROG.
           MOVE EO-COMPLETE-STAT TO EORUN-ERR-MSG.

           IF EO-COMPLETE-STAT = "NOTFND"
              MOVE SPACES TO ER-MESSAGE
              STRING "PROGRAM: " WS-PROG-PATHNAME " IS MISSING"
                     DELIMITED BY SIZE INTO ER-MESSAGE
           ELSE
           IF EO-COMPLETE-STAT = "FORMNF"
              MOVE "PROGRAM FORM MISSING" TO ER-MESSAGE
           ELSE
           IF EO-COMPLETE-STAT = "SECVIZ"
              MOVE "CANNOT BE RUN--MEMORY OVERFLOW" TO ER-MESSAGE
           ELSE
           IF EO-COMPLETE-STAT = "SECVIO"
              MOVE "CANNOT BE RUN--OPTION NOT PURCHASED" TO ER-MESSAGE
              MOVE 1 TO SECURITY-ERROR
           ELSE
           IF EO-COMPLETE-STAT = "SECVIS"
              MOVE "CANNOT BE RUN--SECURITY VIOLATION" TO ER-MESSAGE
              MOVE 1 TO SECURITY-ERROR
           ELSE
           IF EO-COMPLETE-STAT = "MEMOVF"
              MOVE "MEMORY OVERFLOW OCCURRED" TO ER-MESSAGE
           ELSE
           IF EO-COMPLETE-STAT = "D.VERS"
              MOVE "NEW VERSION--RANGES MUST BE RE-ENTERED"
                                         TO ER-MESSAGE
           ELSE
              MOVE SPACES TO ER-MESSAGE
              STRING "EXECUTION ERROR OCCURRED (" EO-COMPLETE-STAT
                        ")" DELIMITED BY SIZE INTO ER-MESSAGE.
           PERFORM UPDATE-SPOOL-LOG.
           MOVE ER-MESSAGE TO CLOSELOG-MSG.
           MOVE "BR #" TO CLOSELOG-BRHEAD.
           MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO.
           MOVE "ERR" TO CLOSELOG-PATH-TYPE.
           PERFORM CREATE-CLOSELOG.
           MOVE 1 TO AUDIT-ERROR.
       AUDIT-PAUSE.
           IF SECURITY-ERROR = 0
              GO TO NEXT-AUDIT.
       END-AUDIT.
           IF SECURITY-ERROR = 1
              MOVE "INVALID AUTHORIZATION TO RUN THIS PROCEDURE"
                 TO ER-MESSAGE CLOSELOG-MSG
              PERFORM UPDATE-SPOOL-LOG
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO RESTART-RC.
           IF AUDIT-ERROR = 1
              MOVE "CANNOT RUN UNTIL ERRORS ARE CORRECTED" TO ER-MESSAGE
                                                            CLOSELOG-MSG
              PERFORM UPDATE-SPOOL-LOG
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO RESTART-RC.
           IF STEP-COUNT = ZERO
              MOVE "INVALID RANGE" TO ER-MESSAGE CLOSELOG-MSG
              PERFORM UPDATE-SPOOL-LOG
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO RESTART-RC.

      *****************************************
       START-PROCESS.
      *****************************************
           PERFORM CLEAR-EO-FILE.
           MOVE "70" TO EXT-ROUTE-BUF.
           MOVE SPACES TO LASTNEXT-BUF.
           MOVE FIRST-STEP TO CURRENT-STEP.
           MOVE "Y" TO FIRST-TIME.
           MOVE SPACES TO EXT-EOBUF-ERRCD.
           MOVE 0 TO SKIP-SWITCH SKIP-STEP.

       NEXT-PROCESS.
           PERFORM GET-NEXT-REPORT-ENTRY.
           IF IO-FG = 9
              GO TO END-PROCESS.

           IF SKIP-SWITCH = 2
              IF EO-STEP NOT < SKIP-STEP
                 MOVE ZERO TO SKIP-SWITCH SKIP-STEP.
           IF SKIP-SWITCH = 0
              PERFORM PROGRAM-LOAD.

           PERFORM UPDATE-REPORT-ENTRY.

           MOVE EO-COMPLETE-STAT TO ER-MESSAGE.

           PERFORM UPDATE-SPOOL-LOG.

           GO TO NEXT-PROCESS.

      **************************
       END-PROCESS.
      **************************
           CALL "C$TOLOWER" USING CHMOD-REPORTS-CMD, VALUE 5.
           MOVE 777     TO CHMOD-REPORTS-ARGS.
           MOVE PCODE TO CHMOD-REPORTS-PCODE.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO CHMOD-REPORTS-DATE-MMDDYY.
           MOVE "/*" TO CHMOD-REPORTS-STAR.
           MOVE CHMOD-REPORTS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           IF RC-STATUS = "C1"
              MOVE "D1" TO RC-STATUS.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO RC-EOD-BATCH-DATE.
           MOVE CUR-TIME TO RC-EOD-BATCH-TIME.
           IF EORUN-ERROR = SPACES
              MOVE "Y" TO RC-EOD-BATCH-FG
           ELSE
              MOVE "N"  TO RC-EOD-BATCH-FG.
           PERFORM REWRITE-RC1-FILE.

       RESTART-RC.
           MOVE HOLD-RC2-KEY TO RC2-KEY.

           PERFORM START-RC2-FILE.
           GO TO GET-NEXT-RC.

      ********************************************
       CHECK-CASH-DRAWERS SECTION.
      ********************************************
           MOVE 0 TO WK-BEGBAL
                     WK-RUNNING
                     WK-NET-CASH-NOTES
                     WK-ENDBAL
                     WK-DEPOSIT.

           PERFORM OPEN-CA1-FILE.

           MOVE LPEOD5-BRANCH     TO CA-BRNO.
           MOVE LPEOD5-TRANS-DATE TO CA-DATE.
           MOVE 0                 TO CA-TILLNO.


           PERFORM START-CA1-FILE.
       NEXT-CASH-TILL.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CLOSE-CASH-DRAWERS-COMPLETE.

           IF CA-DATE NOT = LPEOD5-TRANS-DATE
              GO TO CLOSE-CASH-DRAWERS-COMPLETE.
           IF CA-BRNO NOT = LPEOD5-BRANCH
              GO TO CLOSE-CASH-DRAWERS-COMPLETE.

           IF CA-BALBY = " "
              MOVE SPACES TO CLOSELOG-MSG
              STRING "CASH DRAWER " CA-TILLNO " IS OPEN"
                  DELIMITED BY SIZE INTO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "X"    TO CLOSING-ERRCD
              PERFORM CREATE-CLOSELOG.
           GO TO NEXT-CASH-TILL.



           IF BR-AUTO-OPEN-BAL = "Y"
              MOVE LPEOD5-BRANCH TO BR-NO
              PERFORM READ-BR-FILE
              MOVE CA-TILLNO TO SUB
              COMPUTE BR-OPEN-BAL(SUB) = CA-ENDBAL - CA-DEPOSIT
              PERFORM REWRITE-BR-FILE.

           ADD CA-BEGBAL TO WK-BEGBAL.
           ADD CA-RUNNING TO WK-RUNNING.
           ADD CA-NET-CASH-NOTES TO WK-NET-CASH-NOTES.
           ADD CA-ENDBAL TO WK-ENDBAL.
           ADD CA-DEPOSIT TO WK-DEPOSIT.
           GO TO NEXT-CASH-TILL.

       CLOSE-CASH-DRAWERS-COMPLETE.
           PERFORM CLOSE-CA1-FILE.

       EXIT-CLOSE-CASH-DRAWERS.
           EXIT.

      *********************************************
       GET-TY-CKWRITTEN SECTION.
      *********************************************
           PERFORM OPEN-TY1-FILE.

           MOVE LPEOD5-BRANCH     TO TY-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TY-DATE.
           MOVE 0                 TO TY-CLASS.

           PERFORM START-TY1-FILE.
       GET-TY-CKWRITTEN-NEXT.
           PERFORM READ-TY1-FILE-NEXT.
           IF IO-FG = 9
              GO TO GET-TY-CKWRITTEN-EXIT.

           IF TY-BRNO NOT = LPEOD5-BRANCH
              GO TO GET-TY-CKWRITTEN-EXIT.

           IF LPEOD5-TRANS-DATE NOT = TY-DATE
              GO TO GET-TY-CKWRITTEN-EXIT.

           ADD TY-CKWRITTEN(1) TO CKWRITTEN(1).
           ADD TY-CKWRITTEN(2) TO CKWRITTEN(2).
           ADD TY-CKWRITTEN(3) TO CKWRITTEN(3).
           ADD TY-CKWRITTEN(4) TO CKWRITTEN(4).
           ADD TY-CKWRITTEN(5) TO CKWRITTEN(5).
           ADD TY-CKWRITTEN(6) TO CKWRITTEN(6).
           ADD TY-CKWRITTEN(7) TO CKWRITTEN(7).
           ADD TY-CKWRITTEN(8) TO CKWRITTEN(8).
           GO TO GET-TY-CKWRITTEN-NEXT.

       GET-TY-CKWRITTEN-EXIT.
           PERFORM CLOSE-TY1-FILE.

      ***********************************
       WRITE-TR-DEPOSIT SECTION.
      ***********************************
           MOVE "CREATING DEPOSIT RECORD...." TO ER-MESSAGE.
           PERFORM UPDATE-SPOOL-LOG.

           MOVE "AUTOC2" TO TD-USERID.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO TD-POSTDATE.
           MOVE CUR-TIME TO TD-POSTTIME.

           PERFORM WRITE-TRD1-FILE.
           IF IO-FG = 9
              IF E-CODE = "22"
                 PERFORM REWRITE-TRD1-FILE
              ELSE
                 GO TO IO-ERROR.

      *************************************
       INITIALIZATION SECTION.
      *************************************

           IF BEG-OFFSET-DAYS NOT NUMERIC
              MOVE "INVALID BEGINNING OFFSET" TO CLOSELOG-MSG
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.
           IF END-OFFSET-DAYS NOT NUMERIC
              MOVE "INVALID ENDING OFFSET" TO CLOSELOG-MSG
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.
           IF BEG-STEPX NOT NUMERIC
              MOVE "INVALID BEGINNING STEP" TO CLOSELOG-MSG
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.
           IF END-STEPX NOT NUMERIC
              MOVE "INVALID ENDING STEP" TO CLOSELOG-MSG
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.

       GET-DATE-RANGE.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO LPEOD5-SYSDATE.
           IF BEG-OFFSET-SIGN = "M"
              MOVE CUR-DATE TO NDTE-DATE
              MOVE BEG-OFFSET-DAYS TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO DATE1
              GO TO GET-ENDING-DATE.

           IF NOT (BEG-OFFSET-SIGN = "-" OR BEG-OFFSET-SIGN = "+")
              IF BEG-DATE NUMERIC
                 MOVE BEG-DATE TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "INVALID BEGINNING DATE" TO CLOSELOG-MSG
                    MOVE SPACES TO CLOSELOG-BRHEAD
                    MOVE 0 TO CLOSELOG-BRNO
                    MOVE "ERR" TO CLOSELOG-PATH-TYPE
                    PERFORM CREATE-CLOSELOG
                    GO TO STOP-RUN
                 ELSE
                    MOVE NUM-DATE TO DATE1
                    GO TO GET-ENDING-DATE
              ELSE
                 MOVE "NON-NUMERIC BEGINNING DATE" TO CLOSELOG-MSG
                 MOVE SPACES TO CLOSELOG-BRHEAD
                 MOVE 0 TO CLOSELOG-BRNO
                 MOVE "ERR" TO CLOSELOG-PATH-TYPE
                 PERFORM CREATE-CLOSELOG
                 GO TO STOP-RUN.
           IF BEG-OFFSET-SIGN = "-"
              COMPUTE NDTE-HOLD = BEG-OFFSET-DAYS * -1
           ELSE
              MOVE BEG-OFFSET-DAYS TO NDTE-HOLD.
           MOVE CUR-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO DATE1.

       GET-ENDING-DATE.
           IF END-OFFSET-SIGN = "M"
              MOVE CUR-DATE TO NDTE-DATE
              MOVE END-OFFSET-DAYS TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO DATE2
              GO TO CHECK-DATE-RANGE.
           IF NOT (END-OFFSET-SIGN = "-" OR END-OFFSET-SIGN = "+")
              IF END-DATE NUMERIC
                 MOVE END-DATE TO NUM-DATE
                 PERFORM TSTDAT
                 IF NUM-DATE = 0
                    MOVE "INVALID ENDING DATE" TO CLOSELOG-MSG
                    MOVE SPACES TO CLOSELOG-BRHEAD
                    MOVE 0 TO CLOSELOG-BRNO
                    MOVE "ERR" TO CLOSELOG-PATH-TYPE
                    PERFORM CREATE-CLOSELOG
                    GO TO STOP-RUN
                 ELSE
                    MOVE NUM-DATE TO DATE2
                    GO TO CHECK-DATE-RANGE
              ELSE
                 MOVE "NON-NUMERIC ENDING DATE" TO CLOSELOG-MSG
                 MOVE SPACES TO CLOSELOG-BRHEAD
                 MOVE 0 TO CLOSELOG-BRNO
                 MOVE "ERR" TO CLOSELOG-PATH-TYPE
                 PERFORM CREATE-CLOSELOG
                 GO TO STOP-RUN.
           IF END-OFFSET-SIGN = "-"
              COMPUTE NDTE-HOLD = END-OFFSET-DAYS * -1
           ELSE
              MOVE END-OFFSET-DAYS TO NDTE-HOLD.
           MOVE CUR-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO DATE2.

       CHECK-DATE-RANGE.
           IF DATE2 < DATE1
              MOVE "INVALID DATE RANGE" TO CLOSELOG-MSG
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.

      *  DON'T WANT TO CROSS MONTHS....IF THEY CROSS, FORCE
      *  DATE1 TO BE 1ST OF MONTH FOR DATE2 MONTH & YEAR
      *  NOT SURE WHY I DON'T WANT TO CROSS MONTHS, JUST SEEMS WRONG...

           IF NOT (DATE1-MM = DATE2-MM AND DATE1-YY = DATE2-YY)
              MOVE DATE2 TO DATE1
              MOVE 1 TO DATE1-DD.

           MOVE "Y" TO GROUP-FLAG.
           MOVE GROUP-ID TO GR-ID WGR-WORD.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO CLOSELOG-MSG
              MOVE SPACES TO CLOSELOG-BRHEAD
              MOVE 0 TO CLOSELOG-BRNO
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              PERFORM CREATE-CLOSELOG
              GO TO STOP-RUN.
           PERFORM OPEN-GR1-FILE.
           PERFORM SEARCH-GR-INIT.

           PERFORM OPEN-BR-FILE.

           MOVE DATE1    TO BEG-RC-TRANS-DATE.
           MOVE DATE2    TO END-RC-TRANS-DATE.

           MOVE "A1"     TO RC-STATUS.
           MOVE 0        TO RC-BRNO.
           MOVE 19000101 TO RC-TRANS-DATE.
           PERFORM OPEN-RC1-FILE.

           PERFORM START-RC2-FILE.

      *************************************
       CLEAR-EO-FILE SECTION.
      *************************************
           MOVE PCODE      TO EO-PCODE.
           MOVE FIRST-STEP TO EO-STEP.

           PERFORM OPEN-EO1-FILE.
           PERFORM START-EO1-FILE.
       CLEAR-NEXT-EO.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CLEAR-EO-EXIT.

           IF EO-PCODE NOT = PCODE
              GO TO CLEAR-EO-EXIT.
           IF EO-STEP > LAST-STEP
              GO TO CLEAR-EO-EXIT.

           MOVE ZERO TO EO-START-TIME EO-COMPLETE-TIME
                        EO-START-DATE EO-COMPLETE-DATE.
           MOVE SPACES TO EO-COMPLETE-STAT EO-MESS.

           PERFORM REWRITE-EO1-FILE.

           GO TO CLEAR-NEXT-EO.

       CLEAR-EO-EXIT.
           PERFORM CLOSE-EO1-FILE.

      *************************************
       GET-NEXT-REPORT-ENTRY SECTION.
      *************************************

           PERFORM OPEN-EO1-FILE.

       GET-NEXT-REPORT2.
           MOVE PCODE TO EO-PCODE.
           MOVE CURRENT-STEP TO EO-STEP.
           IF FIRST-TIME = "Y"
              PERFORM START-EO1-FILE
              MOVE "N" TO FIRST-TIME
           ELSE
              PERFORM START-EO1-FILE-GREATER.
           IF IO-FG = 9
              GO TO GET-NEXT-EXIT.

           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO GET-NEXT-EXIT.

           IF EO-PCODE NOT = PCODE OR EO-STEP > LAST-STEP
              MOVE 9 TO IO-FG
              GO TO GET-NEXT-EXIT.

           MOVE EO-STEP TO CURRENT-STEP.
           IF EXT-ROUTE-BUF = "60"
              GO TO GET-NEXT-EXIT.

           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO EO-START-DATE.
           MOVE CUR-TIME TO EO-START-TIME.
           PERFORM REWRITE-EO1-FILE.

       GET-NEXT-EXIT.
           MOVE EO-PCODE          TO EXT-EOBUF-PCODE.
           MOVE SPACES            TO EXT-EOBUF-MESS.
           MOVE EO-PARAMETERS     TO EXT-EOBUF-DATA.
           MOVE EO-COPIES         TO EXT-EOBUF-COPIES.
           MOVE LPEOD5-TRANS-DATE TO EXT-EOBUF-DATE1.
           MOVE LPEOD5-TRANS-DATE TO EXT-EOBUF-DATE2.
           MOVE "/"               TO EO-SPOOL-FILE.
           MOVE EO-PROGRAM        TO EO-SPOOL-NAME.
           MOVE EO-STEP           TO EO-SPOOL-STEP.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO EXT-EOBUF-PRPATH.
           MOVE "~" TO EXT-EOBUF-ERRCD.
      * PRINT THE COMMENT IN THE H-TITLE FOR CPCORP
           IF EXT-ROUTE-BUF = "70"
              MOVE EO-COMMENT TO EXT-EOBUF-MESS.

           MOVE EO-DIR TO WS-PGM-DIR.
           MOVE "/" TO WS-PGM-SL.
           MOVE EO-PROGRAM TO WS-PGM-NAME.
           MOVE WS-PROG-PATHNAME TO FORM-NAM.
           PERFORM CLOSE-EO1-FILE.

      *************************************
       UPDATE-REPORT-ENTRY SECTION.
      *************************************

           PERFORM OPEN-EO1-FILE.

           MOVE PCODE        TO EO-PCODE.
           MOVE CURRENT-STEP TO EO-STEP.
           PERFORM READ-EO1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-EOBUF-MESS TO EO-MESS.
           IF LOAD-STAT = "NOTFND"
              MOVE SPACES TO EO-MESS
              STRING "PROGRAM: " WS-PROG-PATHNAME " IS MISSING"
                     DELIMITED BY SIZE INTO EO-MESS.
           IF SKIP-SWITCH = 2
              MOVE "SKIP'D" TO EO-COMPLETE-STAT
           ELSE
              MOVE 1 TO SKIP-SWITCH
              IF LOAD-STAT NOT = SPACES
                 MOVE LOAD-STAT TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "!"
                 MOVE "FATAL!" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "V"
                 MOVE "D.VERS" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "|"
                 MOVE "SECVIS" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "S"
                 MOVE SPACES TO EO-COMPLETE-STAT
                 STRING "ERR :" ERRCD
                    DELIMITED BY SIZE INTO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD = "I"
                 MOVE "CANCEL" TO EO-COMPLETE-STAT
              ELSE
              IF EXT-EOBUF-ERRCD NOT = " "
                 MOVE SPACES TO EO-COMPLETE-STAT
                 STRING "FAIL:" ERRCD
                    DELIMITED BY SIZE INTO EO-COMPLETE-STAT
                 MOVE EO-COMPLETE-STAT TO EORUN-ERROR
              ELSE
                 MOVE 0 TO SKIP-SWITCH
                 MOVE "RUN OK" TO EO-COMPLETE-STAT.

           IF EXT-ROUTE-BUF = "70"
              IF EXT-EOBUF-ERRCD NOT = " "
                 MOVE "Y" TO BATCH-ERROR.

           IF SKIP-SWITCH = 1
              MOVE 0 TO SKIP-SWITCH.
           IF EXT-ROUTE-BUF = "60"
              IF EO-COMPLETE-STAT = "RUN OK"
                 MOVE EXT-EOBUF-DATA TO EO-PARAMETERS
                 MOVE "READY" TO EO-COMPLETE-STAT
                 GO TO UPDATE-ENTRY-EXIT
              ELSE
                 GO TO UPDATE-ENTRY-EXIT.
           PERFORM GET-DATE-TIME.
           MOVE CUR-TIME TO EO-COMPLETE-TIME.
           MOVE CUR-DATE TO EO-COMPLETE-DATE.
       UPDATE-ENTRY-EXIT.
           IF EO-MESS = SPACES GO TO UPDATE-MESS-SKIP.
       UPDATE-MESS-SKIP.
           PERFORM REWRITE-EO1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-EO1-FILE.

      ********************************
       PROGRAM-LOAD SECTION.
      ********************************
       PGM-LOAD.

           MOVE SPACES TO NEXT-PROGRAM.
           MOVE SPACES TO LOAD-STAT.
           IF LOADCO-OP1 = " " OR LOADCO-OP1 = "1"
              GO TO LOAD-IT.

           MOVE FORM-NAM TO LOADCO-ARG.
           MOVE LOADCO-OPT TO SYSTEM-BUF.
           CALL "SYSTEM" USING SYSTEM-BUF GIVING STAT-99.

       LOAD-IT.
           MOVE "CO" TO FORM-OBJ.
           MOVE FORM-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "NOTFND" TO LOAD-STAT
              GO TO LOAD-POST.

      *    PERFORM LOGUID-CALL.

           CALL FORM-PROGX USING FORM-PATH ON OVERFLOW
              MOVE "MEMOVF" TO LOAD-STAT
              GO TO LOAD-POST.
           CANCEL FORM-PROGX.
           IF FORM-OBJ = " "
              MOVE "FORMNF" TO LOAD-STAT
              GO TO LOAD-POST.

      * SECURITY
           IF FORM-OBJ = "3Z"
              MOVE "SECVIZ" TO LOAD-STAT
              GO TO LOAD-POST.
           IF FORM-OBJ = "3O"
              MOVE "SECVIO" TO LOAD-STAT
              GO TO LOAD-POST.
           IF FORM-OBJ = "3S"
              MOVE "SECVIS" TO LOAD-STAT
              GO TO LOAD-POST.

       LOAD-POST.

           IF EXT-EOBUF-ERRCD NOT = " " OR LOAD-STAT NOT = SPACES
              MOVE SPACES TO LAST-PROGRAM NEXT-PROGRAM
           ELSE
              MOVE WS-PROG-PATHNAME TO LAST-PROGRAM
              IF NEXT-PROGRAM NOT = SPACES
                 MOVE NEXT-PROGRAM TO FORM-NAM WS-PROG-PATHNAME
                 GO TO PGM-LOAD.

      ***********************************************
       UPDATE-RC1-RECAP SECTION.
      ***********************************************
           MOVE HOLD-RC-BRNO TO RC-BRNO.
           MOVE HOLD-RC-TRANS-DATE TO RC-TRANS-DATE.
           PERFORM READ-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * IF BAD GI, SET STATUS BACK TO OPEN, MUST BUILD GI & RERUN EOD:
           IF HOLD-RC-ERRCD = "I"
              MOVE "A1" TO RC-STATUS
           ELSE
              MOVE "C1" TO RC-STATUS.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO RC-UPDATED-DATE.
           MOVE CUR-TIME TO RC-UPDATED-TIME.
           IF HOLD-RC-ERRCD = " "
              MOVE "Y" TO RC-RECAP-FG
           ELSE
              MOVE HOLD-RC-ERRCD  TO RC-RECAP-FG.
           PERFORM REWRITE-RC1-FILE.


      *************************************
       UPDATE-SPOOL-LOG SECTION.
      *************************************
       UPDATE-ER-LOG.
           MOVE EO-SPOOL-PCODE     TO ER-ALL-SPOOL-PCODE.
           MOVE EO-SPOOL-DATE-NAME TO ER-ALL-SPOOL-DATE.
           PERFORM LOAD-ER-FILE.
           MOVE ER-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "ER" TO E-FILE
              OPEN OUTPUT ER-FILE
              CLOSE ER-FILE.

           MOVE E-FILE TO E-FILEX.
           PERFORM OPEN-ER-FILE.
           PERFORM GET-DATE-TIME.
           MOVE CUR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ER-DATE.
           MOVE CUR-TIME    TO ER-TIME.
           INSPECT ER-TIME REPLACING ALL "/" BY ":".
           MOVE EXT-CONAME-USER-ID TO ER-USER.
           MOVE EO-DIR TO ER-DIR.
           MOVE "/" TO ER-SLASH.
           MOVE EO-PROGRAM TO ER-PROGRAM.
           PERFORM WRITE-ER-REC.
           PERFORM CLOSE-ER-FILE.

           CALL "C$TOLOWER" USING CHMOD-REPORTS-CMD, VALUE 5.
           MOVE 777     TO CHMOD-REPORTS-ARGS.
           MOVE PCODE TO CHMOD-REPORTS-PCODE.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO CHMOD-REPORTS-DATE-MMDDYY.
           MOVE "/*" TO CHMOD-REPORTS-STAR.
           MOVE CHMOD-REPORTS-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      ******************************************
       CREATE-CLOSELOG SECTION.
      ******************************************

      * CREATE AUTO CLOSE DAY LOGS
      * BASE AND MACHINE ARE SET AT BEGINNING OF PROGRAM

           MOVE CUR-YY                TO CLOSELOG-PATH-YY.
           MOVE CUR-MO                TO CLOSELOG-PATH-MM.
           MOVE CUR-DD                TO CLOSELOG-PATH-DD.
           MOVE LPEOD5-TRANS-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO CLOSELOG-DATE.
           MOVE CLOSELOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-CLOSELOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-CLOSELOG-FILE-OUTPUT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO CLOSELOG-TIME.
           PERFORM WRITE-CLOSELOG-FILE.
           PERFORM CLOSE-CLOSELOG-FILE.

      *************************************
       UPDATE-LG-LOG SECTION.
      *************************************
           PERFORM GET-DATE-TIME.
           MOVE "ERR" TO ERRLOG-LOG-TYPE.
           MOVE CUR-YY TO ERRLOG-YY.
           MOVE CUR-MM TO ERRLOG-MM.
           MOVE CUR-DD TO ERRLOG-DD.
           PERFORM LOAD-ERRLOG-FILE.
           MOVE ERRLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-ERRLOG-FILE-OUTPUT
              PERFORM CLOSE-ERRLOG-FILE.

           PERFORM OPEN-ERRLOG-FILE-EXTEND.
           MOVE CUR-TIME TO ERRLOG-TIME.
           INSPECT ERRLOG-TIME REPLACING ALL "/" BY ":".
           MOVE EXT-CONAME-USER-ID TO ERRLOG-USERID.
           MOVE "SP/AUTOC2" TO ERRLOG-PROG.
           PERFORM WRITE-ERRLOG-FILE.
           PERFORM CLOSE-ERRLOG-FILE.

           CALL "C$TOLOWER" USING CHMOD-ERRLOG-CMD, VALUE 5.
           MOVE 777     TO CHMOD-ERRLOG-ARGS.
           MOVE DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO CHMOD-ERRLOG-DATE-YYMMDD.
           MOVE CHMOD-ERRLOG-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *************************************
       CHECK-1497-GL SECTION.
      *************************************

      *GL 1497000 (P2 TRANSACTIONS) MUST BE IN BALANCE

           MOVE 0                 TO CK1497-AMOUNT.
           MOVE LPEOD5-BRANCH     TO CK1497-BRNO.
           MOVE LPEOD5-TRANS-DATE TO CK1497-TRANS-DATE.
           MOVE "CL/CK1497" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH     
                                CK1497-WORKERS.
           CANCEL FORM-PROGX.
           IF CK1497-AMOUNT NOT = 0
              MOVE SPACES TO CLOSELOG-MSG
              MOVE "GL ACCOUNT 1497000 DOESNT NET TO 0"
                                      TO CLOSELOG-MSG
              MOVE "ERR" TO CLOSELOG-PATH-TYPE
              MOVE "BR #" TO CLOSELOG-BRHEAD
              MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO
              MOVE "X" TO CLOSING-ERRCD
              PERFORM CREATE-CLOSELOG.

      ******************************************************************
       CHECK-CX-REQUESTS SECTION.
      ******************************************************************
           PERFORM OPEN-CX1-FILE.

           MOVE LPEOD5-BRANCH TO CX-BRNO.
           MOVE "  "          TO CX-TYPE.
           MOVE 0             TO CX-SEQNO.

           PERFORM START-CX1-FILE.

       NEXT-CX1.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = LPEOD5-BRANCH)    
              GO TO END-CHECKS.

      * IF CX-RESCISSION = "U" IT MEANS THESE CHECKS ARE READY TO PRINT
      * AND THEY NEED TO STOP YOU FROM CLOSING THE DAY, THE LOAN WAS
      * 'U'PDATED OUT OF RESCISSION, REGWAN PL# 556
      * THEIR CX-DATE IS BACK WHEN THE LOAN WAS BOOKED

           IF CX-DATE NOT = LPEOD5-TRANS-DATE
              IF CX-RESCISSION NOT = "U"
                  GO TO NEXT-CX1.

      * DONT LOOK AT CHECKS FROM LOANS IN RESCISSION PERIOD
           IF CX-RESCISSION = "Y"
              GO TO NEXT-CX1.

           IF CX-TYPE NOT = "LN"
              IF CX-TYPE NOT = "DL"
                 GO TO NEXT-CX1.

      *WORLD DOES NOT WANT TO CREATE CHECKS AUTO, WRITE TO LOG.
           MOVE SPACES TO CLOSELOG-MSG.
           STRING "UNWRITTEN CHECK FOR ACCOUNT NO: "
                     CX-ACCTNO
                  DELIMITED BY SIZE INTO CLOSELOG-MSG.
           MOVE "ERR" TO CLOSELOG-PATH-TYPE.
           MOVE "BR #" TO CLOSELOG-BRHEAD.
           MOVE LPEOD5-BRANCH TO CLOSELOG-BRNO.
           MOVE "X" TO CLOSING-ERRCD.
           PERFORM CREATE-CLOSELOG.

           GO TO NEXT-CX1.

       END-CHECKS.
           PERFORM CLOSE-CX1-FILE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBLP/LPTRDCL.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ********************************
       MISC SECTION.
      ********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
      *COPY "LIBGB/LOGUID.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       GET-DATE-TIME.
           ACCEPT CUR-DATE FROM CENTURY-DATE.
           ACCEPT ACC-TIME FROM TIME.
           MOVE ACC-HR TO CUR-HR.
           MOVE ACC-MN TO CUR-MN.
           MOVE ACC-SC TO CUR-SC.
      *----------------------------------------------------------------
       SEND-LEGEND.
           EXIT.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPTY1RN.CPY".
       COPY "LIBLP/LPTRD1IN.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBEO1IN.CPY".
       COPY "LIBGB/GBERI.CPY".
       COPY "LIBLP/LPPRINT.CPY".
       OPEN-CLOSELOG-FILE-EXTEND.
           PERFORM OPEN-IT.   
           OPEN EXTEND CLOSELOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-CLOSELOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE CLOSELOG-FILE
              GO TO OPEN-CLOSELOG-FILE-EXTEND.

       OPEN-CLOSELOG-FILE-OUTPUT.
           PERFORM OPEN-IT.   
           OPEN OUTPUT CLOSELOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-CLOSELOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE CLOSELOG-FILE
              GO TO OPEN-CLOSELOG-FILE-OUTPUT.

       WRITE-CLOSELOG-FILE.
           PERFORM WRITE-IT.   
           WRITE CLOSELOG-REC.

       CLOSE-CLOSELOG-FILE.
           PERFORM CLOSE-IT.   
           CLOSE CLOSELOG-FILE.


       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       EXIT-PROG.
       STOP-RUN.

      * RESTORE USERS ENVIRONMENT BACK TO WHAT IT WAS WHEN 
      * AUTOCLOSE PROCDURE STARTED

           MOVE HOLD-EXT-FILPATH      TO EXT-FILPATH.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.

           CALL "C$TOLOWER" USING CHMOD-CLOSELOG-CMD, VALUE 5.
           MOVE 777     TO CHMOD-CLOSELOG-ARGS.
           MOVE "/*" TO CHMOD-CLOSELOG-STAR.
           MOVE CHMOD-CLOSELOG-COMMAND TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           EXIT PROGRAM.

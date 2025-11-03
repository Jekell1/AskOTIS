       IDENTIFICATION DIVISION.
       PROGRAM-ID.   COLRP2.
       DATE-WRITTEN. 050296.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  COLLECTOR JOURNAL UNWORKED ACCOUNTS REPORT
      *
      * REV:
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  BAH 080696 ADDED TEST OF LN-POCD = "**"
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BLV 060697 ADDED OPTION TO NOT USE CD-RE-WORKED-ACCOUNT FG
      *  BLV 061097 FIXED CUTOFF ROUTINE
      *   CS 102497 ADDED OPTION FOR P&L-ISO
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  KEC 040998 REVISED THE COPY MEMBER 'LIBGB/EDITOR(W)' SO FOR
      *              MORE EFFICENT SYSTEM OF EXECUTING.
      *  KEC 052798 REVISED EDITOR (AGAIN) FOR A 'LIBGB/EDITOR' PROBLEM.
      *  BAH 061298 DONT DISPLAY EACH LN-ACCTNO IF PIGGYBACK OR TERMINAL
      *  090398 BLV FIXED LOGIC FOR LOCAL SPOOLING BY GROUP (BREXEC)
      *  110998 GLF MADE MEMO DATE 2000 COMPATIBLE
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BLV 991008 ADD OPTION TO PRINT BY BRANCH BY BORROWER LAST NAME
      *             INSTEAD OF BY ACCOUNT#, FFG, PR# 968
      *  BLV 991011 ADD BEG & ENDING BORROWER LAST NAME PARAMETERS,
      *             FFG, PR# 968
      *  MJD 001101 ADDED DEALER RANGE, CHANGED VERSION (SMART PR#1342)
      *  BLV 001211 ADD OPTION TO PRINT IN DESCENDING BALANCE ORDER,
      *             SHORTEN "NO ACTIVITY" MESSAGE AND PRINT BALANCE,
      *             TFC PR# 86
      *  BLV 010517 PRINT COLLECTOR ID FROM LN REC IF NO ACTIVITY,
      *             TFC PR# 89; ALSO PRINTED CON & REC STATUS, & PROMISE
      *             INFO FROM LOAN IF NO ACTIVITY
      *  BLV 021121 ADD NEW PARAMETERS:  COLLECTOR ID RANGE, INCLUDE
      *             PROMISED, PROM DATE RANGE, OPTION TO PRINT IN
      *             COLLECTOR ID ORDER, TFC PR# 97
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160601 CHANGES FOR CJ-DATE TO BE YYYYMMDD
      *
      * KEC 2016.1004 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SINCE THE COMP-3 WAS REMOVED FROM CJ-BRNO, CJ-ACCTNO,
      *         CJ-DATE & CJ-START-TIME, NO LONGER NEED HOLD-FIELDS
      *         TO UNCOMP THE FIELDS TO BE USED IN A STRING.
      *
      *  BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181127 GLOBAL LMFILE
      *  BAH 181205 GLOBAL LNFILE
      *   CS 190114 GLOBAL CJFILE
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE, CJFILE
      * BAH 20220803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-LNAME          PIC X(16).
               05  WK-FNAME          PIC X(16).
               05  WK-ACCTNO         PIC 9(8).
           03  WK-BALANCE-KEY REDEFINES WK-KEY.
               05  WK-REV-BALANCE    PIC 9(6)V99.
               05  WK-ACCTNO2        PIC 9(8).
               05  FILLER            PIC X(24).
           03  WK-COLLECTOR-KEY REDEFINES WK-KEY.
               05  WK-COLLID         PIC X(3).
               05  WK-ACCTNO3        PIC 9(8).
               05  FILLER            PIC X(29).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/COLRP2.CBL J35 12/21/23-08:57:54 >".
      ******************************************************************
      *
      *   F I L E - P A T H S
      *
      ******************************************************************
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN               PIC X(2)     VALUE "WK".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  ACCTNO-SUB           PIC 9(2)  COMP-3.

       01  RECORDS-FOUND        PIC X     VALUE "Y".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      *-----------------------------------------------------------------
       01  HOLD-NAME                     PIC X(21) VALUE SPACES.
       01  HOLD-TODAY-DATE               PIC 9(8)  VALUE ZEROES.
       01  TEST-NAME                     PIC X(4)     VALUE SPACES.
       01  HOLD-COLLID                   PIC X(3)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  MESSAGES-WS.
           03  MESSAGE-IN-PROGRESS           PIC X(42) VALUE
               "COLLECTOR UNWORKED ACCOUNTS IN PROGRESS...".

           03  MESSAGE-NO-CJ-REC.
               05  FILLER                    PIC X(38) VALUE
               "***** NO ACTIVITY FOR DATE RANGE *****".

      *-----------------------------------------------------------------
       01  TEST-MEMO1.
           03  FILLER                        PIC X(6).
           03  TEST-COLON                    PIC X.
           03  FILLER                        PIC X(3).
           03  TEST-AM-PM                    PIC XX.
           03  FILLER                        PIC X(18).

      *-----------------------------------------------------------------
       01  WS-ACCTNO                     PIC 9(8)  VALUE ZERO.
       01  WS-CONVERT-DATE               PIC 9(8)  VALUE ZERO.
       01  WS-INVALID-LM-FG              PIC X(1)  VALUE SPACES.
       01  WS-NEXT-CJ                    PIC X(1)  VALUE SPACES.
       01  WS-NEXT-LN                    PIC X(1)  VALUE SPACES.

       01  CON-STAT-ALP.
           03  CON-STAT-NUM        PIC 999B.
       01  REC-STAT-ALP.
           03  REC-STAT-NUM        PIC 999B.

      *-----------------------------------------------------------------
       COPY "LIBSP/COLRPT_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-ACCTNO      PIC 9(8).
           03  END-ACCTNO      PIC 9(8).
           03  BEG-CONTRACT    PIC 9(4).
           03  END-CONTRACT    PIC 9(4).
           03  POTENTIAL-30    PIC X(1).
           03  BEG-PASTDUE     PIC 9(4).
           03  END-PASTDUE     PIC 9(4).

           03  PL-ISO          PIC X.
               88  PL-ISO-INCLUDE  VALUE "I".
               88  PL-ISO-SKIP     VALUE "S".
               88  PL-ISO-ONLY     VALUE "O".
               88  PL-ISO-VALID    VALUE "I" "S" "O".

           03  PL-AIB          PIC X.
               88  PL-AIB-ACTIVE   VALUE "A".
               88  PL-AIB-INACTIVE VALUE "I".
               88  PL-AIB-BOTH     VALUE "B".
               88  PL-AIB-VALID    VALUE "A" "I" "B".

           03  CUTOFF-DATE     PIC 9(8).
           03  RECODE-FG       PIC X.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  PRINT-MEMO      PIC X(1).
           03  GROUP-FLAG      PIC X(1).
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  PRINT-SEQ       PIC X(1).
           03  BEG-ALPHA       PIC X(4).
           03  END-ALPHA       PIC X(4).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  BEG-COLLID      PIC X(4).
           03  END-COLLID      PIC X(4).
           03  PROMISED        PIC X.
           03  BEG-PROMDATE    PIC 9(8).
           03  END-PROMDATE    PIC 9(8).

       01  VERSION-CONTROL     PIC X VALUE "C".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      *****************************************************************
      *
      *  P R I N T  B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H1-DATE       PIC X(8)    VALUE " ".
           03  FILLER        PIC XX      VALUE " ".
           03  H1-TIME       PIC X(8)    VALUE " ".
           03  FILLER        PIC X(28)   VALUE " ".
           03  H1-NAME       PIC X(40)   VALUE " ".
           03  FILLER        PIC X(37)   VALUE " ".
           03  FILLER        PIC X(5)    VALUE "PAGE".
           03  H1-PAGE-NO    PIC ZZZ9.

      *---------------------------------------------------------------
       01  HEAD2.
           03  FILLER        PIC X(10)   VALUE "USER ID:".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(25)   VALUE " ".
           03  FILLER        PIC X(38)   VALUE
               "COLLECTOR JOURNAL OF UNWORKED ACCOUNTS".
           03  FILLER        PIC X(35)   VALUE " ".
           03  H2-COL-PROMPT PIC X(11)   VALUE SPACES.
           03  H2-COLLID     PIC X(3)    VALUE SPACES.

      *---------------------------------------------------------------
       01  HEAD3.
           03  FILLER        PIC X(10)   VALUE "BRANCH :".
           03  H3-BRNO       PIC 9(4).
           03  FILLER        PIC X(2)     VALUE " ".
           03  H3-BRDESC     PIC X(40)    VALUE " ".
           03  FILLER        PIC X(76)    VALUE " ".

      *---------------------------------------------------------------
       01  HEAD4.
           03  FILLER        PIC X(63)   VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "ATV".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "RST".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "ACTN".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "CON".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "REC".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "PROMISE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "PROMISE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "CURRENT".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "PAST-DUE".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "COLL".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "NEW".

      *---------------------------------------------------------------
       01  HEAD5.
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "ACCOUNT".
           03  FILLER        PIC X(4)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "NAME".
           03  FILLER        PIC X(20)   VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "BEG-TIME".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "END-TIME".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COD".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "COD".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "CODE".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "STA".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "STA".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "DATE".
           03  FILLER        PIC X(6)    VALUE SPACES.
           03  FILLER        PIC X(6)    VALUE "AMOUNT".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "BALANCE".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(7)    VALUE "BALANCE".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(2)    VALUE "ID".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(4)    VALUE "COLL".

      *****************************************************************
      *
      *    D E T A I L  L I N E
      *
      *****************************************************************
       01  DETAIL-LINE                  PIC X(132)   VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1            REDEFINES DETAIL-LINE.
           03  D1-ACCTNO                PIC X(13).
           03  FILLER                   PIC X(1).
           03  D1-BW-NAME               PIC X(21).
           03  FILLER                   PIC X(1).
           03  D1-NO-CJ-REC             PIC X(96).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D1-CJ-REC-FOUND          REDEFINES D1-NO-CJ-REC.
               05  D1-DATE              PIC 99/99/99.
               05  FILLER               PIC X(1).
               05  D1-START-TIME        PIC X(8).
               05  FILLER               PIC X(1).
               05  D1-END-TIME          PIC X(8).
               05  FILLER               PIC X(1).
               05  D1-ACTIVITY-CD       PIC X(3).
               05  FILLER               PIC X(1).
               05  D1-RESULT-CD         PIC X(3).
               05  FILLER               PIC X(1).
               05  D1-ACTION-CD         PIC X(4).
               05  FILLER               PIC X(1).
               05  D1-CON-STATUS        PIC X(3).
               05  FILLER               PIC X(1).
               05  D1-REC-STATUS        PIC X(3).
               05  FILLER               PIC X(1).
               05  D1-PROMISE-DATE      PIC 99/99/99    BLANK WHEN ZERO.
               05  FILLER               PIC X(1).
               05  D1-PROMISE-AMT       PIC ZZZZZ9.99.
               05  FILLER               PIC X(1).
               05  D1-CURRENT-BAL       PIC ZZZZZ9.99.
               05  FILLER               PIC X(1).
               05  D1-PASTDUE-BAL       PIC ZZZZZ9.99.
               05  FILLER               PIC X(2).
               05  D1-COLLID            PIC X(3).
               05  FILLER               PIC X(2).
               05  D1-NEW-COLLID        PIC X(3).

      *----------------------------------------------------------------
       01  DETAIL2            REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(36).
           03  D2-LM-MEMO1              PIC X(30).
           03  D2-LM-MEMO2              PIC X(30).
           03  FILLER                   PIC X(36).

      *-----------------------------------------------------------------
       01  RANGE-LINE         REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(35).
           03  RANGE-SELECTION-1.
               04  FILLER               PIC X(1).
               04  R-NUM                PIC Z(5)9.
               04  FILLER               PIC X(90).
           03  RANGE-SELECTION-2        REDEFINES RANGE-SELECTION-1.
               04  R-DATE               PIC 9999/99/99.
               04  FILLER               PIC X(87).
           03  RANGE-SELECTION-3        REDEFINES RANGE-SELECTION-1.
               04  FILLER               PIC X(1).
               04  R-WORD               PIC X(96).
           03  RANGE-SELECTION-4.
               04  FILLER               PIC X(1).
               04  R-NUM2               PIC Z(5)9.99.
               04  FILLER               PIC X(87).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/COLRP2_DEF.CPY".
       COPY "LIBLP/COLRP2_WKS.CPY".
      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/COLRP2_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CJ2-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "COLRP2" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG NOT = 0 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CJ-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           MOVE BR-NO     TO H3-BRNO.
           MOVE BR-NAME   TO H3-BRDESC.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
              STRING MESSAGE-IN-PROGRESS,BR-NO
                     DELIMITED BY "  " INTO MESS
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-CJ2-FILE.
           PERFORM OPEN-LN1-FILE.

           IF PRINT-SEQ = "A" OR PRINT-SEQ = "B" OR PRINT-SEQ = "C"
              PERFORM OPEN-WK-FILE-OUTPUT.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN1-WBEG-OWNBR
                                 QLN1-WEND-OWNBR.
           MOVE BEG-ACCTNO    TO LN-ACCTNO WS-ACCTNO
                                 QLN1-WBEG-ACCTNO.
           MOVE END-ACCTNO    TO QLN1-WEND-ACCTNO.

           PERFORM START-LN1-FILE.

      *=================================================================
      *                  PROCESSING LN/CJ FILES:
      *
      *    READ LN-FILE TO FIND VALID ACCOUNT.  WHEN VALID ACCOUNT IS
      *    FOUND, THEN READ CJ2-RECORDS (AT THE POINT OF CUTOFF-DATE) TO
      *    SEE IF THIS ACCOUNT HAS BEEN WORKED AFTER THE CUTOFF-DATE.
      *    IF SO, THEN READ THE LN-FILE FOR THE NEXT ACCOUNT; ELSE
      *    RE-START THE CJ-FILE TO THE BEGINNIN ACTIVITY DATE AND PRINT
      *    EVERY CJ2-RECORD THRU TO THE ENDING ACTIVITY DATE.
      *=================================================================
       NEXT-LN1-RECORD.

           PERFORM READ-LN1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO END-OF-GROUP.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN1-RECORD.

           IF ( LN-ACCTNO > END-ACCTNO )
              GO TO END-OF-GROUP.

           IF ( LN-CLASS < BEG-CLASS ) OR ( LN-CLASS > END-CLASS )
              GO TO NEXT-LN1-RECORD.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN1-RECORD.

           IF ( LN-ACCTNO < BEG-ACCTNO )
              GO TO NEXT-LN1-RECORD.

           IF ( LN-COLLECTOR < BEG-COLLID ) OR
              ( LN-COLLECTOR > END-COLLID )
              GO TO NEXT-LN1-RECORD.

           IF PROMISED = "O" AND LN-PROMDATE = 0
              GO TO NEXT-LN1-RECORD.

           IF PROMISED = "S" AND LN-PROMDATE NOT = 0
              GO TO NEXT-LN1-RECORD.

           IF LN-PROMDATE NOT = 0
              IF LN-PROMDATE < BEG-PROMDATE OR
                  LN-PROMDATE > END-PROMDATE
              GO TO NEXT-LN1-RECORD.

           IF (PL-ISO-ONLY) AND (LN-PLDATE = 0)
              GO TO NEXT-LN1-RECORD.

           IF (PL-ISO-SKIP) AND (LN-PLDATE NOT = 0)
              GO TO NEXT-LN1-RECORD.

           IF LN-PLDATE NOT = 0
              IF PL-AIB-ACTIVE
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN1-RECORD.

           IF LN-PLDATE NOT = 0
              IF PL-AIB-INACTIVE
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN1-RECORD.

           PERFORM GET-SP-RECORD.
           IF ( IO-FG NOT = 0 )
              GO TO NEXT-LN1-RECORD.

           PERFORM CHECK-SET-PARAMETERS-LN.

           IF ( WS-NEXT-LN = "Y" )
              GO TO NEXT-LN1-RECORD.

      *-----------------------------------------------------------------
      *    A VALID LN-RECORD; NOW READ CJ-FILE TO SEE IF ACCOUNT HAS
      *    BEEN WORKED AFTER 'NOT WORKED SINCE...' DATE.
      *-----------------------------------------------------------------
       SETUP-CJ2-FILE-1.

           MOVE BR-NO            TO CJ-BRNO
                                    QCJ2-WBEG-BRNO
                                    QCJ2-WEND-BRNO.
           MOVE LN-ACCTNO        TO CJ-ACCTNO
                                    QCJ2-WBEG-ACCTNO
                                    QCJ2-WEND-ACCTNO.
           MOVE CUTOFF-DATE      TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WBEG-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCJ2-WEND-DATE.

      *    MOVE 2500                 TO CJ-START-TIME
           MOVE 0                    TO CJ-START-TIME
                                        QCJ2-WBEG-START-TIME.
           MOVE ALL "9"              TO QCJ2-WEND-START-TIME.

      D    STOP MESS.
           PERFORM START-CJ2-FILE-GREATER.

      *-----------------------------------------------------------------
      *    WHEN IO-FG NOT = 0; OR WHEN THE CJ-RECORD BECOMES INVALID; THEN
      *    THE ACCOUNT IS CONSIDERED NOT TO BE WORKED (OK TO PRINT).
      *-----------------------------------------------------------------
       NEXT-CJ2-RECORD-1.

           PERFORM READ-CJ2-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
                OR  (CJ-ACCTNO NOT = LN-ACCTNO)
              IF PRINT-SEQ = "A"
                 PERFORM CREATE-ALPHA-WORK-REC
                 GO TO NEXT-LN1-RECORD
              END-IF
              IF PRINT-SEQ = "B"
                 PERFORM CREATE-BALANCE-WORK-REC
                 GO TO NEXT-LN1-RECORD
              END-IF
              IF PRINT-SEQ = "C"
                 PERFORM CREATE-COLLECTOR-WORK-REC
                 GO TO NEXT-LN1-RECORD
              END-IF
              PERFORM PROCESS-VALID-LN-CJ-RECORD
              GO TO NEXT-LN1-RECORD.

           IF RECODE-FG = "N"
              GO TO NEXT-LN1-RECORD.

           PERFORM GET-RESULT-CODE.

           IF ( CD-RE-WORKED-ACCOUNT = "Y" )
              GO TO NEXT-LN1-RECORD.

           GO TO NEXT-CJ2-RECORD-1.

       END-OF-GROUP.

           IF PRINT-SEQ = "A"
              PERFORM ALPHA-PRINT-ROUTINE.

           IF PRINT-SEQ = "B"
              PERFORM BALANCE-PRINT-ROUTINE.

           IF PRINT-SEQ = "C"
              PERFORM COLLECTOR-PRINT-ROUTINE.

           PERFORM CLOSE-CJ2-FILE.
           PERFORM CLOSE-LN1-FILE.

           IF ( LOCAL-PRINT-FG = "Y" )
              MOVE 0 TO PAGE-NUMBER.

           MOVE "Y" TO FIRST-TIME.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF ( NO-RECORDS-FOUND )
              GO TO END-PROGRAM.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
      *    THE CURRENT ACCOUNT NUMBER IS VALID IN LN-FILE AND CJ-FILE
      *    AND HAS NOT BEEN WORKED AFTER THE 'NOT WORKED SINCE...' DATE.
      ******************************************************************
       PROCESS-VALID-LN-CJ-RECORD SECTION.
      ************************************************

           MOVE 1                      TO ACCTNO-SUB.
           MOVE BR-NO                  TO CJ-BRNO
                                          QCJ2-WBEG-BRNO
                                          QCJ2-WEND-BRNO.
           MOVE LN-ACCTNO              TO CJ-ACCTNO
                                          QCJ2-WBEG-ACCTNO
                                          QCJ2-WEND-ACCTNO.
                                   
           MOVE BEG-DATE               TO CJ-DATE
                                          SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD    TO QCJ2-WBEG-DATE.
           MOVE END-DATE               TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD    TO QCJ2-WEND-DATE.
           MOVE ZEROES                 TO CJ-START-TIME
                                          QCJ2-WBEG-START-TIME.
           MOVE ALL "9"                TO QCJ2-WEND-START-TIME.

           PERFORM START-CJ2-FILE.

      *-----------------------------------------------------------------
       NEXT-CJ2-RECORD-2.

           PERFORM READ-CJ2-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              IF ( ACCTNO-SUB = 1 )
                 PERFORM PRINT-DETAIL-LINE-LN
                 GO TO EXIT-PROCESS-LN-CJ
              ELSE
                 GO TO EXIT-PROCESS-LN-CJ.

           PERFORM CHECK-SET-PARAMETERS-CJ.

           IF ( WS-NEXT-CJ = "Y" )
              IF ( ACCTNO-SUB = 1 )
                 PERFORM PRINT-DETAIL-LINE-LN
                 GO TO EXIT-PROCESS-LN-CJ
              ELSE
                 GO TO EXIT-PROCESS-LN-CJ.

      * VALID CJ-RECORD - PRINT

           PERFORM PRINT-DETAIL-LINE-CJ.

           GO TO NEXT-CJ2-RECORD-2.

       EXIT-PROCESS-LN-CJ.
           EXIT.

      ******************************************************************
      *    CREATE ALPHA WORK RECORD SO WE CAN PRINT BY BW NAME
      ******************************************************************
       CREATE-ALPHA-WORK-REC SECTION.
      ******************************************************************

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE SPACES TO BW-LNAME BW-FNAME.
           PERFORM CLOSE-BW1-FILE.

           MOVE BW-LNAME               TO WK-LNAME.
           MOVE BW-FNAME               TO WK-FNAME.
           MOVE LN-ACCTNO              TO WK-ACCTNO.

           PERFORM WRITE-WK-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

      **********************************************
       ALPHA-PRINT-ROUTINE SECTION.
      **********************************************
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
       NEXT-WORK-REC.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-ALPHA-PRINT-ROUTINE.

           MOVE WK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM PROCESS-VALID-LN-CJ-RECORD.

           GO TO NEXT-WORK-REC.
       EXIT-ALPHA-PRINT-ROUTINE.
           EXIT.

      ******************************************************************
      * CREATE BALANCE WORK RECORD SO WE CAN PRINT BY DESCENDING BALANCE
      ******************************************************************
       CREATE-BALANCE-WORK-REC SECTION.
      ******************************************************************

           MOVE SPACES                 TO WK-REC.
           COMPUTE WK-REV-BALANCE = 999999.99 - LN-CURBAL.
           MOVE LN-ACCTNO              TO WK-ACCTNO2.

           PERFORM WRITE-WK-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

      ******************************************************************
       BALANCE-PRINT-ROUTINE SECTION.
      ******************************************************************

           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
       NEXT-BALANCE-WORK-REC.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-BALANCE-PRINT-ROUTINE.

           MOVE WK-ACCTNO2 TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM PROCESS-VALID-LN-CJ-RECORD.

           GO TO NEXT-BALANCE-WORK-REC.
       EXIT-BALANCE-PRINT-ROUTINE.
           EXIT.

      ******************************************************************
      *    CREATE COLLECTOR WORK RECORD SO WE CAN PRINT BY COLLECTOR ID
      ******************************************************************
       CREATE-COLLECTOR-WORK-REC SECTION.
      ******************************************************************

           MOVE SPACES                 TO WK-REC.
           MOVE LN-COLLECTOR           TO WK-COLLID.
           MOVE LN-ACCTNO              TO WK-ACCTNO3.

           PERFORM WRITE-WK-FILE.
           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

      ******************************************************************
       COLLECTOR-PRINT-ROUTINE SECTION.
      ******************************************************************
           MOVE SPACES TO HOLD-COLLID.
           MOVE "COLLECTOR: " TO H2-COL-PROMPT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
       NEXT-COLLECTOR-WORK-REC.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-COLLECTOR-PRINT-ROUTINE.

           MOVE WK-ACCTNO3 TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF WK-COLLID NOT = HOLD-COLLID
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE WK-COLLID TO HOLD-COLLID H2-COLLID.
           PERFORM PROCESS-VALID-LN-CJ-RECORD.
           GO TO NEXT-COLLECTOR-WORK-REC.
       EXIT-COLLECTOR-PRINT-ROUTINE.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE COLRP2-QUEUE-POS TO Q-POS.
           MOVE COLRP2-COPIES-POS TO C-POS.
           MOVE COLRP2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0                    TO PAGE-NUMBER.
           MOVE EXT-CONAME-CONAME    TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE   TO H1-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD        TO HOLD-TODAY-DATE.
           MOVE EXT-CONAME-USER-ID   TO H2-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE MESSAGE-IN-PROGRESS TO MESS
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-CD1-FILE.

           IF PRINT-SEQ = "A" OR PRINT-SEQ = "B" OR PRINT-SEQ = "C"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM CLOSE-WK-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE CUTOFF-DATE.

           IF BEG-ALPHA = "    "
              IF END-ALPHA = "    "
      * LOWER "ZZZZ"
                 MOVE X'7A7A7A7A' TO END-ALPHA.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-PRINT-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - DEFAULT TO USER BRANCH  - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( GROUP-FLAG NOT = "Y" )
      *       MOVE "SNNN040BR1." TO VDU
      *       MOVE 0 TO BEG-BRANCH VDUNUM0
      *       PERFORM SCREENIO
      *       MOVE "SNNN040BR2." TO VDU
      *       MOVE 0 TO END-BRANCH VDUNUM0
      *------>PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-BRANCH.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BEGIN-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENDING-BRANCH.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BEGIN-BRANCH.

      *-----------------------------------------------------------------
       BEGIN-CLASS.

           MOVE LEGEND-CLASS-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CLASS.

           IF ( VDUSTATUS = "1U" )
              IF ( GROUP-FLAG = "Y" )
                 GO TO BEGIN-BRANCH
              ELSE
                 GO TO ENDING-BRANCH.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-CLASS.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENDING-CLASS.

           MOVE LEGEND-CLASS-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-CLASS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-CLASS.
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID LOAN CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-CLASS.

           GO TO BEGIN-DEALER.

      *-----------------------------------------------------------------
       ALL-CLASS.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE ZEROES               TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE 99                   TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-DEALER.

           MOVE LEGEND-DEALER-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-DEALER.
           IF UP-KEY GO TO ENDING-CLASS.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO BEGIN-DEALER.
           MOVE VDUNUM2 TO BEG-DEALER.

      *-----------------------------------------------------------------
       ENDING-DEALER.

           MOVE LEGEND-DEALER-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO BEGIN-DEALER.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO ENDING-DEALER.
           MOVE VDUNUM2 TO END-DEALER.

           IF ( BEG-DEALER > END-DEALER )
              MOVE "INVALID LOAN DEALER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DEALER.

           GO TO BEGIN-ACCTNO.

      *-----------------------------------------------------------------
       ALL-DEALER.

           MOVE "SNNN042DL1." TO VDU.
           MOVE ZEROES        TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99       TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-ACCTNO.

           MOVE LEGEND-ACCTNO-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ACCTNO.
           IF VDUSTATUS = "1U" GO TO ENDING-DEALER.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-ACCTNO.
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENDING-ACCTNO.

           MOVE LEGEND-ACCTNO-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-ACCTNO.
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-ACCTNO.

           GO TO ALPHA-01.

      *-----------------------------------------------------------------
       ALL-ACCTNO.

           MOVE "SNNN060BEG-ACCTNO." TO VDU.
           MOVE ZEROES               TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060END-ACCTNO." TO VDU.
           MOVE 99999999             TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.


      *----------------------------------------------------------------
       ALPHA-01.
           MOVE LEGEND-NAME-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040ALP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ALPHA.
           IF VDUSTATUS = "1U" GO TO ENDING-ACCTNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALPHA-01.
           MOVE VDUBUF TO BEG-ALPHA.
       ALPHA-02.
           MOVE LEGEND-NAME-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ALP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ALPHA-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALPHA-02.
           MOVE VDUBUF TO END-ALPHA.
           IF BEG-ALPHA > END-ALPHA
              MOVE "INVALID ALPHA RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ALPHA-01.
           GO TO BEGIN-CONTRACTUAL.

       ALL-ALPHA.
           MOVE "S  A000ALP1." TO VDU.
           MOVE "   " TO BEG-ALPHA VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000ALP2." TO VDU.
      * LOWER "ZZZZ"
           MOVE X'7A7A7A7A' TO END-ALPHA VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-CONTRACTUAL.

           MOVE LEGEND-CONTRACTUAL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BEG-CONTRACT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CONTRACTUAL.
           IF VDUSTATUS = "1U" GO TO ALPHA-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO BEGIN-CONTRACTUAL.

           MOVE VDUNUM0 TO BEG-CONTRACT.

      *-----------------------------------------------------------------
       ENDING-CONTRACTUAL.

           MOVE LEGEND-CONTRACTUAL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-CONTRACT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-CONTRACTUAL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENDING-CONTRACTUAL.

           MOVE VDUNUM0 TO END-CONTRACT.

           IF ( BEG-CONTRACT > END-CONTRACT )
              MOVE "INVALID CONTRACTUAL AGE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-CONTRACTUAL.

           GO TO COLLID-01.

      *-----------------------------------------------------------------
       ALL-CONTRACTUAL.

           MOVE "SNNN040BEG-CONTRACT." TO VDU.
           MOVE ZEROES                 TO BEG-CONTRACT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-CONTRACT." TO VDU.
           MOVE 9999                   TO END-CONTRACT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       COLLID-01.
           MOVE LEGEND-COLLID-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A030BEGCOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-COLLID.
           IF VDUSTATUS = "1U" GO TO ENDING-CONTRACTUAL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COLLID-01.
           MOVE VDUBUF TO BEG-COLLID.
       COLLID-02.
           MOVE LEGEND-COLLID-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ENDCOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO COLLID-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COLLID-02.
           MOVE VDUBUF TO END-COLLID.
           IF BEG-COLLID > END-COLLID
              MOVE "INVALID COLLECTOR ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO COLLID-01.
           GO TO PROMISED-ISO.

       ALL-COLLID.
           MOVE "S  A000BEGCOL." TO VDU.
           MOVE "   " TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000ENDCOL." TO VDU.
      * LOWER "ZZZ"
           MOVE X'7A7A7A' TO END-COLLID VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       PROMISED-ISO.

           MOVE LEGEND-PROMISED-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PROMISED." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO COLLID-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PROMISED-ISO.

           IF ( VDUBUF = "I" OR "S" OR "O" )
              MOVE VDUBUF TO PROMISED
           ELSE
              GO TO PROMISED-ISO.

           IF PROMISED = "S"
              GO TO ALL-PROMDATES.

      *-----------------------------------------------------------------
       BEGIN-PROMDATE.

           MOVE LEGEND-PROMDATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM080BEG-PROMDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO PROMISED-ISO.
           IF VDUSTATUS = "16" GO TO ALL-PROMDATES.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO BEGIN-PROMDATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO BEGIN-PROMDATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-PROMDATE.

      *-----------------------------------------------------------------
       ENDING-PROMDATE.

           MOVE LEGEND-PROMDATE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM080END-PROMDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-PROMDATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENDING-PROMDATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-PROMDATE.

           MOVE VDU-DATE-YYYYMMDD TO END-PROMDATE.

           IF END-PROMDATE < BEG-PROMDATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-PROMDATE.

           GO TO POTENTIAL-30-ISO.

       ALL-PROMDATES.
           MOVE "S  8080BEG-PROMDATE." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-PROMDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080END-PROMDATE." TO VDU.
           MOVE EXT-JULIAN-END TO END-PROMDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       POTENTIAL-30-ISO.

           MOVE LEGEND-POT30-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010POTENTIAL-30." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDING-PROMDATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO POTENTIAL-30-ISO.

           IF ( VDUBUF = "I" OR "S" OR "O" )
              MOVE VDUBUF TO POTENTIAL-30
           ELSE
              GO TO POTENTIAL-30-ISO.

      *-----------------------------------------------------------------
       BEGIN-PASTDUE.

           MOVE LEGEND-PASTDUE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BEG-PASTDUE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-PASTDUE.
           IF VDUSTATUS = "1U" GO TO POTENTIAL-30-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-PASTDUE.

           MOVE VDUNUM0 TO BEG-PASTDUE.

      *-----------------------------------------------------------------
       ENDING-PASTDUE.

           MOVE LEGEND-PASTDUE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-PASTDUE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-PASTDUE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-PASTDUE.

           MOVE VDUNUM0 TO END-PASTDUE.

           IF ( BEG-PASTDUE > END-PASTDUE )
              MOVE "INVALID PAST DUE DAYS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-PASTDUE.

           GO TO ENTER-PL-ISO.

      *-----------------------------------------------------------------
       ALL-PASTDUE.

           MOVE "SNNN040BEG-PASTDUE." TO VDU.
           MOVE ZEROES                TO BEG-PASTDUE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-PASTDUE." TO VDU.
           MOVE 9999                  TO END-PASTDUE VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-PL-ISO.

           MOVE LEGEND-PL-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENDING-PASTDUE.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PL-ISO.

           MOVE VDUBUF TO PL-ISO.

           IF ( NOT PL-ISO-VALID )
              GO TO ENTER-PL-ISO.

           IF ( PL-ISO-SKIP )
              MOVE "S  A010ACTIVE." TO VDU
              MOVE "B" TO VDUBUF PL-AIB
              PERFORM SCREENIO
              GO TO CUT-OFF-DATE.

      *-----------------------------------------------------------------
       ENTER-PL-AIB.

           IF ( UP-KEY ) AND ( PL-ISO-SKIP )
              GO TO ENTER-PL-ISO.

           MOVE LEGEND-PL-02 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PL-ISO.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PL-AIB.

           MOVE VDUBUF TO PL-AIB.

           IF ( NOT PL-AIB-VALID )
              GO TO ENTER-PL-AIB.

      *-----------------------------------------------------------------
       CUT-OFF-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENTER-PL-AIB.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO USE-RECODE-YN.

           MOVE LEGEND-CUTOFF-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080CUTOFF-DATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-PL-AIB.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CUT-OFF-DATE.

           IF ( VDUNUM0 = 0 )
              GO TO CUT-OFF-DATE.

           MOVE VDU-DATE-YYYYMMDD TO CUTOFF-DATE.

      *-----------------------------------------------------------------
       USE-RECODE-YN.

           MOVE LEGEND-USE-RECODE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RECODE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CUT-OFF-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO USE-RECODE-YN.

           IF ( VDUBUF = "Y" OR "N" )
              MOVE VDUBUF TO RECODE-FG
           ELSE
              GO TO USE-RECODE-YN.

      *-----------------------------------------------------------------
       BEGIN-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENDING-PASTDUE.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO PRINT-MEMO-YN.

           MOVE LEGEND-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO CUT-OFF-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = 0 )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENDING-DATE.

           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO ENDING-PASTDUE.

           MOVE LEGEND-DATE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

           IF CUTOFF-DATE > END-DATE
              MOVE "CUT-OFF DATE CANNOT BE GREATER THAN ENDING DATE"
                   TO MESS
              PERFORM SEND-MESS
              GO TO CUT-OFF-DATE.

      *-----------------------------------------------------------------
       PRINT-MEMO-YN.

           MOVE LEGEND-PRINT-MEMO TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRINT-MEMO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDING-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PRINT-MEMO-YN.

           IF ( VDUBUF = "Y" OR "N" )
              MOVE VDUBUF TO PRINT-MEMO
           ELSE
              GO TO PRINT-MEMO-YN.

      *-----------------------------------------------------------------
       ENTER-PRINT-SEQ.

           MOVE LEGEND-PRINT-SEQ TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ALPSEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINT-MEMO-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-PRINT-SEQ.

           IF ( VDUBUF = "A" OR "B"  OR "C" OR "N")
              MOVE VDUBUF TO PRINT-SEQ
           ELSE
              MOVE "ENTER `A', `B', `C', OR `N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-SEQ.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-PRINT-SEQ.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE BEG-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE END-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN042DL1."       TO VDU.
           MOVE BEG-DEALER          TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN042DL2."       TO VDU.
           MOVE END-DEALER          TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BEG-ACCTNO." TO VDU.
           MOVE BEG-ACCTNO           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060END-ACCTNO." TO VDU.
           MOVE END-ACCTNO           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ALP1." TO VDU.
           MOVE BEG-ALPHA TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ALP2." TO VDU.
           MOVE END-ALPHA TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-CONTRACT." TO VDU.
           MOVE BEG-CONTRACT           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040END-CONTRACT." TO VDU.
           MOVE END-CONTRACT           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEGCOL." TO VDU.
           MOVE BEG-COLLID TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ENDCOL." TO VDU.
           MOVE END-COLLID TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000PROMISED." TO VDU.
           MOVE PROMISED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080BEG-PROMDATE." TO VDU.
           MOVE BEG-PROMDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080END-PROMDATE." TO VDU.
           MOVE END-PROMDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010POTENTIAL-30." TO VDU.
           MOVE POTENTIAL-30           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-PASTDUE." TO VDU.
           MOVE BEG-PASTDUE           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040END-PASTDUE." TO VDU.
           MOVE END-PASTDUE           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RECODE." TO VDU.
           MOVE RECODE-FG           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRINT-MEMO." TO VDU.
           MOVE PRINT-MEMO           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ALPSEQ." TO VDU.
           MOVE PRINT-SEQ TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       CHECK-SET-PARAMETERS-LN SECTION.
      ******************************************************************

           MOVE "N" TO WS-NEXT-LN.

           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.

           IF ( CONTRACTUAL < BEG-CONTRACT ) OR
              ( CONTRACTUAL > END-CONTRACT )
              GO TO END-CHECK-SET-PARM-LN.

           IF ( POTENTIAL-30 = "S" ) AND ( CPOT30 = "Y" )
              GO TO END-CHECK-SET-PARM-LN.

           IF ( POTENTIAL-30 = "O" ) AND ( CPOT30 NOT = "Y" )
              GO TO END-CHECK-SET-PARM-LN.

      * VERIFY PAST DUE DAYS IN RANGE
           MOVE HOLD-TODAY-DATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.

           IF ( PDUE-DAYS < BEG-PASTDUE ) OR ( PDUE-DAYS > END-PASTDUE )
              GO TO END-CHECK-SET-PARM-LN.

      * CHECK BOR LAST NAME RANGE
           IF NOT (BEG-ALPHA = "    " AND END-ALPHA = "ZZZZ")
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM GET-BW-NAME
              IF ( IO-FG NOT = ZEROES )
                 MOVE "ZZZZ" TO TEST-NAME
              ELSE
                 MOVE BW-LNAME TO TEST-NAME
              END-IF
              IF TEST-NAME < BEG-ALPHA
                 OR TEST-NAME > END-ALPHA
                    GO TO END-CHECK-SET-PARM-LN.

      * VALID RECORD
           GO TO EXIT-CHECK-SET-PARM-LN.

       END-CHECK-SET-PARM-LN.
           MOVE "Y" TO WS-NEXT-LN.

       EXIT-CHECK-SET-PARM-LN.
           EXIT.

      ******************************************************************
      *    CHECKS FOR IF CJ-RECORDS MATCHES PARAMETERS ENTERED.
      ******************************************************************
       CHECK-SET-PARAMETERS-CJ SECTION.
      ******************************************************************

           MOVE "N" TO WS-NEXT-CJ.

           IF ( CJ-ACCTNO NOT = LN-ACCTNO )
              GO TO END-CHECK-SET-PARM-CJ.

           IF CJ-DATE < BEG-DATE OR CJ-DATE > END-DATE
              GO TO END-CHECK-SET-PARM-CJ.

      *  VALID RECORD
           GO TO EXIT-CHECK-SET-PARM-CJ.

       END-CHECK-SET-PARM-CJ.
           MOVE "Y" TO WS-NEXT-CJ.

       EXIT-CHECK-SET-PARM-CJ.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LINE-CJ SECTION.
      ******************************************************************

           MOVE SPACES TO HOLD-NAME.

      * L I N E - 1 

           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1            TO LN-FEED
              MOVE "Y"          TO RECORDS-FOUND
              MOVE "N"          TO FIRST-TIME
           ELSE
           IF ( ACCTNO-SUB = 1 )
              MOVE ALL "-"   TO DETAIL-LINE
              MOVE 2         TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF ( ACCTNO-SUB = 1 )
              PERFORM GET-BW-NAME
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D1-ACCTNO
              MOVE HOLD-NAME        TO D1-BW-NAME
              ADD  1                TO ACCTNO-SUB
           ELSE
              MOVE SPACES TO D1-ACCTNO
              MOVE SPACES TO D1-BW-NAME
              ADD  1      TO ACCTNO-SUB.

           MOVE CJ-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D1-DATE.

           MOVE CJ-START-TIME TO H-AND-M.
           PERFORM FORMAT-TIME.
           MOVE TIME-EDIT     TO D1-START-TIME.

           MOVE CJ-END-TIME TO H-AND-M.
           PERFORM FORMAT-TIME.
           MOVE TIME-EDIT   TO D1-END-TIME.

           MOVE CJ-ACTIVITY-CD   TO D1-ACTIVITY-CD.
           MOVE CJ-RESULT-CD     TO D1-RESULT-CD.
           MOVE CJ-ACTION-CD     TO D1-ACTION-CD.
           MOVE CJ-CON-STAT      TO D1-CON-STATUS.
           MOVE CJ-REC-STAT      TO D1-REC-STATUS.
           MOVE CJ-PROMISE-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D1-PROMISE-DATE.
           MOVE CJ-PROMISE-AMT   TO D1-PROMISE-AMT.
           MOVE CJ-CUR-BAL       TO D1-CURRENT-BAL.
           MOVE CJ-PAST-DUE      TO D1-PASTDUE-BAL.
           MOVE CJ-COLLID        TO D1-COLLID.
           MOVE CJ-NEW-COLLID    TO D1-NEW-COLLID.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( PRINT-MEMO = "N" )
              GO TO EXIT-PRINT-DETAIL-LINE-CJ.

           PERFORM OPEN-LM1-FILE.

           COMPUTE WS-CONVERT-DATE = 99999999 - CJ-DATE.

           MOVE LM-PATH-OWNBR   TO LM-BRNO
                                   QLM1-WBEG-BRNO
                                   QLM1-WEND-BRNO.
           MOVE CJ-ACCTNO       TO LM-ACCTNO
                                   QLM1-WBEG-ACCTNO
                                   QLM1-WEND-ACCTNO.
           MOVE WS-CONVERT-DATE TO LM-DATE
                                   QLM1-WBEG-DATE-REV.
           MOVE CJ-BEG-LM-SEQ   TO LM-SEQ
                                   QLM1-WBEG-SEQ.
           MOVE ALL "9"         TO QLM1-WEND-DATE-REV
                                   QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

       PRINTING-LINE-2.

           PERFORM READ-LM1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR ( LM-BRNO NOT = LM-PATH-OWNBR )
              PERFORM CLOSE-LM1-FILE
              GO TO EXIT-PRINT-DETAIL-LINE-CJ.

           PERFORM CHECK-LM-RECORD.

           IF ( WS-INVALID-LM-FG = "Y" )
              PERFORM CLOSE-LM1-FILE
              GO TO EXIT-PRINT-DETAIL-LINE-CJ
           ELSE
           IF ( WS-INVALID-LM-FG = "S" )
              GO TO PRINTING-LINE-2
           ELSE
              MOVE LM-MEMO1 TO D2-LM-MEMO1
              MOVE LM-MEMO2 TO D2-LM-MEMO2
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO PRINTING-LINE-2.

       EXIT-PRINT-DETAIL-LINE-CJ.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LINE-LN SECTION.
      ******************************************************************

           MOVE SPACES TO HOLD-NAME.

           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1            TO LN-FEED
              MOVE "Y"          TO RECORDS-FOUND
              MOVE "N"          TO FIRST-TIME
           ELSE
           IF ( ACCTNO-SUB = 1 )
              MOVE ALL "-"   TO DETAIL-LINE
              MOVE 2         TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           PERFORM GET-BW-NAME.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO  TO D1-ACCTNO.
           MOVE HOLD-NAME         TO D1-BW-NAME.
           ADD  1                 TO ACCTNO-SUB.
           MOVE MESSAGE-NO-CJ-REC TO D1-NO-CJ-REC.
           MOVE LN-CURBAL         TO D1-CURRENT-BAL.
           IF CPOT30 = "Y"
              MOVE "POT" TO CON-STAT-ALP
           ELSE
           IF CPOT30 = "M"
              MOVE "MIS" TO CON-STAT-ALP
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "PC" TO CON-STAT-ALP
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "PL" TO CON-STAT-ALP
           ELSE
              MOVE CONTRACTUAL TO CON-STAT-NUM.
           MOVE CON-STAT-ALP      TO D1-CON-STATUS.
           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-RECENCY.
           IF RDPOT30 = "Y"
              MOVE "<P>" TO REC-STAT-ALP
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO REC-STAT-ALP
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO REC-STAT-ALP
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO REC-STAT-ALP
           ELSE
              MOVE RECDEL TO REC-STAT-NUM.
           MOVE REC-STAT-ALP      TO D1-REC-STATUS.
           MOVE LN-PROMDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D1-PROMISE-DATE.
           MOVE LN-PROMAMT        TO D1-PROMISE-AMT.
           MOVE LN-COLLECTOR      TO D1-COLLID.
           MOVE 1                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-DETAIL-LINE-LN.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE
              IF ( DETAIL-LINE = ALL "-" )
                 MOVE SPACES TO DETAIL-LINE
                 GO TO EXIT-WRITE-DETAIL-LINE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H1-TIME.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD5 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 7 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE SPACES TO H2-COL-PROMPT H2-COLLID.
           MOVE 99 TO LN-COUNT.
           MOVE 4  TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "BRANCH GROUP ID                   :" TO RANGE-LINE
              MOVE ENT-GROUP                             TO R-WORD
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER           :" TO RANGE-LINE
              MOVE BEG-BRANCH                            TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER           :" TO RANGE-LINE
              MOVE END-BRANCH                            TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LOAN CLASS              :" TO RANGE-LINE.
           MOVE BEG-CLASS                             TO R-NUM.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    LOAN CLASS              :" TO RANGE-LINE.
           MOVE END-CLASS                             TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DEALER                  :" TO RANGE-LINE.
           MOVE BEG-DEALER                            TO R-NUM2.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    DEALER                  :" TO RANGE-LINE.
           MOVE END-DEALER                            TO R-NUM2.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCOUNT NUMBER          :" TO RANGE-LINE.
           MOVE BEG-ACCTNO                            TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    ACCOUNT NUMBER          :" TO RANGE-LINE.
           MOVE END-ACCTNO                            TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LAST NAME               :" TO RANGE-LINE.
           MOVE BEG-ALPHA TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING LAST NAME                  :" TO RANGE-LINE.
           MOVE END-ALPHA TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CONTRACTUAL AGE         :" TO RANGE-LINE.
           MOVE BEG-CONTRACT                          TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    CONTRACTUAL AGE         :" TO RANGE-LINE.
           MOVE END-CONTRACT                          TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING COLLECTOR ID            :" TO RANGE-LINE.
           MOVE BEG-COLLID TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING COLLECTOR ID               :" TO RANGE-LINE.
           MOVE END-COLLID TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROMISED ACCTS (INCLUDE,SKIP,ONLY):" TO RANGE-LINE.
           MOVE POTENTIAL-30                          TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING PROMISED DATE           :" TO RANGE-LINE.
           MOVE BEG-PROMDATE                          TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    PROMISED DATE           :" TO RANGE-LINE.
           MOVE END-PROMDATE                          TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIAL 30'S (INCLUDE,SKIP,ONLY):" TO RANGE-LINE.
           MOVE POTENTIAL-30                          TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING PAST DUE DAYS           :" TO RANGE-LINE.
           MOVE BEG-PASTDUE                           TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    PAST DUE DAYS           :" TO RANGE-LINE.
           MOVE END-PASTDUE                           TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L                             :" TO RANGE-LINE.
           MOVE PL-ISO                                TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *-----------------------------------------------------------------
           MOVE "ACTIVE, INACTIVE, BOTH            :" TO RANGE-LINE.
           MOVE PL-AIB                                TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACCOUNTS NOT WORKED SINCE.....    :" TO RANGE-LINE.
           MOVE CUTOFF-DATE                           TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "USE RESULT WORKED CODE            :"  TO RANGE-LINE.
           MOVE RECODE-FG                              TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACTIVITY DATE           :" TO RANGE-LINE.
           MOVE BEG-DATE                              TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    ACTIVITY DATE           :" TO RANGE-LINE.
           MOVE END-DATE                              TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT COLLECTOR ACCOUNT MEMOS     :" TO RANGE-LINE.
           MOVE PRINT-MEMO                            TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT SEQUENCE                    :" TO RANGE-LINE.
           IF PRINT-SEQ = "A"
              MOVE "BORROWER NAME"                    TO R-WORD
           ELSE IF PRINT-SEQ = "B"
              MOVE "DESCENDING BALANCE"               TO R-WORD
           ELSE IF PRINT-SEQ = "C"
              MOVE "COLLECTOR ID"                     TO R-WORD
           ELSE IF PRINT-SEQ = "N"
              MOVE "ACCOUNT NUMBER"                   TO R-WORD.
           PERFORM WRITE-DETAIL-LINE.

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           MOVE EXT-CONAME-CONAME TO COLRP2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO COLRP2-SYSDATE.
           DISPLAY COLRP2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE COLRP2-FORM-FIELDS TO WR-BUF.
           MOVE COLRP2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE COLRP2-HELP--FILE TO HELP-LINK-FILE.
           MOVE COLRP2-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/COLRP2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCEPP.CPY".

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       GET-BW-NAME.
           MOVE LN-SSNO(1) TO BW-SSNO
           PERFORM OPEN-BW1-FILE
           PERFORM READ-BW1-FILE
           IF IO-FG NOT = 0
              MOVE "*** NONE ***" TO HOLD-NAME
           ELSE
              STRING BW-LNAME,", ",BW-FNAME
                     DELIMITED BY "   " INTO HOLD-NAME.
           PERFORM CLOSE-BW1-FILE.

      *-----------------------------------------------------------------
       GET-RESULT-CODE.
           MOVE "RE"         TO CD-TYPE.
           MOVE CJ-RESULT-CD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO CD-RE-WORKED-ACCOUNT.

      *-----------------------------------------------------------------
       GET-SP-RECORD.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE.

      *-----------------------------------------------------------------
       CHECK-LM-RECORD.
           IF ( LM-ACCTNO NOT = CJ-ACCTNO )
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
           IF ( LM-DATE NOT = WS-CONVERT-DATE )
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
           IF ( LM-SEQ > CJ-END-LM-SEQ )
              MOVE "Y" TO WS-INVALID-LM-FG
           ELSE
              MOVE "N" TO WS-INVALID-LM-FG.

           IF WS-INVALID-LM-FG = "N"
              MOVE LM-MEMO1 TO TEST-MEMO1
              IF TEST-COLON = ":"
                 IF TEST-AM-PM = "AM" OR "PM"
                    MOVE "S" TO WS-INVALID-LM-FG.

      *-----------------------------------------------------------------
      *    FORMAT CLOCK TIME
      *-----------------------------------------------------------------
       FORMAT-TIME.
           IF ( H-AND-M NOT GREATER THAN 1159 )
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.

           IF ( H-AND-M IS NOT LESS THAN 1300 )
              SUBTRACT 1200 FROM H-AND-M.

           IF ( CUR-HH = 0 )
              MOVE 12 TO CUR-HH.

           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":"    TO T-COL.
           MOVE SPACE  TO T-SPACE.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPCJ2RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              IF ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           IF PRINT-SEQ = "A" OR PRINT-SEQ = "B" OR PRINT-SEQ = "C"
              MOVE MKTEMP-BUF TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.
      *-----------------------------------------------------------------
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY COLRP2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

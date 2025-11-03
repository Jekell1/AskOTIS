       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHBANK.
       DATE-WRITTEN.    080492.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CHANGE BANKRRUPT STATUS
      *                 CALLED FROM F9 INTERRUPT AND LONPF2
      *
      * REV:
      *  083195 BAH ADDED SET OF LN-BNKRPT-CHAPTER
      *  MJD 120696 REMOVED TEST FOR NUMERIC ZIP FOR CANADIAN ZIP CODES
      *             ALLOW ENTRY OF SPACES FOR DEBTOR'S ATTORNEY NAME.
      *             ALLOW ENTRY OF SPACES FOR STATE IF SPACES ARE ENTERED
      *             FOR ADDRESS 1 AND 2.
      *  BAH 101697 ADD PASSWORD FOR MODIFY CAPABILITY, PR# 565 INSTANT
      *  MJD 112597 EXPANDED LT-REC TO 144 BYTES (ADDED 30)
      *  BAH 060998 DONT ALLOW A NEW-RECORD HERE, MUST POST 'BK', MERCURY 174
      *  BAH 070898 DONT ALLOW "DELETE" HERE, MUST POST 'BD', MERCURY 195
      *  BAH 070998 DONT ALLOW CHANGES IF NOT IN BNKRPT STATUS, TRAILER DOES
      *             NOT GET DELETED WHEN 'BD' POSTED, SO DONT ALLOW CHANGES !
      *  BAH 990702 ALLOW ACCESS TO THIS SCREEN BEFORE 'BK' POSTING, TFC # 26
      *             ONLY IF GP-BK-PRIOR-TO-POSTING = "Y"
      *  BAH 990728 DONT FORCE CASE, CHAPTER, AND DONT ALLOW CHANGING OF
      *             FILEDATE FOR TFC GP-BK-PRIOR-TO-POSTING..
      *  BAH 991007 ALLOW F7 TO EXIT ON EACH FIELD FOR GP-BK-PRIOR-TO-POSTING
      *             IF GET IN BY ACCIDENT, CANT GET OUT ....JUDY/TFC
      *  BAH 991008 TFC DOES NOT WANT TO STOP ON CHAPTER WHEN BUILDING
      *             PRIOR TO POSTING THE "BK", JUDY AND SUSAN
      *  BAH 020923 ADDED LT-B-BDDATE, BNKRPT DISMISSAL, REGACC PR# 1901
      *             DISPLAY ONLY
      *  BAH 040305 ADDED LT-B-WHO-FILED, WHO FILED, REGACC PR# 51
      *  BAH 050607 FORCE A DASH IN 3RD POSITION OF CASE #, REGACC PR# 104
      *  BAH 051107 ADDED LT-B-ENDORSER-FILED, MULLEN PR# 2645
      *  BAH 060921 CHANGE PROMPTS "BANKRUPTCY COURT CLERK:" TO "BANKRUPTCY
      *             DISTRICT COURT:" AND "7. NAME: " TO "7. DISTRCT:" FOR
      *             GP-I-AM-WORLD, PR# 493
      *  BAH 060925 DONT STOP ON BANKRUPT CHAPTER IF GP-AUTO-BANKRUPT-WINDOW
      *             = "Y", NEW CHANGE FOR WORLD PR# 493
      *  BAH 070130 ACT AS A NEW RECORD IF GP-AUTO-BANKRUPT-WINDOW AND YOU
      *             CALLED CHBANK FROM LONPF2, WORLD PR# 523
      *             THEY DONT WANT TO HAVE TO MODIFY EACH FIELD, THEY WANT
      *             TO STEP THRU IT.
      *  BLF 070815 IF ARCHIVED LOAN, SET UP ARCHIVE PATH FOR LT & DON'T
      *             ALLOW ANY CHANGES OR UPDATES, WORLD PR# 547
      *  BAH 080221 ADDED 22. CLAIM AMT: FOR WORLD, PAYOFF WITH INS REBATES
      *             ADDED BACK IN, WORLD PR# 570
      *  BAH 081013 ADDED 23. ACTIVE/INACTIVE FOR REGACC, PR# 223
      *  BAH 090505 ALLOW " " IN WHO-FILED FIELD 20 FOR THE CASE OF JUST
      *             AN ENDORSER FILING, IN WHICH CASE, THEY WILL ENTER
      *             SOMETHING IN FIELD 21 ENDORSER FILED, AUTOM CREDIT PL# 653
      *  BAH 111014 ADDED LT-B-INT-LC-DUE, LT-B-OTH-DUE, LT-B-TDUEAMT
      *             FOR PROOF OF CLAIM REQUIREMENTS, WORLD PR# 774
      *   CS 111128 PER PR#752, IF ANY STATUSFLAG IN LOAN RECORD IS 'B' BUT THE
      *             BANKRUPT DATE IS ZEROS, SEARCH THE LP FOR A NON-REVERSED
      *             'BK' & PUT THAT DATE IN THE LOAN RECORD. BUT THAT CREATES
      *             A PROBLEM WHEN GP-BK-PRIOR-TO-POSTING NOT = "Y", IT WONT
      *             ALLOW YOU TO CREATE THE BK TRAILER, ADDED IF THIS FLAG IS
      *             SET & LN-BNKRPTDATE = ZEROS, TELL THEM TO POST THE 'BK'
      *             IF ITS NOT ZEROS, ALLOW ACCESS TO TRAILER RECORD, THIS WOULD
      *             HAPPEN ON ODDBALL POSTING OF: BK,BD,BK,RV,RV WHICH SETS
      *             THE 'B' STATUS, BUT NOT BK DATE IN LOAN RECORD, I ADDED A
      *             SEARCH OF LP WHICH UPDATES THE BNKRPTDATE IF FINDS A NON
      *             REVERSED BK, BUT WERENT ALLOWED TO ACCESS TRAILER, SO CHANGE      *             TO GP-BK-PRIOR-TO-POSTING NOT = "Y" AND THE LN-BNKRPTDATE =
      *             ZEROS PR#752
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      * 160427 BAH ADDED PAGE 2 CHBNK2 FOR REAFFIRMATIONS AND DISCHARGES
      *                                                           WORLD PR# 1150
      *  BAH 170407 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181130 GLOBAL LMFILE
      *  BAH 181218 REMOVED ARCHIVE
      *  BAH 200929 ADDED DISPLAY OF LTB-GROSS-BAL #1461
      * BAH 2022.0803 HARD CODED START
      * BAH 2024.0417 REMOVED CHBANKW, CALL PROGRAM NOT WINDOW, NOTHING TO PASS
      *               CALL CHBNK2, NOT WINDOW, REMOVED CHBNK2W
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *******************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CHBANK.CBL S35 02/22/24-12:51:09 >".

       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       COPY "LIBLP/LP01NO.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

      *********************************************************
      *   LTBFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTB-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE "/LP/LTBFILE".

       01  CHBANK-LINES              PIC 99  VALUE 22.
       01  CHBANK-COLUMNS            PIC 99  VALUE 78.
       01  CHBANK-BEGIN-Y            PIC 99  VALUE 2.
       01  CHBANK-BEGIN-X            PIC 99  VALUE 2.

       01  ERRCD           PIC X      VALUE SPACES.
       01  LEGEND-01.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F2-MEMO, (98)F5-FILEDATE, F".
           03  FILLER       PIC X(28)  VALUE
           "7-UPDATE, ENTER FIELD #, P2:".
       01  LEGEND          PIC X(70).
       01  CHAN            PIC X(4).

       01  HOLD-SYS-DATE   PIC 9(8).
       01  HOLD-CH-SEQ     PIC 9(3).
       01  BRANCH          PIC 9(4)   VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 99.
       01  HOLD-BNKRPTDATE PIC 9(8)    COMP-3.
       01  CHG-BNKRPTDATE  PIC 9(8)    COMP-3.
       01  CHG-B-CHAPTER   PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

      * ORIGINAL RECORD
           COPY "LIBLP/LP01LTB.CPY" REPLACING LEADING "LTB" BY "LTB-OR".
      * CHANGED RECORD
           COPY "LIBLP/LP01LTB.CPY" REPLACING LEADING "LTB" BY "LTB-CH".

       01  CLAIM-PASSWD-FLAG     PIC X VALUE SPACES.

      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE " ".
           03  FILEDATE    PIC 99/99/99.
           03  D-GROSS-BAL PIC Z(6).99.
           03  F01         PIC X(9).
           03  F011        PIC 9(2).
           03  BD-DATE     PIC 99/99/99.

           03  F02         PIC X(24).
           03  F03         PIC X(24).
           03  F031        PIC X(24).
           03  F04         PIC X(15).
           03  F05         PIC XX.
           03  F051        PIC X(10).
           03  F06         PIC X(12).

           03  F07         PIC X(24).
           03  F08         PIC X(24).
           03  F081        PIC X(24).
           03  F09         PIC X(15).
           03  F10         PIC XX.
           03  F101        PIC X(10).
           03  F11         PIC X(12).

           03  F12         PIC X(24).
           03  F13         PIC X(24).
           03  F131        PIC X(24).
           03  F14         PIC X(15).
           03  F15         PIC XX.
           03  F151        PIC X(10).
           03  F16         PIC X(12).

           03  F17         PIC X(30).
           03  F18         PIC X(30).
           03  F19         PIC Z(6).99.
           03  F191        PIC Z(6).99.
           03  F192        PIC Z(6).99.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC Z(7).99.
           03  F23         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(30) VALUE
           "FILEDT BALANCE 010 011 BDDATE ".
           03  FILLER      PIC X(28) VALUE
           "020 030 031 040 050 051 060 ".
           03  FILLER      PIC X(28) VALUE
           "070 080 081 090 100 101 110 ".
           03  FILLER      PIC X(28) VALUE
           "120 130 131 140 150 151 160 ".
           03  FILLER      PIC X(36) VALUE
           "170 180 190 191 192 200 210 220 230.".

       01  MAX             PIC 99   VALUE 23.

      **********************************
      *   ELEMENT SPECIFICATIONS
      **********************************
       01  SPEC-TABLE.
      * 1. CASE NO
           03  FILLER      PIC X(8) VALUE "E  A090 ".

      * 2. ATTORNEY NAME
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 3. ATTORNEY ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 4. ATTORNEY CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 5. ATTORNEY STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6. ATTORNEY PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".

      * 7. CLERK NAME
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 8. CLERK ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 9. CLERK CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      *10. CLERK STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *11. CLERK PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".

      *12. TRUSTEE NAME
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *13. TRUSTEE ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *14. TRUSTEE CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      *15. TRUSTEE STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *16. TRUSTEE PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".

      *17. MISC 1
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      *18. MISC 1
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      *19. INT/LC DUE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      *20. WHO FILED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *21. ENDORSER FILED
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *22. CLAIM AMOUNT
           03  FILLER      PIC X(8) VALUE "ENNN072 ".
      *23. ACATIVE/INACTIVE
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 23 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ************(WINDOW INFORMATION):
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBWI/CHBANK_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *01  DELETE-SEQ            PIC 99 VALUE 00.
       01  MODIFY-SEQ            PIC 99 VALUE 01.
       01  CLAIM-AMT-SEQ         PIC 99 VALUE 02.

       01  CHBANK-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHBANK_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHBANK_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CHBANK_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHBANK" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CHBANK-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CHBANK-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CHBANK-COLUMNS + VDU-WIN-W-MOD
                      LINES   CHBANK-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CHBANK-WINDOW-HANDLE.

           MOVE "S  A240TITLE:FR." TO VDU.
           MOVE " BANKRUPTCY INFORMATION" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM GET-GPENV.

           MOVE "S  A1107P." TO VDU.
           MOVE "7. DISTRCT:" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A260TITLE1." TO VDU.
           MOVE DISTRICT-TITLE TO VDUBUF.
           PERFORM SCREENIO.

      * GET LOAN FOR TESTING WHO FILED, NEED TO KNOW IF SECONDARY MAKER
           PERFORM TEST-LOAN-FILE.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           PERFORM OPEN-SP1-FILE.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID SPR" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM GET-LT-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "P2 "
              GO TO MAIN-MODULE.

           GO TO MAIN-MODULE.

      *************************
       GET-LT-MODULE SECTION.
      *************************

      * ORIGINAL
           MOVE SPACES TO LTB-OR-REC.

           MOVE " " TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE LN-OWNBR         TO LTB-BRNO.   
           MOVE EXT-WI-CL-ACCTNO TO LTB-ACCTNO.
           MOVE 1                TO LTB-SEQNO.
           PERFORM GET-LT.
           IF IO-FG = 9
              IF (GP-BK-PRIOR-TO-POSTING NOT = "Y" AND
                     LN-BNKRPTDATE = ZEROS)
                 MOVE "YOU MUST POST A 'BK' TO ESTABLISH BANKRUPTCY"
                       TO MESS
                 PERFORM SEND-MESS
                 MOVE "END" TO IO-TYPE
                 GO TO END-GET-LT-MODULE
              ELSE
                 PERFORM NEW-RECORD-SETUP
                 GO TO END-GET-LT-MODULE.

           MOVE 0 TO NEW-FLAG.
           MOVE LTB-REC TO LTB-OR-REC.

      * THIS IS A TRIGGER THAT YOU CALLED FROM LONPF2 AND WANT TO STEP THRU
      * FIELDS
           IF GP-AUTO-BANKRUPT-WINDOW = "Y" AND EXT-WI-CL-SALEFIN = "F"
              PERFORM COMPUTE-CLAIM-AMT
              COMPUTE LTB-CLAIM-AMT = PASS-NETDUE + PASS-PRINSRB
              MOVE LTB-CLAIM-AMT TO LTB-CLAIM-AMT
              MOVE PASS-PRINTDUE TO LTB-INT-LC-DUE
                                    LTB-INT-LC-DUE
              MOVE LN-OTHBAL TO LTB-OTH-DUE LTB-OTH-DUE
              PERFORM COMPUTE-TDUEAMT
              MOVE NO-TDUEAMT TO LTB-TDUEAMT LTB-TDUEAMT
              MOVE 1 TO NEW-FLAG.

           PERFORM DISPLAY-FIELDS.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       END-GET-LT-MODULE.
           EXIT.

      *******************************
       NEW-RECORD-SETUP SECTION.
      *******************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LTB-FILE.

           MOVE LN-OWNBR         TO LTB-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LTB-ACCTNO.
           MOVE 1                TO LTB-SEQNO.
           MOVE 0 TO ELE.
           MOVE 0 TO HOLD-BNKRPTDATE CHG-BNKRPTDATE.

           IF LN-SSNO(2) = 0
              MOVE "S  A010200." TO VDU
              MOVE "M" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A010200." TO VDU
              MOVE "B" TO F20 VDUBUF
              PERFORM SCREENIO.

           PERFORM COMPUTE-CLAIM-AMT.
           COMPUTE LTB-CLAIM-AMT = PASS-NETDUE + PASS-PRINSRB.
           MOVE "SNNN072220." TO VDU.
           MOVE LTB-CLAIM-AMT TO VDUNUM2 F22 LTB-CLAIM-AMT.

      D    STOP MESS.
           MOVE PASS-PRINTDUE TO LTB-INT-LC-DUE.
           MOVE "SNNN062190." TO VDU.
           MOVE LTB-INT-LC-DUE TO VDUNUM2 F19 LTB-INT-LC-DUE.
           MOVE LN-OTHBAL TO LTB-OTH-DUE.
           MOVE "SNNN062191." TO VDU.
           MOVE LTB-OTH-DUE TO VDUNUM2 F191 LTB-OTH-DUE.
           PERFORM COMPUTE-TDUEAMT.
           MOVE NO-TDUEAMT TO LTB-TDUEAMT.
           MOVE "SNNN062192." TO VDU.
           MOVE LTB-TDUEAMT TO VDUNUM2 F192 LTB-TDUEAMT.

           PERFORM SCREENIO.

       NEW-RECORD-SETUP-EXIT.
           EXIT.

      *******************************
       ENTER-FILEDATE SECTION.
      *******************************
           MOVE " " TO IO-TYPE.

      * TEST FOR PASSWORD TO BE ABLE TO MODIFY ANY FIELD...
           IF NEW-FLAG NOT = 1
              MOVE MODIFY-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-FILEDATE-EXIT.

           MOVE " F1-RESET, F7-EXIT, ENTER BANKRUPT DATE:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060FILEDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-FILEDATE-EXIT.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO ENTER-FILEDATE-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FILEDATE.
           IF VDUNUM0 = 0
              GO TO ENTER-FILEDATE.
           MOVE VDU-DATE-YYYYMMDD TO LTB-FILEDATE
                                     CHG-BNKRPTDATE.

       ENTER-FILEDATE-EXIT.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "L" TO MEMOSC-ONLY
              MOVE "B" TO MEMOSC-MEMO-TYPE
              MOVE HOLD-SYS-DATE    TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1>"
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

      *    IF EXT-WI-CL-ARCHIVE-FG = "Y"
      *       GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
      * REMOVED DELETE OPTION, MUST POST "BD", MERCURY #195 7/8/98
      * ONLY ALLOW DELETE IF GP FLAG FOR 'TFC' IS ON, AND YOU HAVE
      * NOT POSTED A 'BK' TO MAKE THE ACCT BANKRUPT., TFC#26 7/2/99
           IF ACTION = "DELETE"
              IF GP-BK-PRIOR-TO-POSTING = "Y" AND HOLD-BNKRPTDATE = 0
                 IF NEW-FLAG = 0
                    MOVE "DEL" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.

           IF FORM-KEY1 = " " 
              IF FORM-P1 = "P"
                 IF FORM-SEL1 = 2
                    PERFORM WRITE-MODULE
                    MOVE "WI/CHBNK2" TO FORM-NAM
                    CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                    CANCEL FORM-PROGX
                    MOVE "WI/CHBANK" TO FORM-NAM
                    MOVE FORM-PATH TO FORM-PATHNAME
                    MOVE "P2 " TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE
                 ELSE
                    GO TO MODIFY-ROUTINE.


           INSPECT ACTION REPLACING ALL " " BY ZEROS.
           IF VDUSTATUS = "1:"
              IF ACTION = "000098"
      * TFC DOES NOT WANT THEM TO BE ABLE TO CHANGE FILEDATE, THEY MUST
      * REVERSE THE 'BK' AND DO IT RIGHT !! 7/28/99
                 IF GP-BK-PRIOR-TO-POSTING NOT = "Y"
                    PERFORM ENTER-FILEDATE
                    IF IO-TYPE NOT = " "
                       GO TO EXIT-ENTRY-MODULE
                    ELSE
                       GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      * DONT ALLOW CHANGES IF ACCOUNT IS NOT CURRENTLY IN BANKRUPT STATUS
           PERFORM TEST-LOAN-FILE.
           IF IO-FG NOT = 0 OR LN-BNKRPTDATE = 0
              IF GP-BK-PRIOR-TO-POSTING NOT = "Y"
                 MOVE "ACCOUNT NOT IN BANKRUPT STATUS, CAN'T MODIFY"
                   TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE.

      * TEST FOR PASSWORD TO BE ABLE TO MODIFY ANY FIELD...
           MOVE MODIFY-SEQ TO PASSWDW-SEQ.
           MOVE "TO ALLOW MODIFY:" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
      * DONT STOP ON CLAIM AMT WHEN FILLING IN THE RECORD FOR THE FIRST TIME
      * DONT STOP ON ANY FIELD 19'S EITHER, INT-LC-DUE, OTH-DUE, TDUEAMT
           IF NEW-FLAG = 1
              IF ELE = 22 OR 19
                 IF GP-AUTO-BANKRUPT-WINDOW = "Y"
                    GO TO CREATE-ROUTINE.

           MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F7.
           IF GP-BK-PRIOR-TO-POSTING = "Y"
              IF VDUSTATUS = "1>"
                 MOVE "END" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
       TEST-F2.
           IF VDUSTATUS NOT = "12"
              GO TO TEST-F4.

      * F2 COMPUTE THE CLAIM AMOUNT AGAIN, IN CASE YOU WIPED IT OUT
           IF ELE NOT = 22 AND NOT = 19
              GO TO TEST-F4.

           IF ELE = 22
      * ONLY ASK PASSWORD IF ORIGINAL AMOUNT NOT = 0
            IF LTB-CLAIM-AMT NOT = 0
             IF CLAIM-PASSWD-FLAG NOT = "Y"
               MOVE CLAIM-AMT-SEQ TO PASSWDW-SEQ
               MOVE "TO CHANGE AMOUNT:" TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  GO TO TEST-F4.

           IF ELE = 22
              MOVE "Y" TO CLAIM-PASSWD-FLAG
              PERFORM COMPUTE-CLAIM-AMT
              MOVE "S  N062220." TO VDU
              COMPUTE VDUNUM2 = PASS-NETDUE + PASS-PRINSRB
              PERFORM SCREENIO.

           IF ELE = 19
      * ONLY ASK PASSWORD IF ORIGINAL AMOUNT NOT = 0
            IF LTB-INT-LC-DUE NOT = 0
             IF CLAIM-PASSWD-FLAG NOT = "Y"
               MOVE CLAIM-AMT-SEQ TO PASSWDW-SEQ
               MOVE "TO CHANGE AMOUNT:" TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  GO TO TEST-F4.

           IF ELE = 19
              MOVE "Y" TO CLAIM-PASSWD-FLAG
              PERFORM COMPUTE-CLAIM-AMT
              MOVE "SNNN062190." TO VDU
              MOVE PASS-PRINTDUE TO VDUNUM2
              PERFORM SCREENIO.


       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      **********************************
      *      SET RECORD ELEMENTS
      **********************************
       F-01.
           IF VDUBUF = " "
      * TFC DOES NOT WANT CASE # FORCED ENTRY  7/28/99
              IF GP-BK-PRIOR-TO-POSTING NOT = "Y"
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO LTB-CASE-NO.

       F-011.
      * TFC DOES NOT WANT TO STOP ON CHAPTER
           IF GP-BK-PRIOR-TO-POSTING = "Y" OR
               GP-AUTO-BANKRUPT-WINDOW = "Y"
      * DONT CLEAR IT IN CASE YOU ALREADY ENTERED IT BEFORE !!
      *       MOVE 0 TO F011 CHG-B-CHAPTER
              GO TO CREATE-ROUTINE.
           MOVE "ENNN020011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.
           MOVE VDUNUM0 TO LTB-CHAPTER CHG-B-CHAPTER.
           IF NOT (LTB-CHAPTER = 7 OR 11 OR 13)
              GO TO F-011.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUBUF TO LTB-NAME(1).
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO LTB-ADR1(1).
       F-031.
           MOVE "E  A024031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-031.
           MOVE VDUBUF TO LTB-ADR2(1).
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO LTB-CITY(1).
           GO TO CREATE-ROUTINE.
       F-05.
      * IF THERE IS NO ADDRESS, THEN ALLOW "  " FOR STATE
           IF VDUBUF = SPACES AND LTB-ADR1(1) = SPACES AND
                                  LTB-ADR2(1) = SPACES
              MOVE VDUBUF TO LTB-STATE(1)
              GO TO F-051.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST1-KEY.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           MOVE VDUBUF TO LTB-STATE(1).
       F-051.
           MOVE "E  A010051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-051.
           MOVE VDUBUF TO LTB-ZIP(1).
           GO TO CREATE-ROUTINE.
       F-06.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO LTB-PHONE(1).
           PERFORM SEND-PHONE.
           MOVE "S  A120060." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUBUF TO LTB-NAME(2).
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LTB-ADR1(2).
       F-081.
           MOVE "E  A024081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-031.
           MOVE VDUBUF TO LTB-ADR2(2).
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO LTB-CITY(2).
           GO TO CREATE-ROUTINE.
       F-10.
           IF VDUBUF = " " AND LTB-NAME(2) = SPACES
              MOVE VDUBUF TO LTB-STATE(2)
              GO TO F-101.
           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST1-KEY.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           MOVE VDUBUF TO LTB-STATE(2).
       F-101.
           IF VDUBUF = " " AND LTB-NAME(2) = " "
              MOVE VDUBUF TO F101
              MOVE "S  A010101." TO VDU
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE "E  A010101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-101.
           MOVE VDUBUF TO LTB-ZIP(2).
           GO TO CREATE-ROUTINE.
       F-11.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO LTB-PHONE(2).
           PERFORM SEND-PHONE.
           MOVE "S  A120110." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDUBUF TO LTB-NAME(3).
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO LTB-ADR1(3).
       F-131.
           MOVE "E  A024131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-131.
           MOVE VDUBUF TO LTB-ADR2(3).
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO LTB-CITY(3).
           GO TO CREATE-ROUTINE.
       F-15.
           IF VDUBUF = " " AND LTB-NAME(3) = " "
              MOVE VDUBUF TO LTB-STATE(3)
              GO TO F-151.
           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST1-KEY.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           MOVE VDUBUF TO LTB-STATE(3).
       F-151.
           IF VDUBUF = " " AND LTB-NAME(3) = " "
              MOVE VDUBUF TO F151
              MOVE "S  A010151." TO VDU
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE "E  A010151." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-151.
           MOVE VDUBUF TO LTB-ZIP(3).
           GO TO CREATE-ROUTINE.
       F-16.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO LTB-PHONE(3).
           PERFORM SEND-PHONE.
           MOVE "S  A120160." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-17.
           MOVE VDUBUF TO LTB-MISC1.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO LTB-MISC2.
           GO TO CREATE-ROUTINE.
       F-19.
      * NEED PASSWORD TO CHANGE ORIGINAL CALCULATED AMOUNT
      * CLAIM-PASSWD-FLAG IS TO PREVENT THIS PASSWORD BEING ASKED
      * NUMEROUS TIMES IF PRESSING F2 TO RECALC AND THEN HERE ALSO
      * DONT ASK PASSWORD IF ORIGINAL AMOUNT IS ZERO
           IF LTB-INT-LC-DUE NOT = 0
              IF VDUNUM2 NOT = LTB-INT-LC-DUE
                 IF CLAIM-PASSWD-FLAG NOT = "Y"
                    MOVE CLAIM-AMT-SEQ TO PASSWDW-SEQ
                    MOVE "TO CHANGE AMOUNT:" TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       MOVE "SNNN062190." TO VDU
                       MOVE LTB-INT-LC-DUE TO VDUNUM2
                       PERFORM SCREENIO
                       GO TO ENTER-ELE
                    ELSE
                       MOVE "Y" TO CLAIM-PASSWD-FLAG.

           MOVE VDUNUM2 TO LTB-INT-LC-DUE.

       F-191.
           MOVE "ENNN062191." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "12"
              MOVE "SNNN062191." TO VDU
              MOVE LN-OTHBAL TO VDUNUM2
              PERFORM SCREENIO.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-191.

           IF LTB-OTH-DUE NOT = 0
              IF VDUNUM2 NOT = LTB-OTH-DUE
                IF CLAIM-PASSWD-FLAG NOT = "Y"
                   MOVE CLAIM-AMT-SEQ TO PASSWDW-SEQ
                   MOVE "TO CHANGE AMOUNT:" TO PASSWDW-PROMPT2
                   PERFORM VERIFY-PASSWORD
                   IF PASSWDW-STAT NOT = "OK"
                      MOVE "SNNN062191." TO VDU
                      MOVE LTB-OTH-DUE TO VDUNUM2 
                      PERFORM SCREENIO
                      GO TO F-191
                   ELSE
                      MOVE "Y" TO CLAIM-PASSWD-FLAG.

           MOVE VDUNUM2 TO LTB-OTH-DUE.

       F-192.
           MOVE "ENNN062192." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-191.
           IF VDUSTATUS = "12"
              PERFORM COMPUTE-TDUEAMT
              MOVE "SNNN062192." TO VDU
              MOVE NO-TDUEAMT TO VDUNUM2
              PERFORM SCREENIO.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-192.

           IF LTB-TDUEAMT NOT = 0
              IF VDUNUM2 NOT = LTB-TDUEAMT
                 IF CLAIM-PASSWD-FLAG NOT = "Y"
                    MOVE CLAIM-AMT-SEQ TO PASSWDW-SEQ
                    MOVE "TO CHANGE AMOUNT:" TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       MOVE "SNNN062192." TO VDU
                       MOVE LTB-TDUEAMT TO VDUNUM2 
                       PERFORM SCREENIO
                       GO TO F-192
                    ELSE
                       MOVE "Y" TO CLAIM-PASSWD-FLAG.

           MOVE VDUNUM2 TO LTB-TDUEAMT.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO LTB-WHO-FILED.
           IF VDUBUF NOT = "M" AND NOT = "S" AND NOT = "B"  AND
                      NOT = " "
              MOVE "M=MAKER, S=SECONDARY, B=BOTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF VDUBUF = "S" OR "B"
              IF LN-MAKERCD(2) NOT = "J" AND
                  LN-MAKERCD(3) NOT = "J" AND
                   LN-MAKERCD(4) NOT = "J"
                 MOVE "NO JOINT MAKER ON ACCOUNT" TO MESS
                 PERFORM SEND-MESS
                 MOVE "S  A010200." TO VDU
                 MOVE "M" TO VDUBUF LTB-WHO-FILED
                 PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-21.
           MOVE VDUBUF TO LTB-ENDORSER-FILED.
           IF VDUBUF NOT = "1" AND NOT = "2"
                   AND NOT = "B" AND NOT = " "
              MOVE "1=1ST END, 2=2ND END, B=BOTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF VDUBUF NOT = " "
              IF LN-MAKERCD(2) NOT = "E" AND
                  LN-MAKERCD(3) NOT = "E" AND
                   LN-MAKERCD(4) NOT = "E"
                 MOVE "NO ENDORSERS ON THIS ACCOUNT" TO MESS
                 PERFORM SEND-MESS
                 MOVE "S  A010210." TO VDU
                 MOVE " " TO LTB-ENDORSER-FILED VDUBUF
                 PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-22.

      * NEED PASSWORD TO CHANGE ORIGINAL CALCULATED AMOUNT
      * CLAIM-PASSWD-FLAG IS TO PREVENT THIS PASSWORD BEING ASKED
      * NUMEROUS TIMES IF PRESSING F2 TO RECALC AND THEN HERE ALSO
      * DONT ASK PASSWORD IF ORIGINAL AMOUNT IS ZERO
           IF LTB-CLAIM-AMT NOT = 0
              IF VDUNUM2 NOT = LTB-CLAIM-AMT
                 IF CLAIM-PASSWD-FLAG NOT = "Y"
                    MOVE CLAIM-AMT-SEQ TO PASSWDW-SEQ
                    MOVE "TO CHANGE AMOUNT:" TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       MOVE "SNNN072220." TO VDU
                       MOVE LTB-CLAIM-AMT TO VDUNUM2 
                       PERFORM SCREENIO
                       GO TO ENTER-ELE
                    ELSE
                       MOVE "Y" TO CLAIM-PASSWD-FLAG.

           MOVE VDUNUM2 TO LTB-CLAIM-AMT.
           GO TO CREATE-ROUTINE.

       F-23.
           MOVE VDUBUF TO LTB-ACTIVE-INACTIVE.
           IF VDUBUF NOT = "A" AND NOT = "I"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************
       WRITE-MODULE SECTION.
      ************************************

           PERFORM OPEN-LTB1-FILE.

      * CAN'T TEST NEW-FLAG BECAUSE IT IS MANIPULATED FOR AUTO BANKRUPT 
      * FROM PAYMENT POSTING
           IF LTB-OR-REC = SPACES
              GO TO WRITE-RECORD.

           MOVE LTB-REC TO LTB-CH-REC.

           IF LTB-CH-REC = LTB-OR-REC
              GO TO END-WRITE-MODULE.

       REWRITE-RECORD.
           PERFORM READ-LTB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           MOVE LTB-CH-REC TO LTB-REC.
           PERFORM REWRITE-LTB1-FILE.

           PERFORM CREATE-CHANGE-REC.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-LTB1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           EXIT.


      **************************************
       DELETE-MODULE SECTION.
      **************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LTB-REC NOT = LTB-OR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-LTB1-FILE.

           PERFORM CREATE-CHANGE-REC.

           PERFORM DELETE-LM1-MODULE.
       END-DELETE-MODULE.
           EXIT.

      ****************************************
       DELETE-LM1-MODULE SECTION.
      ****************************************
           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE EXT-WI-CL-ACCTNO  TO LM-ACCTNO
                                     QLM1-WBEG-ACCTNO
                                     QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ 
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.
       DELETE-LM1-MODULE-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO DELETE-LM1-MODULE-EXIT.

           IF LM-ACCTNO NOT = EXT-WI-CL-ACCTNO
              GO TO DELETE-LM1-MODULE-EXIT.
           IF LM-MEMO-TYPE NOT = "B"
              GO TO DELETE-LM1-MODULE-NEXT.

           PERFORM DELETE-LM1-FILE.
           GO TO DELETE-LM1-MODULE-NEXT.

       DELETE-LM1-MODULE-EXIT.
           PERFORM CLOSE-LM1-FILE.


      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHBANK" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       CREATE-CHANGE-REC SECTION.
      ***************************************************
           IF HOLD-BNKRPTDATE = CHG-BNKRPTDATE
              GO TO END-CREATE-CH-REC.

           PERFORM OPEN-CH-FILE.

           MOVE 1 TO HOLD-CH-SEQ.

       CREATE-CHANGE-REC-AGAIN.
           PERFORM CLEAR-CH-FILE.
           MOVE BRANCH             TO CH-BRNO. 
           MOVE HOLD-SYS-DATE      TO CH-DATE.
           MOVE "B"                TO CH-CODE.
           MOVE EXT-CONAME-USER-ID TO CH-USERID.
           PERFORM GET-TIME.
           MOVE HH-MM-SS           TO CH-TIME.
           MOVE HOLD-CH-SEQ        TO CH-SEQ.

           MOVE LN-ACCTNO          TO CH-ACCTNO.

           MOVE "BK FILEDATE"    TO CH-DESCRIPT.

           MOVE HOLD-BNKRPTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO CH-ORG-DATE.
           MOVE CHG-BNKRPTDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO CH-NEW-DATE.

           PERFORM WRITE-CH-FILE.
           IF IO-FG NOT = 0
              IF HOLD-CH-SEQ < 999
                 ADD 1 TO HOLD-CH-SEQ
                 GO TO CREATE-CHANGE-REC-AGAIN.

           PERFORM CLOSE-CH-FILE.

           PERFORM OPEN-BR-FILE.
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF BR-CHANGE-MEMO = "Y"
              MOVE "CL/CHMEMO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CH-REC
              CANCEL FORM-PROGX.

       END-CREATE-CH-REC.
           EXIT.

      ******************************
       DISPLAY-FIELDS SECTION.
      ******************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE LTB-FILEDATE TO HOLD-BNKRPTDATE DATE-YYYYMMDD
                                CHG-BNKRPTDATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO FILEDATE.

           MOVE LTB-GROSS-BAL TO D-GROSS-BAL.

           MOVE LTB-CASE-NO TO F01.
           MOVE LTB-CHAPTER TO F011 CHG-B-CHAPTER.
           IF LTB-BDDATE = 02020202
              MOVE 0 TO BD-DATE
           ELSE
              MOVE LTB-BDDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO BD-DATE.

           MOVE LTB-MISC1 TO F17.
           MOVE LTB-MISC2 TO F18.
           IF LTB-INT-LC-DUE = 2020202.02 OR 0202020.20
              MOVE 0 TO LTB-INT-LC-DUE F19 LTB-INT-LC-DUE
           ELSE
              MOVE LTB-INT-LC-DUE TO F19 LTB-INT-LC-DUE.
           IF LTB-OTH-DUE = 2020202.02 OR 0202020.20
              MOVE 0 TO LTB-OTH-DUE F191 LTB-OTH-DUE
           ELSE
              MOVE LTB-OTH-DUE TO F191 LTB-OTH-DUE.
           IF LTB-TDUEAMT = 2020202.02 OR 0202020.20
              MOVE 0 TO LTB-TDUEAMT F192 LTB-TDUEAMT
           ELSE
              MOVE LTB-TDUEAMT TO F192 LTB-TDUEAMT.
           MOVE LTB-WHO-FILED TO F20.
           MOVE LTB-ENDORSER-FILED TO F21.
           IF LTB-CLAIM-AMT = 2020202.02 OR 0202020.20
              MOVE 0 TO LTB-CLAIM-AMT F22 LTB-CLAIM-AMT
           ELSE
              MOVE LTB-CLAIM-AMT TO F22 LTB-CLAIM-AMT.
           MOVE LTB-ACTIVE-INACTIVE TO F23.

      * DEBTOR'S ATTORNEY (1)
           MOVE LTB-NAME(1)  TO F02.
           MOVE LTB-ADR1(1)  TO F03.
           MOVE LTB-ADR2(1)  TO F031.
           MOVE LTB-CITY(1)  TO F04.
           MOVE LTB-STATE(1) TO F05.
           MOVE LTB-ZIP(1)   TO F051.
           IF LTB-PHONE(1) NOT = 0
              MOVE LTB-PHONE(1) TO TEL-BUF 
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F06.

      * CLERK OF BANKRUPTCY COURT (2)
           MOVE LTB-NAME(2)  TO F07.
           MOVE LTB-ADR1(2)  TO F08.
           MOVE LTB-ADR2(2)  TO F081.
           MOVE LTB-CITY(2)  TO F09.
           MOVE LTB-STATE(2) TO F10.
           MOVE LTB-ZIP(2)   TO F101.
           IF LTB-PHONE(2) NOT = 0
              MOVE LTB-PHONE(2) TO TEL-BUF 
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F11.

      * TRUSTEE (3)
           MOVE LTB-NAME(3) TO F12.
           MOVE LTB-ADR1(3) TO F13.
           MOVE LTB-ADR2(3) TO F131.
           MOVE LTB-CITY(3) TO F14.
           MOVE LTB-STATE(3) TO F15.
           MOVE LTB-ZIP(3) TO F151.
           IF LTB-PHONE(3) NOT = 0
              MOVE LTB-PHONE(3) TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F16.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPLTBCL.CPY".
       COPY "LIBLP/LPCHCL.CPY".

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY SPACES UPON CHBANK-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHBANK-SCREEN UPON CHBANK-WINDOW-HANDLE.
           MOVE CHBANK-FORM-FIELDS TO WR-BUF.
           MOVE CHBANK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE CHBANK-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHBANK-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHBANK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************
       MISC-PARAGRAPHS SECTION.
      ********************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COMPUTE-CLAIM-AMT.
           MOVE LN-ACCTNO     TO PASS-PRLNNO.
           MOVE LN-BNKRPTDATE TO PASS-TRANS-DATE.
           MOVE "PO"          TO PASS-CURRENT-PD-SRCD.
           MOVE "LP/PAYOFX" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASS-PAYOFF-FLDS.
           CANCEL FORM-PROGX.
       COMPUTE-TDUEAMT.
           MOVE LN-ACCTNO          TO CRNOR2-ACCTNO.
           MOVE LN-BNKRPTDATE      TO CRNOR2-SYSDATE.
           MOVE "PO"               TO CRNOR2-LPTRCD.
           MOVE SPACES             TO CRNOR2-GET-PAYOFF-ONLY.
           MOVE "Y"                TO CRNOR2-RECAST-FG.
           MOVE "N"                TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK
                                          NO-REC POFF-WORKERS
                                          LN-REC.
           CANCEL FORM-PROGX.
       TEST-LOAN-FILE.
           PERFORM OPEN-LN1-FILE.
           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE
           " THIS RECORD WAS ALREADY CREATED BY ANOTHER USER,"
           TO MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
       GET-LT.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.

      **************************
       IO-ROUTINE SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCHIN.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHBANK-SCREEN.
           DESTROY CHBANK-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.

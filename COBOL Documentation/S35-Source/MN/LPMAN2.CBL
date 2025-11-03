       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPMAN2.
       DATE-WRITTEN.    11-15-2000.
      ******************************************************************
      *
      *  IDENTIFICATION:  MN/LPMAN2.C
      *
      *  DESCRIPTION   :  LOAN PAYMENT FILE MAINTENANCE - PAGE 2
      *
      *-----------------------------------------------------------------
      * REV:
      * MJD 001115 CREATED NEW PAGE 2,
      * MG  010301 ADDED LP-EXTRA-PRIN-LFP, LP-REMAIN-INT-SINCE-LFP AND
      *            LP-REMAIN-PRIN-SINCE-LFP PER JOHN & KEN.
      * JTG 011004 ADDED HOLD-LXZ(FIELDS) FOR UPDATE LX-Z RECORD
      *            RE: NEW RATE TYPE 'Z' AMORTIZED    LENDMARK PR#1045 & PR#1380
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED LP-INS-TAX                                   WORLD #288
      * JTG 020322 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            OPENED UP USE OF LP-PRORATED-INS-REB TO HOLD PRORATED PORTION
      *            OF INSURANCE REFUND                                WORLD #288
      * JTG 020718 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS    REGENCY #1801
      *  AMM 051024 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *             /USR/DATA/R1/LG/CHG.YYMMDD
      * BLM 100305 ADDED FIELD 33, LP-APPLIED-TO-ESCROW, REGWAN PR# 3506
      *  CS 121024 REPLACED CALL TMMSG WITH MOVE OF MESS TO TMMSG-MESS,PERFORM
      *            CALL-TMMSG  A30
      *  CS 130315 REMOVED MISC CALL-TMMSG, ITS NOW A COPY MEMBER LIBGB/TMMSG
      *            AND ITS IN DECLARATIVES
      *  CS 160216 ADD MOVE OF LN-OWNBR TO LX-BRNO (NEW LXA)
      * MJD 160613 ADDED LXR-REB-AMT REGMGMT PR#4915
      *            FIXED ISSUE WITH LOGGING CHANGES, NOT USING CORRECT
      *            CHANGE/ORIGINAL/REC AND WRONG DATA TYPES IN CHANGE LINE.
      *            CORRECTED FOR ALL LX?
      * BLM 2016.0623 REMOVE LN-CYCLE-DAY FROM CALL OF PUINFO
      *  CS 2017.0426 ACTIVATE 8 DIGIT DATES
      * BAH 2019.0103 GLOBAL LPFILE
      * BAH 2019.0116 GLOBAL LXFILE, COMBINED ALL LX REDEFINES
      * BAH 2019.0307 REMOVED LP1-KEY9, USE A WORKING STORAGE SCAN-LP1-KEY9
      * BAH 2019.0627 REMOVED REVOLVING, PT0153
      * KEC 2020.0214 CHANGED THE FORM ENTRY FOR LP-SEQNO FROM 4 TO 5 BYTES
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LPMAN2.CBL S35 02/21/24-12:33:54 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".

      * ORIGINAL PAYMENT RECORD LPOR
      * CHANGED  PAYMENT RECORD LPCH
      
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPOR".
       COPY "LIBLP/LP01LX.CPY" REPLACING LEADING "LX" BY "LXOR".
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPCH".
       COPY "LIBLP/LP01LX.CPY" REPLACING LEADING "LX" BY "LXCH".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(12).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(11).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(10)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(7).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(11).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(10)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(7).99-.


      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ACTION                                    VALUE SPACES.
           03  CHKEY                   PIC X(04).
           03  CHNO                    PIC 9(02).
           03  CHNO-X                  REDEFINES CHNO
                                       PIC X(02).

       01  FORM-NUM1                   REDEFINES ACTION.
           03  FORM-KEY1               PIC X(04).
           03  FORM-P1                 PIC X(01).
           03  FORM-SEL1               PIC 9(01).

      *-----------------------------------------------------------------
       01  FIRST-SCAN                  PIC 9(01)     VALUE ZEROES.

       01  SCAN-LP1-KEY9   PIC 9(13).
       01  SCAN-LP1-KEY REDEFINES SCAN-LP1-KEY9.
           03  SCAN-LP-ACCTNO  PIC 9(8).
           03  SCAN-LP-SEQNO   PIC 9(5).

       01  IO-TYPE                     PIC X(03)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)     VALUE ZEROES.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG            PIC X(01)     VALUE "N".
           88  PAGE-CHANGE-NO                        VALUE "N".
           88  PAGE-CHANGE-YES                       VALUE "Y".

       01  RECORD-SIZE-WORKERS.
           03  ORIGINAL-LX-REC        PIC X(LX-SIZE)      VALUE SPACES.
           03  CHANGED-LX-REC         PIC X(LX-SIZE)      VALUE SPACES.

       01  VERIFY-LXA-LP-TRCD          PIC X(02)     VALUE SPACES.
           88  VERIFY-LXA-LP-TRCD-VALID    VALUE "ET" "LP" "NP"
                                                 "RT" "ST".

       01  LPMAIN-LINK                 VALUE SPACES.
           03  FORM-NAME               PIC X(20).
           03  SAVEKEY.                           
               05  SAVEKEY-BRNO        PIC 9(4).
               05  SAVEKEY-ACCTNO      PIC 9(8).
               05  SAVEKEY-SEQNO       PIC 9(5).
           03  NEW-REC                 PIC X(01).
           03  ERRCD                   PIC X(01).
           03  FORMSEL                 PIC 9(02).
           03  LPMAIN-WINDOW-HANDLE    PIC X(10).

       
      *
      *    D I S P L A Y - W O R K E R S
      *
       
       01  DISPLAY-BUF                 VALUE SPACES.
           03  F01                     PIC ZZZZZZ9.99- BLANK ZERO.
      * LP-REPAY-TRANS-ID
           03  F02                     PIC 9(10).
           03  F03                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F04                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F05                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F06                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F07                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F08                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F09                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F10                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F11                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F12                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F13                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F14                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F15                     PIC 99/99/99    BLANK ZERO.
           03  F16                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F17                     PIC 99/99/99    BLANK ZERO.
           03  F18                     PIC ZZZZZZ9.99- BLANK ZERO.
           03  F19                     PIC X(01).
           03  F20                     PIC X(02).
           03  F21                     PIC 9(03)       BLANK ZERO.
           03  F211                    PIC 9(03)       BLANK ZERO.
           03  F212                    PIC 9(03)       BLANK ZERO.
           03  F22                     PIC 9(03)       BLANK ZERO.
           03  F221                    PIC 9(03)       BLANK ZERO.
           03  F222                    PIC 9(03)       BLANK ZERO.
           03  F23                     PIC ZZZ,ZZ9.99- BLANK ZERO.
           03  F231                    PIC ZZZ,ZZ9.99- BLANK ZERO.
           03  F232                    PIC ZZZ,ZZ9.99- BLANK ZERO.
           03  F24                     PIC 9(02)       BLANK ZERO.
           03  F25                     PIC X(01).
           03  F26                     PIC 99/99/99    BLANK ZERO.
           03  F27                     PIC 99/99/99    BLANK ZERO.
           03  F28                     PIC Z,ZZ9.99-   BLANK ZERO.
           03  F29                     PIC ZZZ,ZZ9.99- BLANK ZERO.
           03  F30                     PIC Z,ZZ9.99-   BLANK ZERO.
           03  F31                     PIC X(01).
           03  F32                     PIC X(03).
           03  F33                     PIC ZZ,ZZ9.99-  BLANK ZERO.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(20) VALUE "110 120 130 140 150 ".
           03  FILLER PIC X(20) VALUE "160 170 180 190 200 ".
           03  FILLER PIC X(12) VALUE "210 211 212 ".
           03  FILLER PIC X(12) VALUE "220 221 222 ".
           03  FILLER PIC X(12) VALUE "230 231 232 ".
           03  FILLER PIC X(08) VALUE "240 250 ".
           03  FILLER PIC X(20) VALUE "260 270 280 290 300 ".
           03  FILLER PIC X(12) VALUE "310 320 330.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-LXA-LP-TRCD             VALUE 20 THRU 32.

       77  ONE                         PIC S9(1)     VALUE ZEROES.
       78  MAX                                       VALUE 33.

      *-----------------------------------------------------------------
       01  SPEC-TABLE.
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 2. LP-REPAY-TRANS-ID
           03  FILLER PIC X(8) VALUE "ENNN100N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 11. LXZ CUR APINT
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 12. LXZ CUR APPRIN
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 13. INS TAX REB AMT
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 14. LXM MIP CL PAID
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 15. LXM MIP CL PDTH
           03  FILLER PIC X(8) VALUE "ERNM060N".
      * 16. LXM MIP AH PAID
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 17. LXM MIP CL PDTH
           03  FILLER PIC X(8) VALUE "ERNM060N".
      * 18. LXR REB AM
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * 19. N/A
           03  FILLER PIC X(8) VALUE "E  A010N".
      * 20. LXA SOURCE CODE 
           03  FILLER PIC X(8) VALUE "E  A020N".
           03  FILLER PIC X(8) VALUE "ENNN030N".
           03  FILLER PIC X(8) VALUE "ENNN030N".
           03  FILLER PIC X(8) VALUE "ENNS062N".
           03  FILLER PIC X(8) VALUE "ENNN020N".
           03  FILLER PIC X(8) VALUE "E  A010N".
           03  FILLER PIC X(8) VALUE "ERNM060N".
           03  FILLER PIC X(8) VALUE "ERNM060N".
           03  FILLER PIC X(8) VALUE "ENNS042N".
           03  FILLER PIC X(8) VALUE "ENNS062N".
           03  FILLER PIC X(8) VALUE "ENNS042N".
      * 31
           03  FILLER PIC X(8) VALUE "E  A010N".
           03  FILLER PIC X(8) VALUE "E  A030N".
           03  FILLER PIC X(8) VALUE "ENNS052N".

       01  SPEC-TABLE2                 REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LPMAN2_DEF.CPY".
       COPY "LIBMN/LPMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA          PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LPMAN2_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE FORM-LINK-AREA TO LPMAIN-LINK.
           MOVE "LPMAN2"      TO VDU-FORM-NAME.

           MOVE SPACES TO VDU-WIMENU-SELECTION
                          VDU-PGMENU-SELECTION.

       MAIN-PROGRAM-LOOP.
           PERFORM FORM-RESET.

           PERFORM ENTER-FILE-KEY.

           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO MAIN-PROGRAM-EXIT.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-PROGRAM-LOOP.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "P" TO FORM-P1
              MOVE 3   TO FORM-SEL1.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE LP1-KEY   TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO MAIN-PROGRAM-EXIT.

           GO TO MAIN-PROGRAM-LOOP.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       ENTER-FILE-KEY SECTION.
      ******************************************************************

           MOVE 0 TO LP-BRNO LP-ACCTNO LP-SEQNO FIRST-SCAN.
           MOVE " " TO IO-TYPE.

           IF SAVEKEY = " "
              MOVE 0    TO NEW-FLAG
              MOVE "N"  TO PAGE-CHANGE-FLAG
              GO TO ENTER-FILE-KEY-ACCTNO.

      * KEY WAS PASSED FROM ANOTHER PAGE 
           MOVE "Y"             TO PAGE-CHANGE-FLAG.
           MOVE SAVEKEY         TO LP1-KEY.
           MOVE " "             TO SAVEKEY.

           MOVE "SN N080LPNO."  TO VDU.
           MOVE LP-ACCTNO       TO VDUNUM0 LN-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SN N050LPSEQ." TO VDU.
           MOVE LP-SEQNO        TO VDUNUM0.
           PERFORM SCREENIO.

           IF NEW-REC = "Y"
              PERFORM SETUP-NEW-RECORD.

           GO TO ENTER-FILE-KEY-READ-FILE.

       
       ENTER-FILE-KEY-ACCTNO.
           MOVE "EN N080LPNO." TO VDU.
           PERFORM SCREENIO.
 
           IF F6-KEY
              PERFORM ENTER-SCAN-KEY
              GO TO ENTER-FILE-KEY-EXIT.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-FILE-KEY-EXIT.

           IF NOT ENTER-KEY
              GO TO ENTER-FILE-KEY-ACCTNO.

           MOVE VDUNUM0 TO LP-ACCTNO LN-ACCTNO.

           IF LP-ACCTNO = 0
              GO TO ENTER-FILE-KEY-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-FILE-KEY-SEQNO.
           MOVE "EN N050LPSEQ." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY
              GO TO ENTER-FILE-KEY-ACCTNO.
           IF F1-KEY
              GO TO ENTER-FILE-KEY-ACCTNO.
           IF F6-KEY
              PERFORM ENTER-SCAN-KEY
              GO TO ENTER-FILE-KEY-EXIT.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-FILE-KEY-EXIT.
           IF NOT ENTER-KEY
              GO TO ENTER-FILE-KEY-SEQNO.

           MOVE VDUNUM0 TO LP-SEQNO.
           IF LP-SEQNO = 0
              GO TO ENTER-FILE-KEY-SEQNO.

       
       ENTER-FILE-KEY-READ-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-BAD
              MOVE "CLR"               TO IO-TYPE
              MOVE "** NOT ON FILE **" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FILE-KEY-EXIT.

           PERFORM GET-LN1-RECORD.
           IF IO-BAD
              MOVE "CLR"                          TO IO-TYPE
              MOVE "ERROR: LOAN (LN) NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FILE-KEY-EXIT.

      * COMBINED LX'S INTO 1 RECORD, SO READ IT HERE. TO DETERMINE
      * IF A SPECIFIC CODE EXISTS, TEST ITS LX?-CODE

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM OPEN-LX1-FILE.
           PERFORM READ-LX1-FILE.
           PERFORM CLOSE-LX1-FILE.
           IF ( IO-BAD )
              PERFORM CLEAR-LX-FILE
              MOVE LP-BRNO   TO LX-BRNO
              MOVE LP-ACCTNO TO LX-ACCTNO
              MOVE LP-SEQNO  TO LX-SEQNO.

           MOVE LX-REC TO ORIGINAL-LX-REC
                          CHANGED-LX-REC.


           MOVE LP-REC TO LPOR-REC.

           PERFORM DISPLAY-FIELDS.

       ENTER-FILE-KEY-EXIT.
            EXIT.

      ******************************************************************
       ENTER-SCAN-KEY SECTION.
      ******************************************************************

           MOVE " ENTER PAYMENT #, OR F6 TO SCAN" TO TMMSG-MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.

           PERFORM START-LP1-FILE.

       ENTER-SCAN-KEY-ENTRY.

           MOVE "EN N130SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCAN-KEY-END.
           IF F6-KEY
              GO TO ENTER-SCAN-KEY-READ-FILE.
           IF NOT ENTER-KEY
              GO TO ENTER-SCAN-KEY-ENTRY.

           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LP-REC TO LPOR-REC
                 GO TO ENTER-SCAN-KEY-END
              ELSE
                 GO TO ENTER-SCAN-KEY-ENTRY.

      * SCAN IS STILL JUST ACCTNO/SEQNO, NO BRANCH YET
           MOVE VDUNUM0 TO SCAN-LP1-KEY9.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE SCAN-LP-ACCTNO TO LP-ACCTNO.
           MOVE SCAN-LP-SEQNO  TO LP-SEQNO.


           PERFORM START-LP1-FILE.

       ENTER-SCAN-KEY-READ-FILE.

           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCAN-KEY-END.

      *    UNLOCK LP-FILE.

           MOVE "SN N080LPNO." TO VDU.
           MOVE LP-ACCTNO      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N050LPSEQ." TO VDU.
           MOVE LP-SEQNO        TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM GET-LN1-RECORD.
           IF IO-BAD
              MOVE "ERROR: LOAN (LN) NOT ON FILE" TO MESS
              PERFORM SEND-MESS.

      * COMBINED LX'S INTO 1 RECORD, SO READ IT HERE. TO DETERMINE
      * IF A SPECIFIC CODE EXISTS, TEST ITS LX?-CODE

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM OPEN-LX1-FILE.
           PERFORM READ-LX1-FILE.
           PERFORM CLOSE-LX1-FILE.
           IF ( IO-BAD )
              PERFORM CLEAR-LX-FILE
              MOVE LP-BRNO   TO LX-BRNO
              MOVE LP-ACCTNO TO LX-ACCTNO
              MOVE LP-SEQNO  TO LX-SEQNO.

           MOVE LX-REC TO ORIGINAL-LX-REC.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-SCAN-KEY-ENTRY.

       ENTER-SCAN-KEY-END.
           MOVE 0   TO FIRST-SCAN.
           PERFORM CLOSE-LP1-FILE.

           MOVE SPACES TO TMMSG-MESS.
           PERFORM CALL-TMMSG.

       ENTER-SCAN-KEY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE LP-TRCD TO VERIFY-LXA-LP-TRCD.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

       
       CREATE-ROUTINE.

           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE
                        ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9

           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P"    ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 < 1 ) OR
                 ( FORM-SEL1 > 3 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO-X NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO = ZEROES      ) GO TO MODIFY-ROUTINE.
           IF ( CHNO > MAX         ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ( CHKEY NOT    = ZEROES ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( NEW-FLAG  = ZEROES ) AND
              ( CHFG(ELE) = "N"    ) AND
              ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD   )
              GO TO MODIFY-ROUTINE.

           IF ( ELE-LXA-LP-TRCD              ) AND
              ( NOT VERIFY-LXA-LP-TRCD-VALID )
              GO TO CREATE-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

           IF ( DOWN-KEY )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       F-01.
           MOVE VDUNUM2 TO LP-PREPAY-PENALTY.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM0 TO LP-REPAY-TRANS-ID.
      *    MOVE VDUNUM2 TO LP-EXTRA-PRIN-LFP.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO LP-REMAIN-INT-SINCE-LFP.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM2 TO LP-REMAIN-PRIN-SINCE-LFP.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-05.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-POST-REGPY.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-06.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-POST-EXTRA-PRIN.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-07.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-POST-OTH.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-POST-LCHG.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-PDUE-APINT.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-PDUE-APPRIN.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-CUR-APINT.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-12.
           MOVE "Z"             TO LXZ-CODE.
           MOVE VDUNUM2         TO LXZ-CUR-APPRIN.
           GO TO CREATE-ROUTINE.

      *------ INSURANCE TAX REBATE AMOUNT ------------------------------
       F-13.
           MOVE VDUNUM2 TO LP-PRORATED-INS-REB.
           GO TO CREATE-ROUTINE.
      *------ LXM MIP CL PAID -----------------------------------------
       F-14.
           MOVE "M"             TO LXM-CODE.
           MOVE VDUNUM2         TO LXM-MIP-CLPAID.
           GO TO CREATE-ROUTINE.
      *------ LXM MIP CL PDTH ------------------------------------------
       F-15.
           MOVE "M"             TO LXM-CODE.
           MOVE VDU-DATE-YYYYMMDD TO LXM-MIP-CLPDTH-DATE.
           GO TO CREATE-ROUTINE.
      *------ LXM MIP AH PAID -----------------------------------------
       F-16.
           MOVE "M"             TO LXM-CODE.
           MOVE VDUNUM2         TO LXM-MIP-AHPAID.
           GO TO CREATE-ROUTINE.
      *------ LXM MIP AH PDTH ------------------------------------------
       F-17.
           MOVE "M"             TO LXM-CODE.
           MOVE VDU-DATE-YYYYMMDD TO LXM-MIP-AHPDTH-DATE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-18.    
           MOVE "R"             TO LXR-CODE.
           MOVE VDUNUM2         TO LXR-REB-AMT.
           GO TO CREATE-ROUTINE.
       F-19. 
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUBUF          TO LXA-SRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUNUM0         TO LXA-START-PMT(1).
      *-----------------------------------------------------------------
       F-211.
           MOVE "ENNN030211." TO VDU.
           PERFORM SCREENIO.
           IF ( UP-KEY        ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-211.
           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM0 TO LXA-START-PMT(2).

      *-----------------------------------------------------------------
       F-212.
           MOVE "ENNN030212." TO VDU.
           PERFORM SCREENIO.
           IF ( UP-KEY        ) GO TO F-211.
           IF ( NOT ENTER-KEY ) GO TO F-212.

           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM0 TO LXA-START-PMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM0 TO LXA-END-PMT(1).

      *-----------------------------------------------------------------
       F-221.
           MOVE "ENNN030221." TO VDU.
           PERFORM SCREENIO.
           IF ( UP-KEY        ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-221.

           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM0 TO LXA-END-PMT(2).

      *-----------------------------------------------------------------
       F-222.
           MOVE "ENNN030222." TO VDU.
           PERFORM SCREENIO.
           IF ( UP-KEY        ) GO TO F-221.
           IF ( NOT ENTER-KEY ) GO TO F-222.

           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM0 TO LXA-END-PMT(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM2 TO LXA-NET-CHANGE(1).

      *-----------------------------------------------------------------
       F-231.
           MOVE "ENNS062231." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY        ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-231.

           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM2 TO LXA-NET-CHANGE(2).

      *-----------------------------------------------------------------
       F-232.
           MOVE "ENNS062232." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY        ) GO TO F-231.
           IF ( NOT ENTER-KEY ) GO TO F-232.

           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM2 TO LXA-NET-CHANGE(3).

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE "A"     TO LXA-CODE.
           MOVE VDUNUM0 TO LXA-LN-INSCOMP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE "A"     TO LXA-CODE.
           MOVE VDUBUF  TO LXA-LN-INSOURS.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE "A"                 TO LXA-CODE.
           MOVE VDU-DATE-YYYYMMDD   TO LXA-LN-INSEFF-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE "A"               TO LXA-CODE.
           MOVE VDU-DATE-YYYYMMDD TO LXA-LN-INSEXP-DATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUNUM2         TO LXA-LN-INSPREM.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUNUM2         TO LXA-LN-INSCOVR.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUNUM2         TO LXA-LN-INSCOMM.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUBUF          TO LXA-LN-INS-REBATE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE "A"             TO LXA-CODE.
           MOVE VDUBUF          TO LXA-LN-INSCODES.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-33.
           MOVE VDUNUM2 TO LP-APPLIED-TO-ESCROW.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.

           MOVE ZEROES TO FORM-SEL1.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE LP-PREPAY-PENALTY          TO F01.
           MOVE LP-REPAY-TRANS-ID          TO F02.
      *    MOVE LP-EXTRA-PRIN-LFP          TO F02.
           MOVE LP-REMAIN-INT-SINCE-LFP    TO F03.
           MOVE LP-REMAIN-PRIN-SINCE-LFP   TO F04.

           MOVE LXZ-POST-REGPY             TO F05.
           MOVE LXZ-POST-EXTRA-PRIN        TO F06.
           MOVE LXZ-POST-OTH               TO F07.
           MOVE LXZ-POST-LCHG              TO F08.
           MOVE LXZ-PDUE-APINT             TO F09.
           MOVE LXZ-PDUE-APPRIN            TO F10.
           MOVE LXZ-CUR-APINT              TO F11.
           MOVE LXZ-CUR-APPRIN             TO F12.
           MOVE LP-PRORATED-INS-REB        TO F13.

           MOVE LXM-MIP-CLPAID             TO F14.
           MOVE LXM-MIP-CLPDTH-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY                TO F15.
           MOVE LXM-MIP-AHPAID             TO F16.
           MOVE LXM-MIP-AHPDTH-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F17.

           MOVE LXR-REB-AMT                TO F18.
           MOVE SPACES                     TO F19.

           MOVE LXA-SRCD                   TO F20.
           MOVE LXA-START-PMT (1)          TO F21.
           MOVE LXA-START-PMT (2)          TO F211.
           MOVE LXA-START-PMT (3)          TO F212.
           MOVE LXA-END-PMT   (1)          TO F22.
           MOVE LXA-END-PMT   (2)          TO F221.
           MOVE LXA-END-PMT   (3)          TO F222.

           MOVE LXA-NET-CHANGE(1)          TO F23.
           MOVE LXA-NET-CHANGE(2)          TO F231.
           MOVE LXA-NET-CHANGE(3)          TO F232.
           MOVE LXA-LN-INSCOMP             TO F24.
           MOVE LXA-LN-INSOURS             TO F25.
           MOVE LXA-LN-INSEFF-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F26.
           MOVE LXA-LN-INSEXP-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY                TO F27.
           MOVE LXA-LN-INSPREM             TO F28.
           MOVE LXA-LN-INSCOVR             TO F29.
           MOVE LXA-LN-INSCOMM             TO F30.
           MOVE LXA-LN-INS-REBATE          TO F31.
           MOVE LXA-LN-INSCODES            TO F32.
           MOVE LP-APPLIED-TO-ESCROW       TO F33.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       SETUP-NEW-RECORD SECTION.
      ******************************************************************

           MOVE ZEROES TO ELE.
           MOVE 1      TO NEW-FLAG.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

       SETUP-NEW-RECORD-EXIT.
           EXIT.


      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-LP1-FILE.

       WRITE-MODULE-REWRITE-LP.

           MOVE LP-REC TO LPCH-REC.

           IF LPOR-REC = LPCH-REC
              GO TO WRITE-MODULE-REWRITE-LX.

           PERFORM READ-LP1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO WRITE-MODULE-END.

           IF LP-REC NOT = LPOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO WRITE-MODULE-END.

           PERFORM LOG-MAINTENANCE-CHGS-LP.

           MOVE LPCH-REC TO LP-REC.
           PERFORM REWRITE-LP1-FILE.
          
       WRITE-MODULE-REWRITE-LX.

           MOVE LX-REC TO CHANGED-LX-REC.

           IF ORIGINAL-LX-REC = CHANGED-LX-REC
              GO TO WRITE-MODULE-END.

           PERFORM OPEN-LX1-FILE.
           PERFORM READ-LX1-FILE.
           IF IO-BAD
              MOVE CHANGED-LX-REC TO LX-REC
              PERFORM WRITE-LX1-FILE
              PERFORM CLOSE-LX1-FILE
              GO TO WRITE-MODULE-END.

           IF LX-REC NOT = ORIGINAL-LX-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              PERFORM CLOSE-LX1-FILE
              GO TO WRITE-MODULE-END.

           PERFORM LOG-MAINTENANCE-CHGS-LX.
           MOVE CHANGED-LX-REC TO LX-REC.
           PERFORM REWRITE-LX1-FILE.
           PERFORM CLOSE-LX1-FILE.

       WRITE-MODULE-END.
           PERFORM CLOSE-LP1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      *********************************************************
       LOG-MAINTENANCE-CHGS-LP SECTION.
      *********************************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO WORK-ERRLOG-TO-X.

           IF LPOR-PREPAY-PENALTY NOT = LPCH-PREPAY-PENALTY
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "PREPAY PNLTY"          TO WORK-ERRLOG-DESC
             MOVE LPOR-PREPAY-PENALTY     TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-PREPAY-PENALTY     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-EXTRA-PRIN-LFP NOT = LPCH-EXTRA-PRIN-LFP
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "EX PRIN LFP "          TO WORK-ERRLOG-DESC
             MOVE LPOR-EXTRA-PRIN-LFP     TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-EXTRA-PRIN-LFP     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REMAIN-PRIN-SINCE-LFP NOT =
                                          LPCH-REMAIN-PRIN-SINCE-LFP
             MOVE "03 "                      TO WORK-ERRLOG-FIELD
             MOVE "INT SINC LFP"             TO WORK-ERRLOG-DESC
             MOVE LPOR-REMAIN-PRIN-SINCE-LFP TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-REMAIN-PRIN-SINCE-LFP TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REMAIN-INT-SINCE-LFP NOT = LPCH-REMAIN-INT-SINCE-LFP
             MOVE "04 "                     TO WORK-ERRLOG-FIELD
             MOVE "PRIN SNC LFP"            TO WORK-ERRLOG-DESC
             MOVE LPOR-REMAIN-INT-SINCE-LFP TO WORK-ERRLOG-CHG-N2
             MOVE LPCH-REMAIN-INT-SINCE-LFP TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PRORATED-INS-REB NOT = LPCH-PRORATED-INS-REB
             MOVE "13 "                     TO WORK-ERRLOG-FIELD
             MOVE "INS TAX REB "            TO WORK-ERRLOG-DESC
             MOVE LPOR-PRORATED-INS-REB TO WORK-ERRLOG-CHG-N2
             MOVE LPCH-PRORATED-INS-REB TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APPLIED-TO-ESCROW NOT = LPCH-APPLIED-TO-ESCROW
             MOVE "33 "                     TO WORK-ERRLOG-FIELD
             MOVE "ESCROW APPLD"            TO WORK-ERRLOG-DESC
             MOVE LPOR-APPLIED-TO-ESCROW TO WORK-ERRLOG-CHG-N2
             MOVE LPCH-APPLIED-TO-ESCROW TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.


      *****************************************
       LOG-FIELD-CHANGE SECTION.
      *****************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.


      *************************************************
       LOG-MAINTENANCE-CHGS-LX SECTION.
      *************************************************
           MOVE ORIGINAL-LX-REC TO LXOR-REC.
           MOVE CHANGED-LX-REC  TO LXCH-REC.

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO WORK-ERRLOG-TO-X.

           IF LXORA-SRCD NOT = LXCHA-SRCD
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "SOURCE CODE "              TO WORK-ERRLOG-DESC
             MOVE LXORA-SRCD              TO WORK-ERRLOG-ORG-ALP
             MOVE LXCHA-SRCD              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-START-PMT(1) NOT = LXCHA-START-PMT(1)
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "START PMT 1 "          TO WORK-ERRLOG-DESC
             MOVE LXORA-START-PMT(1)      TO WORK-ERRLOG-ORG-N0
             MOVE LXCHA-START-PMT(1)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-START-PMT(2) NOT = LXCHA-START-PMT(2)
             MOVE "211"                   TO WORK-ERRLOG-FIELD
             MOVE "START PMT 2 "          TO WORK-ERRLOG-DESC
             MOVE LXORA-START-PMT(2)      TO WORK-ERRLOG-ORG-N0
             MOVE LXCHA-START-PMT(2)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-START-PMT(3) NOT = LXCHA-START-PMT(3)
             MOVE "212"                   TO WORK-ERRLOG-FIELD
             MOVE "START PMT 3 "          TO WORK-ERRLOG-DESC
             MOVE LXORA-START-PMT(3)      TO WORK-ERRLOG-ORG-N0
             MOVE LXCHA-START-PMT(3)      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-END-PMT(1) NOT = LXCHA-END-PMT(1)
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "END PMT 1  "           TO WORK-ERRLOG-DESC
             MOVE LXORA-END-PMT(1)        TO WORK-ERRLOG-ORG-N0
             MOVE LXCHA-END-PMT(1)        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-END-PMT(2) NOT = LXCHA-END-PMT(2)
             MOVE "221"                   TO WORK-ERRLOG-FIELD
             MOVE "END PMT 2"             TO WORK-ERRLOG-DESC
             MOVE LXORA-END-PMT(2)        TO WORK-ERRLOG-ORG-N0
             MOVE LXCHA-END-PMT(2)        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-END-PMT(3) NOT = LXCHA-END-PMT(3)
             MOVE "222"                   TO WORK-ERRLOG-FIELD
             MOVE "END PMT 3"             TO WORK-ERRLOG-DESC
             MOVE LXORA-END-PMT(3)        TO WORK-ERRLOG-ORG-N0
             MOVE LXCHA-END-PMT(3)        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-NET-CHANGE(1) NOT = LXCHA-NET-CHANGE(1)
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "NET CHG 1"             TO WORK-ERRLOG-DESC
             MOVE LXORA-NET-CHANGE(1)     TO WORK-ERRLOG-ORG-N2
             MOVE LXCHA-NET-CHANGE(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-NET-CHANGE(2) NOT = LXCHA-NET-CHANGE(2)
             MOVE "231"                   TO WORK-ERRLOG-FIELD
             MOVE "NET CHG 2"              TO WORK-ERRLOG-DESC
             MOVE LXORA-NET-CHANGE(2)     TO WORK-ERRLOG-ORG-N2
             MOVE LXCHA-NET-CHANGE(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-NET-CHANGE(3) NOT = LXCHA-NET-CHANGE(3)
             MOVE "232"                   TO WORK-ERRLOG-FIELD
             MOVE "NET CHG 3"              TO WORK-ERRLOG-DESC
             MOVE LXORA-NET-CHANGE(3)     TO WORK-ERRLOG-ORG-N2
             MOVE LXCHA-NET-CHANGE(3)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSCOMP NOT = LXCHA-LN-INSCOMP
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS COMPANY"           TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSCOMP        TO WORK-ERRLOG-CHG-N0
             MOVE LXCHA-LN-INSCOMP        TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSOURS NOT = LXCHA-LN-INSOURS
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS OURS"              TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSOURS        TO WORK-ERRLOG-CHG-ALP
             MOVE LXCHA-LN-INSOURS        TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSEFF-DATE NOT = LXCHA-LN-INSEFF-DATE
             MOVE "26 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS EFFECTIV"              TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSEFF-DATE    TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             MOVE LXCHA-LN-INSEFF-DATE    TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSEXP-DATE NOT = LXCHA-LN-INSEXP-DATE
             MOVE "27 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS EXPIRE "              TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSEXP-DATE    TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             MOVE LXCHA-LN-INSEXP-DATE    TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSPREM NOT = LXCHA-LN-INSPREM
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS PREMIUM"           TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSPREM        TO WORK-ERRLOG-CHG-N2
             MOVE LXCHA-LN-INSPREM        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSCOVR NOT = LXCHA-LN-INSCOVR
             MOVE "29 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS COVRAGE"           TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSCOVR        TO WORK-ERRLOG-CHG-N2
             MOVE LXCHA-LN-INSCOVR        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSCOMM NOT = LXCHA-LN-INSCOMM
             MOVE "30 "                   TO WORK-ERRLOG-FIELD
             MOVE "INS COMMISS"           TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSCOMM        TO WORK-ERRLOG-CHG-N2
             MOVE LXCHA-LN-INSCOMM        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INS-REBATE NOT = LXCHA-LN-INS-REBATE
             MOVE "31 "                   TO WORK-ERRLOG-FIELD
             MOVE "REBATE  "              TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INS-REBATE     TO WORK-ERRLOG-CHG-ALP
             MOVE LXCHA-LN-INS-REBATE     TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LXORA-LN-INSCODES NOT = LXCHA-LN-INSCODES
             MOVE "32 "                   TO WORK-ERRLOG-FIELD
             MOVE "INSCODES"              TO WORK-ERRLOG-DESC
             MOVE LXORA-LN-INSCODES       TO WORK-ERRLOG-CHG-ALP
             MOVE LXCHA-LN-INSCODES       TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-POST-REGPY NOT = LXCHZ-POST-REGPY
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "POST REGPAY "              TO WORK-ERRLOG-DESC
             MOVE LXORZ-POST-REGPY        TO WORK-ERRLOG-ORG-N2
             MOVE LXCHZ-POST-REGPY        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-POST-EXTRA-PRIN NOT = LXCHZ-POST-EXTRA-PRIN
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "POST EXT PRN"              TO WORK-ERRLOG-DESC
             MOVE LXORZ-POST-EXTRA-PRIN   TO WORK-ERRLOG-ORG-N2
             MOVE LXCHZ-POST-EXTRA-PRIN   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-POST-OTH NOT = LXCHZ-POST-OTH
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "POST OTHER  "              TO WORK-ERRLOG-DESC
             MOVE LXORZ-POST-OTH          TO WORK-ERRLOG-ORG-N2
             MOVE LXCHZ-POST-OTH          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-POST-LCHG NOT = LXCHZ-POST-LCHG
             MOVE "08 "                  TO WORK-ERRLOG-FIELD
             MOVE "POST LCHG   "             TO WORK-ERRLOG-DESC
             MOVE LXORZ-POST-LCHG        TO WORK-ERRLOG-CHG-N2
             MOVE LXCHZ-POST-LCHG        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-PDUE-APINT NOT = LXCHZ-PDUE-APINT
             MOVE "09 "                  TO WORK-ERRLOG-FIELD
             MOVE "PDUE APINT "             TO WORK-ERRLOG-DESC
             MOVE LXORZ-PDUE-APINT       TO WORK-ERRLOG-CHG-N2
             MOVE LXCHZ-PDUE-APINT       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-PDUE-APPRIN NOT = LXCHZ-PDUE-APPRIN
             MOVE "10 "                  TO WORK-ERRLOG-FIELD
             MOVE "PDUE APPRIN "             TO WORK-ERRLOG-DESC
             MOVE LXORZ-PDUE-APPRIN      TO WORK-ERRLOG-CHG-N2
             MOVE LXCHZ-PDUE-APPRIN      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-CUR-APINT NOT = LXCHZ-CUR-APINT
             MOVE "11 "                  TO WORK-ERRLOG-FIELD
             MOVE "CUR APINT   "             TO WORK-ERRLOG-DESC
             MOVE LXORZ-CUR-APINT        TO WORK-ERRLOG-CHG-N2
             MOVE LXCHZ-CUR-APINT        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORZ-CUR-APPRIN NOT = LXCHZ-CUR-APPRIN
             MOVE "12 "                  TO WORK-ERRLOG-FIELD
             MOVE "CUR APPRIN "             TO WORK-ERRLOG-DESC
             MOVE LXORZ-CUR-APPRIN       TO WORK-ERRLOG-CHG-N2
             MOVE LXCHZ-CUR-APPRIN       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORM-MIP-CLPAID NOT = LXCHM-MIP-CLPAID
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "MIP CL PAID "              TO WORK-ERRLOG-DESC
             MOVE LXORM-MIP-CLPAID        TO WORK-ERRLOG-ORG-N2
             MOVE LXCHM-MIP-CLPAID        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORM-MIP-CLPDTH-DATE NOT = LXCHM-MIP-CLPDTH-DATE
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "MIP CL PDTH "              TO WORK-ERRLOG-DESC
             MOVE LXORM-MIP-CLPDTH-DATE   TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LXCHM-MIP-CLPDTH-DATE   TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORM-MIP-AHPAID NOT = LXCHM-MIP-AHPAID
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "MIP AH PAID "              TO WORK-ERRLOG-DESC
             MOVE LXORM-MIP-AHPAID        TO WORK-ERRLOG-ORG-N2
             MOVE LXCHM-MIP-AHPAID        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXORM-MIP-AHPDTH-DATE NOT = LXCHM-MIP-AHPDTH-DATE
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "MIP AH PDTH "              TO WORK-ERRLOG-DESC
             MOVE LXORM-MIP-AHPDTH-DATE   TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LXCHM-MIP-AHPDTH-DATE   TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXORR-REB-AMT NOT = LXCHR-REB-AMT
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "REB AMT "              TO WORK-ERRLOG-DESC
             MOVE LXORR-REB_AMT           TO WORK-ERRLOG-ORG-N2
             MOVE LXCHR-REB_AMT           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO LPMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LPMAN2-SYSDATE.
           DISPLAY LPMAN2-SCREEN UPON LPMAIN-WINDOW-HANDLE.
           MOVE LPMAN2-FORM-FIELDS TO WR-BUF.
           MOVE LPMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LPMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LPMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPMAN2-HELP--DIR TO HELP-LINK-DIR.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLXCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-LN1-RECORD.
           MOVE LP-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
             
      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LPMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPMAN1.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   LOAN PAYMENT FILE MAINTENANCE
      *
      * REV:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  JTG 080995 ABILITY TO ENTER 999999999 SSNO
      *  MJD 071296 CHANGED DISPLAY /ENTRY OF PDTH DATES TO ZZ/ZZ/ZZ
      *             FORMAT
      *  JTG 071696 PUT AUDIT ON LP-PDTH-DATE & LP-LCPDTH, TO VERIFY
      *             DATES ARE ON A CYCLIC PAY FREQUENCY
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *  MJD 112597 EXPANDED LX-REC TO 73 BYTES (ADDED 10)
      *  BAH 021898 EXPANDED LP-REC FROM 200 TO 220, EXPANDED SEQNO TO 9(4)
      *  MJD 060498 EXPANDED LP-REC TO 239 BYTES.
      *  BAH 062498 CHANGED LP-TILLNO TO ALPHANUMERIC
      *  JTG 070298 CHANGED TO ALLOW A DELETE 'RANGE' OPTION
      *   CS 081998 ADD LP-POSTING-BR SO WE KNOW WHO POSTED INTERBRANCH PAYMENT
      *  JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      *  JTG 120498 ADDED DISPLAY OF APPLIED WINDOW FOR REVOLVING
      *  JTG 121498 ADDED DISPLAY OF LINKED PURCHASE WINDOW FOR REVOLVING
      *  JTG 990709 ADDED DISPLAY OF LINKED PURCHASE (ADFVANCE) WINDOW REVOLVING
      *  GLF 991709 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT, MAKES IT
      *             CONSISTANT WITH OTHER PROGRAMS
      *  MJD 001016 ADDED LP-COLLECTOR (BAY PR#1371)
      *  BAH 001024 POSTED DATE FIELD 04 WAS NOT BEING DISPLAYED OR ENTERED
      *             AS A DATE !!
      *  JTG 001129 ADDED DELETE OF LX-FILE RECORDS IN 353635,
      *             'DELREV' AND 'DELSEQ'  RANGE DELETE
      *  KEC 001208 {PR#00000} PARADATA FINANCIAL [JOHN]
      *              ADDED THE DELETE OF THE 'LXA' RECORD; ADDED 'LXA'
      *              RECORD TO PAGE 2.
      *  JTG 011004 ADDED HOLD-LXZ(FIELDS) FOR UPDATE LX-Z RECORD
      *             RE: NEW RATE TYPE 'Z' AMORTIZED   LENDMARK PR#1045 & PR#1380
      *  JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99    LENDMARK PR#J021
      *  JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  AMM 051024 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *             /USR/DATA/R1/LG/CHG.YYMMDD
      *  BLF 061213 ADDED DELETE OF PAYMENT RECORD TO CHG LOG IN LG
      *   CS 080501 MOVED LOG-MAINTENANCE-CHG.. EVERYTIME YOU WENT TO THE
      *             MAINTENANCE IT WROTE A CHANGE RECORD.
      *   CS 080502 ADDED CHANGES FOR LXG- (GENERL LEDGER NUMBER & AMOUNT)
      *             WI/RVBALW, PERFORM RVBAL-WIN,SETUP-FOR-PUINFO,RLPYPU
      *   CS 121219 ADDED LPMAIN-WINDOW-HANDLE TO SCREEN
      *   CS 130118 A30 COMMENTED OUT CALL-TMMSG
      *   CS 130121 ADDED COPY TMMSG TO MISC,
      *   CS 130315 CALL-TMMSG IS NOW A COPY MEMBER (LIBGB/TMMSG)AND IN
      *             DECLARATIVES
      *   CS 140811 FIXED RLPYPU WINDOW DISPLAY, WAS NOT DISPLAYING TITLE
      *             SO MOVED THE BEGIN-Y TO 3 FOR RLPYPU WINDOW
      *  BLM 151015 A30 FILE CHANGES, PR# PD0003
      *   CS TESTING FORCED LN-OWNBR TO LP-BRNO. SEQ CHANGED TO 5
      *      SAVED COPY OF LP01LP  IS LP01LP.151016
      *      SAVED COPY OF LPMAN1  IS LPMAN1.151015
      *   CS 151110 USED EXT-FILPATH-BRANCH TO GET BRANCH
      *   CS 151124 CHANGED ORIGINAL-REC/CHANGED-REC TO ORIGINAL-LP-REC
      *                                                 CHANGED-LP-REC
      *   CS 151207 USING LP-ALL-BRNO TO SET LP-BRNO,  
      *  BAH 160304 CHANGED TILLNO TO 9(2), PD0003
      *  MJD 160613 ADDED LXR LOGIC SO RECORD WILL BE DELETED IF THE LP
      *             RECORD IS DELETED                      REGMGMT PR#4915  
      *  BLM 160623 REMOVE LN-CYCLE-DAY FROM CALL OF PUINFO
      *   CS 161103 CHANGED TILLNO ENTRY TO "EN N020"
      *  BAH 170313 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0505 ADDED 'LX' (E) FILE PREFIX FOR WORKING STORAGE FIELDS.
      *               ADDED 'LX' (G) FILE PREFIX FOR WORKING STORAGE FIELDS.
      * KEC 2017.0508 FIXED IN THE WRITE-MODULE AN ISSUE USING CHANGED &
      *               ORIGINAL RECS FOR LXE.
      * KEC 2017.0509 REMOVED THE LOGIC OF ACCESSING LX-REC FOR
      *               LX-CODE = "2"; "2" WAS MERGED INTO "A" IN A30.
      * BAH 2019.0103 GLOBAL LPFILE
      * BAH 2019.0116 GLOBAL LXFILE, COMBINED LX, BROKE OUT LXE AND LXG
      * BAH 2019.0307 REMOVED LP1-KEY9, USE A WORKING STORAGE SCAN-LP1-KEY9
      *  CS 2019.0606 ADD  ALSO CLEAR LXE-EARN(4,5,6,7)
      *               IN CLEAR-LXE, ADD FUTURE FIELDS
      *  CS 2019.0610 CLEAR LXG-FUTURE-GLNO/GLAMT OCCURS 4
      * KEC 2000.0214 CHANGED THE FORM ENTRY FOR LP-SEQNO FROM 4 TO 5 BYTES
      * BAH 2022.0615 CHANGED GLNO ENTRY/DISPLAY, WAS NOT WORKING, USED
      *               SAME LOGIC AS GIMAN1
      * BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LPMAN1.CBL S35 02/21/24-12:37:43 >".
      *******************************
      *       PROGRAM WORKERS       *
      *******************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

      * ORIGINAL RECORD LPOR
      * CHANGED  RECORD LPCH
      
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPOR".
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPCH".
       COPY "LIBLP/LP01LXE.CPY" REPLACING LEADING "LXE" BY "LXEOR".
       COPY "LIBLP/LP01LXE.CPY" REPLACING LEADING "LXE" BY "LXECH".
       COPY "LIBLP/LP01LXG.CPY" REPLACING LEADING "LXG" BY "LXGOR".
       COPY "LIBLP/LP01LXG.CPY" REPLACING LEADING "LXG" BY "LXGCH".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.


       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  LPMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY.      
               05 SAVEKEY-BRNO   PIC 9(4).
               05 SAVEKEY-ACCTNO PIC 9(8).
               05 SAVEKEY-SEQNO  PIC 9(5).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LPMAIN-WINDOW-HANDLE PIC X(10).

       01  WK-EARN         PIC S9(5)V99.
       01  BEG-SEQ         PIC 9999.
       01  END-SEQ         PIC 9999.
       01  RANGE-DELETE-FG PIC X.
       01  SAVE-LP-ACCTNO  PIC 9(8).

       01  SS-GL-EDIT      PIC 999/99/9999.
       01  SS-EDIT REDEFINES SS-GL-EDIT.
           03  FILLER      PIC X(3).
           03  SS-DASH1    PIC X.
           03  FILLER      PIC X(2).
           03  SS-DASH2    PIC X.
           03  FILLER      PIC X(4).

       01  GL-EDIT-NUMBER    PIC 9(11).
       01  GL-EDIT REDEFINES GL-EDIT-NUMBER.
           03  GL-EDIT-BRNO  PIC 9(4).
           03  GL-EDIT-GLNO  PIC 9(7).

       01  SCAN-LP1-KEY9   PIC 9(13).
       01  SCAN-LP1-KEY REDEFINES SCAN-LP1-KEY9.
           03  SCAN-LP-ACCTNO  PIC 9(8).
           03  SCAN-LP-SEQNO   PIC 9(5).

       01  HOLD-LP1-KEY    PIC X(17)   VALUE SPACES.

      * LXE-REC IS {EARNINGS}
      * LXG-REC IS {GL}

       01  CHANGED-LP-REC              PIC X(LP-SIZE)    VALUE SPACES.
       01  CHANGED-LXE-REC             PIC X(LXE-SIZE)   VALUE SPACES.
       01  CHANGED-LXG-REC             PIC X(LXG-SIZE)   VALUE SPACES.
       01  ORIGINAL-LP-REC             PIC X(LP-SIZE)    VALUE SPACES.
       01  ORIGINAL-LXE-REC            PIC X(LXE-SIZE)   VALUE SPACES.
       01  ORIGINAL-LXG-REC            PIC X(LXG-SIZE)   VALUE SPACES.

      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC XX.
           03  F02         PIC 99/99/99  BLANK ZERO.
           03  F03         PIC 99/99/99  BLANK ZERO.
           03  F04         PIC 99/99/99  BLANK ZERO.
           03  F041        PIC X(6).
           03  F05         PIC 9(4).
           03  F051        PIC X(10).
           03  F06         PIC X(5).
           03  F07         PIC X.
           03  F071        PIC XX.
           03  F072        PIC ZZZ9.
           03  F08         PIC X(03).
           03  F09         PIC X.
           03  F091        PIC 9.
           03  F10         PIC X.
           03  F11         PIC X.
      * LP-PDTH
           03  F12         PIC ZZ/ZZ/ZZ  BLANK ZERO.
      * LP-LCPDTH
           03  F121        PIC ZZ/ZZ/ZZ  BLANK ZERO.
      * LP-NOLCHG
           03  F13         PIC ZZZ-.
      * LP-SSNO
           03  F14         PIC X(11).
           03  F141        PIC X(33).

      * LP-GLNO(1)
           03  F15         PIC 9(04).
           03  F152        PIC 9(07).
           03  F151        PIC ZZZZZ.ZZ-.
      * LP-GLNO(2)
           03  F16         PIC 9(04).
           03  F162        PIC 9(07).
           03  F161        PIC ZZZZZ.ZZ-.
      * LP-GLNO(3)
           03  F17         PIC 9(04).
           03  F172        PIC 9(07).
           03  F171        PIC ZZZZZ.ZZ-.

      * LP-TRAMT
           03  F18         PIC ZZZZZZZ.ZZ-.
           03  F19         PIC ZZZZZ.ZZ-.
           03  F191        PIC ZZZZZ.ZZ-.
      * 20. LP-APOTH/LP-APOT2
           03  F20         PIC ZZZZZ.ZZ-.
           03  F201        PIC ZZZZZ.ZZ-.
      * 21. LP-APCUR
           03  F21         PIC ZZZZZZ.ZZ-.
      * 22. LP-OTHBAL/LP-OT2BAL
           03  F22         PIC ZZZZZ.ZZ-.
           03  F221        PIC ZZZZZ.ZZ-.
      * 23. INT/LATE BALANCE
           03  F23         PIC ZZZZZ.ZZ-.
           03  F231        PIC ZZZZZ.ZZ-.
      * 24. LP-CURBAL
           03  F24         PIC ZZZZZZZ.ZZ-.
      * LP-EARNED & LXE-EARNED
           03  F25         PIC ZZZZZ.ZZ-.
           03  F251        PIC ZZZZZ.ZZ-.
           03  F26         PIC ZZZZZ.ZZ-.
           03  F261        PIC ZZZZZ.ZZ-.
           03  F27         PIC ZZZZZ.ZZ-.
           03  F271        PIC ZZZZZ.ZZ-.
      * LP-INTPAID
           03  F28         PIC ZZZZZ.ZZ-.
      * LP-INTDUE
           03  F29         PIC ZZZZZ.ZZ-.
      * 30. LP-APINTOWE
           03  F30         PIC ZZZZZ.ZZ-.
      * 31. NEW STATUS
           03  F31         PIC ZZZ.ZZZ.
      * 32. LC WORKER 1/2
           03  F32         PIC X. 
      * 33. DLPROC
           03  F33         PIC ZZZZZ.ZZ-.
      * 34. LP-DLPARVHELD-ADJ & LP-DLPARVPAID-ADJ
           03  F34         PIC ZZZZZ.ZZ-.
           03  F341        PIC ZZZZZ.ZZ-.
      * 35. DL RSV ADJ 1 2 3
           03  F35         PIC ZZZ.ZZ-.
           03  F351        PIC ZZZ.ZZ-.
           03  F352        PIC ZZZ.ZZ-.
      * 36. LXE-WORKER & LXE-WORKER2
           03  F36         PIC ZZZZZ.ZZ-.
           03  F361        PIC ZZZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(52) VALUE
           "010 020 030 040 041 050 051 060 070 071 072 080 090 ".
           03  FILLER      PIC X(56) VALUE
           "091 100 110 120 121 130 140 141 150 152 151 160 162 161 ".
           03  FILLER      PIC X(12) VALUE
           "170 172 171 ".
           03  FILLER      PIC X(56) VALUE
           "180 190 191 200 201 210 220 221 230 231 240 250 251 260 ".
           03  FILLER      PIC X(52) VALUE
           "261 270 271 280 290 300 310 320 330 340 341 350 351 ".

           03  FILLER      PIC X(12) VALUE
           "352 360 361.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 36.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 2. TRANS DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 3. PAYMENT DATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 4. POSTED DATE / TIME
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 5. POSTING BRANCH
           03  FILLER      PIC X(8) VALUE "ENNN040N".
      * 6. REFERENCE
           03  FILLER      PIC X(8) VALUE "E  A050N".
      * 7. REVERSED / REVERSED TRCD / SEQNO
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 8. COLLECTOR
           03  FILLER      PIC X(8) VALUE "E  A030N".
      * 9. STATUSFG / DRAWER
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 10. INT/PRE
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 11. MANUAL REBATE/INSTYPES
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 12. LP-PDTH
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 13. LP-NOLCHG
           03  FILLER      PIC X(8) VALUE "ENNS030N".
      * 14. LP-SSNO
           03  FILLER      PIC X(8) VALUE "ERNX090N".
      * 15. LP-GLNO(1).
           03  FILLER      PIC X(8) VALUE "ENNN040N".
      * 16. LP-GLNO(2).
           03  FILLER      PIC X(8) VALUE "ENNN040N".
      * 17. LP-GLNO(3).
           03  FILLER      PIC X(8) VALUE "ENNN040N".
      * 18. LP-TRAMT
           03  FILLER      PIC X(8) VALUE "ENNS072N".
      * 19. LP-APINT APLC
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 20. LP-APOTH/LP-ATOT2
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 21. LP-APCUR
           03  FILLER      PIC X(8) VALUE "ENNS062N".
      * 22. LP-OTHBAL
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 23. LP-INT/LATE BAL
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 24. CURBAL
           03  FILLER      PIC X(8) VALUE "EN S062N".
      * 25. EARNED(1) (2)
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 26. EARNED(3) (4)
           03  FILLER      PIC X(8) VALUE "ENNN052N".
      * 27. EARNED(5) (6)
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 28. LP-INTPAID
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 29. LP-INTDUE
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 30. APINTOWE
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 31. LP-NEWSTAT-RATE
           03  FILLER      PIC X(8) VALUE "ENNN033N".
      * 32. LCPARTIALS & LCPAID
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 33 DLPROC
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 34. DLPARVHELD-ADJ
           03  FILLER      PIC X(8) VALUE "ENNS052N".
      * 35. DL RSV 1 2 3
           03  FILLER      PIC X(8) VALUE "ENNS032N".
      * 36. LXE-WORKER
           03  FILLER      PIC X(8) VALUE "ENNS052N".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LPMAN1_DEF.CPY".
       COPY "LIBMN/LPMAN1_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01  FORM-LINK-AREA        PIC X(2000).
       SCREEN SECTION.
       COPY "LIBMN/LPMAN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.


           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LPMAN1" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           PERFORM OPEN-BW1-FILE.
           MOVE FORM-LINK-AREA TO LPMAIN-LINK.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-LP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF IO-TYPE = "CLR"
              MOVE "N" TO NEW-REC
              GO TO MAIN-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE LP1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      *********************************************
       REC-LP-MODULE SECTION.
      *********************************************
           MOVE 0 TO LP-BRNO LP-ACCTNO LP-SEQNO FIRST-SCAN.
           MOVE " " TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LP1-KEY
              MOVE " " TO SAVEKEY
              MOVE "SN N080LPNO." TO VDU
              MOVE LP-ACCTNO TO VDUNUM0 LN-ACCTNO
              PERFORM SCREENIO
              MOVE "SN N050LPSEQ." TO VDU
              MOVE LP-SEQNO TO VDUNUM0
              PERFORM SCREENIO
              GO TO READ-LPFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0   TO NEW-FLAG.

       REC-LP-ACCTNO.
           MOVE "EN N080LPNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-LP-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-LP-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-LP-ACCTNO.
           MOVE VDUNUM0 TO LP-ACCTNO LN-ACCTNO.
           IF LP-ACCTNO = 0
              GO TO REC-LP-ACCTNO.
       REC-LP-SEQ-NO.
           MOVE "EN N050LPSEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-LP-ACCTNO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LP-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LP-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LP-SEQ-NO.
           MOVE VDUNUM0 TO LP-SEQNO.
           IF LP-SEQNO = 0
              GO TO REC-LP-SEQ-NO.
           MOVE VDUNUM0 TO LP-SEQNO.

       READ-LPFILE.
           PERFORM OPEN-LP1-FILE.
      * MOVED HOLD-LP1-KEY TO HERE SO OPEN WOULD SET THE BRNO
           MOVE LP1-KEY TO HOLD-LP1-KEY.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG = 0
              MOVE LP-REC TO ORIGINAL-LP-REC
              PERFORM GET-LN
              IF IO-FG = 0
                 PERFORM DISPLAY-FIELDS
              ELSE
                 MOVE "LOAN MASTER RECORD NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE
           ELSE
              PERFORM GET-LN
              IF IO-FG = 0
                 PERFORM NEW-RECORD-SETUP
              ELSE
                 MOVE "LOAN MASTER RECORD NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE.

       END-REC-LP-MODULE.
           EXIT.

      *********************************************
       ENTER-SCANKEY SECTION.
      *********************************************
           MOVE " ENTER PAYMENT #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.

           PERFORM START-LP1-FILE.

       ENTER-KEY.
           MOVE "EN N130SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LP1-KEY TO HOLD-LP1-KEY
                 MOVE LP-REC TO ORIGINAL-LP-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

      * SCAN IS STILL JUST ACCTNO/SEQNO, NO BRANCH YET
           MOVE VDUNUM0 TO SCAN-LP1-KEY9.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE SCAN-LP-ACCTNO TO LP-ACCTNO.
           MOVE SCAN-LP-SEQNO  TO LP-SEQNO.

           PERFORM START-LP1-FILE.

       READ-FILES-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-FG = 9 OR (LP-BRNO NOT = LP-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK LP-FILE.
           MOVE "SN N080LPNO." TO VDU.
           MOVE LP-ACCTNO TO VDUNUM0 LN-ACCTNO.
           PERFORM SCREENIO.
           MOVE "SN N050LPSEQ." TO VDU.
           MOVE LP-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "LOAN MASTER RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
           ELSE
              PERFORM DISPLAY-FIELDS
              MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
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
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      **************************************************************************
      *           ALLOW A DELETE BY RANGE OF REVERSED AND REVERSE RECORDS
      *           -------------------------------------------------------
      *
      *  1. TYPE "353635"
      *  2. TYPE "DELREV"
      *  3. TYPE BEGINNING SEQ NO. , F1 TO CANCEL DELETE
      *  4. TYPE ENDING    SEQ NO. , F1 TO CANCEL DELETE
      **************************************************************************
      *           ALLOW A DELETE BY RANGE
      *           -----------------------
      *
      *  1. TYPE "353635"
      *  2. TYPE "DELSEQ"
      *  3. TYPE BEGINNING SEQ NO. , F1 TO CANCEL DELETE
      *  4. TYPE ENDING    SEQ NO. , F1 TO CANCEL DELETE
      **************************************************************************
           IF ACTION = "353635"
              MOVE "R RA060SEL." TO VDU
              PERFORM SCREENIO
              MOVE VDUBUF TO ACTION
              IF ACTION = "DELSEQ"
                 MOVE "A" TO RANGE-DELETE-FG
              ELSE
                 IF ACTION = "DELREV"
                    MOVE "R" TO RANGE-DELETE-FG
                 ELSE
                    GO TO MODIFY-ROUTINE
                 END-IF
              END-IF
              PERFORM RANGE-DELETE
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      ***************************************************************
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 2 OR FORM-SEL1 > 3
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.


           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-UP.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-DOWN.
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 F-09 F-10 F-11 F-12 F-13 F-14 F-15 F-16
                 F-17 F-18 F-19 F-20 F-21 F-22 F-23 F-24
                 F-25 F-26 F-27 F-28 F-29 F-30 F-31 F-32
                 F-33 F-34 F-35 F-36
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO LP-TRCD.
           GO TO CREATE-ROUTINE.
       F-02.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LP-TRDATE.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LP-PAYDATE.
           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LP-POSTDATE.
       F-041.
           MOVE "ENNN060041." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-041.
           MOVE VDUNUM0 TO LP-POSTTIME.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO LP-POSTING-BR.
       F-051.
           MOVE "E  A100051." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-051.
           MOVE VDUBUF TO LP-USERID.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO LP-REFNO.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO LP-REV.
           IF LP-REV NOT = "Y"
              IF LP-REV NOT = " "
                 MOVE "MUST ENTER 'Y' OR ' '" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-071.
           MOVE "ENNA020071." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ( ENTER-KEY OR DOWN-KEY) GO TO F-071.
           MOVE VDUBUF TO LP-REVTRCD.
           IF LP-REVTRCD = SPACES
              IF LP-REV = "Y"
                 MOVE "MUST ENTER A TRANSACTION CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-071.
       F-072.
           MOVE "ENNN040072." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO F-071.
           IF NOT ( ENTER-KEY OR DOWN-KEY) GO TO F-072.
           MOVE VDUNUM0 TO LP-REVSEQNO.
           IF LP-REVSEQNO = 0
              IF LP-REV = "Y"
                 MOVE "MUST ENTER A PAYMENT SEQ NUMBER" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-072.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LP-COLLECTOR.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO LP-STATUSFG.
       F-091.
           MOVE "EN N020091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-091.
           MOVE VDUNUM0 TO LP-TILLNO.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO LP-IBPC.
           IF LP-IBPC NOT = "I"
              IF LP-IBPC NOT = "P"
                 MOVE "MUST ENTER 'I' OR 'P'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO LP-MANUAL-RB.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDU-DATE-YYYYMMDD TO LP-PDTH-DATE
                                     NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO ENTER-ELE.

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE LP-PDTH-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER-REM NOT = 0
              MOVE "DATE IS NOT ON A PAY PERIOD CYCLE DATE!!!!"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-121.
           MOVE "ERNM060121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-121.
           MOVE VDU-DATE-YYYYMMDD TO LP-LCPDTH-DATE
                                     NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO F-121.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE LP-LCPDTH-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER-REM NOT = 0
              MOVE "DATE IS NOT ON A PAY PERIOD CYCLE DATE!!!!"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO F-121.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM0 TO LP-NOLCHG.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM0 TO LP-SSNO BW-SSNO.
           IF LP-SSNO = 0
              MOVE SPACES TO MESS
              GO TO DISP-BORROWER-NAME.
           MOVE "S  X110140." TO VDU.
           MOVE LP-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "** NOT ON FILE **" TO MESS
              PERFORM SEND-MESS
              IF LP-SSNO NOT = 999999999
                 GO TO ENTER-ELE.
           MOVE BW-LNAME TO MESS.
           IF BW-LNAME NOT = SPACES
              INSPECT MESS REPLACING FIRST "   " BY ", ]"
           ELSE
              MOVE "]" TO MESS.
           INSPECT MESS REPLACING FIRST
              "]                                  " BY BW-FNAME.
       DISP-BORROWER-NAME.
           MOVE "S  A330141." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO GL-EDIT-BRNO.
       F-152.
           MOVE "ENNN070152." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-152.
           MOVE VDUNUM0 TO GL-EDIT-GLNO.
           MOVE GL-EDIT-NUMBER TO  LXG-GLNO(1).
       F-151.
           MOVE "ENNS052151." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-151.
           MOVE VDUNUM2 TO LXG-GLAMT(1).
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM0 TO GL-EDIT-BRNO.
       F-162.
           MOVE "ENNN070162." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-162.
           MOVE VDUNUM0 TO GL-EDIT-GLNO.
           MOVE GL-EDIT-NUMBER TO  LXG-GLNO(2).
       F-161.
           MOVE "ENNS052161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-161.
           MOVE VDUNUM2 TO LXG-GLAMT(2).
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM0 TO GL-EDIT-BRNO.
       F-172.
           MOVE "ENNN070172." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-172.
           MOVE VDUNUM0 TO GL-EDIT-GLNO.
           MOVE GL-EDIT-NUMBER TO  LXG-GLNO(3).
       F-171.
           MOVE "ENNS052171." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-171.
           MOVE VDUNUM2 TO LXG-GLAMT(3).
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM2 TO LP-TRAMT.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO LP-APINT.
       F-191.
           MOVE "ENNS052191." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-191.
           MOVE VDUNUM2 TO LP-APLC.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM2 TO LP-APOTH.
       F-201.
           MOVE "ENNS052201." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-201.
           MOVE VDUNUM2 TO LP-APOT2.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM2 TO LP-APCUR.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO LP-OTHBAL.
       F-221.
           MOVE "ENNS052221." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-221.
           MOVE VDUNUM2 TO LP-OT2BAL.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO LP-INTBAL.
       F-231.
           MOVE "ENNS052231." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-231.
           MOVE VDUNUM2 TO LP-LCBAL.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM2 TO LP-CURBAL.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE 1 TO SUB.
           PERFORM UPDATE-EARNED.
       F-251.
           MOVE "ENNS052251." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-251.
           MOVE 2 TO SUB.
           PERFORM UPDATE-EARNED.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE 3 TO SUB.
           PERFORM UPDATE-EARNED.
       F-261.
           MOVE "ENNS052261." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-261.
           MOVE 4 TO SUB.
           PERFORM UPDATE-EARNED.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE 5 TO SUB.
           PERFORM UPDATE-EARNED.
       F-271.
           MOVE "ENNS052271." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-271.
           MOVE 6 TO SUB.
           PERFORM UPDATE-EARNED.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUNUM2 TO LP-INTPAID.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUNUM2 TO LP-INTDUE.
           GO TO CREATE-ROUTINE.
       F-30.
      * THIS IS NOW JUST LP-APINTOWE AS NO LONGER REDEFINED          
           MOVE VDUNUM2 TO LP-APINTOWE.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE VDUNUM3 TO LP-NEWSTAT-RATE.
           GO TO CREATE-ROUTINE.
       F-32.
      *THIS WAS LP-LCPARTIALS BUT ITS OWN FIELD ON PAGE 3 OF MAINTENANCE
           GO TO CREATE-ROUTINE.
      *THIS WAS LP-LCPAID, BUT ITS OWN FIELD ON PAGE 3 OF MAINTENANCE
       F-33.
           MOVE VDUNUM2 TO LP-DLPROC.
           GO TO CREATE-ROUTINE.
       F-34.
      *LP-LT-SEQNO IS NO LONGER A REDEFINE BUT ITS OWN FIELD
           IF LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST"
              GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO LP-DLPARVHELD-ADJ.
           GO TO CREATE-ROUTINE.
       F-341.
           MOVE "ENNS062341." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-341.
           MOVE VDUNUM2 TO LP-DLPARVPAID-ADJ.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDUNUM2 TO LP-DLRVCHGS-ADJ(1).
       F-351.
           MOVE "ENNS032351." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-351.
           MOVE VDUNUM2 TO LP-DLRVCHGS-ADJ(2).
       F-352.
           MOVE "ENNS032352." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-352.
           MOVE VDUNUM2 TO LP-DLRVCHGS-ADJ(3).
           GO TO CREATE-ROUTINE.
       F-36.
           MOVE VDUNUM2 TO LXE-WORKER.
       F-361.
           MOVE "ENNS032361." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
               GO TO F-361.
           MOVE VDUNUM2 TO LXE-WORKER2.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      *******************************************
       WRITE-MODULE SECTION.
      *******************************************
           PERFORM OPEN-LP1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LP-REC  TO CHANGED-LP-REC.
           MOVE LXG-REC TO CHANGED-LXG-REC.
           MOVE LXE-REC TO CHANGED-LXE-REC.

      *    2017-0508 - JAY & CINDY
      *
      *    CINDY FOUND THE FOLLOWING WHEN WORKING ON LPFILE A30 CHANGES:
      *
      *       IF ORIGINAL-REC = CHANGED-REC
      *          IF ORIGINAL-GL = CHANGED-GL
      *             IF ORIGINAL-GL   = CHANGED-EARN    <-??? IN A15
      *                GO TO END-WRITE-MODULE.
      *
      *    JAY CHANGED SINCE THEN CHANGED THE FIELDS FOR PROPER FILE
      *    PREFIX NAMES:
      *
      *       IF ORIGINAL-LP-REC = CHANGED-LP-REC
      *          IF ORIGINAL-LXG-REC = CHANGED-LXG-REC
      *             IF ORIGINAL-LXE-REC   = CHANGED-LXE-REC 
      *                GO TO END-WRITE-MODULE.
      *
      *    IN A15, WE DECIDED TO LEAVE THE QUESTIONABLE IF CONDITION
      *    AS IS SINCE IT HAS BEEN IN THERE SINCE 2000-1129.  FOR A30,
      *    WE DECIDED TO FIX IT.

           IF ORIGINAL-LP-REC = CHANGED-LP-REC
              IF ORIGINAL-LXG-REC = CHANGED-LXG-REC
                 IF ORIGINAL-LXE-REC = CHANGED-LXE-REC
                    GO TO END-WRITE-MODULE.

           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF LP-REC NOT = ORIGINAL-LP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-LP-REC TO LP-REC.

           PERFORM REWRITE-LP1-FILE.

           IF ORIGINAL-LXG-REC  = CHANGED-LXG-REC
              GO TO WRITE-MODULE-REWRITE-LXE.

           PERFORM OPEN-LXG1-FILE.
           PERFORM SET-LXG-KEY.
           PERFORM READ-LXG1-FILE.
           IF IO-FG NOT = 0
              MOVE CHANGED-LXG-REC TO LXG-REC
              PERFORM WRITE-GL-NUMS
              GO TO WRITE-MODULE-REWRITE-LXE.

           IF LXG-REC NOT = ORIGINAL-LXG-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              PERFORM CLOSE-LXG1-FILE
              GO TO WRITE-MODULE-REWRITE-LXE.

           MOVE CHANGED-LXG-REC TO LXG-REC.
           PERFORM WRITE-GL-NUMS.

           PERFORM LOG-MAINTENANCE-CHGS-LXG.

       WRITE-MODULE-REWRITE-LXE.
           IF ORIGINAL-LXE-REC = CHANGED-LXE-REC
              GO TO END-WRITE-MODULE.

           PERFORM OPEN-LXE1-FILE.
           PERFORM SET-LXE-KEY.
           PERFORM READ-LXE1-FILE.
           IF IO-FG NOT = 0
              MOVE CHANGED-LXE-REC TO LXE-REC
              PERFORM WRITE-EARN
              GO TO END-WRITE-MODULE.

           IF LXE-REC NOT = ORIGINAL-LXE-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              PERFORM CLOSE-LXE1-FILE
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-LXE-REC TO LXE-REC.
           PERFORM WRITE-EARN.

           PERFORM LOG-MAINTENANCE-CHGS-LX-EARN.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-GL-NUMS.

           PERFORM WRITE-EARN.

           PERFORM WRITE-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-LP1-FILE.

      ***********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************************
           MOVE ORIGINAL-LP-REC TO LPOR-REC.
           MOVE CHANGED-LP-REC  TO LPCH-REC.

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LPOR-TRCD NOT = LPCH-TRCD
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRCD    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TRCD               TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-TRCD               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TRDATE NOT = LPCH-TRDATE
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRDATE  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TRDATE             TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-TRDATE             TO DATE-YYYYMMDD           
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PAYDATE NOT = LPCH-PAYDATE
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "PAYDATE "              TO WORK-ERRLOG-DESC
             MOVE LPOR-PAYDATE            TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-PAYDATE            TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-POSTDATE NOT = LPCH-POSTDATE
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "POSTDATE"              TO WORK-ERRLOG-DESC
             MOVE LPOR-POSTDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-POSTDATE           TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-POSTTIME NOT = LPCH-POSTTIME
             MOVE "041"                   TO WORK-ERRLOG-FIELD
             MOVE "POSTTIME"              TO WORK-ERRLOG-DESC
             MOVE LPOR-POSTTIME           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-POSTTIME           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-POSTING-BR NOT = LPCH-POSTING-BR
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "POST BR "              TO WORK-ERRLOG-DESC
             MOVE LPOR-POSTING-BR         TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-POSTING-BR         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-USERID NOT = LPCH-USERID
             MOVE "051"                   TO WORK-ERRLOG-FIELD
             MOVE "USER ID "              TO WORK-ERRLOG-DESC
             MOVE LPOR-USERID             TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-USERID             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REFNO NOT = LPCH-REFNO
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "REFNO   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-REFNO              TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-REFNO              TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REV NOT = LPCH-REV
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "REV     "              TO WORK-ERRLOG-DESC
             MOVE LPOR-REV                TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-REV                TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REVTRCD NOT = LPCH-REVTRCD
             MOVE "071"                   TO WORK-ERRLOG-FIELD
             MOVE "REVTRCD "              TO WORK-ERRLOG-DESC
             MOVE LPOR-REVTRCD            TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-REVTRCD            TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-REVSEQNO NOT = LPCH-REVSEQNO
             MOVE "072"                   TO WORK-ERRLOG-FIELD
             MOVE "REVSEQNO"              TO WORK-ERRLOG-DESC
             MOVE LPOR-REVSEQNO           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-REVSEQNO           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-COLLECTOR NOT = LPCH-COLLECTOR
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "COLLEC  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-COLLECTOR          TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-COLLECTOR          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-STATUSFG NOT = LPCH-STATUSFG
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "STATUSFG"              TO WORK-ERRLOG-DESC
             MOVE LPOR-STATUSFG           TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-STATUSFG           TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TILLNO NOT = LPCH-TILLNO
             MOVE "091"                   TO WORK-ERRLOG-FIELD
             MOVE "TILLNO  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TILLNO             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-TILLNO             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-IBPC NOT = LPCH-IBPC
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "IBPC    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-IBPC               TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-IBPC               TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-MANUAL-RB NOT = LPCH-MANUAL-RB
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "MANUALRB"              TO WORK-ERRLOG-DESC
             MOVE LPOR-MANUAL-RB          TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-MANUAL-RB          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PDTH-DATE NOT = LPCH-PDTH-DATE
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "PDTH    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-PDTH-DATE          TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-PDTH-DATE          TO WORK-ERRLOG-CHG-N0
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCPDTH-DATE NOT = LPCH-LCPDTH-DATE
             MOVE "121"                   TO WORK-ERRLOG-FIELD
             MOVE "LCPDTH-DATE  "         TO WORK-ERRLOG-DESC
             MOVE LPOR-LCPDTH-DATE        TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-LCPDTH-DATE        TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-NOLCHG NOT = LPCH-NOLCHG
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "NOLCHG  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-NOLCHG             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-NOLCHG             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-SSNO NOT = LPCH-SSNO
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "SSNO  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-SSNO             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-SSNO             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TRAMT NOT = LPCH-TRAMT
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRAMT   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-TRAMT              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-TRAMT              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APINT NOT = LPCH-APINT
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "APINT   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APINT              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-APINT              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APLC NOT = LPCH-APLC
             MOVE "191"                   TO WORK-ERRLOG-FIELD
             MOVE "APLC    "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APLC              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-APLC              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APOTH NOT = LPCH-APOTH
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "APOTH   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APOTH              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-APOTH              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APOT2 NOT = LPCH-APOT2
             MOVE "201"                   TO WORK-ERRLOG-FIELD
             MOVE "APOT2   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APOT2              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-APOT2              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-APCUR NOT = LPCH-APCUR
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "APCUR   "              TO WORK-ERRLOG-DESC
             MOVE LPOR-APCUR              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-APCUR              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-OTHBAL NOT = LPCH-OTHBAL
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "OTHBAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-OTHBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-OTHBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-OT2BAL NOT = LPCH-OT2BAL
             MOVE "221"                   TO WORK-ERRLOG-FIELD
             MOVE "OT2BAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-OT2BAL             TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-OT2BAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTBAL NOT = LPCH-INTBAL
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTBAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-INTBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-INTBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCBAL NOT = LPCH-LCBAL
             MOVE "231"                   TO WORK-ERRLOG-FIELD
             MOVE "LC BAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-LCBAL              TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-LCBAL              TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-CURBAL NOT = LPCH-CURBAL
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "CURBAL  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-CURBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-CURBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTPAID NOT = LPCH-INTPAID
             MOVE "28 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTPAID "              TO WORK-ERRLOG-DESC
             MOVE LPOR-INTPAID            TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-INTPAID            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTDUE NOT = LPCH-INTDUE
             MOVE "29 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTDUE  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-INTDUE             TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-INTDUE             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-NEWSTAT-RATE NOT = LPCH-NEWSTAT-RATE
             MOVE "31 "                   TO WORK-ERRLOG-FIELD
             MOVE "STATRATE"              TO WORK-ERRLOG-DESC
             MOVE LPOR-NEWSTAT-RATE       TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-NEWSTAT-RATE       TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCPARTIALS NOT = LPCH-LCPARTIALS
             MOVE "32 "                   TO WORK-ERRLOG-FIELD
             MOVE "LCPARTIALS"            TO WORK-ERRLOG-DESC
             MOVE LPOR-LCPARTIALS         TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-LCPARTIALS         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCPAID NOT = LPCH-LCPAID
             MOVE "321"                   TO WORK-ERRLOG-FIELD
             MOVE "LCPAID  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-LCPAID             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-LCPAID             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLPROC NOT = LPCH-DLPROC
             MOVE "33 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLPROC  "              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLPROC             TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-DLPROC             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLPARVPAID-ADJ NOT = LPCH-DLPARVPAID-ADJ
             MOVE "341"                   TO WORK-ERRLOG-FIELD
             MOVE "PARVPAID"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLPARVPAID-ADJ     TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-DLPARVPAID-ADJ     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLRVCHGS-ADJ(1) NOT = LPCH-DLRVCHGS-ADJ(1)
             MOVE "35 "                   TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 1"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLRVCHGS-ADJ(1)    TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-DLRVCHGS-ADJ(1)    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLRVCHGS-ADJ(2) NOT = LPCH-DLRVCHGS-ADJ(2)
             MOVE "351"                   TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 2"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLRVCHGS-ADJ(2)    TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-DLRVCHGS-ADJ(2)    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DLRVCHGS-ADJ(3) NOT = LPCH-DLRVCHGS-ADJ(3)
             MOVE "352"                   TO WORK-ERRLOG-FIELD
             MOVE "RVCHGS 3"              TO WORK-ERRLOG-DESC
             MOVE LPOR-DLRVCHGS-ADJ(3)    TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-DLRVCHGS-ADJ(3)    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      **************************************************
       LOG-MAINTENANCE-CHGS-LXG SECTION.
      **************************************************
           MOVE ORIGINAL-LXG-REC TO LXGOR-REC.
           MOVE CHANGED-LXG-REC  TO LXGCH-REC.

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO WORK-ERRLOG-TO-X.

           IF LXGOR-GLNO(1)  NOT = LXGCH-GLNO(1)
             MOVE "15 "                 TO WORK-ERRLOG-FIELD
             MOVE "GLNO(1) "            TO WORK-ERRLOG-DESC
             MOVE LXGOR-GLNO(1)         TO WORK-ERRLOG-ORG-N0
             MOVE LXGCH-GLNO(1)         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXGOR-GLAMT(1)  NOT = LXGCH-GLAMT(1)
             MOVE "15 "                 TO WORK-ERRLOG-FIELD
             MOVE "GL AMOUNT(1)"        TO WORK-ERRLOG-DESC
             MOVE LXGOR-GLAMT(1)        TO WORK-ERRLOG-ORG-N2
             MOVE LXGCH-GLAMT(1)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXGOR-GLNO(2)  NOT = LXGCH-GLNO(2)
             MOVE "16 "                 TO WORK-ERRLOG-FIELD
             MOVE "GLNO(2) "            TO WORK-ERRLOG-DESC
             MOVE LXGOR-GLNO(2)         TO WORK-ERRLOG-ORG-N0
             MOVE LXGCH-GLNO(2)          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXGOR-GLAMT(2)  NOT = LXGCH-GLAMT(2)
             MOVE "16 "                 TO WORK-ERRLOG-FIELD
             MOVE "GL AMOUNT(2)"        TO WORK-ERRLOG-DESC
             MOVE LXGOR-GLAMT(2)        TO WORK-ERRLOG-ORG-N2
             MOVE LXGCH-GLAMT(2)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LXGOR-GLNO(3)  NOT = LXGCH-GLNO(3)
             MOVE "17 "                 TO WORK-ERRLOG-FIELD
             MOVE "GLNO(3) "            TO WORK-ERRLOG-DESC
             MOVE LXGOR-GLNO(3)         TO WORK-ERRLOG-ORG-N0
             MOVE LXGCH-GLNO(3)          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LXGOR-GLAMT(3)  NOT = LXGCH-GLAMT(3)
             MOVE "17 "                 TO WORK-ERRLOG-FIELD
             MOVE "GL AMOUNT(3)"        TO WORK-ERRLOG-DESC
             MOVE LXGOR-GLAMT(3)        TO WORK-ERRLOG-ORG-N2
             MOVE LXGCH-GLAMT(3)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINT-CHGS-LX-EXIT.
           EXIT.

      ***********************************************
       LOG-MAINTENANCE-CHGS-LX-EARN SECTION.
      ***********************************************
           MOVE ORIGINAL-LXE-REC TO LXEOR-REC.
           MOVE CHANGED-LXE-REC TO LXECH-REC.

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.
         
           IF LXEOR-EARN(1)  NOT = LXECH-EARN(1)   
              IF LN-EARN-ORDER(1) = 4
                 MOVE "25 "                 TO WORK-ERRLOG-FIELD
                 MOVE "INT ERN     "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(1)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(1)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(2) = 4
                 MOVE "251"                 TO WORK-ERRLOG-FIELD
                 MOVE "ADDON       "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(1)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(1)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(3) = 4
                 MOVE "26 "                 TO WORK-ERRLOG-FIELD
                 MOVE "SC ERN      "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(1)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(1)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(4) = 4
                 MOVE "261"                 TO WORK-ERRLOG-FIELD
                 MOVE "MF  ERN     "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(1)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(1)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(5) = 4
                 MOVE "27 "                 TO WORK-ERRLOG-FIELD
                 MOVE "DISC        "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(1)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(1)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(6) = 4
                 MOVE "271"                 TO WORK-ERRLOG-FIELD
                 MOVE "OTH ERN     "        TO WORK-ERRLOG-DESC
                 MOVE LXEOR-EARN(1)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(1)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE.

           IF LXEOR-EARN(2)  NOT = LXECH-EARN(2)   
              IF LN-EARN-ORDER(1) = 5
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "INT ERN     "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(2)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(2)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(2) = 5
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "ADDON       "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(2)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(2)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(3) = 5
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "SC ERN      "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(2)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(2)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(4) = 5
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "MF  ERN     "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(2)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(2)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(5) = 5
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "DISC        "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(2)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(2)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(6) = 5
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "OTH ERN     "        TO WORK-ERRLOG-DESC
                 MOVE LXEOR-EARN(2)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(2)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE.


           IF LXEOR-EARN(3)  NOT = LXECH-EARN(3)   
              IF LN-EARN-ORDER(1) = 6
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "INT ERN     "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(3)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(3)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(2) = 6
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "ADDON       "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(3)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(3)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(3) = 6
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "SC ERN      "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(3)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(3)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(4) = 6
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "MF  ERN     "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(3)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(3)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(5) = 6
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "DISC        "        TO WORK-ERRLOG-DESC   
                 MOVE LXEOR-EARN(3)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(3)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE
              ELSE
              IF LN-EARN-ORDER(6) = 6
                 MOVE "   "                 TO WORK-ERRLOG-FIELD
                 MOVE "OTH ERN     "        TO WORK-ERRLOG-DESC
                 MOVE LXEOR-EARN(3)         TO WORK-ERRLOG-ORG-N2
                 MOVE LXECH-EARN(3)         TO WORK-ERRLOG-CHG-N2
                 PERFORM LOG-FIELD-CHANGE.


      *************************************
       LOG-FIELD-CHANGE SECTION.
      *************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ****************************************
       DELETE-MODULE SECTION.
      ****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF LP-REC NOT = ORIGINAL-LP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM SET-LXG-KEY.
           PERFORM OPEN-LXG1-FILE.
           PERFORM READ-LXG1-FILE.
           IF ( IO-GOOD )
              PERFORM DELETE-LXG1-FILE.
           PERFORM CLOSE-LXG1-FILE.

           PERFORM SET-LXE-KEY.
           PERFORM OPEN-LXE1-FILE.
           PERFORM READ-LXE1-FILE.
           IF ( IO-GOOD )
              PERFORM DELETE-LXE1-FILE.
           PERFORM CLOSE-LXE1-FILE.

      * DELETE ALL OTHER LX RECORDS WHICH ARE NOW COMBINED INTO 1 RECORD
           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM OPEN-LX1-FILE.
           PERFORM READ-LX1-FILE.
           IF ( IO-GOOD )
              PERFORM DELETE-LX1-FILE.
           PERFORM CLOSE-LX1-FILE.

           PERFORM DELETE-LP1-FILE.

           PERFORM CLOSE-BW1-FILE.

      * WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING LP1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-LP1-FILE.

      ************************************************
       RANGE-DELETE SECTION.
      ************************************************
           IF RANGE-DELETE-FG = "A"
              STRING "CAUTION...YOU'VE ENTERED (RANGE)"
                     " PAYMENT LINE DELETE, MODE!"
                  DELIMITED BY SIZE INTO MESS
           ELSE
              STRING "CAUTION...YOU'VE ENTERED (R E V E R"
                     " S A L) LINE DELETE, MODE!"
                  DELIMITED BY SIZE INTO MESS
           END-IF.
           PERFORM SEND-MESS.

       RANGE-DELETE-BEG.
           IF RANGE-DELETE-FG = "A"
              MOVE
              "F1-CANCEL, ENTER EXACT BEGINNING SEQ. NO. TO DELETE"
                                                          TO MESS
           ELSE
              MOVE
              "F1-CANCEL, ENTER BEGINNING SEQ. NO. TO SCAN TO DELETE"
                                                              TO MESS.
           PERFORM SEND-MESS.
           MOVE "EN N060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO RANGE-DELETE-EXIT.
           IF VDUNUM0 < 1 OR > 9999
              GO TO RANGE-DELETE-BEG
           ELSE
              MOVE VDUNUM0 TO BEG-SEQ.

       RANGE-DELETE-END.
           MOVE "F1-CANCEL, ENTER ENDING SEQ. NO. TO DELETE" TO MESS.
           IF RANGE-DELETE-FG = "A"
              MOVE
              "F1-CANCEL, ENTER EXACT ENDING SEQ. NO TO DELETE"
                                                          TO MESS
           ELSE
              MOVE
              "F1-CANCEL, ENTER ENDING SEQ. NO. TO SCAN TO DELETE"
                                                              TO MESS.
           PERFORM SEND-MESS.
           MOVE "EN N060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO RANGE-DELETE-EXIT.
           IF VDUNUM0 NOT > BEG-SEQ
              GO TO RANGE-DELETE-END
           ELSE
              MOVE VDUNUM0 TO END-SEQ.

           MOVE LP-ACCTNO TO SAVE-LP-ACCTNO.
      *    UNLOCK LP-FILE.

           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LX1-FILE.
           PERFORM OPEN-LXG1-FILE.
           PERFORM OPEN-LXE1-FILE.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE SAVE-LP-ACCTNO TO LP-ACCTNO.
      *    UNLOCK LP-FILE.
           MOVE BEG-SEQ        TO LP-SEQNO.

           PERFORM START-LP1-FILE.
       RANGE-DELETE-LOOP.
           PERFORM READ-LP1-FILE-NEXT.
           IF (IO-FG = 9) OR
               (LP-BRNO NOT = LP-PATH-OWNBR) OR
                (LP-ACCTNO NOT = SAVE-LP-ACCTNO) OR
                  (LP-SEQNO > END-SEQ)
              PERFORM CLOSE-LP1-FILE
              GO TO RANGE-DELETE-EXIT.

           IF RANGE-DELETE-FG = "R"
              IF NOT ( (LP-REV = "Y") OR (LP-TRCD = "RV") )
                 GO TO RANGE-DELETE-LOOP.

           PERFORM DELETE-LP1-FILE.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           PERFORM READ-LXG1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LXG1-FILE.

           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LXE1-FILE.

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LX1-FILE.

           GO TO RANGE-DELETE-LOOP.

       RANGE-DELETE-EXIT.
           EXIT.

      *****************************************
       WRITE-GL-NUMS SECTION.
      *****************************************
           PERFORM OPEN-LXG1-FILE.

           IF LXG-GLAMT(1) = 0 AND
              LXG-GLAMT(2) = 0 AND
              LXG-GLAMT(3) = 0
              PERFORM SET-LXG-KEY
              PERFORM READ-LXG1-FILE
              IF IO-FG = 0
                 PERFORM DELETE-LXG1-FILE
                 GO TO END-WRITE-GL-NUMS
              ELSE
                 GO TO END-WRITE-GL-NUMS.

           PERFORM SET-LXG-KEY.
           PERFORM WRITE-LXG1-FILE.
           IF IO-FG = 9
              PERFORM REWRITE-LXG1-FILE.

       END-WRITE-GL-NUMS.
           PERFORM CLOSE-LXG1-FILE.

       EXIT-WRITE-GL-NUMS.
           EXIT.

      ***********************************************
       WRITE-EARN SECTION.
      ***********************************************
           IF LXE-EARN(1) = 0 AND
              LXE-EARN(2) = 0 AND
              LXE-EARN(3) = 0 AND
              LXE-WORKER  = 0 AND
              LXE-WORKER2 = 0 AND
              LXE-EARN(4) = 0 AND
              LXE-EARN(5) = 0 AND
              LXE-EARN(6) = 0 AND
              LXE-EARN(7) = 0  
              PERFORM SET-LXE-KEY
              PERFORM OPEN-LXE1-FILE
              PERFORM READ-LXE1-FILE
              IF IO-FG = 0
                 PERFORM DELETE-LXE1-FILE
                 PERFORM CLOSE-LXE1-FILE
                 GO TO EXIT-WRITE-EARN
              ELSE
                 PERFORM CLOSE-LXE1-FILE
                 GO TO EXIT-WRITE-EARN.

           PERFORM SET-LXE-KEY.
           PERFORM OPEN-LXE1-FILE.
           PERFORM WRITE-LXE1-FILE.
           IF IO-FG = 9
              PERFORM REWRITE-LXE1-FILE.

           PERFORM CLOSE-LXE1-FILE.
       EXIT-WRITE-EARN.
           EXIT.

      ******************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************
           MOVE "Y" TO NEW-REC.
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LP-FILE.

           MOVE HOLD-LP1-KEY TO LP1-KEY.
           MOVE 0 TO ELE.

           PERFORM CLEAR-LXG-FILE.
           PERFORM SET-LXG-KEY.
           MOVE LXG-REC TO ORIGINAL-LXG-REC CHANGED-LXG-REC.

           PERFORM CLEAR-LXE-FILE.
           PERFORM SET-LXE-KEY.
           MOVE LXE-REC TO ORIGINAL-LXE-REC CHANGED-LXE-REC.

       NEW-RECORD-EXIT.
           EXIT.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE SPACES TO DISPLAY-BUF BW-REC.
           MOVE LP-TRCD TO F01.

           MOVE LP-TRDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F02.

           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.

           MOVE LP-POSTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F04.

           MOVE LP-POSTTIME TO F041.
           MOVE LP-POSTING-BR TO F05.
           MOVE LP-USERID TO F051.
           MOVE LP-REFNO TO F06.
           MOVE LP-REV TO F07.
           MOVE LP-REVTRCD TO F071.
           MOVE LP-REVSEQNO TO F072.
           MOVE LP-COLLECTOR TO F08.
           MOVE LP-STATUSFG TO F09.
           MOVE LP-TILLNO TO F091.
           MOVE LP-IBPC TO F10.
           MOVE LP-MANUAL-RB TO F11.

           MOVE LP-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F12.
           MOVE LP-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F121.
           MOVE LP-NOLCHG TO F13.
           MOVE LP-SSNO TO SS-GL-EDIT BW-SSNO.
           MOVE "-" TO SS-DASH1 SS-DASH2.
           MOVE SS-EDIT TO F14.
           IF LP-SSNO = 0
              GO TO DISPLAY-FIELDS-CONT.

           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "** NOT ON FILE **" TO F141
           ELSE
              MOVE SPACES TO F141
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                            INTO F141.
       DISPLAY-FIELDS-CONT.
           MOVE LP-TRAMT TO F18.
           MOVE LP-APINT TO F19.
           MOVE LP-APLC TO F191.

           MOVE LP-APOTH TO F20.

           MOVE LP-APOT2 TO F201.

           MOVE LP-APCUR TO F21.

           MOVE LP-OTHBAL TO F22.

           MOVE LP-OT2BAL TO F221.

           MOVE LP-INTBAL TO F23.

           MOVE LP-LCBAL TO F231.

           MOVE LP-CURBAL TO F24.

           MOVE LP-INTPAID TO F28.

           MOVE LP-INTDUE TO F29.
           MOVE LP-APINTOWE TO F30.

           MOVE LP-NEWSTAT-RATE TO F31.

           MOVE LP-DLPROC TO F33.
      *LP-LT-SEQNO ITS OWN FIELD DISPLAYED ON LPMAN3
           MOVE LP-DLPARVHELD-ADJ TO F34.

           MOVE LP-DLPARVPAID-ADJ TO F341.

           MOVE LP-DLRVCHGS-ADJ(1) TO F35.

           MOVE LP-DLRVCHGS-ADJ(2) TO F351.

           MOVE LP-DLRVCHGS-ADJ(3) TO F352.

           PERFORM SET-LXG-KEY.
           PERFORM OPEN-LXG1-FILE.
           PERFORM READ-LXG1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LXG-FILE
              PERFORM SET-LXG-KEY.

           MOVE LXG-REC TO ORIGINAL-LXG-REC CHANGED-LXG-REC.

           MOVE LXG-GLNO(1) TO GL-EDIT-NUMBER.
           MOVE GL-EDIT-BRNO TO F15.
           MOVE GL-EDIT-GLNO TO F152.
           MOVE LXG-GLAMT(1) TO F151.

           MOVE LXG-GLNO(2) TO GL-EDIT-NUMBER.
           MOVE GL-EDIT-BRNO TO F16.
           MOVE GL-EDIT-GLNO TO F162.
           MOVE LXG-GLAMT(2) TO F161.

           MOVE LXG-GLNO(3) TO GL-EDIT-NUMBER.
           MOVE GL-EDIT-BRNO TO F17.
           MOVE GL-EDIT-GLNO TO F172.
           MOVE LXG-GLAMT(3) TO F171.

           PERFORM SET-LXE-KEY.
           PERFORM OPEN-LXE1-FILE.
           PERFORM READ-LXE1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-LXE-FILE
              PERFORM SET-LXE-KEY.

           MOVE LXE-REC TO ORIGINAL-LXE-REC CHANGED-LXE-REC.

           MOVE 1 TO SUB.
           PERFORM DISPLAY-EARNED.

           MOVE LXE-WORKER TO F36.
           MOVE LXE-WORKER2 TO F361.

       DISPLAY-FIELDS-EXIT.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF  TO WR-BUF.
           PERFORM CALL-WRFORM.

      **************************************
       DISPLAY-EARNED SECTION.
      **************************************
       DISPLAY-EARN.
           IF LN-EARN-ORDER(SUB) = 1
              MOVE LP-EARNED(1) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 2
              MOVE LP-EARNED(2) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 3
              MOVE LP-EARNED(3) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 4
              MOVE LXE-EARN(1) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 5
              MOVE LXE-EARN(2) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 6
              MOVE LXE-EARN(3) TO WK-EARN
           ELSE
              MOVE ZEROES      TO WK-EARN.

           IF SUB = 1
              MOVE WK-EARN TO F25
           ELSE
           IF SUB = 2
              MOVE WK-EARN TO F251
           ELSE
           IF SUB = 3
              MOVE WK-EARN TO F26
           ELSE
           IF SUB = 4
              MOVE WK-EARN TO F261
           ELSE
           IF SUB = 5
              MOVE WK-EARN TO F27
           ELSE
           IF SUB = 6
              MOVE WK-EARN TO F271.

           ADD 1 TO SUB.
           IF SUB < 7
              GO TO DISPLAY-EARN.


      ****************************************************************
       FORM-RESET SECTION.
      ****************************************************************
           MOVE EXT-CONAME-CONAME TO LPMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LPMAN1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LPMAN1-SCREEN UPON LPMAIN-WINDOW-HANDLE.
           MOVE LPMAN1-FORM-FIELDS TO WR-BUF.
           MOVE LPMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ****************************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************************
           MOVE LPMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPMAN1-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LPMAN1_EVA.CPY".
              WHEN OTHER  MOVE "2A" TO STAT
           END-EVALUATE.


      ****************************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SET-LXG-KEY.
           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
       SET-LXE-KEY.
           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
       UPDATE-EARNED.
           IF LN-EARN-ORDER(SUB) = 1
              MOVE VDUNUM2 TO LP-EARNED(1)
           ELSE
           IF LN-EARN-ORDER(SUB) = 2
              MOVE VDUNUM2 TO LP-EARNED(2)
           ELSE
           IF LN-EARN-ORDER(SUB) = 3
              MOVE VDUNUM2 TO LP-EARNED(3)
           ELSE
           IF LN-EARN-ORDER(SUB) = 4
              MOVE VDUNUM2 TO LXE-EARN(1)
           ELSE
           IF LN-EARN-ORDER(SUB) = 5
              MOVE VDUNUM2 TO LXE-EARN(2)
           ELSE
           IF LN-EARN-ORDER(SUB) = 6
              MOVE VDUNUM2 TO LXE-EARN(3).
       GET-LN.
           PERFORM OPEN-LN1-FILE.
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
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPLXECL.CPY".
       COPY "LIBLP/LPLXGCL.CPY".

      ***********************************************
       IO-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***********************************************
       END-ROUTINE SECTION.
      ***********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LPMAIN-LINK TO FORM-LINK-AREA.

       STOP-RUN.
           EXIT PROGRAM.

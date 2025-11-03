       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GBMAN1.
      ******************************************************************
      *
      *  DESC: GLOBAL FILE MAINTENANCE - PAGE 1
      *        2019-0918 GB/GBMNA1 REPLACED GB/GBMAN1 SINCE IT NOW
      *        NEEDS TO ALWAYS ASK FOR BRANCH NUMBER.
      *
      *=================================================================
      * REV: *****FROM GB/GBMAN1 PRIOR TO 2019-0918*****
      * MJD 100695 TURNED OFF RESET FIELD FOR LAST ACCRUAL DATE.
      * MJD 031196 ADDED LOGIC TO STOP 0 AS ACCOUNT NUMBER.
      * KEC 110596 FIXED THE SUBSCRIPT ACCESS IN 'SEQ-CHECK' FROM
      *            18 TO 16.
      * KEC 011797 ADDED GB-NEXT-APPNO. "12. NEXT APPLICATION #:"
      * KEC 090397 ***REVISED*** RE-MADE THE SCREEN LAYOUT IN ORDER
      *                          TO GET MORE ITEMS ON THE SCREEN;
      *                          ADDED GB-NEXT-GIFTNET-PO
      * GLF 102997 REMOVED RESET OF FIELDS
      * BAH 082698 REMOVED 6U FROM BHFC
      * JTG 101598 ADDED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL REGENCY REVOLVING
      * BLV 990517 ADDED 6U AS VALID CHECK ID
      * GLF 052699 DD NEW CHECK PROGRAM FOR CHALLENGE FINANCIAL
      * GLF 990609 REMOVED 6U AS VALID CHECK ID AND ADD 8H
      * BAH 990622 ADDED GB-INS-PRODUCER FOR REGMGM, PR# 891
      * GLF 990702 ADD 00 AS VALID CHECK ID
      * MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLV 031120 IF I-AM-WORLD, DON'T ENTER INS PRODUCER CODE HERE, ENTER
      *            IN BRANCH MAINTENANCE
      * BAH 051004 RENAMED GB-NEXT-GIFTNET-PO TO GB-NEXT-RECEIPT, WORLD
      *            PR# 436 MEXICO RECEIPT PRINT
      * BAH 140730 REMOVED MUST FILL FROM 32 AND 33
      * BAH 140911 FIXED PHONE ENTRY
      * BAH 180427 ADDED ERRLOG
      * KEC 180822 LOAN APP MODULE REMOVAL 
      *            ONLY PREVENTING ACCESSING FIELD #26.
      *  CS 190409 COMMENT #28-#29,REVOLVING LOAN/SALES BEG ACCTNO
      * BAH 20190625 REMOVED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL PT0153
      *  CS 190709 ADD  GB-FISCAL-EOY-TAG-CCYYMM, GB-FISCAL-EOY-TAG-CCYYMM.
      *     GB-EOM-TAG-CCYYMM WHICH ARE UPDATED AND CHECKED IN END OF MONTH
      *     END OF YEAR, AND OPEN DAYS
      * 190730 CS WAS A CHECK ON F1-CLEAR THAT DIDNT BELONG IN REC-GB-MODULE
      * 190801 CS REMOVED 40/41, I HAD ADDED THESE FOR THE EOM/EOY TAGS AND
      *           THEN MADE A NEW PAGE INSTEAD
      *
      *=================================================================
      * REV: *****FROM GB/GBMNA1 STARTING AS OF 2019-0918*****
      * MJD 031196 ADDED LOGIC TO STOP 0 AS ACCOUNT NUMBER.
      * GLF 102997 REMOVED RESET OF FIELDS
      * BAH 040298 REARRANGED TO MATCH GBMAIN
      * KEC 020899 ADDED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL.
      * BAH 990622 ADDED GB-INS-PRODUCER FOR REGMGM, PR# 891
      * MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLV 000523 DON'T DO SEQ-CHECK ON HOLIDAY TABLE - DOESN'T WORK
      *            & NOT IN GBMAIN
      * KEC 001120 {PR#00000} FINANCIAL MANAGEMENT [JUDY]
      *            ADDED GB-FEDERAL-TAXID AS PER BETH.
      * BLV 031120 IF I-AM-WORLD, DON'T ENTER INS PRODUCER CODE HERE, ENTER
      *            IN BRANCH MAINTENANCE
      * BAH 050811 ALLOW CHECK PRINT 0Z FOR WORLD MX PESOS
      * BAH 051004 RENAMED GB-NEXT-GIFTNET-PO TO GB-NEXT-RECEIPT, WORLD
      *            PR# 436 MEXICO RECEIPT PRINT
      * BAH 140911 FIXED PHONE ENTRY
      * BAH 180430 ADDED ERRLOG
      * KEC 180822 LOAN APP MODULE REMOVAL FROM A30 
      *             ONLY PREVENTING ACCESSING FIELD #26.
      * BAH 181030 GLOBAL GBFILE
      *  CS 190409 COMMENT #28/#29, REVOLVING BEGINNING ACCOUNT NUMBERS
      * BAH 20190625 REMOVED GB-NEXT-SF-REVOL GB-NEXT-LN-REVOL PT0153
      *  CS 20190709 ADED A PAGE2 FOR MONTH END RUN DATES/YEAR END 
      *               RUN DATES REPLACING THE TAG FILES
      *  CS 20190801 FIXED FOR MULTI PAGE 
      * BAH 190918 REMOVED ACCESS-CALL ON GBFILE
      *  CS 190923 ADDED CHECK ON STATE, THIS IS REPLACING GB/GBMAN1
      * BAH 20200519 REMOVED 25,26,27,28,29,34,35,38,39
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/GBMAN1 S35 02/01/22-13:01:48 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  DATE-SW         PIC XXX    VALUE "   ".
           88  BAD-DATE               VALUE "BAD".


      * USED TO VALIDATE MMDD FOR HOLIDAY TABLE AND ALSO FOR MMYY ACCRUAL
       01  WS-DATE         PIC 9(6).
       01  WS-DATE-R REDEFINES WS-DATE.
           03  FILLER      PIC XX.
           03  WS-MM       PIC 99.
           03  WS-DD       PIC 99.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD GBOR-REC
      *    CHANGED RECORD GBCH-REC
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01GB.CPY" REPLACING LEADING "GB" BY "GBOR".
       COPY "LIBGB/GB01GB.CPY" REPLACING LEADING "GB" BY "GBCH".

      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.
       01  GBMAIN-LINK.
           03  SAVEKEY.                        
               05  SAVE-BRNO        PIC 9(4).
               05  SAVE-NO          PIC 9.
           03  NEW-REC              PIC X.
           03  ERRCD                PIC X.
           03  FORMNUM              PIC 9.
           03  GBMAIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *    SCREEN DISPLAY BUFFER
      ******************************************************************
       01  DISPLAY-BUF                  VALUE SPACES.
           03  COMPANY-INFORMATION.
               05  F01                  PIC X(40).
               05  F02                  PIC X(25).
               05  F03                  PIC X(25).
               05  F04                  PIC X(15).
               05  F041                 PIC XX.
               05  F042                 PIC X(10).
               05  F05                  PIC X(12).

           03  HOLIDAY-SCHED.
               05  F06                  PIC 9999 BLANK ZERO.
               05  F07                  PIC 9999 BLANK ZERO.
               05  F08                  PIC 9999 BLANK ZERO.
               05  F09                  PIC 9999 BLANK ZERO.
               05  F10                  PIC 9999 BLANK ZERO.
               05  F11                  PIC 9999 BLANK ZERO.
               05  F12                  PIC 9999 BLANK ZERO.
               05  F13                  PIC 9999 BLANK ZERO.
               05  F14                  PIC 9999 BLANK ZERO.
               05  F15                  PIC 9999 BLANK ZERO.
               05  F16                  PIC 9999 BLANK ZERO.
               05  F17                  PIC 9999 BLANK ZERO.
               05  F18                  PIC 9999 BLANK ZERO.
               05  F19                  PIC 9999 BLANK ZERO.
               05  F20                  PIC 9999 BLANK ZERO.
               05  F21                  PIC 9999 BLANK ZERO.

           03  F22                      PIC Z(6).
           03  F23                      PIC Z(6).
           03  F24                      PIC Z(6).

           03  F30                      PIC Z9.
           03  F31                      PIC XX.
           03  F32                      PIC X.
           03  F33                      PIC X.
           03  F36                      PIC X(8).
           03  F37                      PIC 9(4).

       01  DISPLAY-LIST.
           03  FILLER                   PIC X(48) VALUE
           "010 020 030 040 041 042 050 060 070 080 090 100 ".
           03  FILLER                   PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER                   PIC X(40) VALUE
           "210 220 230 240 300 310 320 330 360 370.".


      ******************************************************************
      *    ELEMENT SPECIFICATIONS
      ******************************************************************
       01  MAX             PIC 99   VALUE 37.
       01  SPEC-TABLE.
      * 1. COMPANY NAME
           03  FILLER      PIC X(8) VALUE "E  A400 ".
      * 2. ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 3. ADDRESS 2
           03  FILLER      PIC X(8) VALUE "E  A250 ".
      * 4. CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 5. PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      *06-21. HOLIDAY SCHEDULE.
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
           03  FILLER           PIC X(8) VALUE "ENNN040 ".
      * 22. NEXT LOAN NUMBER (SALES FINANCE)
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 23. NEXT LOAN NUMBER (SMALL LOAN)
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 24. NEXT LOAN NUMBER (OTHER LOAN)
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 25. N/A
           03  FILLER      PIC X(8) VALUE "ENNN060N".
      * 26. N/A
           03  FILLER      PIC X(8) VALUE "ENNN060N".
      * 27. N/A
           03  FILLER      PIC X(8) VALUE "ENNN060N".
      * 28. N/A
           03  FILLER      PIC X(8) VALUE "ENNN060N".
      * 29. N/A
           03  FILLER      PIC X(8) VALUE "ENNN060N".

      *30. MAX LINES FOR REPORTS
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *31. CHECK PROGRAM ID
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      *32. OTHER BRANCHES POST
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *33. AUTOMATIC DEPOSIT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *34. N/A
           03  FILLER      PIC X(8) VALUE "E  A040N".
      *35. N/A
           03  FILLER      PIC X(8) VALUE "E  A070N".
      *36. UUCP HOME OFFICE MACHINE NAME
           03  FILLER      PIC X(8) VALUE "E  A080 ".
      *37. LAST ACCRUAL
           03  FILLER      PIC X(8) VALUE "ENNN040N".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 37 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GBMAN1_DEF.CPY".
       COPY "LIBGB/GBMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA        PIC X(2000).

       SCREEN SECTION.
       COPY "LIBGB/GBMAN1_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
              USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GBMAN1" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO GBMAIN-LINK.
          
           MOVE " " TO VDU-WIMENU-SELECTION.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-GB-MODULE.
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

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 2   TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
              MOVE GB-KEY TO SAVEKEY
              MOVE FORM-SEL TO FORMNUM
              GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ****************************************************************-
       REC-GB-MODULE SECTION.
      ****************************************************************-
       BEGIN-REC-GB.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO GB-BRNO GB-NO.
           IF SAVEKEY NOT = SPACES
              MOVE SAVEKEY TO GB-KEY
              MOVE SAVE-BRNO TO BR-NO
              MOVE SPACES TO SAVEKEY
              MOVE "S  A040BRNO." TO VDU
              MOVE BR-NO TO VDUBUF
              PERFORM SCREENIO 
              PERFORM LOAD-BRANCH-MODULE 
              IF BR-PATH = SPACES
                  MOVE "INVALID BRANCH" TO MESS
                  PERFORM SEND-MESS
                  GO TO ENTER-BRANCH 
              END-IF
              GO TO READ-GLOBAL.

           MOVE 0 TO NEW-FLAG.
       ENTER-BRANCH.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY 
              IF IO-TYPE = "CLR"
                 GO TO BEGIN-REC-GB            
              ELSE
                 MOVE BR-NO TO VDUNUM0.
               

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GB-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.


           IF VDUNUM0 = 0
              GO TO ENTER-BRANCH.     

           MOVE VDUNUM0 TO BR-NO.
           PERFORM LOAD-BRANCH-MODULE.
           IF BR-PATH = SPACES
              GO TO ENTER-BRANCH.
 
       READ-GLOBAL.
           MOVE 1 TO GB-NO.
           PERFORM OPEN-GB-FILE.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-GOOD
              MOVE GB-REC TO GBOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-GB-MODULE.


      ****************************************************************-
       LOAD-BRANCH-MODULE SECTION.
      ****************************************************************-
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
                MOVE SPACES                  TO BR-PATH
                GO TO LOAD-BRANCH-MODULE-EXIT.
 
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
 
            MOVE "B"                        TO GB-PATH-OVERRIDE.
 
       LOAD-BRANCH-MODULE-EXIT.
            EXIT.
 
      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

      *----------------------------------------------------------------
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

           MOVE "N" TO CHFG(37).

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

      D    STOP ACTION.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF ( FORM-SEL < 2 OR FORM-SEL > 2 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *----------------------------------------------------------------
       ENTER-ELE.

           IF ( ELE = 26 )
              MOVE 1 TO ONE
              GO TO CREATE-ROUTINE.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.

      *----------------------------------------------------------------
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *         S E T     R E C O R D     E L E M E N T S
      *****************************************************************
       F-01.
           MOVE VDUBUF TO GB-COMPANY-NAME.
           MOVE "S  A400CONAME." TO VDU.
           MOVE GB-COMPANY-NAME TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO GB-ADDR-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO GB-ADDR-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO GB-CITY.

      *-----------------------------------------------------------------
       F-041.

           MOVE "E  A020041." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-041.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-BAD
              GO TO F-041 
           ELSE
              MOVE VDUBUF TO GB-STATE.

      *-----------------------------------------------------------------
       F-042.

           MOVE "E  A100042." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO F-041.
           IF ( NOT ENTER-KEY ) GO TO F-042.

           MOVE VDUBUF TO GB-ZIP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           PERFORM ENTER-PHONE.

           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.

           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.

           MOVE TEL-BUF TO GB-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120050." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (01).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (02).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (03).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (04).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (05).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (06).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (07).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (08).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (09).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (11).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (12).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (13).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (14).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (15).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           PERFORM DATE-CHECK-MMDD.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDU-NUM TO GB-HOLIDAY (16).
           GO TO CREATE-ROUTINE.

       F-22.
           MOVE VDUNUM0 TO GB-NEXT-SF.
           IF GB-NEXT-SF = 0
              MOVE "NEXT ACCOUNT NO; MINIMUM = 1, MAXIMUM = 999999"
               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUNUM0 TO GB-NEXT-SMALL.
           IF GB-NEXT-SMALL = 0
              MOVE "NEXT ACCOUNT NO; MINIMUM = 1, MAXIMUM = 999999"
               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUNUM0 TO GB-NEXT-OTHER.
           IF GB-NEXT-OTHER = 0
              MOVE "NEXT ACCOUNT NO; MINIMUM = 1, MAXIMUM = 999999"
               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           IF VDUNUM0 < 20 OR VDUNUM0 > 90
              MOVE "AMOUNT OF LINES; MINIMUM = 20, MAXIMUM = 90"
               TO MESS PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO GB-RPT-LINES.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO GB-CHECK-PROG.

           IF VDUBUF NOT = "M2"
                 AND NOT = "73" AND NOT = "2M"
                 AND NOT = "3T" AND NOT = "2Y"
                 AND NOT = "6J" AND NOT = "6M"
                 AND NOT = "72" AND NOT = "8H"
                 AND NOT = "6T" AND NOT = "00"
                 AND NOT = "0A" AND NOT = "01"
                 AND NOT = "0Z"
              MOVE "INVALID CHECK PROGRAM, SEE HELP" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE VDUBUF TO GB-OTHER-BRANCH-POST.

           IF VDUBUF NOT = "Y" AND NOT = "N"
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO GB-AUTO-DEPOSIT.

           IF VDUBUF NOT = "Y" AND NOT = "N"
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-35.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-36.
           MOVE VDUBUF TO GB-UUCP-HOME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-37.
           PERFORM DATE-CHECK-MMYY.

           IF BAD-DATE
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO GB-LAST-ACCRUE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
      *
      *     LAST ACCRUAL DATE - VALIDATE MMYY 
      *
      ******************************************************************
       DATE-CHECK-MMYY SECTION.
           MOVE "   " TO DATE-SW.
           MOVE VDU-NUM TO WS-DATE.

           IF WS-MM = 0 AND WS-DD = 0
              GO TO DATE-CHECK-MMYY-EXIT.

           IF WS-MM < 1 OR WS-MM > 12
              MOVE "BAD MONTH" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

       DATE-CHECK-MMYY-EXIT.
           EXIT.

      ******************************************************************
      *
      *     HOLIDAY TABLE - VALIDATE MMDD DATES FOR YEAR OF HOLIDAYS
      *
      ******************************************************************
       DATE-CHECK-MMDD SECTION.

           MOVE SPACES TO DATE-SW.
           MOVE VDU-NUM TO WS-DATE.

           IF WS-MM = 0 AND WS-DD = 0
              GO TO DATE-CHECK-MMDD-EXIT.

           IF WS-MM < 1 OR WS-MM > 12
              MOVE "BAD MONTH" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW
              GO TO DATE-CHECK-MMDD-EXIT.

           IF WS-MM =  1 OR WS-MM =  3 OR WS-MM =  5 OR
              WS-MM =  7 OR WS-MM =  8 OR WS-MM = 10 OR
              WS-MM = 12
                  PERFORM DAY-CHK-31
                  GO TO DATE-CHECK-MMDD-EXIT.

           IF WS-MM =  4 OR WS-MM =  6 OR WS-MM =  9 OR
              WS-MM = 11
                  PERFORM DAY-CHK-30
                  GO TO DATE-CHECK-MMDD-EXIT.

           IF WS-MM =  2
                  PERFORM DAY-CHK-29
                  GO TO DATE-CHECK-MMDD-EXIT.

      *----------------------------------------------------------------
       DAY-CHK-31.
           IF WS-DD < 1 OR WS-DD > 31
              MOVE "BAD DAY" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

      *----------------------------------------------------------------
       DAY-CHK-30.
           IF WS-DD < 1 OR WS-DD > 30
              MOVE "BAD DAY" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

      *----------------------------------------------------------------
       DAY-CHK-29.
           IF WS-DD < 1 OR WS-DD > 29
              MOVE "BAD DAY" TO MESS
              PERFORM SEND-MESS
              MOVE "BAD" TO DATE-SW.

       DATE-CHECK-MMDD-EXIT.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GB-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE GB-REC TO GBCH-REC.

           IF GBOR-REC = GBCH-REC
              GO TO END-WRITE-MODULE.

           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF GB-REC NOT = GBOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE GBCH-REC TO GB-REC.
           PERFORM REWRITE-GB-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-GB-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED 
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-GB-FILE.

      *********************************************
       DELETE-MODULE SECTION.
      *********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-GB-FILE.
           PERFORM READ-GB-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF GB-REC NOT = GBOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-GB-FILE.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING GB-BRNO, " ", "DELETED" DELIMITED BY SIZE
               INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-GB-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE GB-BRNO                    TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF GBOR-COMPANY-NAME NOT = GBCH-COMPANY-NAME
              MOVE "01 "                   TO LMC-FIELD
              MOVE "COMP NAME"             TO LMC-DESC
              MOVE GBOR-COMPANY-NAME       TO LMC-ORG-TEXT
              MOVE GBCH-COMPANY-NAME       TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-ADDR-1 NOT = GBCH-ADDR-1
              MOVE "02 "                   TO LMC-FIELD
              MOVE "ADDRESS1"              TO LMC-DESC
              MOVE GBOR-ADDR-1             TO LMC-ORG-TEXT
              MOVE GBCH-ADDR-1             TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-ADDR-2 NOT = GBCH-ADDR-2
              MOVE "03 "                   TO LMC-FIELD
              MOVE "ADDRESS2"              TO LMC-DESC
              MOVE GBOR-ADDR-2             TO LMC-ORG-TEXT
              MOVE GBCH-ADDR-2             TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-CITY NOT = GBCH-CITY
              MOVE "04 "                 TO LMC-FIELD
              MOVE "CITY"                TO LMC-DESC
              MOVE GBOR-CITY             TO LMC-ORG-TEXT
              MOVE GBCH-CITY             TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-STATE NOT = GBCH-STATE
              MOVE "04 "                  TO LMC-FIELD
              MOVE "STATE"                TO LMC-DESC
              MOVE GBOR-STATE             TO LMC-ORG-TEXT
              MOVE GBCH-STATE             TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-ZIP NOT = GBCH-ZIP
              MOVE "04 "                TO LMC-FIELD
              MOVE "ZIP"                TO LMC-DESC
              MOVE GBOR-ZIP             TO LMC-ORG-TEXT
              MOVE GBCH-ZIP             TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-PHONE NOT = GBCH-PHONE
              MOVE "05 "                  TO LMC-FIELD
              MOVE "PHONE"                TO LMC-DESC
              MOVE GBOR-PHONE             TO LMC-ORG-PHONE
              MOVE GBCH-PHONE             TO LMC-CHG-PHONE
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(1) NOT = GBCH-HOLIDAY(1)
              MOVE "06 "                  TO LMC-FIELD
              MOVE "HOLIDAY1"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(1)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(1)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(2) NOT = GBCH-HOLIDAY(2)
              MOVE "07 "                  TO LMC-FIELD
              MOVE "HOLIDAY2"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(2)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(2)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(3) NOT = GBCH-HOLIDAY(3)
              MOVE "08 "                  TO LMC-FIELD
              MOVE "HOLIDAY3"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(3)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(3)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(4) NOT = GBCH-HOLIDAY(4)
              MOVE "09 "                  TO LMC-FIELD
              MOVE "HOLIDAY4"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(4)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(4)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(5) NOT = GBCH-HOLIDAY(5)
              MOVE "10 "                  TO LMC-FIELD
              MOVE "HOLIDAY5"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(5)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(5)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(6) NOT = GBCH-HOLIDAY(6)
              MOVE "11 "                  TO LMC-FIELD
              MOVE "HOLIDAY6"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(6)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(6)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(7) NOT = GBCH-HOLIDAY(7)
              MOVE "12 "                  TO LMC-FIELD
              MOVE "HOLIDAY7"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(7)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(7)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(8) NOT = GBCH-HOLIDAY(8)
              MOVE "13 "                  TO LMC-FIELD
              MOVE "HOLIDAY8"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(8)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(8)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(9) NOT = GBCH-HOLIDAY(9)
              MOVE "14 "                  TO LMC-FIELD
              MOVE "HOLIDAY9"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(9)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(9)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(10) NOT = GBCH-HOLIDAY(10)
              MOVE "15 "                  TO LMC-FIELD
              MOVE "HOLIDAY10"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(10)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(10)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(11) NOT = GBCH-HOLIDAY(11)
              MOVE "16 "                  TO LMC-FIELD
              MOVE "HOLIDAY11"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(11)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(11)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(12) NOT = GBCH-HOLIDAY(12)
              MOVE "17 "                  TO LMC-FIELD
              MOVE "HOLIDAY12"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(12)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(12)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(13) NOT = GBCH-HOLIDAY(13)
              MOVE "18 "                  TO LMC-FIELD
              MOVE "HOLIDAY13"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(13)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(13)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(14) NOT = GBCH-HOLIDAY(14)
              MOVE "19 "                  TO LMC-FIELD
              MOVE "HOLIDAY14"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(14)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(14)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(15) NOT = GBCH-HOLIDAY(15)
              MOVE "20 "                  TO LMC-FIELD
              MOVE "HOLIDAY15"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(15)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(15)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-HOLIDAY(16) NOT = GBCH-HOLIDAY(16)
              MOVE "21 "                   TO LMC-FIELD
              MOVE "HOLIDAY16"             TO LMC-DESC
              MOVE GBOR-HOLIDAY(16)        TO LMC-ORG-NUM
              MOVE GBCH-HOLIDAY(16)        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-NEXT-SF NOT = GBCH-NEXT-SF
              MOVE "22 "               TO LMC-FIELD
              MOVE "NEXT SF#"          TO LMC-DESC
              MOVE GBOR-NEXT-SF        TO LMC-ORG-NUM
              MOVE GBCH-NEXT-SF        TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-NEXT-SMALL NOT = GBCH-NEXT-SMALL
              MOVE "23 "               TO LMC-FIELD
              MOVE "NEXT SM#"          TO LMC-DESC
              MOVE GBOR-NEXT-SMALL     TO LMC-ORG-NUM
              MOVE GBCH-NEXT-SMALL     TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-NEXT-OTHER NOT = GBCH-NEXT-OTHER
              MOVE "24 "               TO LMC-FIELD
              MOVE "NEXT OT#"          TO LMC-DESC
              MOVE GBOR-NEXT-OTHER     TO LMC-ORG-NUM
              MOVE GBCH-NEXT-OTHER     TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-RPT-LINES NOT = GBCH-RPT-LINES
              MOVE "30 "               TO LMC-FIELD
              MOVE "RPT LINES"         TO LMC-DESC
              MOVE GBOR-RPT-LINES      TO LMC-ORG-NUM
              MOVE GBCH-RPT-LINES      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-CHECK-PROG NOT = GBCH-CHECK-PROG
              MOVE "31 "               TO LMC-FIELD
              MOVE "CHK PROG"         TO LMC-DESC
              MOVE GBOR-CHECK-PROG     TO LMC-ORG-TEXT
              MOVE GBCH-CHECK-PROG     TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-OTHER-BRANCH-POST NOT = GBCH-OTHER-BRANCH-POST
              MOVE "32 "                   TO LMC-FIELD
              MOVE "OTH BPOST"             TO LMC-DESC
              MOVE GBOR-OTHER-BRANCH-POST  TO LMC-ORG-TEXT
              MOVE GBCH-OTHER-BRANCH-POST  TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-AUTO-DEPOSIT NOT = GBCH-AUTO-DEPOSIT
              MOVE "33 "              TO LMC-FIELD
              MOVE "AUTO DEP"         TO LMC-DESC
              MOVE GBOR-AUTO-DEPOSIT  TO LMC-ORG-TEXT
              MOVE GBCH-AUTO-DEPOSIT  TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-UUCP-HOME NOT = GBCH-UUCP-HOME
              MOVE "36 "              TO LMC-FIELD
              MOVE "UUCP HOME"        TO LMC-DESC
              MOVE GBOR-UUCP-HOME  TO LMC-ORG-TEXT
              MOVE GBCH-UUCP-HOME  TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GBOR-LAST-ACCRUE NOT = GBCH-LAST-ACCRUE
              MOVE "37 "              TO LMC-FIELD
              MOVE "LAST ACCR"        TO LMC-DESC
              MOVE GBOR-LAST-ACCRUE  TO LMC-ORG-NUM
              MOVE GBCH-LAST-ACCRUE  TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.


      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-GB-FILE.

           MOVE BR-NO  TO GB-BRNO.
           MOVE 1      TO GB-NO.

           MOVE 1 TO GB-NEXT-SF GB-NEXT-SMALL GB-NEXT-OTHER.
           MOVE ZEROS TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES                TO DISPLAY-BUF.
           MOVE GB-COMPANY-NAME       TO F01.
           MOVE GB-ADDR-1             TO F02.
           MOVE GB-ADDR-2             TO F03.
           MOVE GB-CITY               TO F04.
           MOVE GB-STATE              TO F041.
           MOVE GB-ZIP                TO F042.

           MOVE GB-PHONE              TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT              TO F05.

           MOVE GB-HOLIDAY (01)       TO F06.
           MOVE GB-HOLIDAY (02)       TO F07.
           MOVE GB-HOLIDAY (03)       TO F08.
           MOVE GB-HOLIDAY (04)       TO F09.
           MOVE GB-HOLIDAY (05)       TO F10.
           MOVE GB-HOLIDAY (06)       TO F11.
           MOVE GB-HOLIDAY (07)       TO F12.
           MOVE GB-HOLIDAY (08)       TO F13.
           MOVE GB-HOLIDAY (09)       TO F14.
           MOVE GB-HOLIDAY (10)       TO F15.
           MOVE GB-HOLIDAY (11)       TO F16.
           MOVE GB-HOLIDAY (12)       TO F17.
           MOVE GB-HOLIDAY (13)       TO F18.
           MOVE GB-HOLIDAY (14)       TO F19.
           MOVE GB-HOLIDAY (15)       TO F20.
           MOVE GB-HOLIDAY (16)       TO F21.

           MOVE GB-NEXT-SF            TO F22.
           MOVE GB-NEXT-SMALL         TO F23.
           MOVE GB-NEXT-OTHER         TO F24.

           MOVE GB-RPT-LINES          TO F30.
           MOVE GB-CHECK-PROG         TO F31.
           MOVE GB-OTHER-BRANCH-POST  TO F32.
           MOVE GB-AUTO-DEPOSIT       TO F33.
           MOVE GB-UUCP-HOME          TO F36.
           MOVE GB-LAST-ACCRUE        TO F37.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE SPACES TO IO-TYPE.
           MOVE " ENTER BRANCH # OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-BR-FILE.
           MOVE 1 TO BR-NO.
           PERFORM START-BR-FILE.

       ENTER-KEY.
           MOVE "EN N040SCAN." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           IF F6-KEY
              GO TO READ-FILES-NEXT.

           IF NOT ENTER-KEY
              GO TO ENTER-KEY.

           IF VDUNUM0 = 0
              GO TO CLOSE-SCAN.

           MOVE VDUNUM0 TO BR-NO.
           PERFORM START-BR-FILE.

       READ-FILES-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N040BRNO." TO VDU.
           MOVE BR-NO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO GB-PATH-OVERRIDE.

           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-GOOD
              PERFORM DISPLAY-FIELDS
           ELSE
              MOVE SPACES TO DISPLAY-BUF
              MOVE "NO GLOBAL FOR THIS BRANCH!!" TO F01
              MOVE DISPLAY-LIST TO WR-LIST
              MOVE DISPLAY-BUF TO WR-BUF
              PERFORM CALL-WRFORM.

           GO TO ENTER-KEY.

       CLOSE-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM CLOSE-BR-FILE.

       END-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GBGBCL.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO GBMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GBMAN1-SYSDATE.
      * WITHOUT THIS LINE BELOW, CLEAR BACK TO TOP OF SCREEN DOES NOT WORK
      * GO TO MAIN-MODULE WONT CLEAR THE SCREEN
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GBMAN1-SCREEN UPON GBMAIN-WINDOW-HANDLE.
           MOVE GBMAN1-FORM-FIELDS TO WR-BUF.
           MOVE GBMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GBMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE GBMAN1-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GBMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                     TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GBMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GBMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

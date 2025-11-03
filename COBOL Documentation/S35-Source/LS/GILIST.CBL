       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GILIST.
       DATE-WRITTEN.  01/01/88.
      ******************************************************************
      *
      * IDENTIFICATION:  LS/GILIST.C
      *
      * DESCRIPTION   :  GENERAL LEDGER CONTROL FILE LIST
      *
      * REV:
      * BAH 020189 REARRANGED
      * SLC 092589 ADDED GI-ACR-INT-AR-AR
      * BAH 111991 WAS NOT PRINTING 27 ON PAGE 2
      * BAH 062193 CHANGED EXTENSION TO ADDON
      * BAH 120193 CHANGED TO MATCH SCREENS
      * JFF 102095 ADDED CLASS SCAN
      * JTG 111095 ADDED PRINT OF GI-ACR-INT-AR
      * MJD 042696 ADDED GP-FILE GP-DLR-DISP-TYPE.
      * MJD 111396 ADDED GI-CONV-OFFSET TO PAGE 1.
      * GLF 052097 CHANGED PG3 PAYABLES TO UNEARNED INCOME
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * JTG 043098 ADDED GI-PARTIAL-PLEXP (PARTIAL CHARGEOFF)
      * BAH 062998 RENAMED GI-EREXTCHG TO GI-ERADDON AND CHANGED
      *                    GI-UNEXTCHG TO GI-UNADDON
      * KEC 000530 {PR#00000} PARADATA FINANCIAL [JIM ROSENAUER];
      *             ADDED PAGE 6.
      * BLV 112500 FIXED CHECK ON CLASS RANGE, SAID INVALID CLASS
                   "IF BEG-CLASS > END-BRANCH", FIXED TO END-CLASS
      * GLB 020507 ADDED GI-ACH-BANK; REGACC PR#1797
      * MG  020930 CHANGED THE LISTING TO MATCH THE MAINTENANCE
      * JTG 040517 ADDED GL CONTROL FIELDS FOR:
      *                - UNEARNED INTEREST REPO
      *                - UNEARNED ADDON INTEREST REPO
      *            THE UNEARNED WILL MOVE TO THESE ACCOUNTS POSTING A 'SX'
      *            AND REVERSING A 'SV'
      *            THE UNEARNED WILL MOVE (BACK) FROM THESE ACCOUNTS POSTING
      *            A 'SV' AND REVERSING A 'SX'                    REGACC #0049
      * BAH 20190701 REMOVED REVOLVING FIELDS, ALL OF PAGE 6
      ************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GILIST.CBL S34 06/10/21-12:44:12 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME                  PIC X(01)     VALUE "Y".
       01  MESS-LIST                   PIC X(05)     VALUE "MESS.".

       01  LN-COUNT                PIC 9(02)     VALUE 3.
       01  LN-FEED                 PIC 9(02)     VALUE 0.
       01  PAGE-NUMBER             PIC 9(04)     VALUE 0.

       01  RECORDS-FOUND               PIC X(01)     VALUE SPACES.
           88  NO-RECORDS-FOUND                      VALUE "N".

      ******************************************************************
      *
      *    S C R E E N - F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF                  VALUE SPACES.
           03  ERRCD                   PIC X.
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  BEG-CLASS               PIC 9(02).
           03  END-CLASS               PIC 9(02).


      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE                  PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H-HR                    PIC 9(02)     VALUE 0.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H-MIN                   PIC 9(02)     VALUE 0.
           03  FILLER                  PIC X(06)     VALUE SPACES.
           03  H-NAME                  PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(10)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  FILLER                  PIC X(39)     VALUE
                              "- GENERAL LEDGER CONTROL FILE LISTING -".

       
       01  HEAD3.
           03  H-TAG                   PIC X(10).
           03  H-COLON                 PIC X(01).
           03  FILLER                  PIC X(01)     VALUE SPACES.
           03  H-CODE                  PIC X(4).
           03  FILLER                  PIC X(01)     VALUE SPACES.
           03  H-DESC                  PIC X(20).

       
       01  HEAD4.
           03  H-PG1                   PIC X(09).

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(80)     VALUE SPACES.

       
       01  D-LINE                      REDEFINES DETAIL-LINE.
           03  D-DESC-A                PIC X(21).
           03  FILLER                  PIC X(01).
           03  D-ACCT-A                PIC XXXXBX(7).
           03  FILLER                  PIC X(10).
           03  D-DESC-B                PIC X(23).
           03  FILLER                  PIC X(01).
           03  D-ACCT-B                PIC XXXXBX(7).

       
       01  RANGE-LINE                  REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(30).
           03  RANGE-SELECTION.
               05  R-BRANCH            PIC X(04).
               05  FILLER              PIC X(02).
               05  R-CLASS             PIC X(04).
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  GILIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GILIST_DEF.CPY".
       COPY "LIBLS/GILIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLS/GILIST_SCN.CPY".

       
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GILIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GILIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"              
              MOVE "PROGRAM NOT BATCHABLE" TO MESS 
              PERFORM SEND-MESS  
              MOVE "99" TO EXT-ROUTE-BUF 
              GO TO EXIT-PROG.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PRIMARY-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".

           PERFORM GET-GPENV.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO BR-NO LC-CODE.
           PERFORM OPEN-PR-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO ERRCD.

           MOVE GILIST-QUEUE-POS TO Q-POS.
           MOVE GILIST-COPIES-POS TO C-POS.
           MOVE GILIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF ( STAT-BAD )
              MOVE "X" TO ERRCD
              GO TO ENTER-PARAMETERS-EXIT.

           MOVE SPACES TO ERRCD.

       
       ENTER-BEG-BRANCH.

           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUNUM0 TO BEG-BRANCH.

       ENTER-END-BRANCH.

           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

       
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-BEG-CLASS.

       
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BR1." TO VDU.
           MOVE ZEROES        TO VDUNUM0 BEG-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO VDUNUM0 END-BRANCH.
           PERFORM SCREENIO.

       
       ENTER-BEG-CLASS.

           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.
       
       ENTER-END-CLASS.

           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

       
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-ALL-CLASS.

           MOVE "SNNN020LC1." TO VDU.
           MOVE ZEROES        TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020LC2." TO VDU.
           MOVE 99            TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

       
       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

       
           IF ( VDUBUF = "N" ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-END.

           GO TO ENTER-ACCEPT-YN.

       
       ENTER-PARAMETERS-END.

           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F7-KEY ) MOVE "X" TO ERRCD.

       
       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GI1-FILE.
           MOVE BEG-BRANCH TO GI-BRANCH
                              QGI1-WBEG-BRANCH.
           MOVE END-BRANCH TO QGI1-WEND-BRANCH.
           MOVE BEG-CLASS  TO GI-CLASS
                              QGI1-WBEG-CLASS.
           MOVE END-CLASS  TO QGI1-WEND-CLASS.

           PERFORM START-GI1-FILE.

       READ-NEXT-GI-RECORD.

           PERFORM READ-GI1-FILE-NEXT.
           IF ( IO-BAD                 ) OR
              ( GI-BRANCH > END-BRANCH )
              GO TO PRIMARY-MODULE-END.

           IF ( GI-CLASS < BEG-CLASS ) OR
              ( GI-CLASS > END-CLASS )
              GO TO READ-NEXT-GI-RECORD.

           PERFORM PRINT-GI-RECORD.

           GO TO READ-NEXT-GI-RECORD.

       
       PRIMARY-MODULE-END.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO RECORDS-FOUND
              GO TO PRIMARY-MODULE-EXIT.

           PERFORM PRINT-RANGE-LINES.

       PRIMARY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-GI-RECORD SECTION.
      ******************************************************************

           PERFORM HEAD-PAGE.
           PERFORM PRINT-BR.
           PERFORM PRINT-LC.
       
           MOVE "PAGE 1" TO H-PG1.
           MOVE 2        TO LN-FEED.
           PERFORM PRINT-HEAD4.

           MOVE "BANK AND CASH:       "    TO D-DESC-A.
           MOVE "OFFSETS:               "  TO D-DESC-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. CASH ON HAND    :"    TO D-DESC-A.
           MOVE GI-CASH                    TO D-ACCT-A.
           MOVE " 15. P&L OFFSET       :"  TO D-DESC-B.
           MOVE GI-PLOFFSET(1)             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. CASH IN BANK    :"    TO D-DESC-A.
           MOVE GI-BANK                    TO D-ACCT-A.
           MOVE " 16. SERV. ACC OFFSET :"  TO D-DESC-B.
           MOVE GI-SERVOFF                 TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. NSF             :"    TO D-DESC-A.
           MOVE GI-NSF                     TO D-ACCT-A.
           MOVE " 17. CONVERSION OFFSET:"  TO D-DESC-B.
           MOVE GI-CONV-OFFSET             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECEIVABLES:         "    TO D-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. A/R             :"    TO D-DESC-A.
           MOVE GI-AR                      TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 5. REPO A/R        :"    TO D-DESC-A.
           MOVE GI-REPO-AR                 TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. P & L           :"    TO D-DESC-A.
           MOVE GI-PL(1)                   TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. DEALER INS. REB :"    TO D-DESC-A.
           MOVE GI-DLINSREB                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. IB ACCRUE INT AR:"    TO D-DESC-A.
           MOVE GI-ACR-INT-AR-AR           TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTER BRANCH:        "    TO D-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 9. OTH BRANCH RECV :"    TO D-DESC-A.
           MOVE GI-OTHBR-REC               TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. OTH BRANCH PAYBL:"    TO D-DESC-A.
           MOVE GI-OTHBR-PAY               TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXPENSES:            "    TO D-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. P & L EXPENSE   :"    TO D-DESC-A.
           MOVE GI-PLEXP(1)                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. DL PAR. RSV EXP.:"    TO D-DESC-A.
           MOVE GI-DLPARVEX                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "13. PARTIAL CHGOFF  :"    TO D-DESC-A.
           MOVE GI-PARTIAL-PLEXP           TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "14. REPO ADDON EXP  :"    TO D-DESC-A.
           MOVE GI-REPO-ADDON-EXP          TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

      * PAGE 2

           MOVE "PAGE 2" TO H-PG1.
           MOVE 3        TO LN-FEED.
           PERFORM PRINT-HEAD4.

           MOVE "PAYABLES:            "    TO D-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. CREDIT LIFE DECL:"    TO D-DESC-A.
           MOVE GI-PAYCL1                  TO D-ACCT-A.
           MOVE " 16. RESERVE CHGS 1   :"  TO D-DESC-B.
           MOVE GI-DLRVCHGS-1              TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. CREDIT LIFE LEVL:"    TO D-DESC-A.
           MOVE GI-PAYCL2                  TO D-ACCT-A.
           MOVE " 17. RESERVE CHGS 2   :"  TO D-DESC-B.
           MOVE GI-DLRVCHGS-2              TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. ACCIDENT/HEALTH :"    TO D-DESC-A.
           MOVE GI-PAYAH                   TO D-ACCT-A.
           MOVE " 18. RESERVE CHGS 3   :"  TO D-DESC-B.
           MOVE GI-DLRVCHGS-3              TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. PERS PROP INS   :"    TO D-DESC-A.
           MOVE GI-PAYPP                   TO D-ACCT-A.
           MOVE " 19. PARTICIP. RESRV  :"  TO D-DESC-B.
           MOVE GI-DLPARV                  TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 5. OTHER INS #1    :"    TO D-DESC-A.
           MOVE GI-PAYO1                   TO D-ACCT-A.
           MOVE " 20. ALLOTMENT PAYBL  :"  TO D-DESC-B.
           MOVE GI-PAYALLOT                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. OTHER INS #2    :"    TO D-DESC-A.
           MOVE GI-PAYO2                   TO D-ACCT-A.
           MOVE " 21. DIRECT PAYABLE   :"  TO D-DESC-B.
           MOVE GI-DIRECT-PAY              TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. OTHER INS #3    :"    TO D-DESC-A.
           MOVE GI-PAYO3                   TO D-ACCT-A.
           MOVE " 22. LOCKBOX PAYABLE  :"  TO D-DESC-B.
           MOVE GI-PAYLBOX                 TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 23. TPAY    PAYABLE  :"  TO D-DESC-B.
           MOVE GI-PAYTPAY                 TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 24. TCOLL   PAYABLE  :"  TO D-DESC-B.
           MOVE GI-PAYTCOLL                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 25. MGRAM   PAYABLE  :"  TO D-DESC-B.
           MOVE GI-PAYMGRAM                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. DEALER PAYABLE  :"    TO D-DESC-A.
           MOVE GI-DLPAYABLE               TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "13. RESERVE PRIN 1  :"    TO D-DESC-A.
           MOVE GI-DLRVPRIN-1              TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "14. RESERVE PRIN 2  :"    TO D-DESC-A.
           MOVE GI-DLRVPRIN-2              TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "15. RESERVE PRIN 3  :"    TO D-DESC-A.
           MOVE GI-DLRVPRIN-3              TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

       
      * PAGE 3

           PERFORM HEAD-PAGE.
           PERFORM PRINT-BR.
           PERFORM PRINT-LC.
       
           MOVE "PAGE 3" TO H-PG1.
           MOVE 2        TO LN-FEED.
           PERFORM PRINT-HEAD4.

           MOVE "UNEARNED INCOME:     "    TO D-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. INTEREST CHARGE :"    TO D-DESC-A.
           MOVE GI-UNINTCHG                TO D-ACCT-A.
           MOVE " 16. INTEREST REPO CHG:"  TO D-DESC-B.
           MOVE GI-UNINTREPO               TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. ADDON INT CHARGE:"    TO D-DESC-A.
           MOVE GI-UNADDON                 TO D-ACCT-A.
           MOVE " 17. ADDON INT REPO   :"  TO D-DESC-B.
           MOVE GI-UNADDONINTREPO          TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. SERVICE CHARGE  :"    TO D-DESC-A.
           MOVE GI-UNSERCHG                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. MAINTENANCE FEE :"    TO D-DESC-A.
           MOVE GI-UNMAINTFEE              TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE " 5. DEALER COMM     :" TO D-DESC-A
           ELSE
              MOVE " 5. DEALER DISCOUNT :" TO D-DESC-A.
           MOVE GI-UNDLDISC                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. OTHER           :"    TO D-DESC-A.
           MOVE GI-UNOTHER                 TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. CREDIT LIFE DECL:"    TO D-DESC-A.
           MOVE GI-UNCL1                   TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. CREDIT LIFE LEVL:"    TO D-DESC-A.
           MOVE GI-UNCL2                   TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 9. ACCIDENT/HEALTH :"    TO D-DESC-A.
           MOVE GI-UNAH                    TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. PRES PROP INS   :"    TO D-DESC-A.
           MOVE GI-UNPP                    TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. OTHER INS #1    :"    TO D-DESC-A.
           MOVE GI-UNO1                    TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. OTHER INS #2    :"    TO D-DESC-A.
           MOVE GI-UNO2                    TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "13. OTHER INS #3    :"    TO D-DESC-A.
           MOVE GI-UNO3                    TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

       
      * PAGE 4

           MOVE "PAGE 4" TO H-PG1.
           MOVE 3        TO LN-FEED.
           PERFORM PRINT-HEAD4.

           MOVE "INCOME:              "    TO D-DESC-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. INTEREST   ANTIC:"    TO D-DESC-A.
           MOVE GI-ERINTCHG                TO D-ACCT-A.
           MOVE " 16. PERS PROP INS    :"  TO D-DESC-B.
           MOVE GI-PP                      TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. ADDON F/C  ANTIC:"    TO D-DESC-A.
           MOVE GI-ERADDON                 TO D-ACCT-A.
           MOVE " 17. OTHER INS #1     :"  TO D-DESC-B.
           MOVE GI-O1                      TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. SER CHG    ANTIC:"    TO D-DESC-A.
           MOVE GI-ERSERCHG                TO D-ACCT-A.
           MOVE " 18. OTHER INS #2     :"  TO D-DESC-B.
           MOVE GI-O2                      TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. MAINT FEES ANTIC:"    TO D-DESC-A.
           MOVE GI-ERMAINTFEE              TO D-ACCT-A.
           MOVE " 19. OTHER INS #3     :"  TO D-DESC-B.
           MOVE GI-O3                      TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE " 5. DL COMM         :" TO D-DESC-A
           ELSE
              MOVE " 5. DL DISC         :" TO D-DESC-A.
           MOVE GI-ERDLDISC                TO D-ACCT-A.
           MOVE " 20. PENALTY INTEREST :"  TO D-DESC-B.
           MOVE GI-PENALTY-INT             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. OTHER      ANTIC:"    TO D-DESC-A.
           MOVE GI-EROTHER                 TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. LATE CHARGES    :"    TO D-DESC-A.
           MOVE GI-LATECHG                 TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. INTEREST (I/B)  :"    TO D-DESC-A.
           MOVE GI-INTEREST                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 9. DEFERMENT INCOME:"    TO D-DESC-A.
           MOVE GI-DEFERMENTS              TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. SERV ACCT COMMIS:"    TO D-DESC-A.
           MOVE GI-SAPRINCIPAL             TO D-ACCT-A.
           MOVE " 24. REPO INTEREST    :"  TO D-DESC-B.
           MOVE GI-REPO-INT                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. P&L INTEREST    :"    TO D-DESC-A.
           MOVE GI-PLINT(1)                TO D-ACCT-A.
           MOVE " 25. ACCRUED IB INT   :"  TO D-DESC-B.
           MOVE GI-ACR-INT-AR              TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. P&L RECOVERED   :"    TO D-DESC-A.
           MOVE GI-PLREC(1)                TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "13. CREDIT LIFE DECL:"    TO D-DESC-A.
           MOVE GI-CL1                     TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "14. CREDIT LIFE LEVL:"    TO D-DESC-A.
           MOVE GI-CL2                     TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "15. ACCIDENT/HEALTH :"    TO D-DESC-A.
           MOVE GI-AH                      TO D-ACCT-A.
           PERFORM WRITE-DETAIL-LINE.

      * PAGE 5

           PERFORM HEAD-PAGE.
           PERFORM PRINT-BR.
           PERFORM PRINT-LC.
       
           MOVE "PAGE 5" TO H-PG1.
           MOVE 2        TO LN-FEED.
           PERFORM PRINT-HEAD4.

           MOVE "RECEIVABLES:         "    TO D-DESC-A.
           MOVE "INCOME:               "   TO D-DESC-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 1. PL #2 CONTROL   :"    TO D-DESC-A.
           MOVE GI-PL(2)                   TO D-ACCT-A.
           MOVE " 13. PL #2 INT/LC     :"  TO D-DESC-B.
           MOVE GI-PLINT(2)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 2. PL #3 CONTROL   :"    TO D-DESC-A.
           MOVE GI-PL(3)                   TO D-ACCT-A.
           MOVE " 14. PL #3 INT/LC     :"  TO D-DESC-B.
           MOVE GI-PLINT(3)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 3. PL #4 CONTROL   :"    TO D-DESC-A.
           MOVE GI-PL(4)                   TO D-ACCT-A.
           MOVE " 15. PL #4 INT/LC     :"  TO D-DESC-B.
           MOVE GI-PLINT(4)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 4. PL #5 CONTROL   :"    TO D-DESC-A.
           MOVE GI-PL(5)                   TO D-ACCT-A.
           MOVE " 16. PL #5 INT/LC     :"  TO D-DESC-B.
           MOVE GI-PLINT(5)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OFFSETS:             "    TO D-DESC-A.
           MOVE "RECOVERIES:            "  TO D-DESC-B.
           MOVE 2                          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1                          TO LN-FEED.

           MOVE " 5. PL #2 OFFSET    :"    TO D-DESC-A.
           MOVE GI-PLOFFSET(2)             TO D-ACCT-A.
           MOVE " 17. PL #2 RECOVERED  :"  TO D-DESC-B.
           MOVE GI-PLREC(2)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 6. PL #3 OFFSET    :"    TO D-DESC-A.
           MOVE GI-PLOFFSET(3)             TO D-ACCT-A.
           MOVE " 18. PL #3 RECOVERED  :"  TO D-DESC-B.
           MOVE GI-PLREC(3)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 7. PL #4 OFFSET    :"    TO D-DESC-A.
           MOVE GI-PLOFFSET(4)             TO D-ACCT-A.
           MOVE " 19. PL #4 RECOVERED  :"  TO D-DESC-B.
           MOVE GI-PLREC(4)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " 8. PL #5 OFFSET    :"    TO D-DESC-A.
           MOVE GI-PLOFFSET(5)             TO D-ACCT-A.
           MOVE " 20. PL #5 RECOVERED  :"  TO D-DESC-B.
           MOVE GI-PLREC(5)                TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXPENSES:            "    TO D-DESC-A.
           MOVE "EXPENSE NEW AR:        "  TO D-DESC-B.
           MOVE 2                          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1                          TO LN-FEED.

           MOVE " 9. PL #2 EXPENSE   :"    TO D-DESC-A.
           MOVE GI-PLEXP(2)                TO D-ACCT-A.
           MOVE " 21. PL #2 NEW AR/EXP :"  TO D-DESC-B.
           MOVE GI-PLNEW-AR(2)             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "10. PL #3 EXPENSE   :"    TO D-DESC-A.
           MOVE GI-PLEXP(3)                TO D-ACCT-A.
           MOVE " 22. PL #3 NEW AR/EXP :"  TO D-DESC-B.
           MOVE GI-PLNEW-AR(3)             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "11. PL #4 EXPENSE   :"    TO D-DESC-A.
           MOVE GI-PLEXP(4)                TO D-ACCT-A.
           MOVE " 23. DLCOMM GAP (PP)  :"  TO D-DESC-B.
           MOVE GI-PLNEW-AR(4)             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "12. PL #5 EXPENSE   :"    TO D-DESC-A.
           MOVE GI-PLEXP(5)                TO D-ACCT-A.
           MOVE " 24. DLCOMM WARRANTY  :"  TO D-DESC-B.
           MOVE GI-PLNEW-AR(5)             TO D-ACCT-B.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GI-RECORD-EXIT.
           EXIT.

      ******************************************************************
       PRINT-BR SECTION.
      ******************************************************************

           MOVE SPACES       TO HEAD3.
           MOVE 2            TO LN-FEED.
           MOVE "BRANCH NO " TO H-TAG.
           MOVE ":"          TO H-COLON.
           MOVE GI-BRANCH    TO H-CODE.

           IF ( GI-BRANCH NOT = BR-NO )
              MOVE GI-BRANCH TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF ( IO-BAD )
                 MOVE "INVALID" TO BR-NAME.

           MOVE BR-NAME TO H-DESC.
           MOVE HEAD3   TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-BR-EXIT.
           EXIT.

      ******************************************************************
       PRINT-LC SECTION.
      ******************************************************************

           MOVE SPACES       TO HEAD3.
           MOVE "LOAN CLASS" TO H-TAG.
           MOVE ":"          TO H-COLON.
           MOVE GI-CLASS     TO H-CODE.

           IF ( GI-CLASS NOT = LC-CODE )
              PERFORM OPEN-LC1-FILE
              MOVE GI-CLASS TO LC-CODE
              PERFORM READ-LC1-FILE
              PERFORM CLOSE-LC1-FILE
              IF ( IO-BAD )
                 MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-DESC.
           MOVE HEAD3   TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-LC-EXIT.
           EXIT.

      ******************************************************************
       PRINT-HEAD4 SECTION.
      ******************************************************************

           MOVE HEAD4 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO HEAD4.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-HEAD4-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           MOVE "N"         TO FIRST-TIME.
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

       
           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE 2 TO LN-FEED.
           MOVE 1 TO LN-COUNT.
       
       HEAD-PAGE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 1 TO LN-FEED.

       
           MOVE "BEGINNING BRANCH CODE, CLASS" TO RANGE-LINE.
           MOVE BEG-BRANCH                     TO R-BRANCH.
           MOVE BEG-CLASS                      TO R-CLASS.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "ENDING BRANCH CODE, CLASS" TO RANGE-LINE.
           MOVE END-BRANCH                  TO R-BRANCH.
           MOVE END-CLASS                   TO R-CLASS.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO GILIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GILIST-SYSDATE.
           DISPLAY GILIST-SCREEN UPON GILIST-WINDOW-HANDLE.
           MOVE GILIST-FORM-FIELDS TO WR-BUF.
           MOVE GILIST-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GILIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GILIST-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GILIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       
      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.

           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GILIST-SCREEN.
           DESTROY GILIST-WINDOW-HANDLE.
           EXIT PROGRAM.
